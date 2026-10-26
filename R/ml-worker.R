# Client for the persistent Python tensor worker (protocol 1.x).
#
# The worker is started with processx from the configured environment's
# interpreter in isolated mode (`python -I -B .../__main__.py`), with an
# argument vector, never a shell string. stdin/stdout carry newline-
# delimited JSON; the worker's stderr goes to a log file and is never
# parsed. One request is in flight at a time. While waiting, R reads
# heartbeats, progress and the terminal message (result, error or
# cancelled) and checks:
#
#   * every line parses as a JSON object no larger than 8 MiB, otherwise
#     the worker is killed (malformed or oversized output);
#   * the echoed request id, job/attempt ids, operation and hashes equal
#     what was sent, otherwise the worker is killed;
#   * messages for an unknown or already finished request are rejected
#     (counted as stale or duplicate, never used);
#   * silence longer than the stale threshold (15 s) kills the worker;
#   * a stop request sends `cancel`, waits 5 s, then kills the tree.
#
# Inference results are validated again by the caller (shape, finiteness,
# candidate digest) before probabilities are formed in R.

.G3_PROTOCOL_VERSION <- "1.0"
.G3_MAX_LINE_BYTES <- 8L * 1024L * 1024L
.G3_HEARTBEAT_S <- 2
.G3_STALE_S <- 15
.G3_CANCEL_GRACE_S <- 5
.G3_WORKER_ECHO <- c("job_id", "attempt_id", "operation", "state_hash", "rules_hash", "feature_schema_hash",
                     "action_schema_hash", "model_hash", "budget", "deadline")

#' @noRd
.g3_worker_start <- function(env_path, threads = 1L, allowed_roots = character(), fault_injection = FALSE,
                             log_path = NULL, stale_s = .G3_STALE_S, heartbeat_s = .G3_HEARTBEAT_S,
                             startup_timeout = 180, job_id = NULL, attempt_id = NULL) {
  .g3_need_processx()
  py <- .g3_venv_python(env_path)
  if (!file.exists(py)) .g3_abort("g3_incompatible_model", paste0("No Python interpreter in the ML environment ", env_path))
  threads <- as.integer(threads)
  if (is.na(threads) || threads < 1L) .g3_abort("g3_budget_exceeded", "threads must be at least 1.")
  log_path <- log_path %||% tempfile("g3-worker-", fileext = ".log")
  roots <- unique(normalizePath(allowed_roots[nzchar(allowed_roots)], mustWork = FALSE))
  args <- c("-I", "-B", .g3_worker_main(), "--threads", threads, "--max-line-bytes", .G3_MAX_LINE_BYTES,
            "--heartbeat", heartbeat_s, as.vector(rbind(rep("--allowed-root", length(roots)), roots)),
            if (fault_injection) "--enable-fault-injection")
  env <- c("current", OMP_NUM_THREADS = threads, MKL_NUM_THREADS = threads, OPENBLAS_NUM_THREADS = threads,
           PYTHONHASHSEED = "0", CUDA_VISIBLE_DEVICES = "", PYTHONDONTWRITEBYTECODE = "1")
  proc <- processx::process$new(py, as.character(args), stdin = "|", stdout = "|", stderr = log_path,
                                cleanup = TRUE, cleanup_tree = TRUE, supervise = TRUE, env = env)
  w <- new.env(parent = emptyenv())
  w$proc <- proc
  w$pid <- proc$get_pid()
  w$buffer <- ""
  w$inbox <- list()
  w$seq <- 0L
  w$log <- log_path
  w$env_path <- env_path
  w$threads <- threads
  w$allowed <- roots
  w$stale_s <- stale_s
  w$completed <- character()
  w$stats <- list(stale = 0L, duplicate = 0L, heartbeats = 0L, requests = 0L)
  w$dead <- FALSE
  w$job_id <- job_id
  w$attempt_id <- attempt_id %||% paste0("session-", Sys.getpid())
  w$model_hash <- NULL
  w$checkpoint <- NULL
  w$started <- .g3_now()
  class(w) <- "g3_ml_worker"
  hello <- tryCatch(
    .g3_worker_call(w, "hello", list(client = "gambitR3D", client_feature_schema_hash = .g3_feature_schema()$hash),
                    timeout_s = startup_timeout, stale_s = startup_timeout),
    error = function(e) {
      .g3_worker_kill(w)
      stop(e)
    }
  )
  major <- suppressWarnings(as.integer(strsplit(as.character(hello$protocol_version %||% ""), ".", fixed = TRUE)[[1L]][1L]))
  if (is.na(major) || major != 1L) {
    .g3_worker_kill(w)
    .g3_abort("g3_incompatible_model", paste0("The worker speaks protocol ", hello$protocol_version, "; 1.x is required."))
  }
  if (!identical(hello$feature_schema_hash, .g3_feature_schema()$hash)) {
    .g3_worker_kill(w)
    .g3_abort("g3_incompatible_model", "The worker's feature schema differs from this package's (stale worker).")
  }
  w$hello <- hello
  w
}

#' @export
print.g3_ml_worker <- function(x, ...) {
  cat("<g3_ml_worker> pid ", x$pid, if (isTRUE(x$dead)) " (stopped)" else "", ", torch ",
      x$hello$torch %||% "?", ", ", x$threads, " thread(s)\n", sep = "")
  invisible(x)
}

#' @noRd
.g3_worker_alive <- function(w) {
  !is.null(w) && !isTRUE(w$dead) && w$proc$is_alive()
}

#' @noRd
.g3_worker_kill <- function(w) {
  if (!is.null(w$proc) && w$proc$is_alive()) {
    try(w$proc$kill_tree(), silent = TRUE)
    try(w$proc$wait(3000), silent = TRUE)
  }
  w$dead <- TRUE
  invisible(TRUE)
}

#' @noRd
.g3_worker_stop <- function(w, grace_s = .G3_CANCEL_GRACE_S) {
  if (is.null(w)) return(invisible(FALSE))
  if (.g3_worker_alive(w)) {
    try(.g3_worker_call(w, "shutdown", timeout_s = grace_s), silent = TRUE)
    try(w$proc$wait(grace_s * 1000), silent = TRUE)
  }
  .g3_worker_kill(w)
}

#' @noRd
.g3_worker_abort <- function(w, class, message, ...) {
  .g3_worker_kill(w)
  .g3_abort(class, paste0(message, " The worker was stopped; its log is ", w$log, "."), worker_pid = w$pid, ...)
}

# Read complete lines from the worker, parsing each as a JSON object.
#' @noRd
.g3_worker_read <- function(w, timeout_ms = 200L) {
  w$proc$poll_io(as.integer(timeout_ms))
  chunk <- tryCatch(w$proc$read_output(), error = function(e) "")
  if (length(chunk) && nzchar(chunk)) w$buffer <- paste0(w$buffer, chunk)
  out <- list()
  repeat {
    pos <- regexpr("\n", w$buffer, fixed = TRUE)
    if (pos < 0L) break
    line <- substr(w$buffer, 1L, pos - 1L)
    w$buffer <- substr(w$buffer, pos + 1L, nchar(w$buffer))
    if (nchar(line, type = "bytes") > .G3_MAX_LINE_BYTES) {
      .g3_worker_abort(w, "g3_io_error", "The worker sent an oversized line.")
    }
    if (!nzchar(trimws(line))) next
    m <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(m) || is.null(names(m))) {
      .g3_worker_abort(w, "g3_io_error", "The worker sent a malformed protocol line.", line_start = substr(line, 1L, 80L))
    }
    out[[length(out) + 1L]] <- m
  }
  if (nchar(w$buffer, type = "bytes") > .G3_MAX_LINE_BYTES) {
    .g3_worker_abort(w, "g3_io_error", "The worker sent an oversized line.")
  }
  out
}

#' @noRd
.g3_worker_send <- function(w, env) {
  line <- as.character(jsonlite::toJSON(env, auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  if (nchar(line, type = "bytes") > .G3_MAX_LINE_BYTES) {
    .g3_abort("g3_budget_exceeded", "The request exceeds the 8 MiB protocol line limit; use smaller batches.")
  }
  # write_input() may return an unwritten remainder; keep writing it.
  left <- w$proc$write_input(paste0(line, "\n"), sep = "")
  t0 <- proc.time()[["elapsed"]]
  while (length(left)) {
    if (!w$proc$is_alive() || proc.time()[["elapsed"]] - t0 > 60) {
      .g3_worker_abort(w, "g3_io_error", "Could not write the request to the worker.")
    }
    Sys.sleep(0.001)
    left <- w$proc$write_input(left, sep = "")
  }
  invisible(line)
}

# Send one request and wait for its terminal message. Returns the result
# payload; a cancelled request returns its payload with attribute
# `cancelled = TRUE`. Worker errors become typed g3 conditions.
#' @noRd
.g3_worker_call <- function(w, operation, payload = NULL, fields = list(), timeout_s = 120,
                            on_progress = NULL, should_stop = NULL, stale_s = NULL) {
  stale_s <- stale_s %||% w$stale_s
  if (!.g3_worker_alive(w)) {
    w$dead <- TRUE
    .g3_abort("g3_io_error", "The ML worker is not running.")
  }
  w$seq <- w$seq + 1L
  w$stats$requests <- w$stats$requests + 1L
  rid <- sprintf("req-%d-%s", w$seq, substr(.g3_sha256(paste(Sys.time(), w$pid, w$seq, stats::runif(1))), 1L, 10L))
  if (is.null(payload) || !length(payload)) payload <- stats::setNames(list(), character())
  env <- list(
    protocol_version = .G3_PROTOCOL_VERSION, message_type = "request", request_id = rid,
    job_id = fields$job_id %||% w$job_id, attempt_id = fields$attempt_id %||% w$attempt_id,
    operation = operation, payload = payload,
    state_hash = fields$state_hash, rules_hash = .g3_tables()$rules_hash,
    feature_schema_hash = .g3_feature_schema()$hash, action_schema_hash = .g3_action_schema_hash(),
    model_hash = fields$model_hash, budget = fields$budget, deadline = fields$deadline
  )
  sent <- jsonlite::fromJSON(.g3_worker_send(w, env), simplifyVector = FALSE)
  t0 <- proc.time()[["elapsed"]]
  last_seen <- t0
  cancel_at <- NULL
  cancel_rid <- NULL
  timed_out <- FALSE
  repeat {
    # Messages read but not yet handled stay queued on the worker object,
    # so nothing that arrives after a terminal reply is silently lost.
    w$inbox <- c(w$inbox, if (length(w$inbox)) .g3_worker_read(w, 0L) else .g3_worker_read(w, 100L))
    now <- proc.time()[["elapsed"]]
    while (length(w$inbox)) {
      m <- w$inbox[[1L]]
      w$inbox <- w$inbox[-1L]
      last_seen <- now
      mt <- m$message_type %||% ""
      pv <- as.character(m$protocol_version %||% "")
      if (!startsWith(pv, "1.") && !identical(pv, "1")) {
        .g3_worker_abort(w, "g3_io_error", paste0("The worker sent protocol version '", pv, "'."))
      }
      if (!mt %in% c("accepted", "progress", "heartbeat", "result", "error", "cancelled")) {
        .g3_worker_abort(w, "g3_io_error", paste0("Unknown message type '", mt, "'."))
      }
      if (mt == "heartbeat") {
        w$stats$heartbeats <- w$stats$heartbeats + 1L
        next
      }
      mid <- m$request_id
      if (is.null(mid)) {
        # A worker-level rejection (malformed or oversized request line):
        # it can only concern the request in flight.
        if (mt == "error") {
          .g3_abort(.g3_worker_error_class(m$payload$code), paste0("The worker rejected the request (", m$payload$code, "): ",
                                                                    m$payload$message), code = m$payload$code)
        }
        next
      }
      if (!is.null(cancel_rid) && identical(mid, cancel_rid)) next
      if (!identical(mid, rid)) {
        if (mid %in% w$completed) w$stats$duplicate <- w$stats$duplicate + 1L else w$stats$stale <- w$stats$stale + 1L
        next
      }
      for (f in .G3_WORKER_ECHO) {
        if (!identical(m[[f]], sent[[f]])) {
          .g3_worker_abort(w, "g3_io_error", paste0("The worker's reply does not echo '", f, "' exactly."))
        }
      }
      if (mt == "accepted") next
      if (mt == "progress") {
        if (is.function(on_progress)) on_progress(m$payload)
        next
      }
      w$completed <- c(utils::tail(w$completed, 200L), rid)
      if (mt == "result") return(m$payload)
      if (mt == "cancelled") return(structure(m$payload, cancelled = TRUE, timed_out = timed_out))
      code <- m$payload$code %||% "unknown"
      .g3_abort(.g3_worker_error_class(code), paste0("Worker error (", code, "): ", m$payload$message %||% ""), code = code)
    }
    if (!w$proc$is_alive()) {
      st <- w$proc$get_exit_status()
      .g3_worker_abort(w, "g3_io_error", paste0("The worker exited unexpectedly (status ", st %||% NA, ")."))
    }
    if (now - last_seen > stale_s) {
      .g3_worker_abort(w, "g3_io_error", paste0("The worker sent nothing for ", round(now - last_seen, 1), " s (stale)."))
    }
    want_stop <- is.null(cancel_at) && (
      (now - t0 > timeout_s) || (is.function(should_stop) && isTRUE(should_stop()))
    )
    if (want_stop) {
      timed_out <- now - t0 > timeout_s
      w$seq <- w$seq + 1L
      cancel_rid <- paste0(rid, "-cancel")
      cenv <- env
      cenv$request_id <- cancel_rid
      cenv$operation <- "cancel"
      cenv$payload <- list(target_request_id = rid)
      .g3_worker_send(w, cenv)
      cancel_at <- now
    }
    if (!is.null(cancel_at) && now - cancel_at > .G3_CANCEL_GRACE_S) {
      .g3_worker_kill(w)
      return(structure(list(stop_reason = "terminated_after_grace"), cancelled = TRUE, killed = TRUE,
                       timed_out = timed_out))
    }
  }
}

#' @noRd
.g3_worker_error_class <- function(code) {
  if (is.null(code)) return("g3_io_error")
  if (code %in% c("hash_mismatch", "digest_mismatch", "unsupported_protocol", "model_not_initialized",
                  "optimizer_config_mismatch")) return("g3_incompatible_model")
  if (code %in% c("deadline_exceeded")) return("g3_budget_exceeded")
  "g3_io_error"
}

# Run inference for a list of encodings. Validates every output against
# the request and forms probabilities in R. Returns one list per position:
# action_ids, logits, probability, wdl (named), value, expected_score,
# aux_heuristic.
#' @noRd
.g3_ml_infer <- function(w, encs, model_hash = NULL, state_hash = NULL) {
  if (!length(encs)) return(list())
  if (any(vapply(encs, function(e) !length(e$action_ids), TRUE))) {
    .g3_abort("g3_invalid_state", "A position without legal actions is terminal; it is valued by the rules, not the network.")
  }
  res <- .g3_worker_call(w, "infer_batch", list(positions = unname(lapply(encs, .g3_enc_payload))),
                         fields = list(model_hash = model_hash, state_hash = state_hash), timeout_s = 120)
  outs <- res$outputs
  if (length(outs) != length(encs)) .g3_worker_abort(w, "g3_io_error", "The worker returned the wrong number of outputs.")
  lapply(seq_along(encs), function(i) .g3_ml_check_output(w, outs[[i]], encs[[i]]))
}

#' @noRd
.g3_ml_check_output <- function(w, o, enc) {
  n <- length(enc$action_ids)
  num <- function(x) {
    if (!is.list(x) && !is.numeric(x)) return(NULL)
    if (any(vapply(x, function(v) !is.numeric(v) || length(v) != 1L, TRUE))) return(NULL)
    as.numeric(unlist(x))
  }
  lg <- num(o$logits)
  wl <- num(o$wdl_logits)
  if (!identical(o$candidate_digest, enc$candidate_digest)) {
    .g3_worker_abort(w, "g3_incompatible_model", "Candidate digest mismatch in the worker's reply.")
  }
  if (is.null(lg) || length(lg) != n || !identical(as.integer(o$n_candidates), n)) {
    .g3_worker_abort(w, "g3_io_error", "The worker returned the wrong number of candidate logits.")
  }
  if (is.null(wl) || length(wl) != 3L) .g3_worker_abort(w, "g3_io_error", "The worker returned malformed WDL logits.")
  if (any(!is.finite(lg)) || any(!is.finite(wl))) .g3_worker_abort(w, "g3_io_error", "The worker returned non-finite values.")
  # Probabilities over the complete legal set, computed in R.
  p <- exp(lg - max(lg))
  p <- p / sum(p)
  q <- exp(wl - max(wl))
  q <- q / sum(q)
  wdl <- stats::setNames(q, c("win", "draw", "loss"))
  list(
    action_ids = enc$action_ids, logits = lg, probability = p, wdl = wdl,
    value = unname(wdl[["win"]] - wdl[["loss"]]),
    expected_score = unname(wdl[["win"]] + 0.5 * wdl[["draw"]]),
    aux_heuristic = as.numeric(o$aux_heuristic %||% NA_real_)
  )
}

# Perspective flip of a WDL vector: swap win and loss, keep draw.
#' @noRd
.g3_wdl_flip <- function(wdl) stats::setNames(unname(wdl[c(3L, 2L, 1L)]), c("win", "draw", "loss"))

# The session's persistent inference worker, reused across decisions. It
# is (re)started when missing, dead, or when a checkpoint outside its
# allowed roots is needed.
#' @noRd
.g3_ml_session_worker <- function(roots = character(), env_path = NULL) {
  env_path <- .g3_ml_env_path(env_path)
  w <- .g3_env$ml_worker
  roots <- normalizePath(roots, mustWork = FALSE)
  ok <- .g3_worker_alive(w) && identical(w$env_path, env_path) &&
    all(vapply(roots, function(r) any(r == w$allowed | startsWith(r, paste0(w$allowed, "/"))), TRUE))
  if (ok) return(w)
  if (!is.null(w)) .g3_worker_stop(w)
  keep <- if (!is.null(w)) w$allowed else character()
  w <- .g3_worker_start(env_path, threads = 1L, allowed_roots = unique(c(keep, roots)))
  .g3_env$ml_worker <- w
  w
}

# Stop the session worker (used by tests and before unloading).
#' @noRd
.g3_ml_shutdown <- function() {
  w <- .g3_env$ml_worker
  .g3_env$ml_worker <- NULL
  if (!is.null(w)) .g3_worker_stop(w)
  invisible(TRUE)
}
