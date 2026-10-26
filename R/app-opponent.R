# Background computation for the app: the computer opponent and position
# analysis.
#
# Neither runs on the reactive thread. The opponent uses the package's
# durable job mechanism (`g3_start_job(list(type = "search", ...))`, a
# separate R process); the analysis panel runs `g3_analyse_position()` in a
# `callr::r_bg()` process. The app polls both with `invalidateLater()`.
#
# Every request is tagged with the game id, the session revision, the state
# hash and the position key it was computed for. A finished opponent
# decision is offered to the dispatcher only if all four still match; the
# dispatcher then checks legality again through g3_game_apply(). Undo, new
# game, load, cancellation and leaving computer mode make the tag stale, so
# a late answer can never become a move in a later position.

# Opponent agents offered by the app. Labels are i18n keys.
.G3_APP_AGENTS <- c(search = "agent.search", greedy = "agent.greedy", random = "agent.random")
.G3_APP_BUDGETS <- c(quick = 20000L, standard = 100000L, deeper = 300000L)
.G3_OPPONENT_WALL_SECONDS <- 120

#' @noRd
.g3_opponent_new <- function() {
  opp <- new.env(parent = emptyenv())
  opp$job <- NULL
  opp$tag <- NULL
  opp$status <- "idle"
  opp$message <- NA_character_
  opp$for_revision <- NA_integer_
  opp$started <- NULL
  opp$draining <- list()
  opp$log <- character()
  opp
}

# Tag identifying the position a computation was asked for.
#' @noRd
.g3_compute_tag <- function(store) {
  st <- store$game$state
  list(
    game_id = store$game$metadata$game_id,
    revision = store$revision,
    state_hash = .g3_state_hash(st),
    position_key = st$repetition$keys[length(st$repetition$keys)]
  )
}

# Is a result computed for `tag` still about the store's current position?
#' @noRd
.g3_tag_current <- function(tag, store) {
  if (is.null(tag)) return(FALSE)
  cur <- .g3_compute_tag(store)
  identical(tag$game_id, cur$game_id) && identical(as.integer(tag$revision), cur$revision) &&
    identical(tag$state_hash, cur$state_hash) && identical(tag$position_key, cur$position_key)
}

# The computer's settings stored in a game's metadata.
#' @noRd
.g3_opponent_settings <- function(game) {
  if (!identical(game$metadata$mode, "computer")) return(NULL)
  o <- game$metadata$opponent
  ok_agent <- isTRUE(o$agent %in% names(.G3_APP_AGENTS)) ||
    (is.character(o$agent) && length(o$agent) == 1L && startsWith(o$agent, "model:"))
  if (!is.list(o) || !ok_agent || !isTRUE(o$color %in% .G3_COLORS)) {
    return(NULL)
  }
  o$nodes <- as.integer(o$nodes %||% .G3_APP_BUDGETS[["standard"]])
  o
}

# Should the computer act now?
#' @noRd
.g3_computer_to_act <- function(game) {
  o <- .g3_opponent_settings(game)
  !is.null(o) && is.na(game$metadata$result) && !identical(game$state$outcome$status, "terminal") &&
    identical(game$state$side, o$color)
}

# Deterministic seed for sampling agents, so a random-baseline game can be
# reproduced from its record: derived from the game id and ply.
#' @noRd
.g3_opponent_seed <- function(game) {
  h <- strtoi(substr(.g3_sha256(game$metadata$game_id), 1L, 6L), 16L)
  as.integer((h + game$state$ply * 7919L) %% 2147483647L)
}

#' @noRd
.g3_opponent_start <- function(opp, store, jobs_dir) {
  o <- .g3_opponent_settings(store$game)
  tag <- .g3_compute_tag(store)
  opp$for_revision <- store$revision
  dir.create(jobs_dir, recursive = TRUE, showWarnings = FALSE)
  budget <- list(nodes = o$nodes)
  agent <- o$agent
  if (startsWith(agent, "model:")) {
    # Resolve the model here, so the job carries absolute artifact paths and
    # does not depend on this session's options; the job process finds the
    # ML environment through G3_ML_ENV or the default location.
    agent <- tryCatch(.g3_model_resolve(agent), error = function(e) e)
    if (inherits(agent, "error")) {
      opp$status <- "failed"
      opp$message <- conditionMessage(agent)
      return(invisible(opp))
    }
    budget <- list(mode = o$mode %||% "policy", simulations = as.integer(o$simulations %||% 32L))
  }
  # Warnings here (an unwritable folder) precede an error; the error message
  # is what the user sees.
  job <- suppressWarnings(tryCatch(
    g3_start_job(list(
      type = "search", label = "opponent",
      args = list(state = store$game$state, agent = agent, budget = budget,
                  seed = .g3_opponent_seed(store$game)),
      limits = list(wall_seconds = .G3_OPPONENT_WALL_SECONDS),
      job_dir = jobs_dir
    )),
    error = function(e) e
  ))
  if (inherits(job, "error")) {
    opp$status <- "failed"
    opp$message <- conditionMessage(job)
    return(invisible(opp))
  }
  opp$job <- job
  opp$tag <- tag
  opp$status <- "thinking"
  opp$message <- NA_character_
  opp$started <- Sys.time()
  invisible(opp)
}

# Stop the current computation without waiting; the process is drained in
# the background (cooperative stop first, termination after 5 seconds).
#' @noRd
.g3_opponent_cancel <- function(opp, status = "cancelled") {
  if (!is.null(opp$job)) {
    try(g3_cancel_job(opp$job, wait = FALSE), silent = TRUE)
    opp$draining[[length(opp$draining) + 1L]] <- list(job = opp$job, since = Sys.time())
    opp$log <- c(opp$log, paste(.g3_now(), "cancelled", opp$job$job_id))
  }
  opp$job <- NULL
  opp$tag <- NULL
  opp$status <- status
  opp$started <- NULL
  invisible(opp)
}

#' @noRd
.g3_drain_jobs <- function(opp, force = FALSE) {
  keep <- list()
  for (d in opp$draining) {
    alive <- isTRUE(tryCatch(d$job$process$is_alive(), error = function(e) FALSE))
    if (alive && (force || difftime(Sys.time(), d$since, units = "secs") > 5)) {
      try(d$job$process$kill_tree(), silent = TRUE)
      alive <- FALSE
    }
    if (alive) {
      keep[[length(keep) + 1L]] <- d
    } else {
      unlink(d$job$dir, recursive = TRUE)
    }
  }
  opp$draining <- keep
  invisible(length(keep))
}

# Poll the running job. Returns NULL while it runs, otherwise a list
# `status` ("decision", "failed", "stale") with `decision`/`message`.
#' @noRd
.g3_opponent_poll <- function(opp, store) {
  job <- opp$job
  if (is.null(job)) return(NULL)
  st <- tryCatch(g3_job_status(job), error = function(e) list(status = "failed", error = conditionMessage(e)))
  if (st$status %in% c("queued", "running")) return(NULL)
  out <- if (identical(st$status, "succeeded")) {
    d <- tryCatch(g3_job_result(job), error = function(e) NULL)
    if (!inherits(d, "g3_decision") || is.na(d$action_id)) {
      list(status = "failed", message = "The opponent returned no action.", key = "opponent.no_action")
    } else if (!.g3_tag_current(opp$tag, store)) {
      list(status = "stale", decision = d)
    } else {
      list(status = "decision", decision = d, tag = opp$tag, job_id = job$job_id)
    }
  } else {
    list(status = "failed", message = paste0(st$status, if (!is.na(st$error %||% NA)) paste0(": ", st$error) else ""),
         job_status = st$status, stop_reason = st$stop_reason %||% NA_character_, error = st$error %||% NA_character_)
  }
  opp$log <- c(opp$log, paste(.g3_now(), out$status, job$job_id))
  unlink(job$dir, recursive = TRUE)
  opp$job <- NULL
  opp$started <- NULL
  out
}

# Why the opponent produced no move, in the UI language: the job's state
# and stop reason in words, plus the error when it says more.
#' @noRd
.g3_opponent_failure_text <- function(res, lang = "en") {
  if (!is.null(res$key)) return(.g3_t(res$key, lang))
  if (is.null(res$job_status)) return(res$message %||% "")
  reason <- res$stop_reason
  err <- res$error
  paste0(.g3_t(paste0("job.status.", res$job_status), lang),
         if (length(reason) == 1L && !is.na(reason)) paste0(" (", .g3_stop_reason_text(reason, lang), ")"),
         if (length(err) == 1L && !is.na(err) && nzchar(err) && !identical(err, reason)) paste0(": ", err))
}

# Request for the dispatcher built from a current opponent decision.
#' @noRd
.g3_opponent_request <- function(res) {
  list(kind = "action", game_id = res$tag$game_id, revision = res$tag$revision,
       request_id = paste0("cpu-", res$job_id), expected_state_hash = res$tag$state_hash,
       action_id = res$decision$action_id)
}

# ------------------------------------------------------- analysis task

# Run g3_analyse_position() in a background R process.
#' @noRd
.g3_analysis_start <- function(state, nodes) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    .g3_abort("g3_io_error", "Background analysis needs the 'callr' package.")
  }
  loader <- .g3_child_loader()
  callr::r_bg(
    function(state, nodes, loader) {
      if (!is.null(loader$path)) {
        pkgload::load_all(loader$path, quiet = TRUE, export_all = FALSE, helpers = FALSE)
      } else {
        loadNamespace("gambitR3D")
      }
      get("g3_analyse_position", envir = asNamespace("gambitR3D"))(state, budget = list(nodes = nodes))
    },
    args = list(state = state, nodes = as.integer(nodes), loader = loader),
    supervise = TRUE, package = FALSE
  )
}
