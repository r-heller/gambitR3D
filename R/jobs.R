# Durable local jobs.
#
# A job is a directory holding `job.json` (the immutable specification),
# `status.json` (written atomically by the worker), `cancel.flag` (a
# cooperative stop request), `log.txt` (the worker's stdout/stderr, never
# parsed as data) and the job's artifacts. The worker is a separate R
# process started with callr, so long work never blocks the caller or a
# Shiny session. States:
#
#   queued -> running -> succeeded | cancelled | timed_out | failed
#   blocked      a prerequisite is missing (e.g. no ML runtime)
#   interrupted  the process died while the status still said running
#
# Resuming creates a new attempt of the same logical job. Job types map to
# package functions `function(args, ctx)`; `ctx` offers progress(),
# should_stop(), checkpoint paths and the job directory. Only these
# registered types can run: a job file cannot name arbitrary code.

.G3_JOB_SCHEMA <- "g3-job/1"
.G3_JOB_TYPES <- c(
  search = ".g3_job_search",
  analysis = ".g3_job_analysis",
  match = ".g3_job_match",
  self_play = ".g3_job_self_play",
  training = ".g3_job_training",
  evaluation = ".g3_job_evaluation",
  export = ".g3_job_export",
  coach = ".g3_job_coach"
)
.G3_JOB_TERMINAL <- c("succeeded", "cancelled", "timed_out", "failed", "blocked", "interrupted")

#' @noRd
.g3_jobs_root <- function() {
  file.path(tools::R_user_dir("gambitR3D", which = "data"), "jobs")
}

#' Start a background job
#'
#' Starts a registered job type in a separate R process and returns
#' immediately. Use [g3_job_status()] to follow it, [g3_cancel_job()] to
#' stop it and [g3_resume_job()] to continue an interrupted or cancelled
#' resumable job.
#'
#' @param spec A list with `type` (one of `"search"`, `"analysis"`,
#'   `"match"`, `"self_play"`, `"training"`, `"evaluation"`, `"export"`, `"coach"`),
#'   `args` (a list passed to the job function), and optional `limits`
#'   (`wall_seconds`, default 600), `seed`, `job_dir` (default under
#'   `tools::R_user_dir("gambitR3D", "data")`) and `label`.
#' @return A `g3_job` handle: `job_id`, `attempt`, `dir`, `type` and the
#'   process handle (valid only in the starting session).
#' @examples
#' \dontrun{
#' st <- gambitR3D::g3_new_game()$state
#' job <- gambitR3D::g3_start_job(base::list(
#'   type = "search",
#'   args = base::list(state = st, budget = base::list(nodes = 300L))
#' ))
#' gambitR3D::g3_job_status(job)$status
#' }
#' @export
g3_start_job <- function(spec) {
  if (!is.list(spec) || is.null(spec$type) || !spec$type %in% names(.G3_JOB_TYPES)) {
    .g3_abort("g3_io_error", paste0("Unknown job type. Use one of: ", paste(names(.G3_JOB_TYPES), collapse = ", "), "."))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    .g3_abort("g3_io_error", "Background jobs need the 'callr' package: install.packages('callr').")
  }
  limits <- utils::modifyList(list(wall_seconds = 600), spec$limits %||% list())
  if (!is.numeric(limits$wall_seconds) || limits$wall_seconds <= 0) {
    .g3_abort("g3_budget_exceeded", "limits$wall_seconds must be positive.")
  }
  root <- spec$job_dir %||% .g3_jobs_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  job_id <- .g3_new_id("job")
  dir <- file.path(root, job_id)
  dir.create(dir)
  args <- spec$args %||% list()
  saveRDS(args, file.path(dir, "args.rds"))
  job <- list(
    schema = .G3_JOB_SCHEMA,
    job_id = job_id,
    type = spec$type,
    label = spec$label %||% spec$type,
    created = .g3_now(),
    seed = spec$seed %||% NA,
    limits = limits,
    config_hash = digest::digest(args, algo = "sha256"),
    rules_hash = .g3_tables()$rules_hash,
    package_version = as.character(utils::packageVersion("gambitR3D"))
  )
  .g3_atomic_write(.g3_canonical_json(job), file.path(dir, "job.json"))
  .g3_launch_attempt(dir, attempt = 1L, resume = FALSE)
}

#' @noRd
.g3_launch_attempt <- function(dir, attempt, resume) {
  job <- jsonlite::fromJSON(file.path(dir, "job.json"))
  unlink(file.path(dir, "cancel.flag"))
  .g3_write_status(dir, list(
    job_id = job$job_id, attempt = attempt, type = job$type, status = "queued",
    progress = list(unit = NA_character_, done = 0, total = NA_real_),
    started = NA_character_, ended = NA_character_, heartbeat = .g3_now(),
    stop_reason = NA_character_, error = NA_character_, result = NA_character_, pid = NA_integer_
  ))
  loader <- .g3_child_loader()
  proc <- callr::r_bg(
    function(dir, attempt, resume, loader) {
      if (!is.null(loader$path)) {
        pkgload::load_all(loader$path, quiet = TRUE, export_all = FALSE, helpers = FALSE)
      } else {
        loadNamespace("gambitR3D")
      }
      get(".g3_job_main", envir = asNamespace("gambitR3D"))(dir, attempt, resume)
    },
    args = list(dir = dir, attempt = attempt, resume = resume, loader = loader),
    stdout = file.path(dir, "log.txt"), stderr = "2>&1",
    supervise = TRUE, package = FALSE
  )
  structure(
    list(job_id = job$job_id, attempt = attempt, dir = dir, type = job$type, process = proc),
    class = "g3_job"
  )
}

# How a child process should load this package: from source when the
# current session uses a development load, otherwise the installed copy.
#' @noRd
.g3_child_loader <- function() {
  path <- tryCatch(getNamespaceInfo("gambitR3D", "path"), error = function(e) NULL)
  # An installed package also has DESCRIPTION and an R/ folder (holding only
  # the lazy-load database), so a development load is recognised by its R
  # source files; pkgload::load_all() on an installed copy fails.
  dev <- !is.null(path) && file.exists(file.path(path, "DESCRIPTION")) &&
    dir.exists(file.path(path, "R")) &&
    any(grepl("\\.[Rr]$", list.files(file.path(path, "R")))) &&
    requireNamespace("pkgload", quietly = TRUE)
  list(path = if (dev) path else NULL)
}

#' @noRd
.g3_write_status <- function(dir, st) {
  .g3_atomic_write(.g3_canonical_json(st), file.path(dir, "status.json"))
}

#' @noRd
.g3_read_status <- function(dir) {
  p <- file.path(dir, "status.json")
  if (!file.exists(p)) .g3_abort("g3_io_error", paste0("No job status at ", dir))
  jsonlite::fromJSON(p, simplifyVector = TRUE)
}

# Worker entry point (runs in the child process).
#' @noRd
.g3_job_main <- function(dir, attempt, resume) {
  job <- jsonlite::fromJSON(file.path(dir, "job.json"))
  st <- .g3_read_status(dir)
  st$status <- "running"
  st$started <- .g3_now()
  st$pid <- Sys.getpid()
  .g3_write_status(dir, st)
  t0 <- proc.time()[["elapsed"]]
  wall <- job$limits$wall_seconds
  ctx <- new.env(parent = emptyenv())
  ctx$dir <- dir
  ctx$attempt <- attempt
  ctx$resume <- resume
  ctx$seed <- job$seed
  ctx$elapsed <- function() proc.time()[["elapsed"]] - t0
  ctx$timed_out <- function() ctx$elapsed() > wall
  ctx$cancelled <- function() file.exists(file.path(dir, "cancel.flag"))
  ctx$should_stop <- function() ctx$cancelled() || ctx$timed_out()
  ctx$progress <- function(done, total = NA_real_, unit = "steps") {
    s <- .g3_read_status(dir)
    s$progress <- list(unit = unit, done = done, total = total)
    s$heartbeat <- .g3_now()
    .g3_write_status(dir, s)
  }
  fun_name <- .G3_JOB_TYPES[[job$type]]
  fun <- get0(fun_name, envir = asNamespace("gambitR3D"), inherits = FALSE)
  finish <- function(status, stop_reason = NA_character_, error = NA_character_, result = NA_character_) {
    s <- .g3_read_status(dir)
    s$status <- status
    s$stop_reason <- stop_reason
    s$error <- error
    s$result <- result
    s$ended <- .g3_now()
    s$heartbeat <- .g3_now()
    .g3_write_status(dir, s)
    invisible(status)
  }
  if (is.null(fun)) {
    return(finish("blocked", "not_implemented", paste0("Job type '", job$type, "' is not available in this build.")))
  }
  args <- readRDS(file.path(dir, "args.rds"))
  out <- tryCatch(
    fun(args, ctx),
    g3_error = function(e) structure(list(msg = conditionMessage(e), cls = class(e)[1L]), class = "g3_job_err"),
    error = function(e) structure(list(msg = conditionMessage(e), cls = "error"), class = "g3_job_err")
  )
  if (inherits(out, "g3_job_err")) {
    if (out$cls == "g3_incompatible_model") return(finish("blocked", "prerequisite_missing", out$msg))
    return(finish("failed", "error", out$msg))
  }
  result_path <- file.path(dir, paste0("result-", attempt, ".rds"))
  tmp <- paste0(result_path, ".tmp")
  saveRDS(out, tmp)
  file.rename(tmp, result_path)
  stop_reason <- attr(out, "stop_reason") %||% "completed"
  status <- if (ctx$cancelled()) "cancelled" else if (stop_reason == "timed_out" || ctx$timed_out()) "timed_out" else "succeeded"
  if (status != "succeeded") stop_reason <- status
  finish(status, stop_reason, result = basename(result_path))
}

#' Status of a background job
#'
#' @param job A `g3_job` from [g3_start_job()], or a job directory path.
#' @return A list with `job_id`, `attempt`, `type`, `status`, `progress`
#'   (`unit`, `done`, `total`), `started`, `ended`, `heartbeat`,
#'   `stop_reason`, `error` and `result` (file name of the saved result,
#'   readable with [g3_job_result()]). A job whose process has died while
#'   marked running is reported as `"interrupted"`.
#' @examples
#' \dontrun{
#' gambitR3D::g3_job_status(job)$status
#' }
#' @export
g3_job_status <- function(job) {
  dir <- .g3_job_dir(job)
  st <- .g3_read_status(dir)
  if (st$status %in% c("running", "queued")) {
    alive <- if (inherits(job, "g3_job") && !is.null(job$process)) {
      job$process$is_alive()
    } else if (!is.na(st$pid %||% NA)) {
      .g3_pid_alive(st$pid)
    } else {
      TRUE
    }
    if (!alive) {
      # Give the worker's final atomic write a moment, then re-read.
      Sys.sleep(0.05)
      st <- .g3_read_status(dir)
      if (st$status %in% c("running", "queued")) {
        st$status <- "interrupted"
        st$stop_reason <- "process_exited"
        .g3_write_status(dir, st)
      }
    }
  }
  st
}

#' @noRd
.g3_pid_alive <- function(pid) {
  if (dir.exists("/proc")) return(dir.exists(file.path("/proc", pid)))
  # Signal 0 only tests whether the process exists.
  isTRUE(tryCatch(tools::pskill(pid, 0L), error = function(e) TRUE))
}

#' @noRd
.g3_job_dir <- function(job) {
  dir <- if (inherits(job, "g3_job")) job$dir else if (is.character(job)) job else NULL
  if (is.null(dir) || !file.exists(file.path(dir, "job.json"))) {
    .g3_abort("g3_io_error", "Not a job (expected a g3_job or a job directory).")
  }
  dir
}

#' Read a finished job's result
#'
#' @param job A `g3_job` or job directory.
#' @return The value returned by the job function for the latest attempt,
#'   or `NULL` if no result has been written.
#' @export
g3_job_result <- function(job) {
  st <- g3_job_status(job)
  r <- st$result
  if (is.null(r) || length(r) != 1L || is.na(r) || !nzchar(r)) return(NULL)
  p <- file.path(.g3_job_dir(job), r)
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

#' Cancel a background job
#'
#' Requests a cooperative stop at the job's next safe point. If
#' `wait = TRUE`, waits up to `grace_seconds` and then terminates the
#' process tree. The returned acknowledgement says what actually happened;
#' it never claims a stop that has not occurred.
#'
#' @param job A `g3_job` (process termination needs the handle from the
#'   starting session) or a job directory (cooperative request only).
#' @param wait Wait for the job to stop.
#' @param grace_seconds Seconds to wait before terminating the process.
#' @return A list: `job_id`, `requested_at`, `outcome` (`"stopped"`,
#'   `"terminated"`, `"requested"` or `"already_finished"`), `status`.
#' @export
g3_cancel_job <- function(job, wait = TRUE, grace_seconds = 5) {
  dir <- .g3_job_dir(job)
  st <- g3_job_status(job)
  ack <- list(job_id = st$job_id, requested_at = .g3_now(), outcome = "requested", status = st$status)
  if (st$status %in% .G3_JOB_TERMINAL) {
    ack$outcome <- "already_finished"
    return(ack)
  }
  writeLines(.g3_now(), file.path(dir, "cancel.flag"))
  if (!wait) return(ack)
  proc <- if (inherits(job, "g3_job")) job$process else NULL
  t_end <- Sys.time() + grace_seconds
  while (Sys.time() < t_end) {
    s <- g3_job_status(job)
    if (s$status %in% .G3_JOB_TERMINAL) {
      ack$outcome <- "stopped"
      ack$status <- s$status
      return(ack)
    }
    Sys.sleep(0.1)
  }
  if (!is.null(proc) && proc$is_alive()) {
    proc$kill_tree()
    proc$wait(3000)
    s <- .g3_read_status(dir)
    s$status <- "cancelled"
    s$stop_reason <- "terminated_after_grace"
    s$ended <- .g3_now()
    .g3_write_status(dir, s)
    ack$outcome <- "terminated"
    ack$status <- "cancelled"
  }
  ack
}

#' Resume a job
#'
#' Starts a new attempt of a cancelled, timed-out, interrupted or failed
#' job. The job specification and its configuration hash are checked
#' first; the job function receives `ctx$resume = TRUE` and continues from
#' its last complete checkpoint where it supports that.
#'
#' @param job A `g3_job` or job directory.
#' @return A new `g3_job` handle for the next attempt of the same job.
#' @export
g3_resume_job <- function(job) {
  dir <- .g3_job_dir(job)
  st <- g3_job_status(job)
  if (!st$status %in% c("cancelled", "timed_out", "interrupted", "failed")) {
    .g3_abort("g3_io_error", paste0("Only a stopped job can be resumed (status: ", st$status, ")."))
  }
  spec <- jsonlite::fromJSON(file.path(dir, "job.json"))
  args <- readRDS(file.path(dir, "args.rds"))
  if (!identical(digest::digest(args, algo = "sha256"), spec$config_hash)) {
    .g3_abort("g3_io_error", "The job's arguments changed since it started; refusing to resume.")
  }
  if (!identical(spec$rules_hash, .g3_tables()$rules_hash)) {
    .g3_abort("g3_unsupported_rules", "The rules profile changed since the job started; refusing to resume.")
  }
  .g3_launch_attempt(dir, attempt = as.integer(st$attempt) + 1L, resume = TRUE)
}

#' @export
print.g3_job <- function(x, ...) {
  st <- tryCatch(g3_job_status(x), error = function(e) list(status = "unknown"))
  cat("<g3_job> ", x$job_id, " (", x$type, ", attempt ", x$attempt, "): ", st$status, "\n", sep = "")
  invisible(x)
}

# Job type: one search decision (used by the app's computer opponent).
#' @noRd
.g3_job_search <- function(args, ctx) {
  # Let the search poll the job's cancel flag and wall-time limit.
  op <- options(gambitR3D.stop_check = ctx$should_stop)
  on.exit(options(op), add = TRUE)
  g3_choose_action(args$state, agent = args$agent %||% "search", budget = args$budget %||% list(nodes = 2000L),
                   seed = args$seed)
}
