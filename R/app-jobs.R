# Background work for app sections.
#
# Long work never runs on the reactive thread. Two mechanisms are used:
#
# * `.g3_job_controller()` wraps the package's durable jobs
#   (`g3_start_job()`: analysis, coach, training, self_play, ...). It keeps
#   the job handle, polls `g3_job_status()` with `invalidateLater()`,
#   reads the result when the job ends, supports cancel (cooperative, then
#   terminated after the grace period by the drain) and resume (a new
#   attempt of the same job), and reports every state honestly: queued,
#   running (with the job's own progress unit, or "progress cannot be
#   estimated"), succeeded, failed, cancelled, timed_out, blocked,
#   interrupted.
# * `.g3_bg_controller()` runs one internal function in a `callr::r_bg()`
#   process, for work that has no registered job type (database import and
#   export, the ML environment check). It is cancellable but not resumable.
#
# Both report to the header's running-work indicator through `app$set_job()`
# and stop their processes when the session ends.

.G3_JOB_RUNNING <- c("queued", "running")

#' @noRd
.g3_job_controller <- function(app, session, kind, wall_seconds = 600) {
  st <- new.env(parent = emptyenv())
  st$job <- NULL
  st$status <- NULL
  st$result <- NULL
  st$error <- NULL
  st$tag <- NULL
  st$started <- NULL
  st$draining <- list()
  tick <- shiny::reactiveVal(0L)
  bump <- function() tick(shiny::isolate(tick()) + 1L)
  running <- function() !is.null(st$status) && isTRUE(st$status$status %in% .G3_JOB_RUNNING)

  start <- function(type, args, tag = NULL, label = kind, limits = list()) {
    if (running()) cancel()
    st$result <- NULL
    st$error <- NULL
    st$tag <- tag
    jobs_dir <- file.path(shiny::isolate(app$storage())$root, "jobs")
    dir.create(jobs_dir, recursive = TRUE, showWarnings = FALSE)
    job <- suppressWarnings(tryCatch(
      g3_start_job(list(type = type, args = args, label = label, job_dir = jobs_dir,
                        limits = utils::modifyList(list(wall_seconds = wall_seconds), limits))),
      error = function(e) e
    ))
    if (inherits(job, "error")) {
      st$job <- NULL
      st$status <- list(status = "failed", error = conditionMessage(job), stop_reason = "start_failed",
                        progress = list(done = 0, total = NA, unit = NA))
      st$error <- conditionMessage(job)
    } else {
      st$job <- job
      st$status <- list(status = "queued", progress = list(done = 0, total = NA, unit = NA))
      st$started <- Sys.time()
      app$set_job(kind, TRUE)
    }
    bump()
    invisible(st)
  }

  poll <- function() {
    if (is.null(st$job)) return(invisible(FALSE))
    s <- tryCatch(g3_job_status(st$job), error = function(e) {
      list(status = "failed", error = conditionMessage(e), stop_reason = "status_unreadable")
    })
    changed <- !identical(s, st$status)
    st$status <- s
    if (!isTRUE(s$status %in% .G3_JOB_RUNNING)) {
      app$set_job(kind, FALSE)
      res <- tryCatch(g3_job_result(st$job), error = function(e) NULL)
      st$result <- res
      if (identical(s$status, "failed") || identical(s$status, "blocked") || identical(s$status, "interrupted")) {
        st$error <- s$error %||% s$stop_reason %||% s$status
      }
    }
    if (changed) bump()
    invisible(changed)
  }

  cancel <- function() {
    if (is.null(st$job)) return(invisible(NULL))
    ack <- tryCatch(g3_cancel_job(st$job, wait = FALSE), error = function(e) NULL)
    st$draining[[length(st$draining) + 1L]] <- list(job = st$job, since = Sys.time())
    bump()
    invisible(ack)
  }

  resume <- function() {
    if (is.null(st$job) || running()) return(invisible(NULL))
    job <- tryCatch(g3_resume_job(st$job), error = function(e) e)
    if (inherits(job, "error")) {
      st$error <- conditionMessage(job)
    } else {
      st$job <- job
      st$result <- NULL
      st$error <- NULL
      st$status <- list(status = "queued", progress = list(done = 0, total = NA, unit = NA))
      app$set_job(kind, TRUE)
    }
    bump()
    invisible(job)
  }

  # Cancelled processes get 5 seconds for a cooperative stop, then the tree
  # is terminated; their directories stay (they hold the job's record).
  drain <- function(force = FALSE) {
    keep <- list()
    for (d in st$draining) {
      alive <- isTRUE(tryCatch(d$job$process$is_alive(), error = function(e) FALSE))
      if (alive && (force || difftime(Sys.time(), d$since, units = "secs") > 5)) {
        try(d$job$process$kill_tree(), silent = TRUE)
        alive <- FALSE
      }
      if (alive) keep[[length(keep) + 1L]] <- d
    }
    st$draining <- keep
  }

  shiny::observe({
    tick()
    if (!running() && !length(st$draining)) return()
    shiny::invalidateLater(400)
    shiny::isolate({
      drain()
      if (running()) poll()
    })
  })
  session$onSessionEnded(function() {
    if (!is.null(st$job)) {
      try(g3_cancel_job(st$job, wait = FALSE), silent = TRUE)
      try(st$job$process$kill_tree(), silent = TRUE)
    }
    drain(force = TRUE)
  })
  list(tick = tick, state = st, start = start, cancel = cancel, resume = resume, running = running,
       poll = poll, bump = bump)
}

# A status panel for a job controller: state chip, progress, elapsed time,
# stop reason, error, and cancel / resume buttons with module ids
# `<prefix>_cancel` and `<prefix>_resume`.
#' @noRd
.g3_job_panel <- function(ctl, ns, prefix, lang = "en", resumable = TRUE) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  s <- ctl$state$status
  if (is.null(s)) return(htmltools::p(class = "g3-muted g3-small", t("job.idle")))
  status <- s$status %||% "failed"
  p <- s$progress %||% list()
  prog <- if (status %in% .G3_JOB_RUNNING) {
    if (!is.null(p$total) && isTRUE(is.finite(as.numeric(p$total))) && as.numeric(p$total) > 0) {
      t("job.progress", done = p$done %||% 0, total = p$total, unit = p$unit %||% "")
    } else if (!is.null(p$done) && isTRUE(as.numeric(p$done) > 0)) {
      t("job.progress_open", done = p$done, unit = p$unit %||% "")
    } else {
      t("job.progress_unknown")
    }
  }
  elapsed <- if (!is.null(ctl$state$started)) round(as.numeric(difftime(Sys.time(), ctl$state$started, units = "secs")))
  shape <- switch(status, succeeded = "\u2713", failed = "!", blocked = "\u2205", interrupted = "!",
                  cancelled = "\u25A0", timed_out = "\u231B", "\u25CB")
  htmltools::div(
    class = paste0("g3-job g3-job-", status), role = "status",
    htmltools::p(
      htmltools::span(class = "g3-badge", htmltools::span(`aria-hidden` = "true", shape), t(paste0("job.status.", status))),
      if (status %in% .G3_JOB_RUNNING) htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
      " ", prog,
      if (!is.null(elapsed) && status %in% .G3_JOB_RUNNING) htmltools::span(class = "g3-muted g3-tnum", t("job.elapsed", s = elapsed))
    ),
    if (!is.null(s$stop_reason) && !is.na(s$stop_reason) && !status %in% .G3_JOB_RUNNING) {
      htmltools::p(class = "g3-small", t("job.stop_reason", reason = s$stop_reason))
    },
    if (!is.null(ctl$state$error) && !is.na(ctl$state$error)) htmltools::p(class = "g3-error", ctl$state$error),
    htmltools::div(
      class = "g3-btn-row",
      if (status %in% .G3_JOB_RUNNING) shiny::actionButton(ns(paste0(prefix, "_cancel")), t("job.cancel"),
                                                          class = "g3-btn btn-outline-secondary"),
      if (resumable && status %in% c("cancelled", "timed_out", "interrupted", "failed") && !is.null(ctl$state$job)) {
        shiny::actionButton(ns(paste0(prefix, "_resume")), t("job.resume"), class = "g3-btn btn-outline-secondary")
      }
    )
  )
}

# ------------------------------------------------- one-off background calls

#' @noRd
.g3_bg_controller <- function(app, session, kind) {
  st <- new.env(parent = emptyenv())
  st$proc <- NULL
  st$result <- NULL
  st$error <- NULL
  st$status <- "idle"
  st$tag <- NULL
  tick <- shiny::reactiveVal(0L)
  bump <- function() tick(shiny::isolate(tick()) + 1L)
  start <- function(fun_name, args, tag = NULL) {
    if (!is.null(st$proc)) try(st$proc$kill_tree(), silent = TRUE)
    st$result <- NULL
    st$error <- NULL
    st$tag <- tag
    loader <- .g3_child_loader()
    st$proc <- tryCatch(callr::r_bg(
      function(fun_name, args, loader) {
        if (!is.null(loader$path)) {
          pkgload::load_all(loader$path, quiet = TRUE, export_all = FALSE, helpers = FALSE)
        } else {
          loadNamespace("gambitR3D")
        }
        do.call(get(fun_name, envir = asNamespace("gambitR3D")), args)
      },
      args = list(fun_name = fun_name, args = args, loader = loader), supervise = TRUE, package = FALSE
    ), error = function(e) {
      st$error <- conditionMessage(e)
      NULL
    })
    st$status <- if (is.null(st$proc)) "failed" else "running"
    app$set_job(kind, !is.null(st$proc))
    bump()
  }
  cancel <- function() {
    if (!is.null(st$proc)) try(st$proc$kill_tree(), silent = TRUE)
    st$proc <- NULL
    st$status <- "cancelled"
    app$set_job(kind, FALSE)
    bump()
  }
  shiny::observe({
    tick()
    if (is.null(st$proc)) return()
    shiny::invalidateLater(300)
    shiny::isolate({
      if (st$proc$is_alive()) return()
      res <- tryCatch(st$proc$get_result(), error = function(e) e)
      st$proc <- NULL
      app$set_job(kind, FALSE)
      if (inherits(res, "error")) {
        st$error <- conditionMessage(res)
        st$status <- "failed"
      } else {
        st$result <- res
        st$status <- "succeeded"
      }
      bump()
    })
  })
  session$onSessionEnded(function() if (!is.null(st$proc)) try(st$proc$kill_tree(), silent = TRUE))
  list(tick = tick, state = st, start = start, cancel = cancel,
       running = function() identical(st$status, "running"), bump = bump)
}
