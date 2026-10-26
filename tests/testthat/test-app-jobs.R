# Repair pass: the app's job controllers (audit F01, F11). Real background
# processes, so these need callr and are skipped on CRAN.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("callr")

jobs_server <- function(dir) .g3_app_server(.g3_app_config(storage_dir = dir, db_path = FALSE))

jobs_wait <- function(session, cond, seconds = 60) {
  deadline <- Sys.time() + seconds
  while (!isTRUE(cond()) && Sys.time() < deadline) {
    Sys.sleep(0.2)
    session$elapse(500)
  }
  isTRUE(cond())
}

panel_html <- function(ctl, lang = "en") {
  paste(as.character(.g3_job_panel(ctl, shiny::NS("t"), "probe", lang)), collapse = "")
}

long_search <- function() {
  list(state = g3_new_game()$state, agent = "search", budget = list(nodes = 5e8), seed = 1L)
}

test_that("a job the user cancels reads Cancelled, also when it had to be terminated after the grace", {
  skip_on_cran()
  skip_on_os("windows") # suspends the job process
  dir <- withr::local_tempdir()
  shiny::testServer(jobs_server(dir), {
    app <- session$userData$g3
    # A short grace, and a process that cannot react to the cancel request
    # (suspended), so the drain has to terminate it.
    ctl <- .g3_job_controller(app, session, "probe", wall_seconds = 120, grace_seconds = 1)
    ctl$start("search", long_search(), label = "probe")
    expect_true(jobs_wait(session, function() identical(ctl$state$status$status, "running"), 60))
    ctl$state$job$process$suspend()
    ctl$cancel()
    # Until the job has stopped, the panel says it is stopping (no second cancel button).
    p <- panel_html(ctl)
    expect_match(p, "Stopping", fixed = TRUE)
    expect_false(grepl("t-probe_cancel", p, fixed = TRUE))
    expect_true(jobs_wait(session, function() !ctl$running(), 60))
    expect_identical(ctl$state$status$status, "cancelled")
    expect_identical(ctl$state$status$stop_reason, "terminated_after_grace")
    p <- panel_html(ctl)
    expect_match(p, "Cancelled", fixed = TRUE)
    expect_false(grepl("Interrupted|process_exited|terminated_after_grace", p))
    expect_match(p, "did not reach a safe point in time and was terminated", fixed = TRUE)
    # The stop reason is written once, in words.
    expect_identical(lengths(regmatches(p, gregexpr("Stop reason", p, fixed = TRUE))), 1L)
    expect_match(panel_html(ctl, "de"), "Abgebrochen", fixed = TRUE)
  })
})

test_that("a cancelled job whose process ends during the grace period is reported as cancelled", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  shiny::testServer(jobs_server(dir), {
    app <- session$userData$g3
    ctl <- .g3_job_controller(app, session, "probe", wall_seconds = 120, grace_seconds = 60)
    ctl$start("search", long_search(), label = "probe")
    expect_true(jobs_wait(session, function() identical(ctl$state$status$status, "running"), 60))
    ctl$cancel()
    # The process dies before it reaches a safe point (as a hard kill would).
    ctl$state$job$process$kill_tree()
    expect_true(jobs_wait(session, function() !ctl$running(), 30))
    expect_identical(ctl$state$status$status, "cancelled")
    p <- panel_html(ctl)
    expect_match(p, "Cancelled", fixed = TRUE)
    expect_false(grepl("Interrupted", p, fixed = TRUE))
    expect_identical(lengths(regmatches(p, gregexpr("Stop reason", p, fixed = TRUE))), 1L)
    expect_null(ctl$state$error)
  })
})

test_that("stop reasons are shown in words; the deadline termination has its own text", {
  expect_identical(.g3_stop_reason_text("terminated_after_deadline", "en"),
                   "it did not stop at its time limit and was terminated")
  expect_match(.g3_stop_reason_text("terminated_after_deadline", "de"), "Zeitlimit")
  expect_identical(.g3_stop_reason_text("some_new_reason", "en"), "some_new_reason")
  ctl <- list(state = list2env(list(status = list(status = "timed_out", stop_reason = "terminated_after_deadline"),
                                    error = NULL, job = NULL, started = NULL, cancel_requested = FALSE)))
  p <- panel_html(ctl)
  expect_match(p, "Time limit reached", fixed = TRUE)
  expect_match(p, "Stop reason: it did not stop at its time limit and was terminated", fixed = TRUE)
  # The computer opponent's failure line uses the same words.
  res <- list(status = "failed", job_status = "timed_out", stop_reason = "terminated_after_deadline",
              error = NA_character_)
  expect_identical(.g3_opponent_failure_text(res, "en"),
                   "Time limit reached (it did not stop at its time limit and was terminated)")
  expect_match(.g3_opponent_failure_text(res, "de"), "Zeitlimit")
  expect_identical(.g3_opponent_failure_text(list(status = "failed", key = "opponent.no_action"), "de"),
                   "Der Gegner hat keinen Zug geliefert.")
})

test_that("a one-off background call is terminated at its wall limit and reads timed out", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  shiny::testServer(jobs_server(dir), {
    app <- session$userData$g3
    ctl <- .g3_bg_controller(app, session, "import", wall_seconds = 1)
    ctl$start("Sys.sleep", list(time = 60))
    proc <- ctl$state$proc
    expect_true(proc$is_alive())
    expect_true(isTRUE(app$jobs$import))
    expect_true(jobs_wait(session, function() identical(ctl$state$status, "timed_out"), 30))
    expect_true(jobs_wait(session, function() !proc$is_alive(), 10))
    expect_false(isTRUE(app$jobs$import))
    rep <- paste(as.character(.g3_db_import_report(ctl$state, shiny::NS("d"), "en")), collapse = "")
    expect_match(rep, "The import did not finish within 1 s and was stopped.", fixed = TRUE)
    rep <- paste(as.character(.g3_db_export_report(ctl$state, shiny::NS("d"), "de")), collapse = "")
    expect_match(rep, "nicht innerhalb von 1 s fertig", fixed = TRUE)
    env <- paste(as.character(.g3_ml_status_panel(ctl$state, dir, "en")), collapse = "")
    expect_match(env, "The environment check did not finish within 1 s", fixed = TRUE)
  })
})
