# Phase 04/14: background jobs (the minimal coordinator; phase 14 audits it).

skip_if_no_callr <- function() {
  skip_if_not_installed("callr")
  skip_on_cran()
}

test_that("a search job runs in the background and returns a legal decision", {
  skip_if_no_callr()
  root <- withr::local_tempdir()
  st <- g3_new_game()$state
  job <- g3_start_job(list(type = "search", args = list(state = st, budget = list(nodes = 100L)), job_dir = root))
  expect_s3_class(job, "g3_job")
  deadline <- Sys.time() + 60
  repeat {
    s <- g3_job_status(job)
    if (s$status %in% c("succeeded", "failed", "blocked", "interrupted") || Sys.time() > deadline) break
    Sys.sleep(0.2)
  }
  expect_identical(s$status, "succeeded")
  d <- g3_job_result(job)
  expect_s3_class(d, "g3_decision")
  expect_true(d$action_id %in% g3_legal_actions(st)$action_id)
})

test_that("cancellation is acknowledged honestly and leaves no running process", {
  skip_if_no_callr()
  root <- withr::local_tempdir()
  st <- g3_new_game()$state
  job <- g3_start_job(list(type = "search", args = list(state = st, budget = list(nodes = 1e6)), job_dir = root))
  Sys.sleep(1)
  ack <- g3_cancel_job(job, wait = TRUE, grace_seconds = 1)
  expect_true(ack$outcome %in% c("stopped", "terminated", "already_finished"))
  expect_false(job$process$is_alive())
  expect_true(g3_job_status(job)$status %in% c("cancelled", "succeeded", "interrupted"))
})

test_that("unknown job types are rejected before anything starts", {
  expect_error(g3_start_job(list(type = "mine_bitcoin")), class = "g3_io_error")
})
