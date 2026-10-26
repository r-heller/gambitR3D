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

wait_for <- function(job, done = c("succeeded", "failed", "blocked", "interrupted", "cancelled", "timed_out"),
                     seconds = 90) {
  deadline <- Sys.time() + seconds
  repeat {
    s <- g3_job_status(job)
    if (s$status %in% done || Sys.time() > deadline) return(s)
    Sys.sleep(0.2)
  }
}

test_that("the wall-time limit stops a job as timed_out with its partial result", {
  skip_if_no_callr()
  root <- withr::local_tempdir()
  st <- g3_new_game()$state
  job <- g3_start_job(list(type = "search", args = list(state = st, budget = list(nodes = 1e9)),
                           limits = list(wall_seconds = 2), job_dir = root))
  s <- wait_for(job)
  expect_identical(s$status, "timed_out")
  d <- g3_job_result(job)
  expect_true(d$action_id %in% g3_legal_actions(st)$action_id)
  expect_false(job$process$is_alive())
})

test_that("a killed worker is reported as interrupted, never as succeeded", {
  skip_if_no_callr()
  root <- withr::local_tempdir()
  st <- g3_new_game()$state
  job <- g3_start_job(list(type = "search", args = list(state = st, budget = list(nodes = 1e9)), job_dir = root))
  s <- wait_for(job, done = "running", seconds = 30)
  job$process$kill()
  s <- wait_for(job)
  expect_identical(s$status, "interrupted")
  expect_null(g3_job_result(job))
})

test_that("a missing prerequisite blocks a job with its reason", {
  skip_if_no_callr()
  root <- withr::local_tempdir()
  job <- g3_start_job(list(
    type = "self_play",
    args = list(config = list(model = "model:no-such-checkpoint", games = 1L), output_dir = file.path(root, "out")),
    job_dir = root
  ))
  s <- wait_for(job)
  expect_true(s$status %in% c("blocked", "failed"))
  expect_true(nzchar(s$error))
})

test_that("a cancelled job resumes as a new attempt of the same job", {
  skip_if_no_callr()
  root <- withr::local_tempdir()
  st <- g3_new_game()$state
  job <- g3_start_job(list(type = "search", args = list(state = st, budget = list(nodes = 3e5)), job_dir = root))
  wait_for(job, done = "running", seconds = 30)
  ack <- g3_cancel_job(job, wait = TRUE, grace_seconds = 5)
  expect_true(ack$outcome %in% c("stopped", "terminated", "already_finished"))
  if (g3_job_status(job)$status == "cancelled") {
    job2 <- g3_resume_job(job)
    expect_identical(job2$job_id, job$job_id)
    expect_identical(job2$attempt, 2L)
    s2 <- wait_for(job2)
    expect_identical(s2$status, "succeeded")
    expect_identical(s2$attempt, 2L)
  }
  expect_error(g3_resume_job(job), class = "g3_io_error") # finished jobs do not resume
})
