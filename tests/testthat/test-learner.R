# Phase 13: local learner profile, spaced practice and privacy controls.

learner_set <- function() g3_exercise_catalogue()
attempt_on <- function(set, def_id, kind = c("solved", "incorrect", "acceptable"), hints = 0L) {
  kind <- match.arg(kind)
  ex <- Filter(function(e) identical(e$provenance$definition_id, def_id), set$exercises)[[1L]]
  a <- switch(kind,
              solved = ex$primary_action_id,
              acceptable = ex$solutions$action_id[ex$solutions$grade == "acceptable"][1L],
              incorrect = setdiff(g3_legal_actions(ex$start_state)$action_id, ex$solutions$action_id)[1L])
  g3_score_attempt(ex, a, hints_used = hints)
}

test_that("reading a missing profile creates nothing and gives typed empty tables", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "sub", "profile.json")
  pr <- g3_learner_progress(path)
  expect_s3_class(pr, "g3_learner_progress")
  expect_false(pr$summary$exists)
  expect_false(file.exists(path))
  expect_identical(nrow(pr$attempts), 0L)
  expect_identical(names(pr$themes)[1:3], c("theme", "attempts", "solved"))
  expect_false(pr$settings$include_in_training_dataset)
  expect_false(pr$settings$upload)
  expect_true(pr$settings$use_for_practice)
})

test_that("attempts are recorded atomically with a transparent Leitner schedule", {
  set <- learner_set()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.json")
  d0 <- as.Date("2026-10-26")
  r1 <- g3_learner_record(path, attempt_on(set, "fork-d6-twins", "solved"), date = d0)
  expect_true(r1$recorded)
  expect_identical(r1$box, 1L)
  expect_identical(r1$due, "2026-10-28")
  r2 <- g3_learner_record(path, attempt_on(set, "capture-queen", "incorrect"), date = d0)
  expect_identical(r2$box, 0L)
  expect_identical(r2$due, "2026-10-27")
  r3 <- g3_learner_record(path, attempt_on(set, "fork-d6-twins", "solved", hints = 2L), date = d0 + 2)
  expect_identical(r3$box, 1L)
  pr <- g3_learner_progress(path)
  expect_true(pr$summary$exists)
  expect_identical(nrow(pr$attempts), 3L)
  expect_identical(sort(pr$themes$theme), c("capture", "fork"))
  expect_identical(pr$themes$state[pr$themes$theme == "capture"], "recently_missed")
  expect_identical(pr$hints$hints_total[pr$hints$theme == "fork"], 2L)
  expect_true(all(c("box", "due", "last_status") %in% names(pr$schedule)))
  # Atomic writes leave no temporary files behind.
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "profile.json")
  # Only exercise ids, actions, statuses, hints and dates are stored.
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_setequal(names(raw$attempts[[1]]), c("attempt_id", "exercise_id", "theme", "status", "hints_used",
                                              "actions", "grade_basis", "date", "scored_at", "rules_hash"))
})

test_that("the plan respects the daily limit, reintroduces missed themes and mixes mastered ones", {
  set <- learner_set()
  path <- file.path(withr::local_tempdir(), "profile.json")
  d0 <- as.Date("2026-10-01")
  # Checkmate solved three times without hints: mastered.
  for (k in 0:2) g3_learner_record(path, attempt_on(set, "checkmate-e5-twins", "solved"), date = d0 + 10 * k)
  # Fork missed: recently missed.
  g3_learner_record(path, attempt_on(set, "fork-d6-twins", "incorrect"), date = d0 + 20)
  pr <- g3_learner_progress(path)
  expect_identical(pr$themes$state[pr$themes$theme == "checkmate"], "mastered")
  expect_identical(pr$themes$state[pr$themes$theme == "fork"], "recently_missed")
  day <- d0 + 21
  plan <- g3_practice_plan(path, set, date = day, daily_limit = 5L)
  expect_lte(nrow(plan), 5L)
  expect_identical(attr(plan, "rules"), "leitner-6/1")
  expect_true(all(nzchar(plan$reason)))
  # The missed fork is due and comes first.
  expect_identical(plan$reason_code[1], "due_review")
  expect_identical(plan$theme[1], "fork")
  # A mastered theme is mixed in although it is not due.
  expect_true("mastered_mix" %in% plan$reason_code)
  expect_identical(plan$theme[plan$reason_code == "mastered_mix"][1], "checkmate")
  expect_true("new_theme" %in% plan$reason_code)
  # The selection is logged for inspection.
  log <- g3_learner_progress(path)$selection_log
  expect_identical(log$exercise_id, plan$exercise_id)
  expect_identical(unique(log$date), format(day))
  # Attempts made today count against the limit.
  g3_learner_record(path, attempt_on(set, "capture-queen", "solved"), date = day)
  plan2 <- g3_practice_plan(path, set, date = day, daily_limit = 2L, record = FALSE)
  expect_identical(nrow(plan2), 1L)
  # German reasons.
  plan3 <- g3_practice_plan(path, set, date = day, daily_limit = 3L, record = FALSE, lang = "de")
  expect_true(any(grepl("Thema|Wiederholung|Aufgabe", plan3$reason)))
})

test_that("privacy flags are separate, local and default to off for datasets", {
  set <- learner_set()
  path <- file.path(withr::local_tempdir(), "profile.json")
  expect_false(.g3_learner_dataset_allowed(path))
  g3_learner_settings(path, use_for_practice = FALSE)
  rec <- g3_learner_record(path, attempt_on(set, "fork-d6-twins", "solved"))
  expect_false(rec$recorded)
  expect_identical(nrow(g3_learner_progress(path)$attempts), 0L)
  plan <- g3_practice_plan(path, set, daily_limit = 3L)
  expect_true(all(plan$reason_code == "personalisation_off"))
  expect_identical(nrow(g3_learner_progress(path)$selection_log), 0L)
  g3_learner_settings(path, use_for_practice = TRUE)
  expect_false(g3_learner_settings(path)$include_in_training_dataset)
  expect_false(.g3_learner_dataset_allowed(path))
  g3_learner_settings(path, include_in_training_dataset = TRUE)
  expect_true(.g3_learner_dataset_allowed(path))
  expect_error(g3_learner_settings(path, upload = TRUE), class = "g3_io_error")
  expect_false(g3_learner_settings(path)$upload)
  expect_error(g3_learner_settings(path, daily_limit = 0L), class = "g3_io_error")
})

test_that("profiles can be exported, reset and deleted", {
  set <- learner_set()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "profile.json")
  g3_learner_settings(path, daily_limit = 4L)
  g3_learner_record(path, attempt_on(set, "fork-d6-twins", "solved"))
  g3_learner_record(path, attempt_on(set, "capture-queen", "acceptable"))
  out <- file.path(dir, "export.json")
  man <- g3_learner_export(path, out)
  expect_identical(man$file, out)
  expect_identical(man$sha256, digest::digest(file = out, algo = "sha256"))
  exp <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_identical(length(exp$attempts), 2L)
  expect_false(is.null(exp$exported))
  csv <- g3_learner_export(path, file.path(dir, "csv"), format = "csv")
  expect_setequal(basename(csv$file), c("attempts.csv", "hints.csv", "themes.csv", "schedule.csv", "selection_log.csv"))
  expect_identical(nrow(utils::read.csv(file.path(dir, "csv", "attempts.csv"))), 2L)
  # Reset and delete need explicit confirmation.
  expect_error(g3_learner_reset(path), class = "g3_io_error")
  g3_learner_reset(path, confirm = TRUE)
  pr <- g3_learner_progress(path)
  expect_identical(nrow(pr$attempts), 0L)
  expect_identical(pr$settings$daily_limit, 4L)
  expect_error(g3_learner_delete(path), class = "g3_io_error")
  expect_true(g3_learner_delete(path, confirm = TRUE)$deleted)
  expect_false(file.exists(path))
  expect_false(g3_learner_delete(path, confirm = TRUE)$deleted)
  expect_error(g3_learner_export(path, out), class = "g3_io_error")
})
