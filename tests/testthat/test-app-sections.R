# Integration pass: Database, Analyse, Coach, Training and Models sections
# with shiny::testServer(). Background jobs are real (separate R processes),
# with small budgets.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")

sec_server <- function(dir, ...) .g3_app_server(.g3_app_config(storage_dir = dir, ...))

sec_req <- function(store, action_id) {
  tok <- .g3_store_token(store)
  list(kind = "action", game_id = tok$game_id, revision = tok$revision, request_id = .g3_new_id("t"),
       expected_state_hash = tok$state_hash, action_id = action_id, sent_at = stats::runif(1))
}

play_moves <- function(session, app, ids) {
  for (a in ids) session$setInputs(`play-board-request` = sec_req(app$live, a))
}

wait_until <- function(session, cond, seconds = 90) {
  deadline <- Sys.time() + seconds
  while (!isTRUE(cond()) && Sys.time() < deadline) {
    Sys.sleep(0.2)
    session$elapse(500)
  }
  isTRUE(cond())
}

html_of <- function(x) paste(unlist(x), collapse = "")

# The slower background-process tests (import, coach review) run when
# GAMBITR3D_SLOW_TESTS=true, keeping the default suite short.
slow_ok <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("GAMBITR3D_SLOW_TESTS"), "true"), "Set GAMBITR3D_SLOW_TESTS=true to run.")
}

test_that("the catalogue section lists, details, deletes, restores and backs up games", {
  skip_if_not_installed("RSQLite")
  dir <- withr::local_tempdir()
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    play_moves(session, app, c("m:W:1:1-W:1:3", "m:B:1:2-B:1:1"))
    id <- app$live$game$metadata$game_id
    expect_match(html_of(output$`database-table`), id, fixed = TRUE)
    expect_match(html_of(output$`database-status`), "games.sqlite", fixed = TRUE)
    session$setInputs(`database-detail` = list(value = id, nonce = 1))
    expect_match(html_of(output$`database-detail`), "W:c2-W:c4", fixed = TRUE)
    # Filters go to g3_query_games(); a non-matching player gives an empty page.
    session$setInputs(`database-player` = "nobody-here", `database-apply` = 1)
    expect_match(html_of(output$`database-table`), "No games match")
    session$setInputs(`database-player` = "", `database-apply` = 2)
    # Soft delete needs the confirmation, then restore.
    session$setInputs(`database-selected` = list(id), `database-delete_sel` = 1)
    expect_identical(nrow(g3_query_games(app$storage()$db, list(deleted = "only"))), 0L)
    session$setInputs(`database-delete_confirm` = 1)
    expect_identical(g3_query_games(app$storage()$db, list(deleted = "only"))$game_id, id)
    session$setInputs(`database-restore_sel` = 1)
    expect_identical(nrow(g3_query_games(app$storage()$db, list(deleted = "only"))), 0L)
    session$setInputs(`database-backup` = 1)
    b <- list.files(file.path(dir, "backups"), full.names = TRUE)
    expect_length(b, 1L)
    expect_match(html_of(output$`database-backup_report`), "Backup written")
    # Open in Analyse makes a branch; the live game stays as it was.
    live <- app$live$game
    session$setInputs(`database-analyse_game` = 1)
    expect_identical(app$section(), "analyse")
    expect_identical(app$analysis_branch()$game$metadata$branch_of, id)
    expect_identical(app$live$game, live)
  })
})

test_that("an import runs in the background and reports quarantine with the first invalid event", {
  slow_ok()
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("callr")
  dir <- withr::local_tempdir()
  src <- withr::local_tempdir()
  g <- g3_new_game()
  for (a in c("m:W:1:1-W:1:3", "m:B:1:2-B:1:1", "m:W:0:0-W:1:2")) g <- g3_game_apply(g, a)
  good <- file.path(src, "good.g3.json")
  g3_write_game(g, good)
  raw <- jsonlite::fromJSON(good, simplifyVector = FALSE)
  raw$events[[2]]$action_id <- "m:B:0:2-B:0:0"
  raw$game_id <- "tampered_game"
  bad <- file.path(src, "bad.g3.json")
  writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null", digits = NA), bad)
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    session$setInputs(`database-import_files` = data.frame(
      name = c("good.g3.json", "bad.g3.json"), size = file.size(c(good, bad)),
      type = "application/json", datapath = c(good, bad), stringsAsFactors = FALSE
    ))
    expect_true(wait_until(session, function() !grepl("Importing", html_of(output$`database-import_report`))))
    rep <- html_of(output$`database-import_report`)
    expect_match(rep, "1 accepted")
    expect_match(rep, "1 quarantined")
    expect_match(rep, "event 2")
    session$setInputs(`database-show_quarantine` = 1)
    expect_match(html_of(output$`database-quarantine`), "bad.g3.json", fixed = TRUE)
    expect_identical(nrow(g3_query_games(app$storage()$db)), 1L)
  })
})

test_that("whole-game analysis runs as a job; plot, table and board share the ply", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("ggplot2")
  dir <- withr::local_tempdir()
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    play_moves(session, app, c("m:W:1:1-W:1:3", "m:B:1:2-B:1:1", "m:W:0:0-W:1:2"))
    live <- app$live$game
    session$setInputs(`analyse-open` = 1, `analyse-game_budget` = "fast", `analyse-metric` = "expected_score")
    session$setInputs(`analyse-run_game` = 1)
    expect_true(wait_until(session, function() !is.null(app$analysis_result())))
    r <- app$analysis_result()
    expect_s3_class(r, "g3_analysis")
    expect_identical(r$game$n_plies, 3L)
    session$setInputs(`analyse-plot_click` = list(x = 1.2, y = 0.5))
    expect_identical(app$analysis_viewed(), 1L)
    expect_match(html_of(output$`analyse-move_table`), "g3-row-current")
    session$setInputs(`analyse-goto` = list(value = "2", nonce = 1))
    expect_identical(app$analysis_viewed(), 2L)
    expect_true(inherits(output$`analyse-trajectory`, "list"))
    # The report download is an offline HTML report with its tables.
    zipf <- output$`analyse-report`
    expect_true("report/report.html" %in% utils::unzip(zipf, list = TRUE)$Name)
    # A variation from ply 1 is labelled and never touches the live game.
    session$setInputs(`analyse-first` = 1, `analyse-next_ply` = 1, `analyse-branch_here` = 1)
    b <- app$analysis_branch()
    session$setInputs(`analyse-board-request` = sec_req(b, "m:B:3:2-B:3:0"))
    expect_identical(.g3_variation_ply(b), 2L)
    expect_match(html_of(output$`analyse-branch_info`), "Variation from ply 2")
    expect_identical(app$live$game, live)
  })
})

test_that("a coach review runs as a job; claims, line preview and explanations stay off the game", {
  slow_ok()
  skip_if_not_installed("callr")
  dir <- withr::local_tempdir()
  # A short deterministic game: greedy plays both sides.
  g <- g3_new_game()
  for (i in 1:8) g <- g3_game_apply(g, g3_choose_action(g$state, "greedy")$action_id)
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    app$resume_in_play(g)
    live_hash <- g3_game_hash(app$live$game)
    session$setInputs(`coach-source` = "live", `coach-budget` = "fast", `coach-review` = 1)
    expect_true(wait_until(session, function() !is.null(app$review()), 120))
    rv <- app$review()
    expect_s3_class(rv, "g3_coach_review")
    expect_match(html_of(output$`coach-decisions`), "g3-table")
    lines <- Filter(function(e) identical(e$kind, "line"), rv$evidence)
    if (length(lines)) {
      id <- names(lines)[1]
      session$setInputs(`coach-pick_ply` = list(value = as.character(lines[[1]]$ply), nonce = 1))
      session$setInputs(`coach-preview_line` = list(value = id, nonce = 2))
      expect_match(html_of(output$`coach-board_info`), "not the game")
      session$setInputs(`coach-line_next` = 1)
      session$setInputs(`coach-line_return` = 1)
      expect_identical(html_of(output$`coach-board_info`), "")
    }
    # Explaining an action of the shown position answers instead of playing.
    p <- max(rv$decisions$ply)
    session$setInputs(`coach-pick_ply` = list(value = as.character(p), nonce = 3))
    a <- g3_legal_actions(g3_replay_game(g, p - 1L))$action_id[1]
    session$setInputs(`coach-explain_action` = a, `coach-explain` = 1)
    expect_match(html_of(output$`coach-explanation`), "rules fact")
    expect_identical(g3_game_hash(app$live$game), live_hash)
  })
})

test_that("practice exercises give hints in order, score attempts and record progress", {
  dir <- withr::local_tempdir()
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    set <- g3_exercise_catalogue()
    ex <- set$exercises[[1]]
    session$setInputs(`training-start` = list(value = ex$exercise_id, nonce = 1))
    expect_match(html_of(output$`training-exercise_head`), g3_exercise_objective(ex), fixed = TRUE)
    session$setInputs(`training-hint` = 1)
    session$setInputs(`training-hint` = 2)
    expect_match(html_of(output$`training-hints`), "Hint 2")
    expect_false(grepl("Hint 3", html_of(output$`training-hints`)))
    tok <- list(game_id = paste0("exercise_", substr(ex$exercise_id, 1L, 12L)), revision = 0L,
                state_hash = .g3_state_hash(ex$start_state))
    wrong <- setdiff(g3_legal_actions(ex$start_state)$action_id, ex$solutions$action_id)[1]
    session$setInputs(`training-board-request` = c(tok, list(kind = "action", request_id = "a1", action_id = wrong)))
    expect_match(html_of(output$`training-feedback`), "Not this time")
    session$setInputs(`training-board-request` = c(tok, list(kind = "action", request_id = "a2",
                                                             action_id = ex$primary_action_id)))
    expect_match(html_of(output$`training-feedback`), "Solved")
    prof <- file.path(dir, "learner", "profile.json")
    expect_true(file.exists(prof))
    expect_identical(nrow(g3_learner_progress(prof)$attempts), 2L)
    # The training-dataset flag is off by default and only changes on request.
    expect_false(g3_learner_settings(prof)$include_in_training_dataset)
    session$setInputs(`training-training_dataset` = TRUE)
    expect_true(g3_learner_settings(prof)$include_in_training_dataset)
    session$setInputs(`training-reset_profile` = 1, `training-reset_confirm` = 1)
    expect_identical(nrow(g3_learner_progress(prof)$attempts), 0L)
    # The game itself never moved.
    expect_identical(app$live$game$state$ply, 0L)
  })
})

test_that("the model registry shows statuses; reference candidates cannot be played", {
  dir <- withr::local_tempdir()
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    lst <- html_of(output$`models-list`)
    expect_match(lst, "builtin-search", fixed = TRUE)
    expect_match(lst, "reference only")
    cand <- g3_model_capabilities()
    ref <- cand$model_id[cand$kind == "reference_candidate"][1]
    session$setInputs(`models-pick` = list(value = ref, nonce = 1))
    det <- html_of(output$`models-detail`)
    expect_match(det, "Cannot be used as an opponent")
    expect_match(det, "disabled")
    session$setInputs(`models-pick` = list(value = "builtin-search", nonce = 2))
    expect_match(html_of(output$`models-detail`), "built-in search")
  })
})

test_that("without an ML environment training is refused with a reason and nothing starts", {
  dir <- withr::local_tempdir()
  withr::local_options(gambitR3D.ml_env = file.path(dir, "no-such-env"))
  shiny::testServer(sec_server(dir), {
    app <- session$userData$g3
    expect_match(html_of(output$`training-env_status`), "No ML environment")
    session$setInputs(`training-train` = 1)
    expect_null(app$jobs$training)
    expect_false(dir.exists(file.path(dir, "training")))
  })
})
