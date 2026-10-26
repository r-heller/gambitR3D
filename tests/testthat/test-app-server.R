# Phase 05-07: the Shiny server with shiny::testServer(). Every change goes
# through the inputs a browser would send.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")

app_server <- function(dir, ...) .g3_app_server(.g3_app_config(storage_dir = dir, ...))

board_req <- function(store, action_id, request_id = .g3_new_id("t")) {
  tok <- .g3_store_token(store)
  list(kind = "action", game_id = tok$game_id, revision = tok$revision, request_id = request_id,
       expected_state_hash = tok$state_hash, action_id = action_id, sent_at = stats::runif(1))
}

upload <- function(path) {
  data.frame(name = basename(path), size = file.size(path), type = "application/json",
             datapath = path, stringsAsFactors = FALSE)
}

mate_in_one_file <- function(dir) {
  # Black to act; Qb5-e5 on B (or N) mates. Same fixture as test-search.R.
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  path <- file.path(dir, "mate-in-one.g3.json")
  g3_write_game(.g3_game_new(st, metadata = list(mode = "hotseat")), path)
  path
}

test_that("board requests commit once per request id; stale requests change nothing", {
  dir <- withr::local_tempdir()
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    r1 <- board_req(app$live, "m:W:1:1-W:1:3", "dup-1")
    session$setInputs(`play-board-request` = r1)
    expect_identical(app$live$revision, 1L)
    key1 <- g3_position_key(app$live$game$state)
    q <- g3_query_games(app$storage()$db)
    expect_identical(q$game_id, app$live$game$metadata$game_id)
    expect_identical(q$plies, 1L)
    # The browser re-sends the same request id (for example after a hiccup).
    r1$sent_at <- 99
    session$setInputs(`play-board-request` = r1)
    expect_identical(app$live$commits, 1L)
    # A request built on the old revision is rejected without mutation.
    stale <- r1
    stale$request_id <- "stale-1"
    stale$action_id <- "m:B:1:2-B:1:1"
    before <- app$live$game
    session$setInputs(`play-board-request` = stale)
    expect_identical(app$live$revision, 1L)
    expect_identical(app$live$game, before)
    # An illegal action is rejected without mutation.
    session$setInputs(`play-board-request` = board_req(app$live, "m:B:1:2-W:1:1"))
    expect_identical(app$live$game, before)
    expect_identical(g3_position_key(app$live$game$state), key1)
  })
})

test_that("two sessions never share a game", {
  dir <- withr::local_tempdir()
  stores <- list()
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:1:1-W:1:3"))
    stores[[1]] <<- app$live
  })
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    stores[[2]] <<- app$live
    expect_identical(app$live$game$state$ply, 0L)
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:2:1-W:2:2"))
  })
  expect_false(identical(stores[[1]]$game$metadata$game_id, stores[[2]]$game$metadata$game_id))
  expect_identical(stores[[1]]$game$events$action_id, "m:W:1:1-W:1:3")
  expect_identical(stores[[2]]$game$events$action_id, "m:W:2:1-W:2:2")
})

test_that("a failed save is not acknowledged and leaves the game as it was", {
  dir <- withr::local_tempdir()
  shiny::testServer(app_server(dir, saver = function(game) stop("disk full")), {
    app <- session$userData$g3
    before <- app$live$game
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:1:1-W:1:3"))
    expect_identical(app$live$revision, 0L)
    expect_identical(app$live$game, before)
    expect_false(dir.exists(file.path(dir, "games")))
  })
})

test_that("presentation changes and section switches keep the game exactly", {
  dir <- withr::local_tempdir()
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:1:1-W:1:3"))
    game <- app$live$game
    key <- g3_position_key(game$state)
    ids <- g3_legal_actions(game$state)$action_id
    prefs <- list(
      list(name = "neutral_height", value = "lowest"), list(name = "view", value = "2d"),
      list(name = "exploded", value = TRUE), list(name = "xray", value = TRUE),
      list(name = "level", value = "N"), list(name = "camera", value = "black"),
      list(name = "view", value = "3d"), list(name = "exploded", value = FALSE),
      list(name = "neutral_height", value = "nonsense")
    )
    for (p in prefs) session$setInputs(`play-board-pref` = c(p, list(nonce = stats::runif(1))))
    expect_identical(app$prefs$neutral_height, "lowest")
    expect_identical(app$prefs$camera, "black")
    expect_identical(app$prefs$level, "N")
    expect_false(app$prefs$exploded)
    session$setInputs(`settings-theme` = "dark", `settings-reduced_motion` = TRUE,
                      `settings-default_view` = "2d", `settings-lang` = "de")
    expect_identical(app$prefs$theme, "dark")
    expect_true(app$prefs$reduced_motion)
    expect_identical(app$prefs$view, "2d")
    expect_identical(app$lang(), "de")
    for (sec in c("analyse", "database", "coach", "models", "training", "settings", "play")) {
      session$setInputs(nav = list(section = sec, focus = TRUE, nonce = stats::runif(1)))
      expect_identical(app$section(), sec)
    }
    session$setInputs(nav = list(section = "no-such-section", nonce = 1))
    expect_identical(app$section(), "play")
    expect_identical(app$live$game, game)
    expect_identical(app$live$revision, 1L)
    expect_identical(g3_position_key(app$live$game$state), key)
    expect_identical(g3_legal_actions(app$live$game$state)$action_id, ids)
    # Section outputs render in the chosen language without errors.
    expect_match(paste(unlist(output$`models-list`), collapse = ""), "builtin-search")
    expect_match(paste(unlist(output$`database-status`), collapse = ""), "Katalog")
  })
})

test_that("save, load and resume round-trip a game; bad files are refused", {
  dir <- withr::local_tempdir()
  saved_path <- NULL
  saved_hash <- NULL
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    for (a in c("m:W:1:1-W:1:3", "m:B:1:2-B:1:1", "m:W:0:0-W:1:2")) {
      session$setInputs(`play-board-request` = board_req(app$live, a))
    }
    expect_identical(app$live$revision, 3L)
    # Autosaved into the game catalogue; also written as a file for upload.
    expect_identical(app$live$receipt$backend, "db")
    saved_path <<- file.path(dir, "saved.g3.json")
    g3_write_game(app$live$game, saved_path)
    saved_hash <<- g3_game_hash(app$live$game)
    expect_match(paste(unlist(output$`play-files-status`), collapse = ""), "Saved")
  })
  expect_true(file.exists(saved_path))
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    # A fresh session offers the unfinished game for resuming.
    expect_match(paste(unlist(output$`play-resume`), collapse = ""), "ply 3")
    session$setInputs(`play-resume_latest` = 1)
    expect_identical(g3_game_hash(app$live$game), saved_hash)
    expect_identical(app$live$game$state$ply, 3L)
    # Load through the upload input as well.
    session$setInputs(`play-new_game` = 1, `play-mode` = "hotseat", `play-start_game` = 1)
    expect_identical(app$live$game$state$ply, 0L)
    session$setInputs(`play-files-upload` = upload(saved_path))
    expect_identical(g3_game_hash(app$live$game), saved_hash)
    good <- app$live$game
    # A file from a newer format version is refused with a clear message.
    raw <- jsonlite::fromJSON(saved_path, simplifyVector = FALSE)
    raw$format_version <- 7L
    newer <- file.path(dir, "newer.g3.json")
    writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null", digits = NA), newer)
    session$setInputs(`play-files-upload` = upload(newer))
    expect_identical(app$live$game, good)
    expect_match(paste(unlist(output$`play-files-upload_msg`), collapse = ""), "cannot be used")
    # A tampered file fails validation.
    raw <- jsonlite::fromJSON(saved_path, simplifyVector = FALSE)
    raw$events[[2]]$action_id <- "m:B:0:2-B:0:0"
    bad <- file.path(dir, "tampered.g3.json")
    writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null", digits = NA), bad)
    session$setInputs(`play-files-upload` = upload(bad))
    expect_identical(app$live$game, good)
    expect_match(paste(unlist(output$`play-files-upload_msg`), collapse = ""), "not a valid game")
  })
})

test_that("a loaded mate-in-one is finished through the board and then refuses moves", {
  dir <- withr::local_tempdir()
  path <- mate_in_one_file(dir)
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    session$setInputs(`play-files-upload` = upload(path))
    expect_identical(app$live$game$state$side, "black")
    session$setInputs(`play-board-request` = board_req(app$live, "m:B:0:0-B:3:0"))
    expect_identical(app$live$game$metadata$result, "black")
    expect_identical(app$live$game$metadata$termination, "checkmate")
    expect_match(paste(unlist(output$`play-gamebar`), collapse = ""), "Checkmate")
    rev <- app$live$revision
    session$setInputs(`play-board-request` = board_req(app$live, "m:B:0:3-B:1:3"))
    expect_identical(app$live$revision, rev)
    # Undo after a rule result is allowed (casual takeback, history kept).
    session$setInputs(`play-undo` = 1)
    expect_true(is.na(app$live$game$metadata$result))
    expect_identical(app$live$game$events$type[nrow(app$live$game$events)], "takeback")
  })
})

test_that("resignation and agreed draws are explicit, confirmed session events", {
  dir <- withr::local_tempdir()
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:1:1-W:1:3"))
    session$setInputs(`play-draw` = 1)
    expect_true(is.na(app$live$game$metadata$result))
    session$setInputs(`play-draw_confirm` = 1)
    expect_identical(app$live$game$metadata$result, "draw")
    expect_identical(app$live$game$metadata$termination, "draw_agreed")
    expect_identical(.g3_undo_count(app$live$game), 0L)
    session$setInputs(`play-new_game` = 1, `play-mode` = "hotseat", `play-start_game` = 2)
    session$setInputs(`play-resign` = 1, `play-resign_confirm` = 1)
    expect_identical(app$live$game$metadata$result, "black")
  })
})

test_that("analysis works on a separate branch and never touches the live game", {
  dir <- withr::local_tempdir()
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    for (a in c("m:W:1:1-W:1:3", "m:B:1:2-B:1:1")) session$setInputs(`play-board-request` = board_req(app$live, a))
    live <- app$live$game
    key <- g3_position_key(live$state)
    session$setInputs(`analyse-open` = 1)
    b <- app$analysis_branch()
    expect_false(identical(b$game$metadata$game_id, live$metadata$game_id))
    expect_identical(b$game$metadata$branch_of, live$metadata$game_id)
    session$setInputs(`analyse-prev` = 1)
    expect_identical(app$analysis_viewed(), 1L)
    session$setInputs(`analyse-first` = 1)
    expect_identical(app$analysis_viewed(), 0L)
    session$setInputs(`analyse-last` = 1)
    # Play on in the branch.
    session$setInputs(`analyse-board-request` = board_req(b, "m:W:2:1-W:2:2"))
    expect_identical(b$game$state$ply, 3L)
    # Continue from ply 1: takes back two branch actions, keeps them logged.
    session$setInputs(`analyse-first` = 2, `analyse-next_ply` = 1)
    session$setInputs(`analyse-branch_here` = 1)
    expect_identical(b$game$state$ply, 1L)
    expect_true("takeback" %in% b$game$events$type)
    # The live game is exactly as before, and nothing was saved for the branch.
    expect_identical(app$live$game, live)
    expect_identical(g3_position_key(app$live$game$state), key)
    expect_false(file.exists(file.path(dir, "games", paste0(b$game$metadata$game_id, ".g3.json"))))
  })
})

test_that("the computer opponent moves through the dispatcher from a background job", {
  skip_on_cran()
  skip_if_not_installed("callr")
  dir <- withr::local_tempdir()
  shiny::testServer(app_server(dir), {
    app <- session$userData$g3
    session$setInputs(`play-new_game` = 1, `play-mode` = "computer", `play-human_color` = "white",
                      `play-agent` = "search", `play-budget` = "quick", `play-start_game` = 1)
    expect_identical(app$live$game$metadata$mode, "computer")
    expect_identical(app$live$game$metadata$opponent$color, "black")
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:1:1-W:1:3"))
    expect_identical(app$opponent$status, "thinking")
    expect_true(app$jobs$opponent)
    deadline <- Sys.time() + 90
    while (app$live$revision < 3L && Sys.time() < deadline) {
      Sys.sleep(0.25)
      session$elapse(300)
    }
    expect_identical(app$live$revision, 3L)
    last <- app$live$game$events[nrow(app$live$game$events), ]
    expect_identical(last$actor, "black")
    expect_identical(app$live$game$state$side, "white")
    expect_true(app$live$receipt$ok)
    expect_false(app$jobs$opponent)
    # Undo while the computer is thinking cancels it; no late move appears.
    session$setInputs(`play-board-request` = board_req(app$live, "m:W:2:1-W:2:2"))
    expect_identical(app$opponent$status, "thinking")
    session$setInputs(`play-undo` = 1)
    rev <- app$live$revision
    expect_identical(app$live$game$state$side, "white")
    for (i in 1:12) {
      Sys.sleep(0.25)
      session$elapse(300)
    }
    expect_identical(app$live$revision, rev)
    expect_identical(app$live$game$state$side, "white")
    expect_identical(length(app$opponent$draining), 0L)
  })
})

test_that("a failing opponent leaves the game playable and says why", {
  opp <- .g3_opponent_new()
  store <- .g3_new_store(.g3_build_new_game("computer", "black", "search", "quick"), "live")
  blocker <- withr::local_tempfile()
  writeLines("not a directory", blocker)
  .g3_opponent_start(opp, store, file.path(blocker, "jobs"))
  expect_identical(opp$status, "failed")
  expect_true(nzchar(opp$message))
  expect_identical(store$revision, 0L)
  expect_null(opp$job)
})
