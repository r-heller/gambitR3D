# Phase 05/07: the session store and the single action dispatcher, tested
# without Shiny. (The Shiny wiring is tested in test-app-server.R.)

dispatch_req <- function(store, action_id, request_id = .g3_new_id("t"), ...) {
  tok <- .g3_store_token(store)
  utils::modifyList(list(kind = "action", game_id = tok$game_id, revision = tok$revision,
                         request_id = request_id, expected_state_hash = tok$state_hash,
                         action_id = action_id), list(...))
}

counting_saver <- function(dir) {
  env <- new.env()
  env$n <- 0L
  env$fun <- function(game) {
    env$n <- env$n + 1L
    path <- file.path(dir, paste0(game$metadata$game_id, ".g3.json"))
    g3_write_game(game, path, overwrite = TRUE)
    list(ok = TRUE, backend = "file", path = path, time = .g3_now())
  }
  env
}

test_that("a legal request is validated, saved, then committed exactly once", {
  dir <- withr::local_tempdir()
  sv <- counting_saver(dir)
  store <- .g3_new_store(g3_new_game(), "live", saver = sv$fun)
  r <- dispatch_req(store, "m:W:1:1-W:1:3", request_id = "req-1")
  ack <- .g3_dispatch(store, r)
  expect_identical(ack$status, "committed")
  expect_identical(store$revision, 1L)
  expect_identical(sv$n, 1L)
  saved <- g3_read_game(file.path(dir, paste0(store$game$metadata$game_id, ".g3.json")))
  expect_identical(g3_game_hash(saved), g3_game_hash(store$game))
  # The same request id again: idempotent, nothing changes, no second save.
  again <- .g3_dispatch(store, r)
  expect_identical(again$status, "duplicate")
  expect_identical(again$original_status, "committed")
  expect_identical(store$revision, 1L)
  expect_identical(store$commits, 1L)
  expect_identical(sv$n, 1L)
})

test_that("stale, wrong-game and mismatched-hash requests are rejected without mutation", {
  store <- .g3_new_store(g3_new_game(), "live")
  r0 <- dispatch_req(store, "m:W:1:1-W:1:3")
  expect_identical(.g3_dispatch(store, r0)$status, "committed")
  before <- store$game
  # Old revision (a delayed click or computer answer).
  stale <- r0
  stale$request_id <- "late"
  stale$action_id <- "m:B:1:2-B:1:1"
  a1 <- .g3_dispatch(store, stale)
  expect_identical(a1[c("status", "reason")], list(status = "rejected", reason = "stale_revision"))
  # Current revision but another game's id.
  other <- dispatch_req(store, "m:B:1:2-B:1:1", game_id = "game_other")
  expect_identical(.g3_dispatch(store, other)$reason, "wrong_game")
  # Current revision but a wrong expected state hash.
  bad_hash <- dispatch_req(store, "m:B:1:2-B:1:1", expected_state_hash = strrep("0", 64))
  expect_identical(.g3_dispatch(store, bad_hash)$reason, "stale_state")
  # An illegal action.
  illegal <- dispatch_req(store, "m:B:1:2-W:1:1")
  expect_identical(.g3_dispatch(store, illegal)$reason, "illegal_action")
  # A request without an id.
  noid <- dispatch_req(store, "m:B:1:2-B:1:1")
  noid$request_id <- NULL
  expect_identical(.g3_dispatch(store, noid)$reason, "invalid_request")
  expect_identical(store$revision, 1L)
  expect_identical(store$game, before)
  # A rejected id stays rejected when repeated.
  expect_identical(.g3_dispatch(store, stale)$original_status, "rejected")
})

test_that("concurrent intents for one revision commit at most once", {
  store <- .g3_new_store(g3_new_game(), "live")
  tok <- .g3_store_token(store)
  reqs <- lapply(c("m:W:1:1-W:1:3", "m:W:2:1-W:2:2", "m:W:0:0-W:1:2"), function(a) {
    list(kind = "action", game_id = tok$game_id, revision = tok$revision,
         request_id = .g3_new_id("c"), expected_state_hash = tok$state_hash, action_id = a)
  })
  acks <- lapply(reqs, function(r) .g3_dispatch(store, r))
  expect_identical(vapply(acks, `[[`, "", "status"), c("committed", "rejected", "rejected"))
  expect_identical(store$revision, 1L)
  expect_identical(nrow(store$game$events), 1L)
})

test_that("a failed save leaves the game unchanged and is not acknowledged", {
  store <- .g3_new_store(g3_new_game(), "live", saver = function(game) stop("disk full"))
  before <- store$game
  ack <- .g3_dispatch(store, dispatch_req(store, "m:W:1:1-W:1:3"))
  expect_identical(ack$status, "rejected")
  expect_identical(ack$reason, "save_failed")
  expect_match(ack$message, "disk full")
  expect_identical(store$revision, 0L)
  expect_identical(store$game, before)
  # A saver reporting failure (rather than throwing) is treated the same.
  store$saver <- function(game) list(ok = FALSE, message = "read-only folder")
  ack2 <- .g3_dispatch(store, dispatch_req(store, "m:W:1:1-W:1:3"))
  expect_identical(ack2$reason, "save_failed")
  expect_identical(store$game, before)
})

test_that("session events go through the dispatcher and keep history", {
  store <- .g3_new_store(g3_new_game(), "live")
  .g3_dispatch(store, dispatch_req(store, "m:W:1:1-W:1:3"))
  .g3_dispatch(store, dispatch_req(store, "m:B:1:2-B:1:1"))
  tb <- .g3_dispatch(store, .g3_store_request(store, "event", event = "takeback", n = 1L))
  expect_identical(tb$status, "committed")
  expect_identical(store$game$state$ply, 1L)
  expect_identical(nrow(store$game$events), 3L)
  expect_identical(store$game$events$type[3], "takeback")
  rs <- .g3_dispatch(store, .g3_store_request(store, "event", event = "resign", actor = "black"))
  expect_identical(rs$status, "committed")
  expect_identical(store$game$metadata$result, "white")
  after <- .g3_dispatch(store, dispatch_req(store, "m:B:1:2-B:1:1"))
  expect_identical(after$reason, "game_over")
  expect_identical(.g3_dispatch(store, .g3_store_request(store, "event", event = "explode"))$reason, "invalid_request")
})

test_that("a computer answer is applied only while its tag is current", {
  store <- .g3_new_store(g3_new_game(), "live")
  tag <- .g3_compute_tag(store)
  expect_true(.g3_tag_current(tag, store))
  .g3_dispatch(store, dispatch_req(store, "m:W:1:1-W:1:3"))
  expect_false(.g3_tag_current(tag, store))
  # A late answer for the old position is refused by the dispatcher too.
  res <- list(tag = tag, job_id = "job_x", decision = list(action_id = "m:W:2:1-W:2:2"))
  ack <- .g3_dispatch(store, .g3_opponent_request(res))
  expect_identical(ack$reason, "stale_revision")
  # Undo back to the same position still invalidates it (new revision).
  .g3_dispatch(store, .g3_store_request(store, "event", event = "takeback", n = 1L))
  expect_identical(g3_position_key(store$game$state), tag$position_key)
  expect_false(.g3_tag_current(tag, store))
})

test_that("undo counts follow the mode", {
  g <- g3_new_game()
  g$metadata$mode <- "hotseat"
  expect_identical(.g3_undo_count(g), 0L)
  g <- g3_game_apply(g, "m:W:1:1-W:1:3")
  expect_identical(.g3_undo_count(g), 1L)
  cpu <- .g3_build_new_game("computer", "white", "search", "quick")
  expect_identical(.g3_opponent_settings(cpu)$color, "black")
  expect_identical(cpu$metadata$opponent$nodes, unname(.G3_APP_BUDGETS[["quick"]]))
  expect_identical(.g3_undo_count(cpu), 0L)
  cpu <- g3_game_apply(cpu, "m:W:1:1-W:1:3")
  expect_true(.g3_computer_to_act(cpu))
  expect_identical(.g3_undo_count(cpu), 1L)
  cpu <- g3_game_apply(cpu, "m:B:1:2-B:1:1")
  expect_identical(.g3_undo_count(cpu), 2L)
  cpu <- g3_game_event(cpu, "resign", actor = "white")
  expect_identical(.g3_undo_count(cpu), 0L)
  # The human playing Black cannot undo the computer's opening move.
  cpu_b <- g3_game_apply(.g3_build_new_game("computer", "black", "random", "quick"), "m:W:1:1-W:1:3")
  expect_identical(.g3_undo_count(cpu_b), 0L)
})

test_that("the storage service saves, lists and reads games and positions", {
  dir <- withr::local_tempdir()
  st <- .g3_storage_service(dir)
  expect_identical(st$backend, "file")
  expect_identical(nrow(st$list_games()), 0L)
  g <- g3_game_apply(g3_new_game(), "m:W:1:1-W:1:3")
  g$metadata$mode <- "hotseat"
  rec <- st$save(g, ui = list(view = "3d", neutral_height = "lowest"))
  expect_true(rec$ok)
  lst <- st$list_games()
  expect_identical(lst$game_id, g$metadata$game_id)
  back <- st$read(rec$path)
  expect_identical(g3_game_hash(back), g3_game_hash(g))
  # Presentation preferences live in the separate ui envelope only.
  expect_identical(back$ui$neutral_height, "lowest")
  expect_identical(g3_position_key(back$state), g3_position_key(g$state))
  pos <- file.path(dir, "p.g3pos.json")
  g3_write_position(g$state, pos)
  pg <- st$read(pos)
  expect_s3_class(pg, "g3_game")
  expect_identical(g3_position_key(pg$state), g3_position_key(g$state))
  # Unsupported and invalid files are refused with typed conditions.
  newer <- jsonlite::fromJSON(rec$path, simplifyVector = FALSE)
  newer$format_version <- 99L
  p99 <- file.path(dir, "newer.g3.json")
  writeLines(jsonlite::toJSON(newer, auto_unbox = TRUE, null = "null"), p99)
  expect_error(st$read(p99), class = "g3_unsupported_rules")
  junk <- file.path(dir, "junk.json")
  writeLines('{"format": "something-else"}', junk)
  expect_error(st$read(junk), class = "g3_invalid_state")
  # A database path without the database API falls back to files, honestly.
  if (!.g3_api_available("g3_save_game")) {
    st2 <- .g3_storage_service(dir, db_path = file.path(dir, "x.sqlite"))
    expect_identical(st2$backend, "file")
    expect_match(st2$note, "not available")
  }
})

test_that("analysis branch states equal g3_replay_game() at every ply", {
  g <- g3_new_game()
  for (a in c("m:W:1:1-W:1:3", "m:B:1:2-B:1:1", "m:W:0:0-W:1:2", "m:B:3:3-B:2:1")) g <- g3_game_apply(g, a)
  b <- .g3_new_store(.g3_branch_from(g), "analysis")
  expect_false(identical(b$game$metadata$game_id, g$metadata$game_id))
  expect_identical(b$game$metadata$branch_of, g$metadata$game_id)
  for (p in c(4L, 2L, 0L, 3L, 1L)) {
    expect_identical(g3_state_hash(.g3_branch_state(b, p)), g3_state_hash(g3_replay_game(b$game, p)))
  }
  # Continuing from ply 2 takes back two actions in the branch only.
  ack <- .g3_dispatch(b, .g3_store_request(b, "event", event = "takeback", n = 2L))
  expect_identical(ack$status, "committed")
  expect_identical(g3_state_hash(.g3_branch_state(b, 2L)), g3_state_hash(g3_replay_game(g, 2L)))
  expect_identical(g$state$ply, 4L)
})

test_that("with a database path the storage service saves to and resumes from the database", {
  skip_if_not(.g3_api_available("g3_save_game") && .g3_api_available("g3_load_game"), "no database API in this build")
  skip_if_not_installed("RSQLite")
  dir <- withr::local_tempdir()
  st <- .g3_storage_service(dir, db_path = file.path(dir, "games.sqlite"))
  withr::defer(st$close())
  expect_identical(st$backend, "db")
  store <- .g3_new_store(g3_new_game(), "live", saver = function(game) st$save(game))
  ack <- .g3_dispatch(store, dispatch_req(store, "m:W:1:1-W:1:3"))
  expect_identical(ack$status, "committed")
  expect_identical(store$receipt$backend, "db")
  lst <- st$list_games()
  expect_identical(lst$game_id, store$game$metadata$game_id)
  expect_identical(lst$ply, 1L)
  back <- st$read(lst$path[1])
  expect_identical(g3_game_hash(back), g3_game_hash(store$game))
  expect_false(dir.exists(file.path(dir, "games")))
})

test_that("the board message carries legal actions, castling and platform results from the engine", {
  st0 <- g3_new_game()$state
  st <- g3_setup_position(st0$pieces[, c("id", "type", "color", "square_id")],
                          castling = c(white_kingside = TRUE))
  tok <- list(game_id = "g", revision = 0L, state_hash = g3_state_hash(st))
  p <- .g3_board_payload(st, tok, list(neutral_height = "lowest", exploded = FALSE))
  ids <- vapply(p$actions, `[[`, "", "id")
  expect_identical(sort(ids), sort(g3_legal_actions(st)$action_id))
  cs <- p$actions[[match("c:white:kingside", ids)]]
  expect_identical(cs[c("type", "from", "to", "castle")],
                   list(type = "castle", from = "WK:0:0", to = "WK:1:0", castle = "kingside"))
  expect_length(p$squares, 64L)
  expect_identical(p$geometry$neutral_height, "lowest")
  expect_identical(p$geometry$edge_gap, 2)
  # A platform action says where the carried piece will stand.
  rook <- g3_setup_position(pcs("wK K white W:1:0", "wR R white WK:0:0", "bK K black B:2:3"))
  q <- .g3_board_payload(rook, list(game_id = "g", revision = 0L, state_hash = ""), list())
  pa <- Filter(function(a) identical(a$id, "t:WK>N:R:lo@180"), q$actions)[[1]]
  after <- g3_apply_action(rook, pa$id)$state
  sq <- .g3_scene_squares(after)
  expect_identical(pa$result_coord, sq$coord[sq$square_id == "WK:0:0"])
  expect_identical(pa$result_orientation, 180L)
})
