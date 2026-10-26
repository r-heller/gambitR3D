# Phase 03: lossless native format, replay and rejection of bad records.

roundtrip <- function(game, ui = NULL) {
  path <- tempfile(fileext = ".g3.json")
  on.exit(unlink(path))
  g3_write_game(game, path, ui = ui)
  g3_read_game(path)
}

play <- function(state, ids) {
  game <- .g3_game_new(state)
  for (id in ids) game <- g3_game_apply(game, id)
  game
}

expect_same_game <- function(a, b) {
  expect_identical(g3_state_hash(a$state), g3_state_hash(b$state))
  expect_identical(nrow(a$events), nrow(b$events))
  expect_identical(a$metadata$result, b$metadata$result)
  expect_identical(g3_game_hash(a), g3_game_hash(b))
}

test_that("initial position and a played sequence round-trip exactly", {
  g0 <- g3_new_game(players = list(white = "Ada", black = "Ben"))
  expect_same_game(roundtrip(g0), g0)
  set.seed(11)
  g <- g0
  for (i in 1:24) {
    if (g3_is_terminal(g$state)) break
    a <- g3_legal_actions(g$state)
    g <- g3_game_apply(g, a$action_id[sample.int(nrow(a), 1L)])
  }
  back <- roundtrip(g)
  expect_same_game(back, g)
  expect_identical(back$metadata$players$white, "Ada")
  expect_true(g3_validate_game(back))
})

test_that("special actions survive export and replay", {
  # En passant (see engine tests for the hand analysis).
  ep_state <- g3_setup_position(transform(pcs(
    "wP P white W:1:1", "wK K white W:0:0", "bP P black N:2:1", "bK K black B:3:3"
  ), has_moved = c(FALSE, TRUE, TRUE, TRUE)))
  g_ep <- play(ep_state, c("m:W:1:1-W:1:3", "m:N:2:1-N:1:0"))
  expect_same_game(roundtrip(g_ep), g_ep)
  # Both castling forms.
  base <- pcs("wK K white WK:0:0", "wRa0 R white WQ:0:0", "wRf0 R white WK:1:0", "bK K black B:0:3")
  rights <- c(white_kingside = TRUE, white_queenside = TRUE)
  for (cs in c("c:white:kingside", "c:white:queenside")) {
    g <- play(g3_setup_position(base, castling = rights), cs)
    expect_same_game(roundtrip(g), g)
  }
  # Promotion choices, and a platform half-turn carrying a pawn to promotion.
  for (pr in c("Q", "R", "B", "N")) {
    g <- play(g3_setup_position(pcs("wP P white B:1:2", "wK K white W:0:0", "bK K black BQ:0:1")),
              paste0("m:B:1:2-B:1:3=", pr))
    back <- roundtrip(g)
    expect_identical(back$state$pieces$type[back$state$pieces$id == "wP"], pr)
  }
  st <- g3_setup_position(pcs("wP P white WQ:0:1", "wK K white W:0:0", "bK K black B:3:3"),
                          docks = c(WQ = "N:L:hi", BQ = "N:R:hi"))
  g <- play(st, c("t:WQ>B:L:hi@180", "m:B:3:3-B:2:3", "m:WQ:0:1-WQ:0:0=Q"))
  back <- roundtrip(g)
  expect_same_game(back, g)
  expect_identical(back$state$boards$orientation[back$state$boards$id == "WQ"], 180L)
})

test_that("session events: resignation, agreed draw, truncation and takeback", {
  g <- g3_new_game()
  g <- g3_game_apply(g, "m:W:0:1-W:0:2")
  g <- g3_game_apply(g, "m:B:0:2-B:0:1")
  tb <- g3_game_event(g, "takeback")
  expect_identical(tb$state$ply, 1L)
  expect_identical(nrow(tb$events), 3L) # history is kept
  expect_same_game(roundtrip(tb), tb)
  r1 <- g3_game_event(g, "resign")
  r2 <- g3_game_event(g, "draw_agreed")
  expect_identical(r1$metadata$result, "black")
  expect_identical(r2$metadata$result, "draw")
  # Same moves, different adjudication: different content hashes.
  expect_false(identical(g3_game_hash(r1), g3_game_hash(r2)))
  tr <- g3_game_event(g, "truncated", reason = "max_plies")
  expect_identical(tr$metadata$result, "truncated")
  expect_false(g3_is_terminal(tr$state)) # truncation is not a rule outcome
  expect_same_game(roundtrip(tr), tr)
  expect_error(g3_game_apply(r1, "m:W:0:2-W:0:3"), class = "g3_illegal_action")
})

test_that("hashes ignore indentation, key order and the ui envelope", {
  g <- g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2")
  path <- tempfile(fileext = ".g3.json")
  g3_write_game(g, path, ui = list(view = "3d", neutral_height = "lowest"))
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  # Reorder top-level keys and drop whitespace.
  raw2 <- raw[rev(names(raw))]
  raw2$ui <- NULL
  path2 <- tempfile(fileext = ".g3.json")
  writeLines(as.character(jsonlite::toJSON(raw2, auto_unbox = TRUE, digits = NA, null = "null", na = "null")), path2)
  a <- g3_read_game(path)
  b <- g3_read_game(path2)
  expect_identical(g3_game_hash(a), g3_game_hash(b))
  expect_identical(g3_position_key(a$state), g3_position_key(b$state))
  expect_identical(a$ui$neutral_height, "lowest")
  expect_null(b$ui)
  expect_identical(g3_legal_actions(a$state)$action_id, g3_legal_actions(b$state)$action_id)
})

test_that("malformed and mismatched records are rejected at the first bad event", {
  g <- g3_new_game()
  for (id in c("m:W:0:1-W:0:2", "m:B:0:2-B:0:1", "m:W:1:1-W:1:2")) g <- g3_game_apply(g, id)
  path <- tempfile(fileext = ".g3.json")
  g3_write_game(g, path)
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  write_raw <- function(x) {
    p <- tempfile(fileext = ".g3.json")
    writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null", na = "null")), p)
    p
  }
  # Illegal action at event 2.
  bad <- raw
  bad$events[[2]]$action_id <- "m:B:0:2-B:0:0"
  err <- tryCatch(g3_read_game(write_raw(bad)), g3_error = function(e) e)
  expect_s3_class(err, "g3_invalid_state")
  expect_identical(err$context$first_invalid_event, 2L)
  # Wrong rules hash.
  bad <- raw
  bad$rules$hash <- strrep("0", 64)
  expect_error(g3_read_game(write_raw(bad)), class = "g3_unsupported_rules")
  # Altered rights in the initial state.
  bad <- raw
  bad$initial_state$rights$castling$white_kingside <- FALSE
  expect_error(g3_read_game(write_raw(bad)), class = "g3_invalid_state")
  # Corrupted post-state checksum.
  bad <- raw
  bad$events[[3]]$post_state_hash <- strrep("a", 64)
  expect_error(g3_read_game(write_raw(bad)), class = "g3_invalid_state")
  # Unknown event type.
  bad <- raw
  bad$events[[1]]$type <- "teleport"
  expect_error(g3_read_game(write_raw(bad)), class = "g3_invalid_state")
  # Duplicate piece ids.
  bad <- raw
  bad$initial_state$pieces[[2]]$id <- bad$initial_state$pieces[[1]]$id
  expect_error(g3_read_game(write_raw(bad)), class = "g3_invalid_state")
  # A future format version is refused with an actionable message.
  bad <- raw
  bad$format_version <- 9L
  expect_error(g3_read_game(write_raw(bad)), "Update the package")
  # Truncated file, non-JSON and non-finite numbers.
  p <- tempfile(fileext = ".g3.json")
  txt <- readChar(path, file.info(path)$size)
  writeChar(substr(txt, 1L, nchar(txt) %/% 2L), p, eos = NULL)
  expect_error(g3_read_game(p), class = "g3_io_error")
  bad <- raw
  bad$initial_state$ply <- "NaN"
  expect_error(g3_read_game(write_raw(bad)), class = "g3_invalid_state")
  # Excessive nesting.
  deep <- list()
  for (i in 1:40) deep <- list(deep)
  bad <- raw
  bad$annotations <- deep
  expect_error(g3_read_game(write_raw(bad)), class = "g3_io_error")
})

test_that("non-strict reading ignores checksums but still enforces legality", {
  g <- g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2")
  path <- tempfile(fileext = ".g3.json")
  g3_write_game(g, path)
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  raw$events[[1]]$post_state_hash <- "x"
  p <- tempfile(fileext = ".g3.json")
  writeLines(as.character(jsonlite::toJSON(raw, auto_unbox = TRUE, digits = NA, null = "null", na = "null")), p)
  expect_error(g3_read_game(p), class = "g3_invalid_state")
  expect_identical(g3_read_game(p, strict = FALSE)$state$ply, 1L)
})

test_that("an interrupted write leaves no partial file and overwrite is explicit", {
  g <- g3_new_game()
  path <- tempfile(fileext = ".g3.json")
  g3_write_game(g, path)
  expect_error(g3_write_game(g, path), class = "g3_io_error")
  expect_silent(g3_write_game(g, path, overwrite = TRUE))
  expect_length(list.files(dirname(path), pattern = "^\\.g3tmp-"), 0L)
  expect_error(g3_write_game(g, file.path(tempfile(), "x.g3.json")), class = "g3_io_error")
})

test_that("position files round-trip and mark edited history", {
  st <- g3_setup_position(pcs("wK K white W:0:0", "wR R white W:2:0", "bK K black B:3:3"))
  p <- tempfile(fileext = ".g3pos.json")
  g3_write_position(st, p, history = "edited")
  back <- g3_read_position(p)
  expect_identical(attr(back, "history"), "edited")
  expect_identical(g3_state_hash(back), g3_state_hash(st))
})

test_that("replay to any ply and the move table", {
  g <- g3_new_game()
  for (id in c("m:W:0:1-W:0:2", "m:B:0:2-B:0:1")) g <- g3_game_apply(g, id)
  expect_identical(g3_replay_game(g, 1L)$ply, 1L)
  expect_identical(g3_state_hash(g3_replay_game(g)), g3_state_hash(g$state))
  mv <- g3_game_moves(g)
  expect_identical(mv$action_id, c("m:W:0:1-W:0:2", "m:B:0:2-B:0:1"))
  expect_match(mv$notation[1], "W:b2-W:b3")
  expect_error(g3_replay_game(g, 5L), class = "g3_invalid_state")
})
