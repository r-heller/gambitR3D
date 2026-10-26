# Phase 06: numeric checks of the display transform (GEOMETRY_CONTRACT.md).
# These mirror the independent fixture scripts/check_geometry.R of the
# specification, and add the platform, exploded and invariance checks. They
# complement, and do not replace, the browser tests.

board_row <- function(b, id) b[b$board == id, ]

test_that("main boards have the contract's centres and extents", {
  st <- g3_new_game()$state
  b <- .g3_scene_boards(st)
  expect_equal(board_row(b, "W")[, c("x", "z")], data.frame(x = 0, z = 3), ignore_attr = TRUE)
  expect_equal(board_row(b, "N")[, c("x", "z")], data.frame(x = 0, z = 0), ignore_attr = TRUE)
  expect_equal(board_row(b, "B")[, c("x", "z")], data.frame(x = 0, z = -3), ignore_attr = TRUE)
  expect_equal(unlist(board_row(b, "W")[, c("z_min", "z_max")]), c(z_min = 1, z_max = 5))
  expect_equal(unlist(board_row(b, "N")[, c("z_min", "z_max")]), c(z_min = -2, z_max = 2))
  expect_equal(unlist(board_row(b, "B")[, c("z_min", "z_max")]), c(z_min = -5, z_max = -1))
  for (id in c("W", "N", "B")) {
    expect_equal(unlist(board_row(b, id)[, c("x_min", "x_max")]), c(x_min = -2, x_max = 2))
  }
})

test_that("the facing W and B edges are exactly two square-widths apart", {
  st <- g3_new_game()$state
  for (h in c("highest", "lowest")) {
    expect_identical(.g3_scene_edge_gap(.g3_scene_boards(st, h)), 2)
  }
  # The same check as the specification's independent fixture: rank centres
  # of every main board transformed one by one.
  min_ranks <- c(W = 1, N = 3, B = 5)
  actual <- vapply(names(min_ranks), function(p) {
    mean(.g3_scene_transform(0, min_ranks[[p]] + 0:3, p, FALSE)$z)
  }, numeric(1))
  expect_identical(unname(actual), c(3, 0, -3))
})

test_that("both Neutral heights keep W and B level and N highest or lowest", {
  st <- g3_new_game()$state
  hi <- .g3_scene_boards(st, "highest")
  lo <- .g3_scene_boards(st, "lowest")
  expect_identical(board_row(hi, "W")$y, board_row(hi, "B")$y)
  expect_identical(board_row(lo, "W")$y, board_row(lo, "B")$y)
  expect_identical(c(board_row(hi, "W")$y, board_row(hi, "N")$y), c(0, 3.4))
  expect_identical(c(board_row(lo, "W")$y, board_row(lo, "N")$y), c(3.4, 0))
  # Platforms sit 0.9 above their parent in both modes.
  expect_identical(board_row(hi, "WQ")$y, 0.9)
  expect_identical(board_row(lo, "WQ")$y, 3.4 + 0.9)
  # Horizontal placement does not depend on height.
  expect_identical(hi[, c("x", "z", "x_min", "x_max", "z_min", "z_max")],
                   lo[, c("x", "z", "x_min", "x_max", "z_min", "z_max")])
})

test_that("starting platform pairs are directly opposite at equal heights", {
  st <- g3_new_game()$state
  for (h in c("highest", "lowest")) {
    b <- .g3_scene_boards(st, h)
    expect_equal(unlist(board_row(b, "WQ")[, c("x", "z")]), c(x = -2, z = 5))
    expect_equal(unlist(board_row(b, "BQ")[, c("x", "z")]), c(x = -2, z = -5))
    expect_equal(unlist(board_row(b, "WK")[, c("x", "z")]), c(x = 2, z = 5))
    expect_equal(unlist(board_row(b, "BK")[, c("x", "z")]), c(x = 2, z = -5))
    expect_identical(board_row(b, "WQ")$y, board_row(b, "BQ")$y)
    expect_identical(board_row(b, "WK")$y, board_row(b, "BK")$y)
  }
})

test_that("tiles keep unit size and files share x across all boards", {
  st <- g3_new_game()$state
  for (h in c("highest", "lowest")) for (ex in c(FALSE, TRUE)) {
    s <- .g3_scene_squares(st, h, ex)
    expect_identical(nrow(s), 64L)
    expect_true(all(s$size == 1))
    expect_identical(s$x, s$file - 2.5)
    # Neighbouring cells of one board are one unit apart in x or z.
    for (id in unique(s$board)) {
      cells <- s[s$board == id, ]
      d <- as.matrix(stats::dist(cells[, c("x", "z")]))
      expect_identical(min(d[d > 0]), 1)
    }
    # Each board's tiles share one height.
    expect_true(all(tapply(s$y, s$board, function(y) length(unique(y))) == 1L))
  }
})

test_that("display coordinates keep the logical coordinates of the engine", {
  st <- g3_new_game()$state
  tabs <- .g3_tables()
  core <- .g3_core(st, tabs)
  co <- .g3_square_coords(core$dock, core$orient, tabs)
  s <- .g3_scene_squares(st, "lowest", TRUE)
  expect_identical(s$square_id, tabs$sq_id)
  expect_identical(s$file, co$file)
  expect_identical(s$rank, co$rank)
  # Light and dark squares alternate by projected parity; b1 is light.
  expect_identical(s$shade[s$square_id == "W:0:0"], "light")
  expect_identical(sum(s$shade == "dark"), 32L)
})

test_that("a platform moved to N follows N's transform in both height modes", {
  # White's only escape from check is WK to N:R:lo (see test-search.R).
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bN N black W:2:1", "bR1 R black W:0:0", "bR2 R black BK:1:1", "bK K black B:0:3"
  ))
  moved <- g3_apply_action(st, "t:WK>N:R:lo@0")$state
  for (h in c("highest", "lowest")) {
    b <- .g3_scene_boards(moved, h)
    heights <- .g3_parent_heights(h)
    wk <- board_row(b, "WK")
    expect_identical(wk$parent, "N")
    expect_identical(wk$y, heights[["N"]] + 0.9)
    # N:R:lo spans files e-f and ranks 2-3: centre x 2, z 4.5 - 2.5 + 0.
    expect_equal(c(wk$x, wk$z), c(2, 2))
    # The squares under the king moved with the platform; their ids did not.
    s <- .g3_scene_squares(moved, h)
    expect_identical(s$parent[s$square_id == "WK:0:0"], "N")
    expect_identical(s$y[s$square_id == "WK:0:0"], heights[["N"]] + 0.9)
  }
  # Exploded spacing moves the platform with its new parent, not with W.
  ex <- .g3_scene_boards(moved, "highest", exploded = TRUE)
  expect_identical(board_row(ex, "WK")$z, 2)
  expect_identical(board_row(ex, "WQ")$z, 5 + 2.25)
})

# A rook alone on WK can relocate the platform, kept or turned.
rook_on_wk <- function() {
  g3_setup_position(pcs("wK K white W:1:0", "wR R white WK:0:0", "bK K black B:2:3"))
}

test_that("a half-turned platform keeps its place and turns its cells", {
  st <- rook_on_wk()
  turned <- g3_apply_action(st, "t:WK>N:R:lo@180")$state
  s0 <- .g3_scene_squares(g3_apply_action(st, "t:WK>N:R:lo@0")$state)
  s1 <- .g3_scene_squares(turned)
  ids <- paste0("WK:", c(0, 1, 0, 1), ":", c(0, 0, 1, 1))
  a <- s0[match(ids, s0$square_id), c("x", "z")]
  b <- s1[match(ids, s1$square_id), c("x", "z")]
  # A 180-degree turn about the platform centre maps (dx, dz) to (-dx, -dz).
  expect_equal(colMeans(a), colMeans(b))
  expect_equal(as.matrix(b) - matrix(colMeans(b), 4, 2, byrow = TRUE),
               -(as.matrix(a) - matrix(colMeans(a), 4, 2, byrow = TRUE)), ignore_attr = TRUE)
  expect_identical(s1$shade[match(ids, s1$square_id)], s0$shade[match(ids, s0$square_id)])
})

test_that("the exploded view is z-only and resetting restores the gap", {
  st <- g3_new_game()$state
  plain <- .g3_scene_boards(st)
  ex <- .g3_scene_boards(st, exploded = TRUE)
  expect_identical(ex$x, plain$x)
  expect_identical(ex$y, plain$y)
  expect_identical(board_row(ex, "W")$z, 3 + 2.25)
  expect_identical(board_row(ex, "N")$z, 0)
  expect_identical(board_row(ex, "B")$z, -3 - 2.25)
  expect_identical(.g3_scene_edge_gap(ex), 2 + 4.5)
  reset <- .g3_scene_boards(st, exploded = FALSE)
  expect_identical(reset, plain)
  expect_identical(.g3_scene_edge_gap(reset), 2)
})

test_that("display options never change keys, hashes or legal actions", {
  st <- rook_on_wk()
  for (state in list(g3_new_game()$state, g3_apply_action(st, "t:WK>N:R:lo@180")$state)) {
    key <- g3_position_key(state)
    hash <- g3_state_hash(state)
    ids <- g3_legal_actions(state)$action_id
    for (h in c("highest", "lowest")) for (ex in c(FALSE, TRUE)) {
      .g3_scene_squares(state, h, ex)
      .g3_scene_boards(state, h, ex)
      expect_identical(g3_position_key(state), key)
      expect_identical(g3_state_hash(state), hash)
      expect_identical(g3_legal_actions(state)$action_id, ids)
    }
  }
})
