# Phase 02: termination, draws and their precedence.

test_that("stalemate: no legal action without check", {
  # Black king f9 and black pawn f8 share BK (two occupants: immobile
  # platform). The pawn has no square at f7 and nothing to capture on e7.
  # A white rook on N:e3 attacks e8 (B:e8 and BK:e8) and e9 up the empty
  # e-file but not f9. The king's only neighbours are e8, e9 and its own
  # pawn on f8: no legal action, no check.
  st <- g3_setup_position(
    pcs("bK K black BK:1:1", "bP P black BK:1:0", "wR R white N:3:0", "wK K white W:0:0"),
    side = "black"
  )
  expect_true(g3_is_terminal(st))
  expect_identical(st$outcome$reason, "stalemate")
  expect_identical(st$outcome$result, "draw")
})

test_that("bare kings end the game in a draw", {
  # White king captures the last black piece: only kings remain.
  st <- g3_setup_position(pcs("wK K white W:0:0", "bN N black W:1:1", "bK K black B:3:3"))
  step <- g3_apply_action(st, "m:W:0:0-W:1:1")
  expect_identical(step$state$outcome$reason, "bare_kings")
  expect_identical(step$state$outcome$result, "draw")
  expect_true(g3_is_terminal(step$state))
  expect_identical(nrow(g3_legal_actions(step$state)), 0L)
  expect_error(g3_apply_action(step$state, "m:B:3:3-B:2:3"), class = "g3_illegal_action")
})

test_that("threefold repetition is an automatic draw", {
  # Knights e1-d3-e1 and b8-c6-b8: the start recurs after 4 and 8 plies.
  st <- g3_setup_position(pcs("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"))
  cycle <- c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3")
  s <- st
  for (id in cycle) s <- g3_apply_action(s, id)$state
  expect_false(g3_is_terminal(s))
  for (id in cycle) s <- g3_apply_action(s, id)$state
  expect_identical(s$outcome$reason, "repetition")
  expect_identical(s$outcome$result, "draw")
})

test_that("100 plies without capture or pawn action is a draw; pawn transport resets", {
  st <- g3_setup_position(
    pcs("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"),
    halfmove = 99L
  )
  s <- g3_apply_action(st, "m:W:3:0-W:2:2")$state
  expect_identical(s$outcome$reason, "halfmove")
  # A pawn carried by a platform resets the counter.
  st2 <- g3_setup_position(
    pcs("wK K white W:0:0", "wP P white WQ:0:1", "bK K black B:3:3"),
    halfmove = 99L
  )
  s2 <- g3_apply_action(st2, "t:WQ>W:L:hi@0")$state
  expect_identical(s2$clocks$halfmove, 0L)
  expect_false(g3_is_terminal(s2))
})

test_that("checkmate on the 100th ply takes precedence over the halfmove draw", {
  # Black queen b5 -> e5 checks e0 down the empty e-file. Rook d6 covers
  # d1, rook f9 covers f0/f1/f3/f5, the queen covers e1, e2, e4 and f5.
  # The WK platform (king alone) has no safe destination; W:L:lo is
  # occupied by the empty WQ platform. Counter 99 -> 100 with mate.
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black", halfmove = 99L)
  s <- g3_apply_action(st, "m:B:0:0-B:3:0")
  expect_identical(s$state$outcome$reason, "checkmate")
  expect_identical(s$state$outcome$result, "black")
  expect_match(s$event$notation, "#$")
})

test_that("the starting position is not terminal", {
  st <- g3_new_game()$state
  expect_false(g3_is_terminal(st))
  expect_identical(st$outcome$status, "ongoing")
})
