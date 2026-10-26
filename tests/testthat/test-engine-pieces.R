# Phase 02: piece movement, worked out by hand from the profile.
#
# Default platform docks unless stated: WQ W:L:lo (a-b/0-1), WK W:R:lo
# (e-f/0-1), BQ B:L:hi (a-b/8-9), BK B:R:hi (e-f/8-9).

test_that("opening actions: overlapping coordinates are distinct destinations", {
  st <- g3_new_game()$state
  a <- g3_legal_actions(st)
  expect_identical(nrow(a), 20L)
  expect_true(all(a$type == "move"))
  # The knight on W:b1 reaches c3 on W and on N; d2 holds its own pawn and
  # a3 has no physical square while no platform covers it.
  expect_identical(dests_of(st, "wNb1"), c("N:1:0", "W:1:2"))
  # A pawn single step to b3 offers W:b3 and N:b3; the double step b4 too.
  expect_identical(dests_of(st, "wPb2"), c("N:0:0", "N:0:1", "W:0:2", "W:0:3"))
  # Platform pawns are blocked: a2/f2 do not exist, b2/e2 hold pawns.
  expect_length(dests_of(st, "wPa1"), 0L)
  expect_length(dests_of(st, "wPe1"), 0L)
})

test_that("a piece on another board blocks an intermediate coordinate", {
  # White rook W:b1, black pawn N:b3, kings e0 (WK) and e9 (BK).
  # Upwards: W:b2 free; b3 has W:b3 (free) and N:b3 (enemy): both are
  # destinations, then the occupied coordinate stops the ray, so W:b4 is
  # unreachable although the W board itself is empty.
  # Rank 1: c1, d1, e1 (W and WK squares), f1 (WK). Left: a1 (WQ). Down: b0 (WQ).
  st <- g3_setup_position(pcs(
    "wR R white W:0:0", "wK K white WK:0:0", "bP P black N:0:0", "bK K black BK:0:1"
  ))
  expect_identical(
    dests_of(st, "wR"),
    sort(c("W:0:1", "W:0:2", "N:0:0", "W:1:0", "W:2:0", "W:3:0", "WK:0:1", "WK:1:1", "WQ:0:1", "WQ:1:0"))
  )
  a <- g3_legal_actions(st)
  expect_identical(a$capture_id[a$action_id == "m:W:0:0-N:0:0"], "bP")
  expect_false("m:W:0:0-W:0:3" %in% a$action_id)
})

test_that("rays cross coordinates without squares but cannot end there", {
  # White rook a0 alone on WQ, king e0 alone on WK, black pawn a8 on BQ.
  # Up the a-file: a1 (WQ), a2..a7 have no squares, a8 captures; right: b0,
  # c0/d0 missing, e0 is the own king. King: f0, d1, e1 (two squares), f1.
  # Both platforms carry one white piece: WQ -> W:L:hi, N:L:lo; WK ->
  # W:R:hi, N:R:lo; two orientations each.
  st <- g3_setup_position(pcs(
    "wR R white WQ:0:0", "wK K white WK:0:0", "bP P black BQ:0:0", "bK K black BK:0:1"
  ))
  expect_identical(dests_of(st, "wR"), sort(c("WQ:0:1", "BQ:0:0", "WQ:1:0")))
  expect_identical(dests_of(st, "wK"), sort(c("WK:1:0", "W:2:0", "W:3:0", "WK:0:1", "WK:1:1")))
  expect_identical(
    ids_of(st, "platform"),
    sort(c(
      "t:WQ>W:L:hi@0", "t:WQ>W:L:hi@180", "t:WQ>N:L:lo@0", "t:WQ>N:L:lo@180",
      "t:WK>W:R:hi@0", "t:WK>W:R:hi@180", "t:WK>N:R:lo@0", "t:WK>N:R:lo@180"
    ))
  )
  expect_identical(nrow(g3_legal_actions(st)), 16L)
})

test_that("zero projected displacement is not a move; same-coordinate kings do not attack", {
  # White king W:b3 and black king N:b3 share a coordinate: neither is in
  # check (zero displacement is not adjacency), so the position is legal.
  # Every coordinate next to b3 is adjacent to the black king, so the white
  # king has no move at all. The rook on W:c4 cannot move to N:c4.
  st <- g3_setup_position(pcs(
    "wK K white W:0:2", "bK K black N:0:0", "wR R white W:1:3"
  ), docks = c(WQ = "W:L:hi", WK = "N:R:hi"))
  expect_true(g3_validate_state(st))
  a <- g3_legal_actions(st)
  expect_false(any(a$piece_id == "wK"))
  expect_false("m:W:1:3-N:1:1" %in% a$action_id)
  # Left along rank 4: three squares at b4 (W, N, WQ), then a4 (WQ).
  expect_true(all(c("m:W:1:3-W:0:3", "m:W:1:3-N:0:1", "m:W:1:3-WQ:1:0", "m:W:1:3-WQ:0:0") %in% a$action_id))
})

test_that("knight and pawn patterns across levels; pawn captures only diagonally forward", {
  # White pawn W:c2 (unmoved), black pieces on N:b3 and N:d3 and W:c3.
  # c3 is blocked on W (enemy) but N:c3 is empty: single step to N:c3 only;
  # the double step needs c3 empty on all boards, so none. Captures: b3 and
  # d3 on N (and W:b3/W:d3 are empty, so not captures).
  st <- g3_setup_position(transform(pcs(
    "wP P white W:1:1", "wK K white WK:0:0", "bK K black BK:0:1",
    "bN N black N:0:0", "bB B black N:2:0", "bR R black W:1:2"
  ), has_moved = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)))
  expect_identical(dests_of(st, "wP"), sort(c("N:1:0", "N:0:0", "N:2:0")))
})
