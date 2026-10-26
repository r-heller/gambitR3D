# Phase 01/02: platform actions, worked out by hand from the dock table.

test_that("the four-piece starting platforms cannot move", {
  expect_length(ids_of(g3_new_game()$state, "platform"), 0L)
})

test_that("a single-occupant platform at N:L:lo has five free destinations", {
  # WQ at N:L:lo carries one white knight. Neighbours: N:R:lo, N:L:hi,
  # W:L:lo, W:L:hi, B:L:lo. Other platforms sit at W:R:lo, B:L:hi, B:R:hi,
  # none of which is a neighbour. 5 docks x 2 orientations.
  st <- g3_setup_position(
    pcs("wN N white WQ:0:0", "wK K white W:3:0", "bK K black B:0:3"),
    docks = c(WQ = "N:L:lo")
  )
  pl <- ids_of(st, "platform")
  expect_length(pl, 10L)
  expect_setequal(unique(sub("@.*$", "", sub("^t:WQ>", "", pl))),
                  c("N:R:lo", "N:L:hi", "W:L:lo", "W:L:hi", "B:L:lo"))
  # Occupying one neighbouring dock removes exactly that destination.
  st2 <- g3_setup_position(
    pcs("wN N white WQ:0:0", "wK K white W:3:0", "bK K black B:0:3"),
    docks = c(WQ = "N:L:lo", BQ = "W:L:hi")
  )
  pl2 <- ids_of(st2, "platform")
  expect_length(pl2, 8L)
  expect_false(any(grepl("W:L:hi", pl2)))
})

test_that("a single-occupant platform at B:R:hi has three destinations", {
  # BK at B:R:hi with only the black king; black to act. Neighbours:
  # B:L:hi (BQ there, so occupied), B:R:lo, N:R:hi. Move BQ away to make
  # all three free: BQ at N:L:lo.
  # (A white pawn on W:c2 avoids the bare-kings draw.)
  st <- g3_setup_position(
    pcs("bK K black BK:0:1", "wK K white W:0:0", "wP P white W:1:1"),
    docks = c(BQ = "N:L:lo"), side = "black"
  )
  pl <- ids_of(st, "platform")
  expect_setequal(unique(sub("@.*$", "", sub("^t:BK>", "", pl))), c("B:L:hi", "B:R:lo", "N:R:hi"))
  # An enemy-occupied platform is not movable by the side to act.
  st_w <- g3_setup_position(
    pcs("bK K black BK:0:1", "wK K white W:0:0", "wP P white W:1:1"),
    docks = c(BQ = "N:L:lo"), side = "white"
  )
  expect_false(any(grepl("^t:BK", ids_of(st_w, "platform"))))
})

test_that("an empty or crowded platform does not move", {
  st <- g3_setup_position(
    pcs("wN N white WQ:0:0", "wB B white WQ:1:1", "wK K white W:3:0", "bK K black B:0:3"),
    docks = c(WQ = "N:L:lo")
  )
  expect_length(ids_of(st, "platform"), 0L)
})

test_that("a platform move exposing the own king is rejected", {
  # White king W:e1, black rook N:e6. A white pawn alone on WK (at N:R:lo,
  # cell WK:0:1 = e3) shields the e-file. Moving WK: only destinations that
  # keep the pawn on e4/e5 still block: W:R:hi@0 (e5) and B:R:lo@0 (e5).
  # Pawn: e3 -> e4 on W or N (still blocking). King: d1, d2, e2 (W and
  # WK:0:0), f2 (WK:1:0). Total 2 + 2 + 5 = 9.
  st <- g3_setup_position(
    pcs("wK K white W:3:0", "wP P white WK:0:1", "bR R black N:3:3", "bK K black B:0:3"),
    docks = c(WK = "N:R:lo")
  )
  expect_identical(ids_of(st, "platform"), sort(c("t:WK>W:R:hi@0", "t:WK>B:R:lo@0")))
  expect_identical(dests_of(st, "wP"), sort(c("W:3:3", "N:3:1")))
  expect_identical(dests_of(st, "wK"), sort(c("W:2:0", "W:2:1", "W:3:1", "WK:0:0", "WK:1:0")))
  expect_identical(nrow(g3_legal_actions(st)), 9L)
  expect_error(g3_apply_action(st, "t:WK>N:R:hi@0"), class = "g3_illegal_action")
})

test_that("a platform carrying the king can escape check", {
  # White king e0 alone on WK (W:R:lo). Black knight W:d2 gives check and
  # covers f1; black rook W:b1 covers d1, e1 (both squares) and f1; black
  # rook BK:f9 covers f0. Platform: W:R:hi@0 -> e4 (knight), @180 -> f5
  # (rook f9), N:R:lo@180 -> f3 (knight, rook f9). Only N:R:lo@0 -> e2 is
  # safe, and it is the only legal action.
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bN N black W:2:1", "bR1 R black W:0:0", "bR2 R black BK:1:1", "bK K black B:0:3"
  ))
  expect_identical(ids_of(st), "t:WK>N:R:lo@0")
  expect_false(g3_is_terminal(st))
  step <- g3_apply_action(st, "t:WK>N:R:lo@0")
  expect_identical(step$state$boards$dock[step$state$boards$id == "WK"], "N:R:lo")
  # The king kept its physical square; its coordinate changed to e2.
  expect_identical(step$state$pieces$square_id[step$state$pieces$id == "wK"], "WK:0:0")
})

test_that("checkmate counts platform actions: none of them helps here", {
  # As above but with black rook N:e6 (check along e-file, covers e1, e2,
  # e4) and bishop N:b3 (covers d1 through c2) instead of the rook on b1.
  # King: f0/f1 rook f9, e1 rook e6, d1 bishop. Platform: e4/f5/e2/f3 all
  # attacked. Double check, no legal action: checkmate.
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bN N black W:2:1", "bR1 R black N:3:3", "bR2 R black BK:1:1", "bB B black N:0:0", "bK K black B:0:3"
  ))
  expect_true(g3_is_terminal(st))
  expect_identical(st$outcome$result, "black")
  expect_identical(st$outcome$reason, "checkmate")
  expect_identical(nrow(g3_legal_actions(st)), 0L)
})

test_that("transport keeps the physical square and changes the projection", {
  st <- g3_setup_position(
    pcs("wN N white WQ:0:0", "wK K white W:3:0", "bK K black B:0:3"),
    docks = c(WQ = "N:L:lo")
  )
  st2 <- g3_apply_action(st, "t:WQ>N:L:hi@180")$state
  expect_identical(st2$pieces$square_id[st2$pieces$id == "wN"], "WQ:0:0")
  expect_identical(st2$boards$orientation[st2$boards$id == "WQ"], 180L)
  # N:L:hi has min rank 6; turned, local (0,0) projects to (b, 7).
  tabs <- .g3_tables()
  core <- .g3_core(st2, tabs)
  co <- .g3_square_coords(core$dock, core$orient, tabs)
  expect_identical(.g3_coord_label(co$file[49L], co$rank[49L]), "b7")
})

test_that("a platform transport can give check, and every reply escapes it", {
  # WQ carries its rook from W:L:lo to N:L:lo turned 180 degrees; the rook
  # lands on b3 and checks the black king on B:b8 up the empty b-file.
  st <- g3_setup_position(pcs("wR R white WQ:0:0", "wK K white W:3:0", "bK K black B:0:3", "bN N black N:3:3"))
  for (engine in c("cpp", "r")) {
    withr::local_options(gambitR3D.engine = engine)
    step <- g3_apply_action(st, "t:WQ>N:L:lo@180")
    expect_true(isTRUE(step$event$check), info = engine)
    expect_match(step$event$notation, "\\+$", info = engine)
    # Only king moves off the b-file escape: c7, c8, a8 and a9 (b7 and b9
    # stay on the file; the knight on N:e6 can neither block nor capture).
    replies <- g3_legal_actions(step$state)$action_id
    expect_setequal(replies, c("m:B:0:3-B:1:2", "m:B:0:3-B:1:3", "m:B:0:3-BQ:0:0", "m:B:0:3-BQ:0:1"))
  }
})
