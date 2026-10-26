# Phase 02: en passant, castling and promotion, positive and negative cases.

test_that("en passant: any empty square at the crossed coordinate, one action only", {
  # White pawn W:c2 (unmoved), black pawn N:d4. White double-steps c2-c4
  # on W; the crossed coordinate is c3 with squares W:c3 and N:c3, both
  # empty, so Black has two en-passant captures removing the W:c4 pawn.
  pieces <- transform(pcs(
    "wP P white W:1:1", "wK K white W:0:0", "bP P black N:2:1", "bK K black B:3:3"
  ), has_moved = c(FALSE, TRUE, TRUE, TRUE))
  st <- g3_setup_position(pieces)
  s1 <- g3_apply_action(st, "m:W:1:1-W:1:3")$state
  expect_identical(s1$ep$victim_id, "wP")
  expect_identical(c(s1$ep$crossed_file, s1$ep$crossed_rank), c(2L, 3L))
  a <- g3_legal_actions(s1)
  ep <- a[a$en_passant, ]
  expect_setequal(ep$action_id, c("m:N:2:1-W:1:2", "m:N:2:1-N:1:0"))
  expect_true(all(ep$capture_id == "wP"))
  s2 <- g3_apply_action(s1, "m:N:2:1-N:1:0")$state
  expect_false("wP" %in% s2$pieces$id)
  # A single step creates no entitlement.
  s1b <- g3_apply_action(st, "m:W:1:1-W:1:2")$state
  expect_null(s1b$ep)
  # The entitlement lapses after Black's next action.
  s3 <- g3_apply_action(s1, "m:B:3:3-B:2:3")$state
  s4 <- g3_apply_action(s3, "m:W:0:0-W:1:0")$state
  expect_false(any(g3_legal_actions(s4)$en_passant))
})

test_that("en passant that would expose the own king is illegal and not in the key", {
  # Black king N:e4, black pawn N:d4, white rook W:b4. After c2-c4 on W
  # the pawns on c4 and d4 both shield rank 4; removing both would expose
  # the king, so en passant is illegal. The repetition key must then
  # equal the key of the same position without an en-passant record.
  pieces <- transform(pcs(
    "wP P white W:1:1", "wR R white W:0:3", "wK K white WK:0:0",
    "bP P black N:2:1", "bK K black N:3:1"
  ), has_moved = c(FALSE, TRUE, TRUE, TRUE, TRUE))
  st <- g3_setup_position(pieces)
  s1 <- g3_apply_action(st, "m:W:1:1-W:1:3")$state
  expect_false(any(g3_legal_actions(s1)$en_passant))
  no_ep <- s1
  no_ep$ep <- NULL
  expect_identical(g3_position_key(s1), g3_position_key(no_ep))
})

test_that("king-side castling swaps king and rook after the first turn", {
  st0 <- g3_new_game()$state
  expect_false(any(g3_legal_actions(st0)$type == "castle"))
  # Same placement, first turn completed: castling is legal.
  pc <- st0$pieces
  st <- g3_setup_position(pc[, c("id", "type", "color", "square_id")],
                          castling = c(white_kingside = TRUE, white_queenside = TRUE,
                                       black_kingside = TRUE, black_queenside = TRUE))
  expect_true("c:white:kingside" %in% ids_of(st))
  # Queen still on b0: no queen-side castling.
  expect_false("c:white:queenside" %in% ids_of(st))
  s1 <- g3_apply_action(st, "c:white:kingside")$state
  sq <- stats::setNames(s1$pieces$square_id, s1$pieces$id)
  expect_identical(unname(sq[c("wK", "wRf0")]), c("WK:1:0", "WK:0:0"))
  expect_false(s1$rights$castling$white_kingside)
  expect_false(s1$rights$castling$white_queenside)
})

test_that("king-side castling is illegal into an attacked final square", {
  # Open f-file: a black rook on f9 attacks f0 once f8 and f1 are empty.
  pieces <- pcs("wK K white WK:0:0", "wRf0 R white WK:1:0", "bRf9 R black BK:1:1", "bK K black B:0:3")
  st <- g3_setup_position(pieces, castling = c(white_kingside = TRUE))
  expect_false("c:white:kingside" %in% ids_of(st))
  # Blocking f1 with a white pawn makes it legal again.
  st2 <- g3_setup_position(rbind(pieces, pcs("wPf1 P white WK:1:1")), castling = c(white_kingside = TRUE))
  expect_true("c:white:kingside" %in% ids_of(st2))
})

test_that("queen-side castling: empty b0, transit probes include virtual c0/d0", {
  base <- pcs("wK K white WK:0:0", "wRa0 R white WQ:0:0", "wRf0 R white WK:1:0", "bK K black B:0:3")
  rights <- c(white_kingside = TRUE, white_queenside = TRUE)
  st <- g3_setup_position(base, castling = rights)
  expect_true("c:white:queenside" %in% ids_of(st))
  s1 <- g3_apply_action(st, "c:white:queenside")$state
  sq <- stats::setNames(s1$pieces$square_id, s1$pieces$id)
  expect_identical(unname(sq[c("wK", "wRa0")]), c("WQ:1:0", "WK:0:0"))
  # A black knight on W:b2 attacks only the virtual coordinate c0 among
  # e0, d0, c0, b0: queen-side castling becomes illegal, king side stays.
  st2 <- g3_setup_position(rbind(base, pcs("bN N black W:0:1")), castling = rights)
  expect_false("c:white:queenside" %in% ids_of(st2))
  expect_true("c:white:kingside" %in% ids_of(st2))
  # Not before the first turn.
  st3 <- g3_setup_position(base, castling = rights, first_turn_completed = c(white = FALSE))
  expect_false(any(grepl("^c:", ids_of(st3))))
  # Not with the king platform away from its home dock.
  st4 <- g3_setup_position(
    pcs("wK K white WK:0:0", "wRa0 R white WQ:0:0", "wRf0 R white WK:1:0", "bK K black B:0:3"),
    docks = c(WK = "W:R:hi"), castling = rights
  )
  expect_false(any(grepl("^c:", ids_of(st4))))
})

test_that("transport revokes castling rights permanently", {
  # The king-side rook alone on WK; moving the platform revokes that right.
  pieces <- pcs("wK K white W:3:0", "wRf0 R white WK:1:0", "bK K black B:0:3")
  st <- g3_setup_position(pieces, castling = c(white_kingside = TRUE))
  s1 <- g3_apply_action(st, "t:WK>W:R:hi@0")$state
  expect_false(s1$rights$castling$white_kingside)
})

test_that("promotion on B rank 8, on a B-high platform rank 9, and by transport", {
  # Piece move onto B:c8 promotes (four choices).
  st <- g3_setup_position(pcs("wP P white B:1:2", "wK K white W:0:0", "bK K black BQ:0:1"))
  a <- g3_legal_actions(st)
  expect_setequal(a$promotion[a$piece_id == "wP"], c("Q", "R", "B", "N"))
  s1 <- g3_apply_action(st, "m:B:1:2-B:1:3=N")$state
  p <- s1$pieces[s1$pieces$id == "wP", ]
  expect_identical(c(p$type, p$origin_type), c("N", "P"))
  # Transport: WQ at N:L:hi carries a white pawn on (a,7). To B:L:hi kept
  # orientation -> (a,9): promotion; turned -> (b,8): no promotion.
  st2 <- g3_setup_position(
    pcs("wP P white WQ:0:1", "wK K white W:0:0", "bK K black B:3:3"),
    docks = c(WQ = "N:L:hi", BQ = "N:R:hi")
  )
  pl <- ids_of(st2, "platform")
  expect_true(all(paste0("t:WQ>B:L:hi@0=", c("Q", "R", "B", "N")) %in% pl))
  expect_false("t:WQ>B:L:hi@0" %in% pl)
  expect_true("t:WQ>B:L:hi@180" %in% pl)
  s2 <- g3_apply_action(st2, "t:WQ>B:L:hi@0=Q")$state
  expect_identical(s2$pieces$type[s2$pieces$id == "wP"], "Q")
  # No passive promotion: the stranded pawn on (b,8) stays a pawn when an
  # unrelated platform moves, and promotes only by its own step to b9.
  s3 <- g3_apply_action(st2, "t:WQ>B:L:hi@180")$state
  expect_identical(s3$pieces$type[s3$pieces$id == "wP"], "P")
  s4 <- g3_apply_action(s3, "m:B:3:3-B:2:3")$state
  expect_true("m:WQ:0:1-WQ:0:0=Q" %in% ids_of(s4))
})

test_that("Black promotes on W rank 1 and on a W-low platform rank 0", {
  st <- g3_setup_position(
    pcs("bP P black W:1:1", "bK K black B:3:3", "wK K white N:3:3"),
    side = "black", docks = c(WQ = "N:L:lo")
  )
  a <- g3_legal_actions(st)
  expect_setequal(a$promotion[a$piece_id == "bP"], c("Q", "R", "B", "N"))
})
