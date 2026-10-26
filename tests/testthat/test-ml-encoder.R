# Phases 11-12: the feature encoder (schema g3-features/1). Pure R.

enc_fixtures <- function() {
  fx <- .g3_ml_fixtures()
  list(start = g3_new_game()$state, mate_in_one = fx$mate_in_one$state, escape = fx$platform_escape$state,
       promotion = fx$promotion_choice$state, platform_promotion = fx$platform_promotion$state)
}

test_that("the schema document, its hash and the encoder agree", {
  sch <- .g3_feature_schema()
  doc <- jsonlite::fromJSON(system.file("schemas", "features-v1.json", package = "gambitR3D"), simplifyVector = FALSE)
  expect_identical(sch$hash, .g3_hash_object(doc))
  expect_identical(sch$name, "g3-features/1")
  expect_identical(sch$n_tokens, 72L)
  e <- .g3_encode_state(g3_new_game()$state)
  expect_identical(colnames(e$tokens_cat), sch$token_cat)
  expect_identical(colnames(e$tokens_num), sch$token_num)
  expect_identical(colnames(e$cand_cat), sch$cand_cat)
  expect_identical(colnames(e$cand_num), sch$cand_num)
  # Every value is an integer inside its vocabulary.
  for (st in enc_fixtures()) {
    e <- .g3_encode_state(st)
    expect_identical(.g3_check_encoding(e), character())
    expect_type(e$tokens_cat, "integer")
    expect_type(e$cand_num, "integer")
  }
})

test_that("tokens follow the canonical physical-square order, then boards, then global", {
  tabs <- .g3_tables()
  e <- .g3_encode_state(g3_new_game()$state)
  tc <- e$tokens_cat
  expect_identical(tc[1:64, "token_type"], rep(1L, 64L))
  expect_identical(tc[1:64, "square"], 1:64)
  expect_identical(tc[1:64, "board"], tabs$sq_board)
  expect_identical(tc[1:64, "local_u"], tabs$sq_u + 1L)
  expect_identical(tc[1:64, "local_v"], tabs$sq_v + 1L)
  expect_identical(tc[65:71, "token_type"], rep(2L, 7L))
  expect_identical(tc[65:71, "board"], 1:7)
  expect_identical(unname(tc[72, "token_type"]), 3L)
  expect_identical(unname(tc[72, "side"]), 1L)
  # 32 pieces on 64 squares; the four starting platforms hold four each.
  expect_identical(sum(tc[1:64, "piece"] > 1L), 32L)
  expect_identical(unname(tc[68:71, "occupants"]), rep(5L, 4L))
  # Overlapping coordinates stay distinct squares: W and N share ranks 3-4.
  w_c3 <- match("W:1:2", tabs$sq_id)
  n_c3 <- match("N:1:0", tabs$sq_id)
  expect_identical(tc[w_c3, c("file", "rank")], tc[n_c3, c("file", "rank")])
  expect_false(identical(tc[w_c3, "square"], tc[n_c3, "square"]))
  expect_gte(unname(tc[w_c3, "coord_squares"]), 2L)
})

test_that("every legal action becomes exactly one candidate, in canonical order", {
  for (st in enc_fixtures()) {
    a <- g3_legal_actions(st)
    e <- .g3_encode_state(st)
    expect_identical(e$action_ids, a$action_id)
    expect_identical(nrow(e$cand_cat), nrow(a))
    expect_identical(e$candidate_digest, .g3_candidate_digest(a$action_id))
  }
  # A large set is not truncated.
  e <- .g3_encode_state(.g3_ml_fixtures()$mate_in_one$state)
  expect_identical(nrow(e$cand_cat), 61L)
})

test_that("resulting coordinates come from the engine, including platform transport", {
  tabs <- .g3_tables()
  for (st in enc_fixtures()) {
    a <- g3_legal_actions(st)
    e <- .g3_encode_state(st)
    for (i in seq_len(nrow(a))) {
      if (a$type[i] == "castle") next
      nxt <- g3_apply_action(st, a$action_id[i])$state
      core <- .g3_core(nxt, tabs)
      co <- .g3_square_coords(core$dock, core$orient, tabs)
      sq <- match(nxt$pieces$square_id[nxt$pieces$id == a$piece_id[i]], tabs$sq_id)
      expect_identical(unname(e$cand_cat[i, "result_file"]) - 1L, co$file[sq], info = a$action_id[i])
      expect_identical(unname(e$cand_cat[i, "result_rank"]) - 1L, co$rank[sq], info = a$action_id[i])
      if (a$type[i] == "platform") {
        # Physical identity is unchanged by transport.
        expect_identical(unname(e$cand_cat[i, "destination"]), match(a$from_square[i], tabs$sq_id))
        expect_identical(unname(e$cand_cat[i, "to_dock"]) - 1L, match(a$to_dock[i], tabs$dock_ids))
      }
    }
  }
  # Transport promotion is encoded as a promotion of a platform action.
  st <- .g3_ml_fixtures()$platform_promotion$state
  a <- g3_legal_actions(st)
  e <- .g3_encode_state(st)
  pp <- which(a$type == "platform" & !is.na(a$promotion))
  expect_gt(length(pp), 0L)
  expect_true(all(e$cand_cat[pp, "promotion"] > 1L))
  expect_true(all(e$cand_cat[pp, "action_type"] == 2L))
  expect_identical(length(unique(e$cand_cat[pp, "promotion"])), 4L)
})

test_that("castling candidates carry side and both participants", {
  st0 <- g3_new_game()$state
  st <- g3_setup_position(st0$pieces[, c("id", "type", "color", "square_id")],
                          castling = c(white_kingside = TRUE, white_queenside = TRUE))
  e <- .g3_encode_state(st)
  i <- match("c:white:kingside", e$action_ids)
  expect_false(is.na(i))
  tabs <- .g3_tables()
  expect_identical(unname(e$cand_cat[i, c("action_type", "castle_side")]), c(3L, 2L))
  expect_identical(unname(e$cand_cat[i, "rook_source"]), match("WK:1:0", tabs$sq_id))
  expect_identical(unname(e$cand_cat[i, "rook_destination"]), match("WK:0:0", tabs$sq_id))
})

test_that("encodings depend only on the logical state", {
  # Display preferences live outside the state: games saved with Neutral
  # lowest and highest read back to byte-identical features.
  g <- g3_new_game()
  g <- g3_game_apply(g, "m:W:1:1-W:1:3")
  dir <- withr::local_tempdir()
  f1 <- file.path(dir, "low.json")
  f2 <- file.path(dir, "high.json")
  g3_write_game(g, f1, ui = list(view = "3d", neutral_height = "lowest"))
  g3_write_game(g, f2, ui = list(view = "2d", neutral_height = "highest"))
  e1 <- .g3_encode_state(g3_read_game(f1)$state)
  e2 <- .g3_encode_state(g3_read_game(f2)$state)
  expect_identical(e1, e2)
  j1 <- jsonlite::toJSON(.g3_enc_payload(e1), auto_unbox = TRUE)
  j2 <- jsonlite::toJSON(.g3_enc_payload(e2), auto_unbox = TRUE)
  expect_identical(digest::digest(as.character(j1), serialize = FALSE), digest::digest(as.character(j2), serialize = FALSE))
  # Piece id strings are not features.
  P <- function(ids) data.frame(id = ids, type = c("K", "R", "K"), color = c("white", "white", "black"),
                                square_id = c("W:0:0", "W:2:0", "B:3:3"), stringsAsFactors = FALSE)
  a <- .g3_encode_state(g3_setup_position(P(c("a", "b", "c"))))
  b <- .g3_encode_state(g3_setup_position(P(c("king", "rook", "enemy"))))
  expect_identical(a, b)
})

test_that("history-dependent rights are encoded", {
  P <- function(...) {
    x <- strsplit(c(...), " ", fixed = TRUE)
    data.frame(id = vapply(x, `[`, "", 1L), type = vapply(x, `[`, "", 2L), color = vapply(x, `[`, "", 3L),
               square_id = vapply(x, `[`, "", 4L), stringsAsFactors = FALSE)
  }
  pieces <- transform(P("wP P white W:1:1", "wK K white W:0:0", "bP P black N:2:1", "bK K black B:3:3"),
                      has_moved = c(FALSE, TRUE, TRUE, TRUE))
  s1 <- g3_apply_action(g3_setup_position(pieces), "m:W:1:1-W:1:3")$state
  e <- .g3_encode_state(s1)
  expect_identical(unname(e$tokens_cat[72, c("ep_file", "ep_rank", "ep_effective")]), c(4L, 5L, 2L))
  expect_true(any(e$tokens_cat[1:64, "en_passant"] == 2L))
  expect_true(any(e$tokens_cat[1:64, "en_passant"] == 3L))
  ep_cands <- e$cand_cat[e$cand_cat[, "special"] == 3L, , drop = FALSE]
  expect_identical(nrow(ep_cands), 2L)
  # Repetition count of the current key (a lossy summary).
  st <- g3_setup_position(P("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"))
  s <- st
  for (id in c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3")) s <- g3_apply_action(s, id)$state
  expect_identical(unname(.g3_encode_state(st)$tokens_cat[72, "repetition"]), 1L)
  expect_identical(unname(.g3_encode_state(s)$tokens_cat[72, "repetition"]), 2L)
  expect_identical(unname(.g3_encode_state(s)$tokens_num[72, "repetition_record_scaled"]), 5L)
})

test_that("candidate rows permute with the legal actions; batches pad with a mask", {
  tabs <- .g3_tables()
  st <- .g3_ml_fixtures()$platform_escape$state
  st2 <- g3_new_game()$state
  core <- .g3_core(st2, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  perm <- rev(seq_len(nrow(legal)))
  a <- .g3_encode_candidates(core, legal, tabs, ctx)
  b <- .g3_encode_candidates(core, legal[perm, , drop = FALSE], tabs, ctx)
  expect_identical(a$cat[perm, ], b$cat)
  expect_identical(a$num[perm, ], b$num)
  e1 <- .g3_encode_state(st)
  e2 <- .g3_encode_state(st2)
  stk <- .g3_enc_stack(list(e1, e2))
  a_max <- max(length(e1$action_ids), length(e2$action_ids))
  expect_identical(stk$cand_cat$shape, c(2L, a_max, 18L))
  expect_identical(stk$n_cand$data, c(length(e1$action_ids), length(e2$action_ids)))
  arr <- array(stk$cand_cat$data, dim = rev(stk$cand_cat$shape))
  # Row-major layout: arr[k, a, b] is candidate a of position b.
  expect_identical(arr[, 1L, 1L], unname(e1$cand_cat[1L, ]))
  expect_true(all(arr[, (length(e1$action_ids) + 1L):a_max, 1L] == 0L))
  expect_identical(arr[, length(e2$action_ids), 2L], unname(e2$cand_cat[length(e2$action_ids), ]))
})
