# Phase 04: search opponent and baselines.

mate_net <- function() {
  # Black to act; Qb5-e5 (on B or N) mates (see the terminal tests).
  g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
}

test_that("search finds a mate in one", {
  st <- mate_net()
  d <- g3_choose_action(st, budget = list(nodes = 3000L, depth = 1L))
  expect_true(d$action_id %in% c("m:B:0:0-B:3:0", "m:B:0:0-N:3:2"))
  expect_identical(d$score_kind, "mate")
  after <- g3_apply_action(st, d$action_id)$state
  expect_identical(after$outcome$reason, "checkmate")
})

test_that("search finds the only escape from check, a platform action", {
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bN N black W:2:1", "bR1 R black W:0:0", "bR2 R black BK:1:1", "bK K black B:0:3"
  ))
  d <- g3_choose_action(st, budget = list(nodes = 500L))
  expect_identical(d$action_id, "t:WK>N:R:lo@0")
})

test_that("search takes an undefended queen", {
  st <- g3_setup_position(pcs("wR R white W:0:0", "wK K white WK:0:0", "bQ Q black W:0:3", "bK K black B:3:3"))
  d <- g3_choose_action(st, budget = list(nodes = 3000L, depth = 1L))
  expect_identical(d$action_id, "m:W:0:0-W:0:3")
})

test_that("promotion choice avoids stalemate", {
  # Black king a9 and pawn b8 share BQ (immobile platform); the white pawn
  # on B:b7 blocks the b8 pawn and covers a8. c7-c8=Q covers b9: no black
  # action and no check, so stalemate (score 0). c8=R leaves b9 free.
  st <- g3_setup_position(pcs(
    "wP P white B:1:2", "wPb P white B:0:2", "wK K white W:0:0",
    "bK K black BQ:0:1", "bP P black BQ:1:0"
  ))
  an <- g3_analyse_position(st, budget = list(nodes = 6000L, depth = 1L))
  alt <- an$alternatives
  q <- alt$score[alt$action_id == "m:B:1:2-B:1:3=Q"]
  r <- alt$score[alt$action_id == "m:B:1:2-B:1:3=R"]
  expect_identical(q, 0)
  expect_gt(r, 0)
  expect_false(identical(an$pv[1], "m:B:1:2-B:1:3=Q"))
})

test_that("illegal en passant is never returned", {
  pieces <- transform(pcs(
    "wP P white W:1:1", "wR R white W:0:3", "wK K white WK:0:0",
    "bP P black N:2:1", "bK K black N:3:1"
  ), has_moved = c(FALSE, TRUE, TRUE, TRUE, TRUE))
  s1 <- g3_apply_action(g3_setup_position(pieces), "m:W:1:1-W:1:3")$state
  d <- g3_choose_action(s1, budget = list(nodes = 300L))
  expect_true(d$action_id %in% g3_legal_actions(s1)$action_id)
})

test_that("every budget returns a legal action; zero budget uses the fallback", {
  st <- g3_new_game()$state
  legal <- g3_legal_actions(st)$action_id
  for (n in c(0L, 3L, 50L)) {
    d <- g3_choose_action(st, budget = list(nodes = n))
    expect_true(d$action_id %in% legal)
  }
  expect_identical(g3_choose_action(st, budget = list(nodes = 0L))$status, "fallback")
  expect_identical(g3_choose_action(st, budget = list(time_ms = 0))$status, "fallback")
  for (ag in c("greedy", "random")) expect_true(g3_choose_action(st, ag, seed = 1L)$action_id %in% legal)
  expect_error(g3_choose_action(st, budget = list(plies = 3)), class = "g3_budget_exceeded")
  expect_error(g3_choose_action(st, "oracle"), class = "g3_incompatible_model")
  expect_error(g3_choose_action(st, "model:missing"), class = "g3_incompatible_model")
})

test_that("finished positions return no action; the input is unchanged", {
  st <- g3_apply_action(mate_net(), "m:B:0:0-B:3:0")$state
  d <- g3_choose_action(st)
  expect_true(is.na(d$action_id))
  expect_identical(d$status, "terminal")
  s0 <- g3_new_game()$state
  h <- g3_state_hash(s0)
  g3_choose_action(s0, budget = list(nodes = 200L))
  expect_identical(g3_state_hash(s0), h)
})

test_that("search is deterministic; random is seeded and restores the RNG", {
  st <- g3_new_game()$state
  a <- g3_choose_action(st, budget = list(nodes = 400L))
  b <- g3_choose_action(st, budget = list(nodes = 400L))
  expect_identical(a$action_id, b$action_id)
  expect_identical(a$score, b$score)
  set.seed(5)
  x1 <- stats::runif(1)
  set.seed(5)
  r1 <- g3_choose_action(st, "random", seed = 9L)$action_id
  x2 <- stats::runif(1)
  expect_identical(x1, x2)
  expect_identical(r1, g3_choose_action(st, "random", seed = 9L)$action_id)
})

test_that("alpha-beta root value equals plain minimax at depth 2", {
  st <- g3_setup_position(pcs(
    "wK K white W:0:0", "wR R white W:2:1", "bK K black B:3:3", "bN N black N:1:2", "bP P black B:1:2"
  ))
  tabs <- .g3_tables()
  core <- .g3_core(st, tabs)
  leaf <- function(c) .g3_qsearch(.g3_search_env(.g3_budget(list(nodes = 1e9)), tabs), c, -Inf, Inf, 2L, 0L)
  minimax <- function(c, depth, ply) {
    ctx <- .g3_ctx(c, tabs)
    legal <- .g3_legal(c, tabs, ctx)
    if (!nrow(legal)) return(if (.g3_in_check(c, c$side, ctx, tabs)) -(.G3_MATE - ply) else 0)
    if (ply > 0L && .g3_rule_draw(c)) return(0)
    if (depth == 0L) return(leaf(c))
    max(vapply(seq_len(nrow(legal)), function(i) -minimax(.g3_make(c, legal[i, ], tabs), depth - 1L, ply + 1L), 0))
  }
  brute <- minimax(core, 2L, 0L)
  ab <- .g3_root_search(core, tabs, .g3_budget(list(nodes = 1e9, depth = 2L)))
  expect_identical(ab$depth, 2L)
  expect_equal(ab$scores[ab$best], brute)
})

test_that("the transposition table never shares scores across draw contexts", {
  tabs <- .g3_tables()
  st <- g3_setup_position(pcs("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"))
  core <- .g3_core(st, tabs)
  # Same board after a knight round trip, but a longer repetition record.
  path <- c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3")
  c2 <- core
  for (id in path) {
    legal <- .g3_legal(c2, tabs)
    c2 <- .g3_make(c2, legal[match(id, .g3_action_ids(c2, legal, tabs)), ], tabs)
  }
  expect_identical(c2$rep[length(c2$rep)], core$rep[length(core$rep)])
  expect_false(identical(.g3_tt_key(c2), .g3_tt_key(core)))
})
