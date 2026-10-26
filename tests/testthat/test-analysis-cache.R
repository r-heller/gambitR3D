# Phase 09: the evaluation cache can never reuse a score across draw
# context, rules, evaluator/model revision or budget.

rep_game <- function() {
  st <- g3_setup_position(pcs("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"))
  g <- .g3_game_new(st)
  for (id in rep(c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3"), 2L)) g <- g3_game_apply(g, id)
  g
}

test_that("the context hash covers draw context, rules, evaluator, model and budget", {
  tabs <- .g3_tables()
  st <- g3_replay_game(rep_game(), ply = 0L)
  c0 <- .g3_core(st, tabs)
  c4 <- .g3_core(g3_replay_game(rep_game(), ply = 4L), tabs)
  expect_identical(c0$rep[length(c0$rep)], c4$rep[length(c4$rep)])
  s <- .g3_an_evaluator("search")
  b <- .g3_budget(list(nodes = 100L))
  h0 <- .g3_an_context_hash(c0, s, b, tabs)
  # Same board, longer repetition record.
  expect_false(identical(h0, .g3_an_context_hash(c4, s, b, tabs)))
  # Same board and record, different halfmove counter.
  ch <- c0
  ch$halfmove <- c0$halfmove + 1L
  expect_false(identical(h0, .g3_an_context_hash(ch, s, b, tabs)))
  # Budget.
  expect_false(identical(h0, .g3_an_context_hash(c0, s, .g3_budget(list(nodes = 101L)), tabs)))
  expect_false(identical(h0, .g3_an_context_hash(c0, s, .g3_budget(list(nodes = 100L, depth = 3L)), tabs)))
  # Evaluator and model revision.
  m1 <- structure(list(model_id = "tiny", revision = "rev-a"), class = "g3_model")
  m2 <- structure(list(model_id = "tiny", revision = "rev-b"), class = "g3_model")
  hm1 <- .g3_an_context_hash(c0, .g3_an_evaluator(m1), b, tabs)
  expect_false(identical(h0, hm1))
  expect_false(identical(hm1, .g3_an_context_hash(c0, .g3_an_evaluator(m2), b, tabs)))
  s2 <- s
  s2$version <- "g3-search/2+g3-eval/1"
  expect_false(identical(h0, .g3_an_context_hash(c0, s2, b, tabs)))
  # Rules hash.
  t2 <- tabs
  t2$rules_hash <- strrep("f", 64)
  expect_false(identical(h0, .g3_an_context_hash(c0, s, b, t2)))
  # Deterministic for identical inputs.
  expect_identical(h0, .g3_an_context_hash(c0, s, b, tabs))
})

test_that("a planted cache entry is used only under its exact context", {
  g <- rep_game()
  cache <- new.env()
  r <- g3_analyse_game(g, budget = list(nodes = 120L), cache = cache)
  expect_false(any(r$evaluations$cache_hit))
  n_entries <- length(ls(cache))
  expect_identical(n_entries, sum(r$positions$status == "ongoing"))
  # Plant an impossible score under the context of ply 0.
  key0 <- r$positions$analysis_context_hash[1]
  ent <- cache[[key0]]
  ent$scores[] <- 12345
  assign(key0, ent, envir = cache)
  again <- g3_analyse_game(g, budget = list(nodes = 120L), cache = cache)
  expect_true(all(again$evaluations$cache_hit[again$positions$status == "ongoing"]))
  expect_identical(again$evaluations$raw_score[1], 12345)
  # Ply 4 has the same board but another draw context: never the planted score.
  expect_identical(again$positions$position_key[5], again$positions$position_key[1])
  expect_false(identical(again$evaluations$raw_score[5], 12345))
  # Another budget or evaluator version cannot see the planted entry.
  other <- g3_analyse_game(g, budget = list(nodes = 121L), cache = cache)
  expect_false(any(other$evaluations$cache_hit))
  expect_false(12345 %in% other$evaluations$raw_score)
  expect_identical(other$provenance$cache$hits, 0L)
  # A model evaluator never reads search entries, and failures are not cached.
  m <- g3_analyse_game(g, evaluator = "model:absent", budget = list(nodes = 120L), cache = cache)
  expect_false(any(m$evaluations$cache_hit))
  expect_true(all(is.na(m$evaluations$raw_score[m$positions$status == "ongoing"])))
  n_before <- length(ls(cache))
  g3_analyse_game(g, evaluator = "model:absent", budget = list(nodes = 120L), cache = cache)
  expect_identical(length(ls(cache)), n_before)
})

test_that("cached and uncached analyses agree", {
  g <- rep_game()
  cache <- new.env()
  a <- g3_analyse_game(g, budget = list(nodes = 150L), cache = cache)
  b <- g3_analyse_game(g, budget = list(nodes = 150L), cache = cache)
  expect_true(all(b$evaluations$cache_hit[b$positions$status == "ongoing"]))
  drop <- c("cache_hit")
  expect_identical(a$evaluations[, setdiff(names(a$evaluations), drop)], b$evaluations[, setdiff(names(b$evaluations), drop)])
})
