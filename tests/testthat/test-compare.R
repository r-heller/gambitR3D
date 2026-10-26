# Phase 09: paired agent comparisons, accounting and uncertainty.

mate_in_one <- function() {
  # Black to act mates at once (see the engine terminal tests).
  g3_setup_position(pcs(
    "wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
}

test_that("a tournament with known outcomes has exact counts, bounds and intervals", {
  pos <- list(m1 = mate_in_one(), m2 = mate_in_one())
  cmp <- g3_compare_agents(c("search", "greedy"), positions = pos, budget = list(nodes = 400L),
                           seeds = 7L, pairs = 1L, max_plies = 4L, bootstrap = list(replicates = 300L, seed = 11L))
  expect_s3_class(cmp, "g3_comparison")
  g <- cmp$games
  expect_identical(nrow(g), 4L)
  # Whoever plays Black mates at once: the reference agent loses as White and wins as Black.
  expect_identical(g$status, rep("completed", 4L))
  expect_identical(g$result, rep("black", 4L))
  expect_identical(g$termination, rep("checkmate", 4L))
  expect_identical(g$agent1_colour, rep(c("white", "black"), 2L))
  expect_identical(g$agent1_score, c(0, 1, 0, 1))
  expect_identical(g$plies, rep(1L, 4L))
  s <- cmp$summary
  expect_identical(c(s$scheduled, s$completed, s$truncated, s$failed, s$cancelled), c(4L, 4L, 0L, 0L, 0L))
  expect_identical(c(s$wins, s$draws, s$losses), c(2L, 0L, 2L))
  expect_identical(s$completed_score, 0.5)
  expect_identical(c(s$scheduled_lower_bound, s$scheduled_upper_bound), c(0.5, 0.5))
  expect_identical(s$illegal_actions, 0L)
  bc <- cmp$by_colour
  expect_identical(bc$completed_score, c(0, 1))
  u <- cmp$uncertainty
  expect_identical(u$n_blocks, 2L)
  expect_identical(u$replicates, 300L)
  expect_identical(u$seed, 11L)
  expect_match(u$method, "bootstrap")
  ci <- u$intervals[u$intervals$statistic == "completed_score", ]
  expect_identical(c(ci$estimate, ci$lower, ci$upper), c(0.5, 0.5, 0.5))
  expect_identical(nrow(cmp$pair_results), 2L)
  expect_identical(cmp$pair_results$agent1_points, c(1, 1))
  # Colour-swapped pairs start from the identical state (role swap, no mirror).
  recs <- cmp$game_records
  expect_identical(g3_state_hash(recs[[1]]$initial_state), g3_state_hash(recs[[2]]$initial_state))
  expect_identical(g$white_agent[1], g$black_agent[2])
  expect_identical(cmp$provenance$budget$nodes, 400)
  expect_identical(nrow(cmp$decisions), 4L)
  expect_true(all(c("latency_median_ms", "latency_p95_ms", "nodes_total") %in% names(cmp$costs)))
  # Deterministic for fixed seeds.
  again <- g3_compare_agents(c("search", "greedy"), positions = pos, budget = list(nodes = 400L),
                             seeds = 7L, pairs = 1L, max_plies = 4L, bootstrap = list(replicates = 300L, seed = 11L))
  expect_identical(again$games[, c("result", "agent1_score", "game_hash")], g[, c("result", "agent1_score", "game_hash")])
  expect_identical(again$uncertainty$intervals, u$intervals)
})

test_that("truncated games are never draws and widen the scheduled bounds", {
  cmp <- g3_compare_agents(c("greedy", "random"), positions = NULL, budget = list(nodes = 50L),
                           seeds = 1:2, pairs = 1L, max_plies = 2L)
  g <- cmp$games
  expect_identical(nrow(g), 4L)
  expect_identical(g$status, rep("truncated", 4L))
  expect_true(all(is.na(g$result)))
  expect_true(all(is.na(g$agent1_score)))
  s <- cmp$summary
  expect_identical(c(s$completed, s$truncated, s$draws), c(0L, 4L, 0L))
  expect_true(is.na(s$completed_score))
  expect_identical(c(s$scheduled_lower_bound, s$scheduled_upper_bound), c(0, 1))
  expect_match(cmp$uncertainty$note, "descriptive")
  ev <- cmp$game_records[[1]]$events
  expect_identical(ev$type[nrow(ev)], "truncated")
  expect_identical(cmp$game_records[[1]]$metadata$result, "truncated")
  expect_identical(nrow(cmp$decisions), 8L)
})

test_that("failures and cancellations stay in the denominator", {
  cmp <- g3_compare_agents(c("greedy", "model:absent"), budget = list(nodes = 50L), seeds = 1L, max_plies = 4L)
  g <- cmp$games
  expect_identical(g$status, c("failed", "failed"))
  expect_identical(g$failed_agent, c("model:absent", "model:absent"))
  expect_match(g$error[1], "model:absent")
  s <- cmp$summary
  expect_identical(c(s$scheduled, s$failed, s$completed), c(2L, 2L, 0L))
  expect_identical(c(s$scheduled_lower_bound, s$scheduled_upper_bound), c(0, 1))
  expect_identical(s$illegal_actions, 0L)
  n <- 0L
  stopper <- function() {
    n <<- n + 1L
    n > 3L
  }
  cc <- g3_compare_agents(c("greedy", "random"), budget = list(nodes = 50L), seeds = 1:2, max_plies = 2L,
                          stop_check = stopper)
  expect_identical(nrow(cc$games), 4L)
  expect_true(any(cc$games$status == "cancelled"))
  expect_identical(cc$summary$scheduled, 4L)
  expect_identical(sum(cc$summary[, c("completed", "truncated", "failed", "cancelled")]), 4L)
  expect_error(g3_compare_agents(c("search", "oracle")), class = "g3_incompatible_model")
  expect_error(g3_compare_agents("search"), class = "g3_invalid_state")
  done <- g3_apply_action(mate_in_one(), "m:B:0:0-B:3:0")$state
  expect_error(g3_compare_agents(c("search", "random"), positions = done), class = "g3_invalid_state")
})

test_that("the bootstrap resamples whole starting-position blocks", {
  sched <- data.frame(
    game_index = 1:6, position_id = rep(c("p1", "p2", "p3"), each = 2L), seed = 1L, pair = 1L,
    white_agent = c("A", "B"), black_agent = c("B", "A"), agent1_colour = c("white", "black"),
    status = c("completed", "completed", "completed", "truncated", "completed", "failed"),
    result = c("white", "white", "draw", NA, "white", NA), termination = NA_character_, plies = 10L,
    agent1_score = NA_real_, illegal_actions = c(0L, 0L, 0L, 0L, 0L, 1L), failed_agent = c(NA, NA, NA, NA, NA, "B"),
    error = NA_character_, game_id = paste0("x", 1:6), game_hash = NA_character_, stringsAsFactors = FALSE
  )
  sched$agent1_score <- .g3_compare_score(sched)
  expect_identical(sched$agent1_score, c(1, 0, 0.5, NA, 1, NA))
  dec <- .g3_an_rows_df(list(), .g3_compare_schema("decisions"))
  meta <- list(agents = list(), budget = .g3_budget(list(nodes = 10L)), seeds = 1L, pairs = 1L, max_plies = 10L,
               positions = data.frame(position_id = c("p1", "p2", "p3")), elapsed_ms = 0)
  res <- .g3_compare_result(sched, dec, meta, list(replicates = 500L, seed = 3L, level = 0.9), "A", "B")
  s <- res$summary
  # Hand count: W 2, D 1, L 1 over 4 completed of 6 scheduled.
  expect_identical(c(s$wins, s$draws, s$losses, s$completed, s$truncated, s$failed), c(2L, 1L, 1L, 4L, 1L, 1L))
  expect_identical(s$completed_score, 2.5 / 4)
  expect_identical(s$scheduled_lower_bound, 2.5 / 6)
  expect_identical(s$scheduled_upper_bound, 4.5 / 6)
  expect_identical(s$illegal_actions_agent2, 1L)
  expect_identical(s$illegal_actions_agent1, 0L)
  iv <- res$uncertainty$intervals
  cs <- iv[iv$statistic == "completed_score", ]
  # Block ratios are 1/2, 1/2 and 1/1: every resample lies in [0.5, 1].
  expect_gte(cs$lower, 0.5)
  expect_lte(cs$upper, 1)
  expect_lte(cs$lower, cs$estimate)
  expect_gte(cs$upper, cs$estimate)
  expect_identical(res$uncertainty$level, 0.9)
  expect_identical(res$uncertainty$seed, 3L)
  res2 <- .g3_compare_result(sched, dec, meta, list(replicates = 500L, seed = 3L, level = 0.9), "A", "B")
  expect_identical(res2$uncertainty$intervals, iv)
  one <- .g3_compare_result(sched[1:2, ], dec, meta, list(replicates = 100L, seed = 3L, level = 0.9), "A", "B")
  expect_true(all(is.na(one$uncertainty$intervals$lower)))
  expect_match(one$uncertainty$note, "descriptive only")
})

test_that("combining per-position comparisons equals one joint comparison", {
  pos <- list(m1 = mate_in_one(), m2 = mate_in_one())
  joint <- g3_compare_agents(c("search", "greedy"), positions = pos, budget = list(nodes = 200L), seeds = 1L,
                             max_plies = 4L, bootstrap = list(replicates = 200L))
  parts <- lapply(names(pos), function(nm) {
    g3_compare_agents(c("search", "greedy"), positions = pos[nm], budget = list(nodes = 200L), seeds = 1L,
                      max_plies = 4L, bootstrap = list(replicates = 200L))
  })
  comb <- .g3_compare_combine(parts, list(replicates = 200L))
  cols <- c("position_id", "status", "result", "agent1_score", "game_hash")
  expect_identical(comb$games[, cols], joint$games[, cols])
  expect_identical(comb$summary[, c("completed", "completed_score")], joint$summary[, c("completed", "completed_score")])
  expect_identical(comb$uncertainty$intervals, joint$uncertainty$intervals)
})

test_that("the match job type wraps the comparison", {
  ctx <- new.env()
  ctx$should_stop <- function() FALSE
  ctx$timed_out <- function() FALSE
  ctx$progress <- function(done, total, unit) invisible(NULL)
  out <- .g3_job_match(list(agents = c("greedy", "random"), budget = list(nodes = 20L), max_plies = 2L), ctx)
  expect_s3_class(out, "g3_comparison")
  expect_identical(out$summary$scheduled, 2L)
})
