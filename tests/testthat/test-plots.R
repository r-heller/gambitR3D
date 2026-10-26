# Phase 10: plot data, scales, perspective and aggregation (no image
# snapshots).

skip_if_not_installed("ggplot2")
skip_if_not_installed("viridisLite")

plot_game <- function() {
  g <- g3_new_game()
  set.seed(31)
  for (i in 1:14) {
    acts <- g3_legal_actions(g$state)
    if (!nrow(acts)) break
    g <- g3_game_apply(g, acts$action_id[sample.int(nrow(acts), 1L)])
  }
  g
}

overlap_game <- function() {
  st <- g3_setup_position(pcs("wK K white WK:0:0", "wN N white W:0:2", "wB B white N:0:0", "bK K black B:3:3"))
  g <- .g3_game_new(st)
  g <- g3_game_apply(g, "m:WK:0:0-WK:1:0")
  g3_game_apply(g, "m:B:3:3-B:2:3")
}

test_that("the evaluation trajectory labels scale and perspective and keeps gaps", {
  r <- g3_analyse_game(plot_game(), budget = list(nodes = 1000L))
  p <- g3_plot_game(r, metric = "expected_score", perspective = "white")
  expect_true(ggplot2::is_ggplot(p))
  meta <- attr(p, "g3_plot_meta")
  expect_false(meta$empty)
  expect_match(meta$y_label, "Expected score")
  expect_match(meta$y_label, "White's perspective")
  labs <- ggplot2::get_labs(p)
  expect_match(labs$subtitle, "uncalibrated")
  expect_match(labs$subtitle, "not a probability")
  expect_false(grepl("centipawn", paste(labs, collapse = " "), ignore.case = TRUE) &&
                 !grepl("not centipawn", paste(labs, collapse = " "), ignore.case = TRUE))
  b <- ggplot2::ggplot_build(p)
  expect_identical(b$layout$panel_params[[1]]$y.range[1] <= 0 && b$layout$panel_params[[1]]$y.range[2] >= 1, TRUE)
  d <- attr(p, "g3_plot_data")
  ev <- r$evaluations
  expect_equal(d$value[!is.na(d$value)], ev$expected_score_white[!is.na(d$value)])
  # Mover perspective is the complement on Black's positions.
  pm <- attr(g3_plot_game(r, "expected_score", "mover"), "g3_plot_data")
  black <- ev$side == "black" & !is.na(pm$value)
  expect_equal(pm$value[black], 1 - d$value[black])
  # A missing middle score splits the line into separate groups.
  r2 <- r
  r2$evaluations$expected_score_white[5] <- NA
  r2$evaluations$expected_score_mover[5] <- NA
  r2$evaluations$expected_score_kind[5] <- NA
  r2$evaluations$raw_score[5] <- NA
  p2 <- g3_plot_game(r2)
  d2 <- attr(p2, "g3_plot_data")
  expect_true(is.na(d2$value[5]))
  expect_false(identical(d2$segment[4], d2$segment[6]))
  line <- ggplot2::ggplot_build(p2)$data[[2]]
  expect_gte(length(unique(line$group[!is.na(line$y)])), 2L)
  expect_identical(attr(p2, "g3_plot_meta")$n_missing, sum(is.na(r2$evaluations$raw_score)))
  # Plotting does not modify the analysis.
  before <- r
  g3_plot_game(r, "score", "mover")
  expect_identical(r, before)
})

test_that("event markers use shapes as well as colour", {
  r <- g3_analyse_game(plot_game(), budget = list(nodes = 1000L))
  p <- g3_plot_game(r)
  mk <- attr(p, "g3_plot_meta")$markers
  n_ev <- sum(r$moves$capture) + sum(r$moves$check) + sum(r$moves$platform_action)
  if (nrow(mk)) {
    sc <- p$scales$get_scales("shape")
    expect_false(is.null(sc))
    expect_true(all(mk$event %in% c("capture", "check", "platform")))
  }
  expect_lte(nrow(mk), n_ev)
  none <- attr(g3_plot_game(r, events = character()), "g3_plot_meta")$markers
  expect_identical(nrow(none), 0L)
})

test_that("incompatible score scales are refused or kept apart", {
  r <- g3_analyse_game(plot_game(), budget = list(nodes = 1000L))
  mixed <- r
  half <- seq_len(nrow(mixed$evaluations)) > 7L & mixed$evaluations$score_kind %in% "heuristic"
  mixed$evaluations$score_unit[half] <- "model_value_units"
  mixed$evaluations$scale_id[half] <- "model:tiny|rev1|model_value_units"
  expect_error(g3_plot_game(mixed, metric = "score"), class = "g3_incompatible_model")
  p <- g3_plot_game(mixed, metric = "expected_score")
  meta <- attr(p, "g3_plot_meta")
  expect_identical(length(meta$scale_ids), 2L)
  d <- attr(p, "g3_plot_data")
  seg_scale <- tapply(d$scale_id, d$segment, function(x) length(unique(x)))
  expect_true(all(seg_scale == 1L))
  expect_match(ggplot2::get_labs(p)$subtitle, "never joined")
})

test_that("model-free and empty inputs give informative empty plots", {
  r <- g3_analyse_game(plot_game(), evaluator = "none")
  p <- g3_plot_game(r)
  meta <- attr(p, "g3_plot_meta")
  expect_true(meta$empty)
  expect_match(meta$reason, "not_requested")
  expect_true(ggplot2::is_ggplot(p))
  expect_false(attr(g3_plot_game(r, "material"), "g3_plot_meta")$empty)
  expect_false(attr(g3_plot_game(r, "mobility"), "g3_plot_meta")$empty)
  expect_true(attr(g3_plot_game(r, "move_loss"), "g3_plot_meta")$empty)
  r0 <- g3_analyse_game(g3_new_game(), evaluator = "none")
  expect_true(attr(g3_plot_board_heatmap(r0, "visits"), "g3_plot_meta")$empty)
  expect_true(attr(g3_plot_board_heatmap(r0, "occupancy", plies = 99L), "g3_plot_meta")$empty)
  run <- g3_analyse_database(list(), evaluator = "none")
  expect_true(attr(g3_plot_collection(run, "outcome"), "g3_plot_meta")$empty)
  expect_error(g3_plot_game(list()), class = "g3_invalid_state")
  expect_error(g3_plot_game(r, metric = "elo"))
})

test_that("board-local heatmaps keep overlapping boards apart; projected mode states its aggregation", {
  r <- g3_analyse_game(overlap_game(), evaluator = "none")
  p <- g3_plot_board_heatmap(r, metric = "occupancy", coordinate_space = "board_local")
  d <- attr(p, "g3_plot_data")
  meta <- attr(p, "g3_plot_meta")
  expect_identical(nrow(d), 64L)
  expect_identical(levels(d$board), c("W", "N", "B", "WQ", "WK", "BQ", "BK"))
  expect_identical(d$value[d$square_id == "W:0:2"], 1)
  expect_identical(d$value[d$square_id == "N:0:0"], 1)
  expect_identical(unique(d$denominator), 3L)
  expect_match(meta$denominator_label, "3 selected positions")
  expect_match(meta$aggregation, "^none")
  b <- ggplot2::ggplot_build(p)
  expect_identical(length(unique(b$layout$layout$PANEL)), 7L)
  fill <- p$scales$get_scales("fill")
  expect_identical(toupper(substr(fill$palette(0), 1L, 7L)), toupper(substr(viridisLite::viridis(256)[1], 1L, 7L)))
  pp <- g3_plot_board_heatmap(r, metric = "occupancy", coordinate_space = "projected")
  dp <- attr(pp, "g3_plot_data")
  mp <- attr(pp, "g3_plot_meta")
  b3 <- dp[dp$file == 1L & dp$rank == 3L, ]
  expect_identical(b3$numerator, 6L)
  expect_identical(b3$value, 2)
  expect_identical(b3$physical_squares, 2L)
  expect_match(mp$aggregation, "summed over all boards")
  expect_match(ggplot2::get_labs(pp)$subtitle, "Aggregation")
  # Attacks and visits carry their own denominators.
  pa <- g3_plot_board_heatmap(r, metric = "attacks", coordinate_space = "board_local", color = "white")
  da <- attr(pa, "g3_plot_data")
  sq <- r$squares
  expect_identical(da$numerator[da$square_id == "N:0:0"], sum(sq$attacked_by_white[sq$square_id == "N:0:0"]))
  pv <- g3_plot_board_heatmap(r, metric = "visits")
  dv <- attr(pv, "g3_plot_data")
  expect_identical(unique(dv$denominator), 2L)
  expect_identical(dv$numerator[dv$square_id == "WK:1:0"], 1L)
  expect_identical(sum(dv$numerator), 2L)
  expect_error(g3_plot_board_heatmap(r, coordinate_space = "screen"))
})

test_that("collection and comparison plots carry counts and denominators", {
  gs <- list(plot_game(), g3_game_event(g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2"), "truncated"))
  gs[[2]]$metadata$game_id <- "short"
  run <- g3_analyse_database(gs, evaluator = "none")
  po <- g3_plot_collection(run, "outcome")
  d <- attr(po, "g3_plot_data")
  expect_identical(sum(d$n_games), 2L)
  expect_true(all(d$denominator == 2L))
  expect_identical(d$n_games[d$category == "truncated"], 1L)
  expect_identical(d$n_games[d$category == "draw"], 0L)
  pl <- attr(g3_plot_collection(run, "platform_use"), "g3_plot_data")
  expect_identical(sum(pl$actions), run$summary$platform_use$total_actions)
  expect_identical(nrow(attr(g3_plot_collection(run, "length"), "g3_plot_data")), 2L)
  cmp <- g3_compare_agents(c("greedy", "random"), budget = list(nodes = 30L), seeds = 1:2, max_plies = 3L,
                           bootstrap = list(replicates = 50L))
  ps <- g3_plot_comparison(cmp, "score")
  ds <- attr(ps, "g3_plot_data")
  expect_identical(ds$scheduled[1], 4L)
  expect_identical(ds$upper_bound[1], cmp$summary$scheduled_upper_bound)
  pc <- attr(g3_plot_comparison(cmp, "outcomes"), "g3_plot_data")
  expect_identical(sum(pc$n_games), 4L)
  expect_true(all(pc$denominator == 2L))
  expect_true(ggplot2::is_ggplot(g3_plot_comparison(cmp, "latency")))
})
