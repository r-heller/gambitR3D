# Phase 09: single-game analysis on hand-verifiable synthetic games.

play_ids <- function(state, ids) {
  g <- .g3_game_new(state)
  for (id in ids) g <- g3_game_apply(g, id)
  g
}

capture_game <- function() {
  # White Rb1 takes the undefended queen on b4 (same board W, file b).
  st <- g3_setup_position(pcs("wR R white W:0:0", "wK K white WK:0:0", "bQ Q black W:0:3", "bK K black B:3:3"))
  play_ids(st, "m:W:0:0-W:0:3")
}

mate_game <- function() {
  # Black to act; Qb5-e5 on B is mate (engine terminal tests).
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  play_ids(st, "m:B:0:0-B:3:0")
}

repetition_game <- function() {
  st <- g3_setup_position(pcs("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"))
  path <- c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3")
  play_ids(st, c(path, path))
}

strip_timing <- function(df) df[, setdiff(names(df), c("elapsed_ms", "cache_hit")), drop = FALSE]

test_that("a capture is recorded exactly, with material and perspective", {
  r <- g3_analyse_game(capture_game(), evaluator = "search", budget = list(nodes = 200L))
  expect_s3_class(r, "g3_analysis")
  expect_true(all(c("game", "moves", "positions", "evaluations", "events", "summary", "provenance") %in% names(r)))
  m <- r$moves
  expect_identical(nrow(m), 1L)
  expect_identical(m$action_type, "piece_move")
  expect_true(m$capture)
  expect_identical(m$captured_piece_id, "bQ")
  expect_identical(m$captured_type, "Q")
  expect_identical(c(m$from_board, m$to_board), c("W", "W"))
  expect_false(m$board_transfer)
  expect_identical(c(m$from_file, m$from_rank, m$to_file, m$to_rank), c(1L, 1L, 1L, 4L))
  p <- r$positions
  expect_identical(p$white_material, c(500L, 500L))
  expect_identical(p$black_material, c(900L, 0L))
  expect_identical(p$material_balance_white, c(-400L, 500L))
  expect_true("capture" %in% r$tactics$label)
  expect_identical(r$tactics$claim_strength[r$tactics$label == "capture"], "exact")
  # Perspective: White is a rook up after the capture; Black acts at ply 1.
  e <- r$evaluations
  expect_identical(e$side, c("white", "black"))
  expect_identical(e$raw_perspective, c("side_to_act", "side_to_act"))
  expect_true(e$score_white[2] > 0)
  expect_equal(e$score_white, ifelse(e$side == "white", e$raw_score, -e$raw_score))
  expect_equal(e$score_mover, e$raw_score)
  expect_equal(e$expected_score_white, ifelse(e$side == "white", e$expected_score_mover, 1 - e$expected_score_mover))
  expect_equal(e$expected_score_mover, 1 / (1 + exp(-e$raw_score / 400)))
  expect_identical(e$expected_score_kind, rep("heuristic_logistic_map", 2L))
  expect_identical(e$calibration, rep("uncalibrated", 2L))
  # The played capture is the best root action; loss is matched and zero.
  expect_identical(e$best_action_id[1], "m:W:0:0-W:0:3")
  expect_identical(e$played_action_id[1], "m:W:0:0-W:0:3")
  expect_equal(e$move_loss_raw[1], 0)
  expect_identical(e$played_rank[1], 1L)
})

test_that("checkmate is exact; the mate in one is a bounded-search claim", {
  r <- g3_analyse_game(mate_game(), budget = list(nodes = 3000L))
  expect_true(r$moves$check)
  expect_true(r$moves$checkmate)
  expect_identical(r$moves$notation, paste0(r$moves$label, "#"))
  expect_identical(r$summary$result, "black")
  expect_identical(r$summary$termination, "checkmate")
  expect_identical(r$summary$outcome_class, "rule_decisive")
  e <- r$evaluations
  expect_identical(e$best_kind[1], "mate")
  expect_identical(e$best_mate_in[1], 1L)
  expect_identical(e$mate_in_mover[1], 1L)
  expect_identical(e$mate_in_white[1], -1L)
  expect_identical(e$expected_score_white, c(0, 0))
  expect_identical(e$score_kind[2], "terminal")
  expect_identical(e$expected_score_kind[2], "terminal_exact")
  expect_true(is.na(e$score_white[1]))
  expect_identical(r$tactics$label, "checkmate")
  expect_identical(r$tactics$claim_strength, "exact")
  # Missing the mate is labelled from the same completed search.
  start <- g3_replay_game(mate_game(), ply = 0L)
  acts <- g3_legal_actions(start)$action_id
  non_mates <- acts[vapply(acts, function(a) !identical(g3_apply_action(start, a)$state$outcome$reason, "checkmate"), TRUE)]
  r2 <- g3_analyse_game(play_ids(start, non_mates[1]), budget = list(nodes = 3000L))
  tl <- r2$tactics
  expect_true("missed_forced_mate" %in% tl$label)
  expect_identical(tl$claim_strength[tl$label == "missed_forced_mate"], "bounded_search")
  expect_true(is.na(r2$evaluations$move_loss_raw[1]))
  expect_gt(r2$evaluations$expected_loss_mover[1], 0)
})

test_that("platform relocation and rotation are typed, with transport coordinates", {
  # The only escape from check is moving the king's platform to N.
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bN N black W:2:1", "bR1 R black W:0:0", "bR2 R black BK:1:1", "bK K black B:0:3"
  ))
  r <- g3_analyse_game(play_ids(st, "t:WK>N:R:lo@0"), evaluator = "none")
  m <- r$moves
  expect_identical(m$action_type, "platform_relocation")
  expect_identical(c(m$platform_id, m$from_dock, m$to_dock), c("WK", "W:R:lo", "N:R:lo"))
  expect_identical(m$rotation_delta, 0L)
  expect_true(is.na(m$to_square))
  expect_identical(c(m$from_level, m$to_level), c("W", "N"))
  expect_true(m$level_transfer)
  expect_false(m$board_transfer)
  expect_identical(m$transfer_mode, "transport")
  expect_true(m$in_check_before)
  expect_true(m$platform_action)
  cpa <- r$tactics[r$tactics$label == "consequential_platform_action", ]
  expect_identical(nrow(cpa), 1L)
  expect_match(cpa$evidence, "answers a check")
  expect_identical(cpa$claim_strength, "heuristic")
  # Half-turn: WQ:0:1 is a1 at W:L:lo; at W:L:hi turned it is b4.
  st2 <- g3_setup_position(pcs("wP P white WQ:0:1", "wK K white W:0:0", "bK K black B:3:3"))
  r2 <- g3_analyse_game(play_ids(st2, "t:WQ>W:L:hi@180"), evaluator = "none")
  m2 <- r2$moves
  expect_identical(m2$action_type, "platform_rotation")
  expect_identical(c(m2$from_file, m2$from_rank, m2$to_file, m2$to_rank), c(0L, 1L, 1L, 4L))
  expect_identical(m2$from_square, "WQ:0:1")
  expect_false(m2$level_transfer)
  pl <- r2$platforms[r2$platforms$platform_id == "WQ", ]
  expect_identical(pl$dock, c("W:L:lo", "W:L:hi"))
  expect_identical(pl$orientation, c(0L, 180L))
  expect_identical(pl$controller, c("white", "white"))
  expect_identical(r2$summary$n_platform_rotations, 1L)
  # The pawn keeps its physical square; its projected coordinates change.
  pc <- r2$pieces[r2$pieces$piece_id == "wP", ]
  expect_identical(pc$square_id, c("WQ:0:1", "WQ:0:1"))
  expect_identical(pc$file, c(0L, 1L))
  expect_identical(pc$rank, c(1L, 4L))
})

test_that("squares sharing a projected coordinate on different boards stay distinct", {
  st <- g3_setup_position(pcs("wK K white WK:0:0", "wN N white W:0:2", "wB B white N:0:0", "bK K black B:3:3"))
  r <- g3_analyse_game(.g3_game_new(st), evaluator = "none")
  sq <- r$squares
  b3 <- sq[sq$file == 1L & sq$rank == 3L, ]
  expect_setequal(b3$square_id, c("W:0:2", "N:0:0"))
  expect_setequal(b3$occupant_id, c("wN", "wB"))
  expect_identical(nrow(sq), 64L)
  expect_identical(anyDuplicated(sq$square_id), 0L)
  # Attack status belongs to the coordinate; both squares share it.
  expect_identical(length(unique(b3$attacked_by_white)), 1L)
})

test_that("attacked squares follow attack semantics, not legal moves", {
  # A pawn on c2 attacks b3 and d3 (on every board there) but not c3,
  # although advancing to c3 is legal.
  st <- g3_setup_position(pcs("wK K white WK:0:0", "wP P white W:1:1", "bK K black B:3:3"))
  r <- g3_analyse_game(.g3_game_new(st), evaluator = "none")
  sq <- r$squares
  att <- stats::setNames(sq$attacked_by_white, sq$square_id)
  expect_true(all(att[c("W:0:2", "N:0:0", "W:2:2", "N:2:0")]))
  expect_false(any(att[c("W:1:2", "N:1:0")]))
  expect_true("m:W:1:1-W:1:2" %in% g3_legal_actions(st)$action_id)
  p <- r$positions
  expect_identical(p$white_attacked_squares, sum(sq$attacked_by_white))
  expect_lte(p$white_attacked_coordinates, p$white_attacked_squares)
  # Promotion choices inflate actions but not destinations.
  st2 <- g3_setup_position(pcs("wP P white B:1:2", "wPb P white B:0:2", "wK K white W:0:0",
                               "bK K black BQ:0:1", "bP P black BQ:1:0"))
  r2 <- g3_analyse_game(.g3_game_new(st2), evaluator = "none")
  mob <- r2$mobility[r2$mobility$color == "white" & r2$mobility$piece_type == "P" & r2$mobility$board == "B", ]
  expect_identical(mob$basis, "legal")
  expect_gte(mob$n_actions - mob$n_destinations, 3L)
  expect_identical(r2$positions$white_actions, nrow(g3_legal_actions(st2)))
  expect_identical(r2$positions$black_mobility_basis, "null_move_legal")
})

test_that("forks and pins are labelled with their definitions and evidence", {
  st <- g3_setup_position(pcs("wK K white WK:0:0", "wN N white W:3:3", "bK K black B:3:3", "bR R black B:1:3"))
  r <- g3_analyse_game(play_ids(st, "m:W:3:3-N:2:3"), evaluator = "none")
  tl <- r$tactics
  expect_setequal(tl$label, c("check", "fork"))
  fk <- tl[tl$label == "fork", ]
  expect_identical(fk$claim_strength, "heuristic")
  expect_match(fk$evidence, "bK")
  expect_match(fk$evidence, "bR")
  expect_identical(r$positions$white_forking_pieces, c(0L, 1L))
  # A rook on e1 pins the e6 knight to the e8 king (removal counterfactual).
  st2 <- g3_setup_position(pcs("wK K white WK:0:0", "wR R white W:0:0", "bK K black B:3:3", "bN N black B:3:1"))
  r2 <- g3_analyse_game(play_ids(st2, "m:W:0:0-W:3:0"), evaluator = "none")
  pin <- r2$tactics[r2$tactics$label == "pin", ]
  expect_identical(pin$claim_strength, "exact")
  expect_identical(pin$pieces, "bN")
  expect_match(pin$evidence, "bN pinned by wR")
  expect_identical(r2$positions$black_pinned, c(0L, 1L))
  pcs2 <- r2$pieces[r2$pieces$ply == 1L & r2$pieces$piece_id == "bN", ]
  expect_identical(pcs2$pinned_by, "wR")
  expect_true(pcs2$hanging)
  defs <- g3_feature_dictionary()
  expect_true(all(paste0("label:", c("check", "pin", "fork", "hanging_piece")) %in% defs$name))
})

test_that("rule draws are draws; truncation is a separate session outcome", {
  r <- g3_analyse_game(repetition_game(), budget = list(nodes = 150L))
  expect_identical(r$positions$repetition_count[c(1, 5, 9)], c(1L, 2L, 3L))
  expect_identical(r$summary$result, "draw")
  expect_identical(r$summary$termination, "repetition")
  expect_identical(r$summary$outcome_class, "rule_draw")
  expect_identical(r$evaluations$score_kind[9], "terminal")
  expect_identical(r$evaluations$expected_score_white[9], 0.5)
  # Same board, different draw context: different analysis contexts.
  expect_identical(r$positions$position_key[1], r$positions$position_key[5])
  expect_false(identical(r$positions$analysis_context_hash[1], r$positions$analysis_context_hash[5]))
  # Stalemate by promotion.
  st <- g3_setup_position(pcs("wP P white B:1:2", "wPb P white B:0:2", "wK K white W:0:0",
                              "bK K black BQ:0:1", "bP P black BQ:1:0"))
  rs <- g3_analyse_game(play_ids(st, "m:B:1:2-B:1:3=Q"), evaluator = "none")
  expect_identical(rs$moves$action_type, "promotion")
  expect_identical(rs$moves$promotion, "Q")
  expect_identical(rs$summary$termination, "stalemate")
  expect_identical(rs$summary$result, "draw")
  # Truncation.
  g <- g3_game_event(play_ids(g3_new_game()$state, "m:W:0:1-W:0:2"), "truncated", reason = "max_plies")
  rt <- g3_analyse_game(g, evaluator = "none")
  expect_identical(rt$summary$result, "truncated")
  expect_identical(rt$summary$outcome_class, "truncated")
  expect_identical(rt$events$type, "truncated")
  expect_identical(rt$events$reason, "max_plies")
  expect_identical(rt$positions$status, c("ongoing", "ongoing"))
})

test_that("invalid logs are rejected with the first invalid ply", {
  g <- capture_game()
  bad <- g
  bad$events$action_id[1] <- "m:W:0:0-W:3:3"
  expect_error(g3_analyse_game(bad, evaluator = "none"), class = "g3_illegal_action")
  err <- tryCatch(g3_analyse_game(bad, evaluator = "none"), error = function(e) e)
  expect_identical(err$context$first_invalid_ply, 1L)
  bad2 <- g
  bad2$events$position_key[1] <- strrep("0", 64)
  expect_error(g3_analyse_game(bad2, evaluator = "none"), class = "g3_invalid_state")
  bad3 <- g
  bad3$state <- bad3$initial_state
  expect_error(g3_analyse_game(bad3, evaluator = "none"), class = "g3_invalid_state")
  expect_error(g3_analyse_game(list(), evaluator = "none"), class = "g3_invalid_state")
  expect_error(g3_analyse_game(g, evaluator = "oracle"), class = "g3_incompatible_model")
  expect_error(g3_analyse_game(g, budget = list(plies = 2)), class = "g3_budget_exceeded")
})

test_that("an empty game gives typed zero-row tables", {
  r <- g3_analyse_game(g3_new_game(), evaluator = "none")
  for (tb in c("moves", "tactics", "events")) {
    expect_identical(nrow(r[[tb]]), 0L)
    sch <- .g3_an_schema(tb)
    expect_identical(names(r[[tb]]), names(sch))
    expect_identical(unname(vapply(r[[tb]], typeof, "")), unname(sch))
  }
  expect_identical(nrow(r$positions), 1L)
  expect_identical(nrow(r$summary), 1L)
  expect_identical(r$summary$result, "unfinished")
  expect_true(r$summary$standard_start)
  expect_identical(r$evaluations$missing_reason, "not_requested")
  expect_true(is.na(r$evaluations$raw_score))
})

test_that("analysis works without any model; a missing model gives NA with a reason", {
  r <- g3_analyse_game(capture_game(), evaluator = "model:not-installed")
  e <- r$evaluations
  expect_true(all(is.na(e$raw_score)))
  expect_true(all(startsWith(e$missing_reason, "evaluator_unavailable")))
  expect_true(all(is.na(e$expected_score_white)))
  expect_identical(nrow(r$positions), 2L)
  expect_identical(r$provenance$evaluator, "model:not-installed")
})

test_that("analysis is deterministic and display settings cannot change it", {
  g <- capture_game()
  a <- g3_analyse_game(g, budget = list(nodes = 300L))
  b <- g3_analyse_game(g, budget = list(nodes = 300L))
  for (tb in c("moves", "positions", "tactics", "events", "mobility", "platforms", "pieces", "squares", "summary")) {
    expect_identical(a[[tb]], b[[tb]])
  }
  expect_identical(strip_timing(a$evaluations), strip_timing(b$evaluations))
  expect_identical(a$provenance$analysis_id, b$provenance$analysis_id)
  # Neutral height, view and camera live in the separate ui envelope.
  dir <- withr::local_tempdir()
  p1 <- file.path(dir, "low.g3.json")
  p2 <- file.path(dir, "high.g3.json")
  g3_write_game(g, p1, ui = list(view = "3d", neutral_height = "lowest", camera = list(theta = 30)))
  g3_write_game(g, p2, ui = list(view = "2d", neutral_height = "highest"))
  r1 <- g3_analyse_game(p1, budget = list(nodes = 300L))
  r2 <- g3_analyse_game(p2, budget = list(nodes = 300L))
  for (tb in c("moves", "positions", "tactics", "squares", "summary")) expect_identical(r1[[tb]], r2[[tb]])
  expect_identical(strip_timing(r1$evaluations), strip_timing(r2$evaluations))
  expect_identical(strip_timing(r1$evaluations), strip_timing(a$evaluations))
})

test_that("tables agree with each other and with the feature dictionary", {
  set.seed(21)
  g <- g3_new_game()
  for (i in 1:16) {
    acts <- g3_legal_actions(g$state)
    if (!nrow(acts)) break
    g <- g3_game_apply(g, acts$action_id[sample.int(nrow(acts), 1L)])
  }
  r <- g3_analyse_game(g, evaluator = "none")
  dict <- g3_feature_dictionary()
  for (tb in c("moves", "positions", "evaluations", "tactics", "events", "mobility", "platforms", "pieces", "squares", "summary")) {
    expect_true(all(names(r[[tb]]) %in% dict$name[dict$table == tb]), info = tb)
  }
  expect_true(all(c("name", "unit", "denominator", "missing_policy", "definition") %in% names(dict)))
  expect_false(anyNA(dict$definition))
  p <- r$positions
  expect_identical(nrow(p), 17L)
  expect_identical(p$position_key[-1], r$moves$position_key)
  for (k in seq_len(nrow(p))) {
    pk <- r$pieces[r$pieces$ply == p$ply[k], ]
    expect_identical(p$white_hanging[k], sum(pk$hanging & pk$color == "white"))
    mb <- r$mobility[r$mobility$ply == p$ply[k] & r$mobility$color == p$side[k], ]
    expect_identical(sum(mb$n_actions), p[[paste0(p$side[k], "_actions")]][k])
    expect_identical(p[[paste0(p$side[k], "_actions")]][k], nrow(g3_legal_actions(g3_replay_game(g, p$ply[k]))))
    expect_identical(sum(r$squares$ply == p$ply[k] & !is.na(r$squares$occupant_id)), nrow(pk))
  }
  expect_identical(r$summary$n_plies, 16L)
  expect_identical(r$summary$n_captures, sum(r$moves$capture))
})

# A stand-in for a network model's position analysis (MCTS mode), so the
# analysis tables can be checked without Python. The second legal action is
# the most visited; the last has the largest Q. White to act: W/D/L
# 0.5/0.3/0.2, Black to act: 0.2/0.3/0.5.
fake_network_analysis <- function(state, evaluator = NULL, budget = NULL, actions = NULL) {
  ids <- g3_legal_actions(state)$action_id
  n <- length(ids)
  q <- seq(-0.4, 0.4, length.out = n)
  visits <- rev(seq_len(n))
  visits[2L] <- n + 5L
  rk <- order(order(-visits, seq_len(n)))
  wdl <- if (identical(state$side, "white")) c(win = 0.5, draw = 0.3, loss = 0.2) else c(win = 0.2, draw = 0.3, loss = 0.5)
  alt <- data.frame(action_id = ids, label = ids, type = "move", score = q, score_kind = "mcts_q", mate_in = NA_integer_,
                    bound = "model", rank = as.integer(rk), probability = 1 / n, logit = 0, visits = as.integer(visits),
                    q = q, stringsAsFactors = FALSE)
  if (!is.null(actions)) {
    alt <- alt[alt$action_id %in% actions, , drop = FALSE]
    alt$rank <- as.integer(rank(alt$rank, ties.method = "first"))
  }
  structure(list(
    alternatives = alt, root_score = unname(wdl[["win"]] - wdl[["loss"]]), score_kind = "model_value",
    perspective = "side_to_act", side = state$side, pv = alt$action_id[alt$rank == 1L], pv_labels = character(),
    depth = 2L, completeness = "complete", cost = list(nodes = budget$simulations %||% 32, elapsed_ms = 1),
    provenance = list(model_id = "fake-net", weights_digest = "rev-x", budget = budget, mode = "mcts"),
    wdl = wdl, expected_score = unname(wdl[["win"]] + 0.5 * wdl[["draw"]]), value_source = "network"
  ), class = "g3_position_analysis")
}

test_that("model analyses have White and mover views and WDL expected scores (F15)", {
  local_mocked_bindings(g3_analyse_position = fake_network_analysis, .package = "gambitR3D")
  m <- structure(list(model_id = "fake-net", revision = "rev-x"), class = "g3_model")
  g <- play_ids(g3_new_game()$state, c("m:W:0:1-W:0:2", "m:B:2:2-B:2:1"))
  r <- g3_analyse_game(g, evaluator = m, budget = list(simulations = 4L, mode = "mcts"))
  e <- r$evaluations
  expect_identical(e$score_kind, rep("model_value", 3L))
  expect_identical(e$side, c("white", "black", "white"))
  # Raw scores are the model's value for the side to act; views are signed.
  expect_equal(e$raw_score, c(0.3, -0.3, 0.3))
  expect_equal(e$score_mover, e$raw_score)
  expect_equal(e$score_white, c(0.3, 0.3, 0.3))
  # Expected scores come from the WDL head, p(win) + 0.5 p(draw), not the logistic map.
  expect_equal(e$expected_score_mover, c(0.65, 0.35, 0.65))
  expect_equal(e$expected_score_white, c(0.65, 0.65, 0.65))
  expect_identical(unique(e$expected_score_kind), "model_wdl")
  expect_identical(unique(e$calibration), "uncalibrated")
  expect_false(isTRUE(all.equal(e$expected_score_mover[1], 1 / (1 + exp(-0.3 / 400)))))
  # The best action is the model's first choice (most visited), not the largest Q.
  ids <- g3_legal_actions(g3_new_game()$state)$action_id
  n <- length(ids)
  q <- seq(-0.4, 0.4, length.out = n)
  pi <- match("m:W:0:1-W:0:2", ids)
  expect_identical(e$best_action_id[1], ids[2])
  expect_identical(e$best_kind[1], "mcts_q")
  expect_equal(e$best_score[1], q[2])
  expect_equal(e$played_score[1], q[pi])
  expect_identical(e$played_rank[1], as.integer(if (pi == 2L) 1L else if (pi == 1L) 2L else pi))
  expect_equal(e$expected_loss_mover[1], (q[2] - q[pi]) / 2)
  expect_true(all(is.na(e$move_loss_raw)))
  # Provenance names the model scale and the model budget.
  pv <- r$provenance
  expect_identical(pv$model_revision, "rev-x")
  expect_identical(pv$budget$simulations, 4L)
  expect_identical(pv$budget$mode, "mcts")
  expect_match(pv$score_scale$raw_unit, "p(win) - p(loss)", fixed = TRUE)
  expect_match(pv$score_scale$definition, "p(win) + 0.5 p(draw)", fixed = TRUE)
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridisLite")
  p <- g3_plot_game(r, metric = "expected_score")
  expect_false(attr(p, "g3_plot_meta")$empty)
  expect_match(ggplot2::get_labs(p)$subtitle, "WDL head", fixed = TRUE)
  ps <- g3_plot_game(r, metric = "score")
  expect_false(attr(ps, "g3_plot_meta")$empty)
  expect_match(attr(ps, "g3_plot_meta")$y_label, "Model value", fixed = TRUE)
})

test_that("a model given as an id records its registry revision (F16)", {
  g <- capture_game()
  r1 <- g3_analyse_game(g, evaluator = "model:builtin-search", budget = list(nodes = 150L))
  mobj <- .g3_model_resolve("model:builtin-search")
  rev <- .g3_hash_object(mobj$manifest)
  expect_identical(r1$provenance$model_revision, rev)
  expect_identical(r1$provenance$evaluator_version, rev)
  expect_true(all(r1$evaluations$model_revision == rev))
  # The same revision whether the model is given as an id or as an object.
  r2 <- g3_analyse_game(g, evaluator = mobj, budget = list(nodes = 150L))
  expect_identical(r2$provenance$model_revision, rev)
  expect_identical(r2$positions$analysis_context_hash, r1$positions$analysis_context_hash)
  # An unknown id keeps NA (and its positions NA scores with the reason).
  r3 <- g3_analyse_game(g, evaluator = "model:not-installed")
  expect_true(is.na(r3$provenance$model_revision))
})

test_that("model budget fields are accepted for model evaluators only (F17)", {
  g <- capture_game()
  expect_error(g3_analyse_game(g, budget = list(simulations = 8L)), "simulations", class = "g3_budget_exceeded")
  expect_error(g3_analyse_game(g, budget = list(mode = "mcts")), class = "g3_budget_exceeded")
  r <- g3_analyse_game(g, evaluator = "model:builtin-search", budget = list(nodes = 120L, simulations = 8L, mode = "mcts"))
  expect_identical(r$provenance$budget$simulations, 8L)
  expect_match(.g3_an_budget_text(r$provenance$budget), "8 simulations", fixed = TRUE)
  r2 <- g3_analyse_game(g, evaluator = "model:builtin-search", budget = list(nodes = 120L, simulations = 16L))
  expect_false(identical(r$positions$analysis_context_hash, r2$positions$analysis_context_hash))
  # Unknown fields and bad values are still refused.
  expect_error(g3_analyse_game(g, evaluator = "model:builtin-search", budget = list(plies = 2L)),
               "Unknown budget field: plies", class = "g3_budget_exceeded")
  expect_error(g3_analyse_game(g, evaluator = "model:builtin-search", budget = list(mode = "beam")),
               class = "g3_budget_exceeded")
  expect_error(g3_analyse_game(g, evaluator = "model:builtin-search", budget = list(simulations = 0L)),
               class = "g3_budget_exceeded")
  # Collections take the same budgets.
  run <- g3_analyse_database(list(g), evaluator = "model:builtin-search", budget = list(nodes = 100L, simulations = 4L))
  expect_identical(run$manifest$budget$simulations, 4L)
  expect_output(print(run), "4 simulations", fixed = TRUE)
  expect_error(g3_analyse_database(list(g), evaluator = "none", budget = list(simulations = 4L)),
               class = "g3_budget_exceeded")
})
