# Phase 13: post-game review with evidence (small budgets, fast).

coach_fixture_game <- function() {
  # A short real game: search (100 nodes) against a seeded random baseline.
  g <- g3_new_game(players = list(white = "search-100", black = "random"))
  for (k in 1:8) {
    if (g3_is_terminal(g$state)) break
    ag <- if (g$state$side == "white") "search" else "random"
    d <- g3_choose_action(g$state, agent = ag, budget = list(nodes = 100L), seed = 40L + k)
    g <- g3_game_apply(g, d$action_id)
  }
  g
}

# States of a game's active line keyed by full-state hash.
states_by_hash <- function(game) {
  ids <- g3_game_moves(game)$action_id
  st <- game$initial_state
  out <- list()
  out[[g3_state_hash(st)]] <- st
  for (id in ids) {
    st <- g3_apply_action(st, id)$state
    out[[g3_state_hash(st)]] <- st
  }
  out
}

test_that("a short real game is reviewed with typed claims and evidence", {
  g <- coach_fixture_game()
  rev <- g3_coach_game(g, budget = list(nodes = 300L), max_exercises = 3L,
                       exercise_budget = list(nodes = 100L, depth = 2L))
  expect_s3_class(rev, "g3_coach_review")
  expect_identical(rev$game_hash, g3_game_hash(g))
  expect_identical(rev$rules_hash, g3_rules_profile()$rules_hash)
  expect_identical(nrow(rev$decisions), nrow(g3_game_moves(g)))
  expect_true(all(rev$decisions$perspective == "side_to_act"))
  expect_true(all(rev$claims$type %in% c("legal_fact", "searched", "heuristic", "learning_suggestion")))
  expect_true(all(rev$claims$strength %in% c("exact", "exact_at_depth", "proven", "analysed_line", "heuristic", "suggestion")))
  # Every claim except the overall summary points at stored evidence.
  ref <- rev$claims$evidence_id[!is.na(rev$claims$evidence_id)]
  expect_true(all(ref %in% names(rev$evidence)))
  expect_true(all(rev$claims$template[is.na(rev$claims$evidence_id)] %in% c("review.summary")))
  # Searched claims carry budget, depth and completeness in their evidence.
  for (e in rev$evidence[vapply(rev$evidence, function(x) identical(x$kind, "search"), TRUE)]) {
    expect_true(all(c("budget", "depth", "completeness", "bound", "nodes", "evaluator", "candidates") %in% names(e)))
    expect_identical(e$perspective, "side_to_act")
    expect_lte(e$nodes, 301)
  }
  # No centipawn, Elo, rating or accuracy labels in claims.
  expect_false(any(grepl("centipawn|\\bElo\\b|rating|accuracy", rev$claims$text, ignore.case = TRUE)))
  expect_true(any(grepl("not centipawns", rev$limitations, fixed = TRUE)))
  # Exercises created from the reviewed positions.
  expect_s3_class(rev$exercises, "g3_exercise_set")
  expect_lte(length(rev$exercises$exercises), 3L)
})

test_that("every displayed line replays through the rules engine", {
  g <- coach_fixture_game()
  rev <- g3_coach_game(g, budget = list(nodes = 300L), threshold = 1, max_exercises = 0L)
  lines <- rev$evidence[vapply(rev$evidence, function(x) identical(x$kind, "line"), TRUE)]
  expect_gt(length(lines), 0L)
  states <- states_by_hash(g)
  for (ln in lines) {
    expect_true(ln$verified)
    st <- states[[ln$start_state_hash]]
    if (is.null(st)) {
      # Reply lines start after the played action of their decision.
      d <- rev$decisions[rev$decisions$ply == ln$ply, ]
      st <- g3_apply_action(states[[d$state_hash]], d$played_action_id)$state
    }
    expect_identical(g3_state_hash(st), ln$start_state_hash)
    for (k in seq_along(ln$actions)) {
      step <- g3_apply_action(st, ln$actions[[k]])
      expect_identical(step$event$notation, ln$notation[[k]])
      expect_identical(step$event$post_state_hash, ln$post_state_hashes[[k]])
      st <- step$state
    }
    expect_identical(g3_state_hash(st), ln$final_state_hash)
    # The displayed claim text contains exactly this verified notation.
    shown <- rev$claims$text[rev$claims$evidence_id %in% ln$evidence_id & rev$claims$topic == "line"]
    expect_true(all(grepl(paste(ln$notation, collapse = "  "), shown, fixed = TRUE)))
  }
})

test_that("a missed mate in one is a rules fact; mate is never claimed without proof", {
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  g <- .g3_game_new(st)
  g <- g3_game_apply(g, "m:B:0:0-B:0:1")
  rev <- g3_coach_game(g, budget = list(nodes = 400L, depth = 2L), max_exercises = 1L,
                       exercise_budget = list(nodes = 0L))
  d <- rev$decisions
  expect_true(d$critical[1])
  expect_identical(d$critical_reason[1], "missed_mate")
  expect_identical(d$best_mate_in[1], 1L)
  m1 <- rev$claims[rev$claims$template == "review.mate_in_one", ]
  expect_identical(nrow(m1), 1L)
  expect_identical(m1$type, "legal_fact")
  expect_true(g3_is_terminal(g3_apply_action(st, d$best_action_id)$state))
  # "forces checkmate" appears only with strength "proven".
  forced <- grepl("forces checkmate", rev$claims$text, fixed = TRUE)
  expect_true(all(rev$claims$strength[forced] == "proven"))
  # The exercise from this decision is the validated mate in one.
  expect_true("checkmate" %in% rev$exercises$summary$theme)
})

test_that("the exhaustive mate prover distinguishes proven, refuted and unresolved", {
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  tabs <- .g3_tables()
  core <- .g3_core(st, tabs)
  legal <- .g3_legal(core, tabs)
  ids <- .g3_action_ids(core, legal, tabs)
  mate <- .g3_prove_mate(core, legal[match("m:B:0:0-N:3:2", ids), ], 1L, tabs)
  expect_identical(mate$status, "proven")
  expect_identical(mate$replies, 0L)
  quiet <- .g3_prove_mate(core, legal[match("m:B:0:0-B:0:1", ids), ], 1L, tabs)
  expect_identical(quiet$status, "refuted")
  capped <- .g3_prove_mate(core, legal[match("m:B:0:0-B:0:1", ids), ], 3L, tabs, limit = 5L)
  expect_identical(capped$status, "unresolved")
})

test_that("model evaluators without a runtime raise g3_incompatible_model", {
  g <- g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2")
  expect_error(g3_coach_game(g, evaluator = "model:not-installed", budget = list(nodes = 50L), max_exercises = 0L),
               class = "g3_incompatible_model")
  expect_error(g3_coach_game(g, evaluator = "greedy", max_exercises = 0L), class = "g3_incompatible_model")
})

test_that("model evaluators dispatch through the model hook and are named in the review", {
  skip_if(!nzchar(system.file("models", "registry.json", package = "gambitR3D")), "no model registry in this build")
  g <- g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2")
  # The registry's search entry is a real evaluator reached through the hook.
  rev <- tryCatch(g3_coach_game(g, evaluator = "model:builtin-search", budget = list(nodes = 100L), max_exercises = 0L),
                  g3_incompatible_model = function(e) NULL)
  skip_if(is.null(rev), "registry has no usable search entry")
  expect_identical(rev$evaluator$model_id, "builtin-search")
  expect_true("review.evaluator_model" %in% rev$claims$template)
  expect_true(any(grepl("model estimates", rev$limitations, fixed = TRUE)))
  # An untrained architecture is not a usable evaluator: honest failure.
  expect_error(g3_coach_game(g, evaluator = "model:builtin-square-transformer", budget = list(nodes = 50L),
                             max_exercises = 0L), class = "g3_incompatible_model")
})

test_that("stale analyses and foreign rules hashes are rejected", {
  g <- coach_fixture_game()
  rev <- g3_coach_game(g, budget = list(nodes = 100L), max_exercises = 0L)
  bad <- rev
  bad$decisions$state_hash[2] <- strrep("0", 64)
  expect_error(g3_create_exercises(bad, max_n = 2L), class = "g3_stale_revision")
  moved <- rev
  moved$game <- g3_game_apply(g, g3_legal_actions(g$state)$action_id[1])
  expect_error(g3_create_exercises(moved, max_n = 2L), class = "g3_stale_revision")
  foreign <- rev
  foreign$rules_hash <- strrep("a", 64)
  expect_error(g3_create_exercises(foreign, max_n = 2L), class = "g3_unsupported_rules")
})

test_that("exercises can be created from g3_analyse_game() output", {
  skip_if_not(exists("g3_analyse_game", mode = "function"), "g3_analyse_game() not available")
  g <- coach_fixture_game()
  an <- g3_analyse_game(g, budget = list(nodes = 100L))
  set <- g3_create_exercises(an, max_n = 3L, game = g, budget = list(nodes = 50L))
  expect_s3_class(set, "g3_exercise_set")
  expect_true(all(set$summary$source == "game"))
  expect_true(all(set$summary$game_id == g$metadata$game_id))
  expect_error(g3_create_exercises(an, max_n = 1L), class = "g3_invalid_state")
  other <- g3_game_apply(g, g3_legal_actions(g$state)$action_id[1])
  expect_error(g3_create_exercises(an, max_n = 1L, game = other), class = "g3_stale_revision")
})

test_that("German reviews use the German catalogue", {
  g <- g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2")
  rev <- g3_coach_game(g, budget = list(nodes = 50L), max_exercises = 0L, lang = "de")
  expect_identical(rev$lang, "de")
  expect_match(rev$claims$text[1], "Weiß zieht", fixed = TRUE)
  expect_true(any(grepl("Zentibauern", rev$limitations, fixed = TRUE)))
})

test_that("the coaching job function reports progress and stops cooperatively", {
  g <- coach_fixture_game()
  calls <- new.env()
  calls$n <- 0L
  ctx <- list(
    progress = function(done, total, unit) calls$n <- calls$n + 1L,
    should_stop = function() calls$n >= 2L,
    timed_out = function() FALSE
  )
  out <- .g3_job_coach(list(game = g, budget = list(nodes = 50L), max_exercises = 0L), ctx)
  expect_identical(nrow(out$decisions), 2L)
  expect_identical(out$stop_reason, "stopped")
  expect_identical(attr(out, "stop_reason"), "cancelled")
})

test_that("a missed mate in two is proven exhaustively before it is called forced", {
  # Position from an engine self-play game (search vs random, seed 4, ply
  # 43), written out explicitly: Black's Q N:d6-W:d4 forces mate in two.
  p <- pcs(
    "wPb2 P white W:0:3", "wPa1 P white WQ:0:1", "wK K white W:3:0", "wPe1 P white W:3:1",
    "wPf1 P white WK:1:1", "bNb8 N black W:0:2", "bBc8 B black N:1:1", "bBd8 B black B:2:3",
    "bNe8 N black B:2:1", "bPb7 Q black WQ:1:0", "bPc7 P black N:0:3", "bPd7 P black W:3:3",
    "bPe7 P black N:2:1", "bRa9 R black BQ:0:1", "bQ Q black N:2:3", "bPa8 P black BQ:0:0",
    "bPb8 P black BQ:1:0", "bK K black B:3:3", "bRf9 R black BK:1:1", "bPe8 P black BK:0:0",
    "bPf8 P black BK:1:0"
  )
  p$has_moved <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
                   TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  p$origin_type <- p$type
  p$origin_type[p$id == "bPb7"] <- "P"
  st <- g3_setup_position(p, side = "black")
  tabs <- .g3_tables()
  core <- .g3_core(st, tabs)
  legal <- .g3_legal(core, tabs)
  ids <- .g3_action_ids(core, legal, tabs)
  pf <- .g3_prove_mate(core, legal[match("m:N:2:3-W:2:3", ids), ], 3L, tabs)
  expect_identical(pf$status, "proven")
  expect_identical(pf$replies, 8L)
  # Review a game in which Black played something else. Finding the mate
  # needs about 9000 nodes (0.1 s with the compiled backend, far longer with
  # the R reference engine).
  skip_if_not(identical(.g3_engine(), "cpp"), "needs the compiled search backend for a 10000-node search")
  quiet <- setdiff(ids, "m:N:2:3-W:2:3")[1L]
  g <- g3_game_apply(.g3_game_new(st), quiet)
  rev <- g3_coach_game(g, budget = list(nodes = 10000L, depth = 3L), max_candidates = 200L,
                       max_exercises = 0L)
  d <- rev$decisions
  expect_identical(d$best_action_id, "m:N:2:3-W:2:3")
  expect_identical(d$best_mate_in, 3L)
  expect_identical(d$critical_reason, "missed_mate")
  cl <- rev$claims[rev$claims$template == "review.mate_proven", ]
  expect_identical(nrow(cl), 1L)
  expect_identical(cl$strength, "proven")
  ev <- rev$evidence[[cl$evidence_id]]
  expect_identical(ev$status, "proven")
  expect_identical(ev$method, "exhaustive_and_or")
  expect_identical(ev$replies, 8L)
})

# Stand-in for a network model's candidate analysis (MCTS mode): Q values
# and visit counts rise along the legal-action order, so the last legal
# candidate is the model's choice. No Python is needed.
fake_mcts_analysis <- function(state, evaluator = NULL, budget = NULL, actions = NULL) {
  ids <- g3_legal_actions(state)$action_id
  n <- length(ids)
  q <- seq(-0.6, 0.6, length.out = n)
  visits <- seq_len(n)
  alt <- data.frame(action_id = ids, label = ids, type = "move", score = q, score_kind = "mcts_q", mate_in = NA_integer_,
                    bound = "model", rank = as.integer(order(order(-visits))), probability = 1 / n, logit = 0,
                    visits = as.integer(visits), q = q, stringsAsFactors = FALSE)
  if (!is.null(actions)) {
    alt <- alt[alt$action_id %in% actions, , drop = FALSE]
    alt$rank <- as.integer(rank(alt$rank, ties.method = "first"))
  }
  structure(list(
    alternatives = alt, root_score = 0.1, score_kind = "model_value", perspective = "side_to_act", side = state$side,
    pv = alt$action_id[alt$rank == 1L], pv_labels = character(), depth = 3L, completeness = "complete",
    cost = list(nodes = budget$simulations %||% 32, elapsed_ms = 1),
    provenance = list(model_id = "fake-net", weights_digest = "rev-x", budget = budget, mode = "mcts"),
    wdl = c(win = 0.4, draw = 0.3, loss = 0.3), expected_score = 0.55, value_source = "network"
  ), class = "g3_position_analysis")
}

test_that("model-assisted coaching states the model's budget and units, not search text (F14, F17)", {
  local_mocked_bindings(g3_analyse_position = fake_mcts_analysis, .package = "gambitR3D")
  m <- structure(list(model_id = "fake-net", manifest = list(kind = "trained_checkpoint", weights_digest = "rev-x")),
                 class = "g3_model")
  g <- g3_new_game()
  for (id in c("m:W:0:1-W:0:2", "m:B:2:2-B:2:1", "m:W:1:1-W:1:3")) g <- g3_game_apply(g, id)
  # Model budget fields are accepted for coaching (they were refused before).
  rev <- g3_coach_game(g, evaluator = m, budget = list(simulations = 8L), max_exercises = 0L, threshold = 0.05)
  expect_identical(rev$budget$simulations, 8L)
  expect_identical(rev$evaluator$model_revision, "rev-x")
  lim <- rev$limitations
  expect_true(any(grepl("8 MCTS simulations", lim, fixed = TRUE)))
  expect_true(any(grepl("p(win) - p(loss)", lim, fixed = TRUE)))
  expect_true(any(grepl("model estimates", lim, fixed = TRUE)))
  expect_false(any(grepl("heuristic units", lim, fixed = TRUE)))
  expect_false(any(grepl("Search limit", lim, fixed = TRUE)))
  # Decision sentences name the model budget and value kind, never heuristic units or nodes.
  txt <- rev$claims$text[rev$claims$type == "searched"]
  expect_true(length(txt) > 0L)
  expect_false(any(grepl("heuristic units|nodes", txt)))
  expect_true(all(c("review.summary_model") %in% rev$claims$template))
  expect_true(any(rev$claims$template %in% c("review.compare_model", "review.not_critical_model")))
  expect_true(any(rev$decisions$critical_reason %in% "value_gap"))
  expect_true("review.gap_model" %in% rev$claims$template)
  expect_true(any(grepl("MCTS Q value", txt, fixed = TRUE)))
  # Gaps in model values are not rounded to whole heuristic units.
  gap_txt <- txt[rev$claims$template[rev$claims$type == "searched"] == "review.gap_model"]
  expect_false(any(grepl(" 0 lower", gap_txt, fixed = TRUE)))
  # German templates exist for the same sentences.
  de <- g3_coach_game(g, evaluator = m, budget = list(simulations = 8L), max_exercises = 0L, threshold = 0.05, lang = "de")
  expect_true(any(grepl("8 MCTS-Simulationen", de$limitations, fixed = TRUE)))
  expect_false(any(grepl("heuristische Einheiten", de$limitations, fixed = TRUE)))
  expect_identical(.g3_coach_catalogue_problems("de"), character())
  # The policy and ordering modes get their own limitation lines.
  pol <- g3_coach_game(g, evaluator = m, budget = list(mode = "policy"), max_exercises = 0L)
  expect_true(any(grepl("one network evaluation", pol$limitations, fixed = TRUE)))
  ord <- g3_coach_game(g, evaluator = m, budget = list(mode = "ordering", nodes = 300L), max_exercises = 0L)
  expect_true(any(grepl("ordered by the network", ord$limitations, fixed = TRUE)))
  # Search coaching still refuses model fields, and unknown fields stay refused.
  expect_error(g3_coach_game(g, budget = list(simulations = 8L), max_exercises = 0L), class = "g3_budget_exceeded")
  expect_error(g3_coach_game(g, evaluator = m, budget = list(rollouts = 8L), max_exercises = 0L),
               class = "g3_budget_exceeded")
})
