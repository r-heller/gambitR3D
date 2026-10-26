# Game analysis.
#
# `g3_analyse_game()` replays a game's active line exactly once through the
# engine's internal transition (`.g3_make()` on the legal action matrix, the
# same code `g3_apply_action()` uses), verifying every recorded repetition
# key on the way. For each position it records engine-derived features
# (R/features.R) and, optionally, an evaluation:
#
#   evaluator = "search"  full-window root search of every legal action at
#                         each completed depth (`.g3_root_search()` with exact
#                         alternatives), so the played action and the best
#                         alternative are scored by one search under identical
#                         settings;
#   evaluator = "none"    features only (no score; missing_reason
#                         "not_requested");
#   a model evaluator     dispatched to `g3_analyse_position()`; when no model
#                         runtime exists the score is NA with the reason.
#
# Score semantics. Raw scores are the evaluator's output from the side to
# act's perspective (heuristic units for search, not centipawns; mates are
# +/-(1e6 - plies)). White and mover views are stored separately. The
# expected-score columns map heuristic scores through a documented logistic
# display transform, E = 1 / (1 + exp(-s / 400)), labelled uncalibrated; it
# is not a probability. Terminal positions use their exact rule result.
#
# Evaluation cache. Entries are keyed by an analysis-context hash over the
# position key, the full repetition record and halfmove counter (the draw
# context), the rules hash, the evaluator identity/version/model revision,
# the feature and action schemas, the budget and numeric precision. Two
# positions with the same board but different draw history, or the same
# position under another model or budget, can never share an entry.

.G3_ANALYSIS_SCHEMA <- "g3-analysis/1"
.G3_CONTEXT_SCHEMA <- "g3-analysis-context/1"
.G3_LOGISTIC_SCALE <- 400

#' Analyse a game
#'
#' Replays a game once through the rules engine and returns per-action,
#' per-position and per-evaluation tables, tactical labels, session events,
#' a one-row summary and provenance. Works without Python or model weights:
#' with `evaluator = "search"` the built-in search scores every position;
#' with `evaluator = "none"` only engine-derived features are computed.
#'
#' Every feature is defined in [g3_feature_dictionary()]. Display settings
#' (2D/3D, Neutral height, camera) are not inputs and cannot change any
#' result.
#'
#' @param game A `g3_game`, or a path to a native `.g3.json` game file (read
#'   with [g3_read_game()]).
#' @param evaluator `"search"` (default), `"none"`, or a model agent
#'   (`"model:<id>"` or a `g3_model`). A model without a runtime yields
#'   `NA` scores with the reason, never a substitute.
#' @param budget Search budget per position: a list with any of `nodes`
#'   (default 2000), `time_ms` and `depth` (see [g3_choose_action()]). Only
#'   node and depth budgets give run-to-run identical results.
#' @param cache Optional environment used as an evaluation cache, shared
#'   across calls (for example `new.env()`). Entries are keyed by the full
#'   analysis context (see Details in the source header and
#'   `positions$analysis_context_hash`). `NULL` uses a fresh cache.
#' @param stop_check Optional function returning `TRUE` to cancel. It is
#'   polled by the search and between positions; a cancelled analysis
#'   raises a `g3_budget_exceeded` condition with `context$reason ==
#'   "cancelled"` and returns nothing partial.
#' @return A `g3_analysis` list:
#'   * `game`: list with `game_id`, `game_content_hash`, `players`, `result`,
#'     `termination`, `n_plies`, `n_events`, `initial_ply`, `rules_version`,
#'     `rules_hash`, `start_position_key`, `annotations`.
#'   * `moves`: one row per action (see [g3_feature_dictionary()], table
#'     `"moves"`), with `action_type` in `piece_move`, `promotion`,
#'     `en_passant`, `castling`, `platform_relocation`,
#'     `platform_rotation`.
#'   * `positions`: one row per position (plies 0..n) of features.
#'   * `evaluations`: one row per position with raw, White and mover scores,
#'     expected scores, best and played alternatives, move losses and
#'     `missing_reason`.
#'   * `tactics`: one row per tactical label with `claim_strength`.
#'   * `events`: session events (resign, draw_agreed, truncated, takeback).
#'   * `mobility`, `platforms`, `pieces`, `squares`: long per-position
#'     tables (mobility by piece type and board, platform state, piece
#'     threats, the 64 physical squares).
#'   * `summary`: one-row data frame.
#'   * `provenance`: list with `analysis_schema`, `feature_schema`,
#'     `rules_version`, `rules_hash`, `action_schema`, `evaluator`,
#'     `evaluator_version`, `model_revision`, `budget`, `budget_semantics`,
#'     `seed`, `score_scale`, `cache`, `timings_ms`, `package_version`,
#'     `r_version`, `created`, `analysis_id`.
#'   All tables are typed and keep their columns when empty.
#' @examples
#' # Engine-derived features without Python or model weights.
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' game <- gambitR3D::g3_game_apply(game, "m:B:2:2-B:2:1")
#' result <- gambitR3D::g3_analyse_game(game, evaluator = "search",
#'                                      budget = base::list(nodes = 1000L))
#' result$moves[, c("ply", "actor", "action_type", "label")]
#' result$evaluations[, c("ply", "score_white", "expected_score_white", "move_loss_raw")]
#' # Inspect provenance rather than interpreting an unexplained number.
#' result$provenance$score_scale$definition
#' @export
g3_analyse_game <- function(game, evaluator = "search", budget = list(nodes = 2000L), cache = NULL,
                            stop_check = NULL) {
  t_start <- proc.time()[["elapsed"]]
  game <- .g3_an_as_game(game)
  ev <- .g3_an_evaluator(evaluator)
  b <- .g3_budget(budget)
  if (is.null(cache)) cache <- new.env(hash = TRUE, parent = emptyenv())
  if (!is.environment(cache)) .g3_abort("g3_invalid_state", "`cache` must be an environment or NULL.")
  if (!is.null(stop_check) && !is.function(stop_check)) {
    .g3_abort("g3_invalid_state", "`stop_check` must be a function or NULL.")
  }
  if (!is.null(stop_check)) {
    op <- options(gambitR3D.stop_check = stop_check)
    on.exit(options(op), add = TRUE)
  }
  cancelled <- function() {
    if (!is.null(stop_check) && isTRUE(stop_check())) {
      .g3_abort("g3_budget_exceeded", "Analysis cancelled.", reason = "cancelled")
    }
  }
  tabs <- .g3_tables()
  .g3_check_state(game$initial_state)
  g3_validate_state(game$initial_state)
  game_id <- as.character(game$metadata$game_id %||% NA_character_)
  ev_tab <- game$events
  line <- .g3_active_line(ev_tab)
  n <- length(line)
  budget_text <- .g3_an_budget_text(b)

  core <- .g3_core(game$initial_state, tabs)
  pos_rows <- vector("list", n + 1L)
  eval_rows <- vector("list", n + 1L)
  move_rows <- vector("list", n)
  feats <- vector("list", n + 1L)
  long <- list(mobility = vector("list", n + 1L), platforms = vector("list", n + 1L),
               pieces = vector("list", n + 1L), squares = vector("list", n + 1L))
  stats <- new.env(parent = emptyenv())
  stats$hits <- 0L
  stats$misses <- 0L
  t_eval <- 0
  rec_hash <- c(.g3_state_hash(game$initial_state), ev_tab$post_state_hash[line])

  for (k in 0:n) {
    cancelled()
    ctx <- .g3_ctx(core, tabs)
    legal_all <- .g3_legal(core, tabs, ctx)
    outcome <- .g3_outcome_core(core, tabs, ctx, legal_all)
    terminal <- identical(outcome$status, "terminal")
    if (terminal && k < n) {
      .g3_abort("g3_invalid_state",
                paste0("Invalid game log: an action follows the finished position at ply ", core$ply, "."),
                game_id = game_id, first_invalid_ply = core$ply + 1L)
    }
    legal <- if (terminal) legal_all[0L, , drop = FALSE] else legal_all
    ids <- .g3_action_ids(core, legal, tabs)

    # Features.
    f <- .g3_feat_position(core, ctx, legal, terminal, tabs)
    feats[[k + 1L]] <- f
    ctx_hash <- .g3_an_context_hash(core, ev, b, tabs)
    row <- c(
      list(game_id = game_id, ply = as.integer(core$ply), status = outcome$status,
           result = outcome$result, reason = outcome$reason,
           analysis_context_hash = ctx_hash, recorded_state_hash = rec_hash[k + 1L]),
      f$row
    )
    pos_rows[[k + 1L]] <- row
    gp <- list(game_id = game_id, ply = as.integer(core$ply))
    long$mobility[[k + 1L]] <- lapply(Filter(Negate(is.null), f$mobility), function(x) c(gp, x))
    long$platforms[[k + 1L]] <- c(gp, f$platforms)
    long$pieces[[k + 1L]] <- c(gp, f$pieces)
    long$squares[[k + 1L]] <- c(gp, f$squares)

    # Evaluation.
    played <- if (k < n) ev_tab$action_id[line[k + 1L]] else NA_character_
    t0 <- proc.time()[["elapsed"]]
    er <- .g3_an_evaluate(core, outcome, ids, played, ev, b, cache, ctx_hash, stats, tabs, cancelled)
    t_eval <- t_eval + (proc.time()[["elapsed"]] - t0)
    eval_rows[[k + 1L]] <- c(list(game_id = game_id, ply = as.integer(core$ply)), er)

    if (k == n) break

    # Transition through the engine, then verify the recorded key.
    i <- match(played, ids)
    if (is.na(i)) {
      .g3_abort("g3_illegal_action",
                paste0("Invalid game log: action '", played, "' is not legal at ply ", core$ply + 1L, "."),
                game_id = game_id, first_invalid_ply = core$ply + 1L, action_id = played)
    }
    a <- legal[i, ]
    new <- .g3_make(core, a, tabs)
    rec_key <- ev_tab$position_key[line[k + 1L]]
    if (!identical(rec_key, new$rep[length(new$rep)])) {
      .g3_abort("g3_invalid_state",
                paste0("Invalid game log: the recorded position key at ply ", new$ply, " does not match the replay."),
                game_id = game_id, first_invalid_ply = new$ply)
    }
    ctx2 <- .g3_ctx(new, tabs)
    legal2 <- .g3_legal(new, tabs, ctx2)
    out2 <- .g3_outcome_core(new, tabs, ctx2, legal2)
    check2 <- .g3_in_check(new, new$side, ctx2, tabs)
    move_rows[[k + 1L]] <- c(
      list(game_id = game_id),
      .g3_an_move_row(core, ctx, a, new, ctx2, out2, check2, isTRUE(f$row$in_check), tabs)
    )
    core <- new
  }
  final_key <- core$rep[length(core$rep)]
  st_keys <- game$state$repetition$keys
  if (!identical(final_key, st_keys[length(st_keys)])) {
    .g3_abort("g3_invalid_state", "Invalid game: the stored current state does not match the replayed log.",
              game_id = game_id)
  }

  moves <- .g3_an_rows_df(move_rows, .g3_an_schema("moves"))
  positions <- .g3_an_rows_df(pos_rows, .g3_an_schema("positions"))
  evaluations <- .g3_an_finish_evals(.g3_an_rows_df(eval_rows, .g3_an_schema("evaluations")))

  # Tactical labels need the features and evaluations of both positions.
  tac_rows <- list()
  for (k in seq_len(n)) {
    mv <- move_rows[[k]]
    evk <- eval_rows[[k]]
    ev1 <- eval_rows[[k + 1L]]
    labs <- .g3_feat_tactics(mv, feats[[k]], feats[[k + 1L]], evk, ev1, budget_text)
    for (x in labs) {
      tac_rows[[length(tac_rows) + 1L]] <- c(
        list(game_id = game_id, ply = mv$ply, actor = mv$actor, action_id = mv$action_id), x
      )
    }
  }
  tactics <- .g3_an_rows_df(tac_rows, .g3_an_schema("tactics"))
  events <- .g3_an_session_events(game, game_id)

  mobility <- .g3_an_cols_df(unlist(long$mobility, recursive = FALSE), .g3_an_schema("mobility"))
  platforms <- .g3_an_cols_df(long$platforms, .g3_an_schema("platforms"))
  pieces <- .g3_an_cols_df(long$pieces, .g3_an_schema("pieces"))
  squares <- .g3_an_cols_df(long$squares, .g3_an_schema("squares"))

  info <- .g3_an_game_info(game, game_id, n, positions)
  summary <- .g3_an_game_summary(info, moves, positions, evaluations, tactics, events)
  t_total <- (proc.time()[["elapsed"]] - t_start) * 1000
  prov <- .g3_an_provenance(info, ev, b, stats, list(
    total = t_total, evaluation = t_eval * 1000, replay_and_features = t_total - t_eval * 1000
  ))
  structure(
    list(
      game = info, moves = moves, positions = positions, evaluations = evaluations,
      tactics = tactics, events = events, mobility = mobility, platforms = platforms,
      pieces = pieces, squares = squares, summary = summary, provenance = prov
    ),
    class = "g3_analysis"
  )
}

#' @export
print.g3_analysis <- function(x, ...) {
  s <- x$summary
  cat("<g3_analysis> ", x$game$game_id, ": ", s$n_plies, " plies, result ", s$result,
      " (", s$termination, ")\n", sep = "")
  cat("Evaluator: ", x$provenance$evaluator, " [", x$provenance$evaluator_version %||% "-", "], budget ",
      .g3_an_budget_text(x$provenance$budget), " per position\n", sep = "")
  cat("Positions evaluated: ", s$n_evaluated, " of ", s$n_positions, " (missing ", s$n_eval_missing, ")\n", sep = "")
  cat("Scores: raw ", x$provenance$score_scale$raw_unit, " from the side to act; expected score is ",
      x$provenance$score_scale$expected_score_map, "\n", sep = "")
  if (nrow(x$tactics)) {
    tb <- table(x$tactics$label)
    cat("Tactical labels: ", paste(names(tb), tb, sep = " ", collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

# Accept a game or a game file path.
#' @noRd
.g3_an_as_game <- function(game) {
  if (is.character(game) && length(game) == 1L) {
    return(g3_read_game(game))
  }
  .g3_check_game(game)
  game
}

# Evaluator identity: id, kind, version, model revision and score unit.
#' @noRd
.g3_an_evaluator <- function(evaluator) {
  if (is.null(evaluator) || identical(evaluator, "none")) {
    return(list(id = "none", kind = "none", version = NA_character_, model_revision = NA_character_,
                score_unit = NA_character_, scale_id = NA_character_, object = NULL))
  }
  if (identical(evaluator, "search")) {
    v <- paste(.G3_SEARCH_VERSION, .G3_EVAL_VERSION, sep = "+")
    return(list(id = "search", kind = "search", version = v, model_revision = NA_character_,
                score_unit = "heuristic_units", scale_id = paste("search", v, "heuristic_units", sep = "|"),
                object = "search"))
  }
  if (inherits(evaluator, "g3_model") || (is.character(evaluator) && length(evaluator) == 1L && grepl("^model", evaluator))) {
    id <- if (is.character(evaluator)) evaluator else paste0("model:", evaluator$model_id %||% evaluator$id %||% "unnamed")
    rev <- if (is.character(evaluator)) NA_character_ else
      as.character(evaluator$revision %||% evaluator$sha256 %||% evaluator$hash %||% .g3_hash_object(list(id = id)))
    return(list(id = id, kind = "model", version = rev, model_revision = rev, score_unit = NA_character_,
                scale_id = paste(id, rev, sep = "|"), object = evaluator))
  }
  .g3_abort("g3_incompatible_model", paste0("Unknown evaluator: ", format(evaluator)[1L],
                                            ". Use 'search', 'none' or a registered model."))
}

#' @noRd
.g3_an_budget_text <- function(b) {
  parts <- c(
    if (is.finite(b$nodes)) paste0(format(b$nodes, scientific = FALSE), " nodes"),
    if (is.finite(b$time_ms)) paste0(format(b$time_ms), " ms"),
    paste0("max depth ", b$depth)
  )
  paste(parts, collapse = ", ")
}

# Analysis-context hash (the evaluation cache key).
#' @noRd
.g3_an_context_hash <- function(core, ev, b, tabs) {
  .g3_hash_object(list(
    schema = .G3_CONTEXT_SCHEMA,
    rules_hash = tabs$rules_hash,
    action_schema = .G3_ACTION_SCHEMA,
    feature_schema = .G3_FEATURE_SCHEMA,
    position_key = core$rep[length(core$rep)],
    repetition_record = as.list(core$rep),
    halfmove = as.integer(core$halfmove),
    evaluator = ev$id,
    evaluator_version = ev$version,
    model_revision = ev$model_revision,
    budget = list(nodes = format(b$nodes, scientific = FALSE), time_ms = format(b$time_ms), depth = b$depth),
    precision = "double"
  ))
}

#' @noRd
.g3_an_mate_in <- function(s) {
  as.integer(ifelse(!is.na(s) & abs(s) > .G3_MATE_BOUND, sign(s) * (.G3_MATE - abs(s)), NA_real_))
}

# Expected score from the side to act's perspective.
#' @noRd
.g3_an_expected_score <- function(raw, kind) {
  out <- rep(NA_real_, length(raw))
  h <- !is.na(raw) & !is.na(kind) & kind == "heuristic"
  out[h] <- 1 / (1 + exp(-raw[h] / .G3_LOGISTIC_SCALE))
  o <- !is.na(raw) & !is.na(kind) & kind %in% c("mate", "terminal")
  out[o] <- ifelse(raw[o] > 0, 1, ifelse(raw[o] < 0, 0, 0.5))
  out
}

# Evaluate one position (or reuse a cached evaluation) and describe the
# played action against the best alternative.
#' @noRd
.g3_an_evaluate <- function(core, outcome, ids, played, ev, b, cache, ctx_hash, stats, tabs, cancelled) {
  out <- list(
    side = .G3_COLORS[core$side], evaluator = ev$id, evaluator_version = ev$version,
    model_revision = ev$model_revision, scale_id = NA_character_, score_unit = NA_character_,
    raw_score = NA_real_, score_kind = NA_character_, raw_perspective = "side_to_act",
    depth = 0L, completeness = "not_evaluated", nodes = 0, elapsed_ms = 0, cache_hit = FALSE,
    analysis_context_hash = ctx_hash, played_action_id = played, n_alternatives = length(ids),
    missing_reason = NA_character_
  )
  if (identical(outcome$status, "terminal")) {
    # Exact rule outcome: the side to act has lost (checkmate) or it is a draw.
    out$raw_score <- if (identical(outcome$reason, "checkmate")) -.G3_MATE else 0
    out$score_kind <- "terminal"
    out$completeness <- "terminal"
    out$scale_id <- "terminal|exact"
    out$score_unit <- "rule_outcome"
    out$n_alternatives <- 0L
    return(out)
  }
  if (ev$kind == "none") {
    out$missing_reason <- "not_requested"
    return(out)
  }
  ent <- cache[[ctx_hash]]
  if (!is.null(ent)) {
    stats$hits <- stats$hits + 1L
    out$cache_hit <- TRUE
  } else {
    stats$misses <- stats$misses + 1L
    ent <- .g3_an_compute_eval(core, outcome, ev, b, tabs)
    cancelled()
    if (isTRUE(ent$cacheable)) assign(ctx_hash, ent, envir = cache)
  }
  out$depth <- as.integer(ent$depth)
  out$completeness <- ent$completeness
  out$nodes <- as.numeric(ent$nodes)
  out$elapsed_ms <- as.numeric(ent$elapsed_ms)
  out$score_unit <- ent$score_unit
  out$scale_id <- ent$scale_id
  if (!is.null(ent$missing_reason)) {
    out$missing_reason <- ent$missing_reason
    return(out)
  }
  sc <- ent$scores[match(ids, ent$ids)]
  kinds <- ent$kinds[match(ids, ent$ids)]
  exp_s <- if (is.null(ent$expected)) .g3_an_expected_score(sc, kinds) else ent$expected[match(ids, ent$ids)]
  if (all(is.na(sc))) {
    out$missing_reason <- "no_completed_iteration_within_budget"
    return(out)
  }
  bi <- match(ent$best_id, ids)
  out$raw_score <- sc[bi]
  out$score_kind <- kinds[bi]
  out$best_action_id <- ent$best_id
  out$best_score <- sc[bi]
  out$best_kind <- kinds[bi]
  out$best_mate_in <- .g3_an_mate_in(sc[bi])
  out$expected_score_mover <- exp_s[bi]
  if (!is.na(played)) {
    pi <- match(played, ids)
    out$played_score <- sc[pi]
    out$played_kind <- kinds[pi]
    out$played_mate_in <- .g3_an_mate_in(sc[pi])
    rk <- rank(-ifelse(is.na(sc), -Inf, sc), ties.method = "min")
    out$played_rank <- as.integer(rk[pi])
    if (!is.na(sc[pi]) && kinds[bi] == "heuristic" && kinds[pi] == "heuristic") {
      out$move_loss_raw <- sc[bi] - sc[pi]
    }
    out$expected_loss_mover <- exp_s[bi] - exp_s[pi]
  }
  out
}

# Run the evaluator on one position. Returns a cache entry.
#' @noRd
.g3_an_compute_eval <- function(core, outcome, ev, b, tabs) {
  if (ev$kind == "search") {
    r <- .g3_root_search(core, tabs, b, exact_alternatives = TRUE)
    stopped_early <- r$depth == 0L
    return(list(
      ids = r$ids, scores = r$scores, kinds = .g3_score_kind(r$scores), bounds = r$bounds,
      best_id = if (stopped_early) NA_character_ else r$ids[r$best],
      depth = r$depth, nodes = r$nodes, elapsed_ms = r$elapsed,
      completeness = if (r$depth == 0L) "none" else if (r$depth >= b$depth || r$status == "completed") "complete" else "partial",
      score_unit = ev$score_unit, scale_id = ev$scale_id,
      missing_reason = if (stopped_early) "no_completed_iteration_within_budget" else NULL,
      cacheable = TRUE
    ))
  }
  # Model evaluators go through the public position-analysis API.
  st <- .g3_state_from_core(core, outcome, tabs)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(g3_analyse_position(st, evaluator = ev$object, budget = b), g3_error = function(e) e)
  el <- (proc.time()[["elapsed"]] - t0) * 1000
  if (inherits(res, "error")) {
    return(list(depth = 0L, nodes = 0, elapsed_ms = el, completeness = "none", score_unit = NA_character_,
                scale_id = NA_character_, missing_reason = paste0("evaluator_unavailable: ", conditionMessage(res)),
                cacheable = FALSE))
  }
  alt <- res$alternatives
  unit <- as.character(res$score_unit %||% res$score_kind %||% "model_units")
  expected <- NULL
  if (all(c("p_win", "p_draw", "p_loss") %in% names(alt))) {
    tot <- alt$p_win + alt$p_draw + alt$p_loss
    expected <- ifelse(abs(tot - 1) < 1e-6, alt$p_win + 0.5 * alt$p_draw, NA_real_)
  }
  kinds <- if (!is.null(alt$score_kind)) as.character(alt$score_kind) else rep("model", nrow(alt))
  best <- if (all(is.na(alt$score))) NA_integer_ else which.max(alt$score)
  list(
    ids = alt$action_id, scores = as.numeric(alt$score), kinds = kinds, expected = expected,
    best_id = if (is.na(best)) NA_character_ else alt$action_id[best],
    depth = as.integer(res$depth %||% 0L), nodes = as.numeric(res$cost$nodes %||% 0),
    elapsed_ms = as.numeric(res$cost$elapsed_ms %||% el), completeness = as.character(res$completeness %||% "complete"),
    score_unit = unit, scale_id = paste(ev$scale_id, unit, sep = "|"),
    missing_reason = if (is.na(best)) "no_score_from_model" else NULL,
    cacheable = TRUE
  )
}

# Derived perspective columns and the sequential and clipped losses.
#' @noRd
.g3_an_finish_evals <- function(e) {
  white <- e$side == "white"
  heur <- !is.na(e$score_kind) & e$score_kind == "heuristic"
  e$score_mover <- ifelse(heur, e$raw_score, NA_real_)
  e$score_white <- ifelse(heur, ifelse(white, e$raw_score, -e$raw_score), NA_real_)
  mate <- !is.na(e$score_kind) & e$score_kind == "mate"
  e$mate_in_mover <- ifelse(mate, .g3_an_mate_in(e$raw_score), NA_integer_)
  e$mate_in_white <- as.integer(ifelse(mate, ifelse(white, e$mate_in_mover, -e$mate_in_mover), NA_integer_))
  # Expected score: model rows carry their own; otherwise the documented map.
  mapped <- .g3_an_expected_score(e$raw_score, e$score_kind)
  em <- ifelse(is.na(e$expected_score_mover), mapped, e$expected_score_mover)
  e$expected_score_mover <- em
  e$expected_score_white <- ifelse(white, em, 1 - em)
  e$expected_score_kind <- ifelse(
    is.na(em), NA_character_,
    ifelse(e$score_kind == "heuristic", "heuristic_logistic_map",
           ifelse(e$score_kind == "mate", "mate_within_search",
                  ifelse(e$score_kind == "terminal", "terminal_exact", "model_wdl")))
  )
  e$calibration <- ifelse(is.na(em), NA_character_, ifelse(e$score_kind == "terminal", "exact", "uncalibrated"))
  # Sequential loss: the mover's score before minus the mover's score after
  # (the next position's raw score negated). May be negative.
  nr <- nrow(e)
  e$move_loss_sequential <- NA_real_
  if (nr > 1L) {
    nxt <- c(e$raw_score[-1L], NA_real_)
    nxt_heur <- c(heur[-1L], FALSE)
    ok <- heur & nxt_heur & !is.na(e$played_action_id)
    e$move_loss_sequential[ok] <- e$raw_score[ok] + nxt[ok]
  }
  e$move_loss_display_clipped <- ifelse(is.na(e$move_loss_raw), NA_real_, pmax(e$move_loss_raw, 0))
  e[, names(.g3_an_schema("evaluations")), drop = FALSE]
}

# One moves-table row for action `a` from `core` to `new`.
#' @noRd
.g3_an_move_row <- function(core, ctx, a, new, ctx2, out2, check2, in_check_before, tabs) {
  am <- matrix(a, nrow = 1L, dimnames = list(NULL, .G3_ACOLS))
  type <- a[["type"]]
  p <- a[["piece"]]
  from <- a[["from"]]
  after_sq <- new$psq[p]
  lev <- .g3_feat_levels(core, tabs)
  lev2 <- .g3_feat_levels(new, tabs)
  label <- .g3_action_labels(core, am, tabs, ctx)
  mate <- identical(out2$reason, "checkmate")
  from_board <- tabs$board_ids[tabs$sq_board[from]]
  to_board <- tabs$board_ids[tabs$sq_board[after_sq]]
  action_type <- if (type == 3L) "castling" else if (type == 2L) {
    if (a[["rot"]] == 180L) "platform_rotation" else "platform_relocation"
  } else if (a[["flag"]] == 2L) "en_passant" else if (a[["promo"]] > 0L) "promotion" else "piece_move"
  cap <- a[["cap"]]
  plat_sq <- tabs$sq_board[from] >= 4L || tabs$sq_board[after_sq] >= 4L
  level_transfer <- lev[from] != lev2[after_sq]
  list(
    ply = as.integer(new$ply),
    move_number = as.integer(ceiling(new$ply / 2)),
    actor = .G3_COLORS[core$side],
    action_id = .g3_action_ids(core, am, tabs),
    label = label,
    notation = paste0(label, if (mate) "#" else if (check2) "+" else ""),
    engine_type = c("move", "platform", "castle")[type],
    action_type = action_type,
    piece_id = core$pid[p],
    piece_type = .G3_TYPES[core$ptype[p]],
    from_square = tabs$sq_id[from],
    to_square = if (type == 2L) NA_character_ else tabs$sq_id[a[["to"]]],
    from_board = from_board, to_board = to_board,
    from_level = lev[from], to_level = lev2[after_sq],
    from_file = as.integer(ctx$file[from]), from_rank = as.integer(ctx$rank[from]),
    to_file = as.integer(ctx2$file[after_sq]), to_rank = as.integer(ctx2$rank[after_sq]),
    platform_id = if (type == 2L) .G3_PLATFORMS[a[["plat"]]] else NA_character_,
    from_dock = if (type == 2L) tabs$dock_ids[core$dock[a[["plat"]]]] else NA_character_,
    to_dock = if (type == 2L) tabs$dock_ids[a[["dock"]]] else NA_character_,
    rotation_delta = if (type == 2L) as.integer(a[["rot"]]) else NA_integer_,
    promotion = if (a[["promo"]] > 0L) .G3_TYPES[a[["promo"]]] else NA_character_,
    castle_side = if (type == 3L) c("kingside", "queenside")[a[["cside"]]] else NA_character_,
    en_passant = a[["flag"]] == 2L,
    capture = cap > 0L,
    captured_piece_id = if (cap > 0L) core$pid[cap] else NA_character_,
    captured_type = if (cap > 0L) .G3_TYPES[core$ptype[cap]] else NA_character_,
    check = check2,
    checkmate = mate,
    in_check_before = in_check_before,
    platform_action = type == 2L,
    platform_square = plat_sq,
    board_transfer = type == 1L && from_board != to_board,
    level_transfer = level_transfer,
    transfer_mode = if (type == 2L) "transport" else if (type == 1L && from_board != to_board) "piece" else NA_character_,
    pre_position_key = core$rep[length(core$rep)],
    position_key = new$rep[length(new$rep)],
    outcome_after = if (identical(out2$status, "terminal")) out2$reason else "ongoing"
  )
}

#' @noRd
.g3_an_session_events <- function(game, game_id) {
  ev <- game$events
  s <- ev[ev$type != "action", , drop = FALSE]
  rows <- lapply(seq_len(nrow(s)), function(i) {
    pl <- tryCatch(jsonlite::fromJSON(s$payload[i]), error = function(e) list())
    list(game_id = game_id, sequence = s$sequence[i], type = s$type[i], actor = s$actor[i],
         ply = s$ply[i], reason = if (is.null(pl$reason)) NA_character_ else as.character(pl$reason)[1L])
  })
  .g3_an_rows_df(rows, .g3_an_schema("events"))
}

#' @noRd
.g3_an_game_info <- function(game, game_id, n, positions) {
  meta <- game$metadata
  res <- meta$result %||% NA_character_
  term <- meta$termination %||% NA_character_
  std_key <- .g3_initial_core()$rep
  list(
    game_id = game_id,
    game_content_hash = g3_game_hash(game),
    players = meta$players %||% list(white = "White", black = "Black"),
    result = if (is.na(res)) "unfinished" else res,
    termination = if (is.na(term)) "unfinished" else term,
    n_plies = as.integer(n),
    n_events = nrow(game$events),
    initial_ply = as.integer(game$initial_state$ply),
    rules_version = game$initial_state$rules_version,
    rules_hash = game$initial_state$rules_hash,
    start_position_key = positions$position_key[1L],
    standard_start = identical(positions$position_key[1L], std_key),
    annotations = game$annotations %||% list()
  )
}

#' @noRd
.g3_an_outcome_class <- function(termination) {
  out <- rep("unfinished", length(termination))
  out[termination == "checkmate"] <- "rule_decisive"
  out[termination %in% c("stalemate", "repetition", "halfmove", "bare_kings")] <- "rule_draw"
  out[termination == "resign"] <- "resignation"
  out[termination == "draw_agreed"] <- "agreed_draw"
  out[termination == "truncated"] <- "truncated"
  out
}

#' @noRd
.g3_an_game_summary <- function(info, moves, positions, evaluations, tactics, events) {
  loss_mean <- function(col) {
    x <- evaluations$move_loss_raw[!is.na(evaluations$played_action_id) & evaluations$side == col]
    x <- x[!is.na(x)]
    list(mean = if (length(x)) mean(x) else NA_real_, n = length(x))
  }
  exp_mean <- function(col) {
    x <- evaluations$expected_loss_mover[!is.na(evaluations$played_action_id) & evaluations$side == col]
    x <- x[!is.na(x)]
    if (length(x)) mean(x) else NA_real_
  }
  lw <- loss_mean("white")
  lb <- loss_mean("black")
  has_score <- !is.na(evaluations$raw_score)
  row <- list(
    game_id = info$game_id, game_content_hash = info$game_content_hash,
    n_plies = info$n_plies, n_events = as.integer(info$n_events),
    result = info$result, termination = info$termination,
    outcome_class = .g3_an_outcome_class(info$termination),
    start_position_key = info$start_position_key, standard_start = info$standard_start,
    n_captures = sum(moves$capture), n_checks = sum(moves$check),
    n_platform_actions = sum(moves$platform_action),
    n_platform_rotations = sum(moves$action_type == "platform_rotation"),
    n_promotions = sum(!is.na(moves$promotion)), n_castles = sum(moves$action_type == "castling"),
    n_en_passant = sum(moves$en_passant), n_board_transfers = sum(moves$board_transfer),
    n_level_transfers = sum(moves$level_transfer),
    n_positions = nrow(positions), n_evaluated = sum(has_score), n_eval_missing = sum(!has_score),
    max_repetition_count = max(positions$repetition_count),
    white_mean_move_loss = lw$mean, black_mean_move_loss = lb$mean,
    white_n_move_loss = lw$n, black_n_move_loss = lb$n,
    white_mean_expected_loss = exp_mean("white"), black_mean_expected_loss = exp_mean("black"),
    n_tactic_labels = nrow(tactics)
  )
  .g3_an_rows_df(list(row), .g3_an_schema("summary"))
}

#' @noRd
.g3_an_provenance <- function(info, ev, b, stats, timings) {
  list(
    analysis_schema = .G3_ANALYSIS_SCHEMA,
    feature_schema = .G3_FEATURE_SCHEMA,
    context_schema = .G3_CONTEXT_SCHEMA,
    rules_version = info$rules_version,
    rules_hash = info$rules_hash,
    action_schema = .G3_ACTION_SCHEMA,
    game_content_hash = info$game_content_hash,
    evaluator = ev$id,
    evaluator_version = ev$version,
    model_revision = ev$model_revision,
    budget = b,
    budget_semantics = "per position: each position's root search has its own node/time/depth budget",
    seed = NA_integer_,
    determinism = if (is.finite(b$time_ms)) "time budgets can change results between runs" else
      "search with node and depth budgets is deterministic (no random choices)",
    score_scale = list(
      raw_unit = if (ev$kind == "search") "heuristic units (not centipawns)" else if (ev$kind == "none") "none" else "model units",
      raw_perspective = "side_to_act",
      views = "score_white (negated when Black acts) and score_mover (the side to act)",
      mate_encoding = "raw +/-(1e6 - plies); see mate_in_mover and mate_in_white",
      expected_score_map = paste0("1 / (1 + exp(-score / ", .G3_LOGISTIC_SCALE, ")) for heuristic scores (uncalibrated display map, not a probability); mates 1/0; terminal results exact; model WDL p_win + 0.5 p_draw"),
      logistic_scale = .G3_LOGISTIC_SCALE,
      definition = paste0(
        "Raw scores come from a full-window root search of every legal action in each completed iteration ",
        "(", .G3_SEARCH_VERSION, ", ", .G3_EVAL_VERSION, "). Heuristic units weigh provisional piece values, ",
        "mobility, king pressure, pawn progress and platform options; they are not centipawns. Expected scores ",
        "for heuristic values use a fixed logistic display map with scale ", .G3_LOGISTIC_SCALE, " and are uncalibrated."
      )
    ),
    cache = list(hits = stats$hits, misses = stats$misses,
                 key = "analysis_context_hash (position key, repetition record, halfmove counter, rules hash, evaluator identity/version/model revision, feature and action schema, budget, precision)"),
    timings_ms = timings,
    package_version = as.character(utils::packageVersion("gambitR3D")),
    r_version = R.version.string,
    created = .g3_now(),
    analysis_id = .g3_hash_object(list(
      game = info$game_content_hash, evaluator = ev$id, version = ev$version, model = ev$model_revision,
      budget = list(nodes = format(b$nodes, scientific = FALSE), time_ms = format(b$time_ms), depth = b$depth),
      features = .G3_FEATURE_SCHEMA
    ))
  )
}

# ---- typed tables -----------------------------------------------------------

# Column types of an output table, from the feature dictionary.
#' @noRd
.g3_an_schema <- function(table) {
  key <- paste0("schema:", table)
  if (!is.null(.g3_env[[key]])) {
    return(.g3_env[[key]])
  }
  d <- .g3_feat_dictionary()
  d <- d[d$table == table, , drop = FALSE]
  s <- stats::setNames(d$type, d$name)
  .g3_env[[key]] <- s
  s
}

#' @noRd
.g3_an_typed <- function(x, type) {
  switch(type,
         character = as.character(x),
         integer = as.integer(x),
         double = as.double(x),
         logical = as.logical(x),
         stop("unknown column type ", type))
}

#' @noRd
.g3_an_na <- function(type) {
  switch(type, character = NA_character_, integer = NA_integer_, double = NA_real_, logical = NA)
}

# Rows given as named lists (missing fields become typed NA).
#' @noRd
.g3_an_rows_df <- function(rows, schema) {
  cols <- lapply(names(schema), function(nm) {
    ty <- schema[[nm]]
    v <- lapply(rows, function(r) {
      x <- r[[nm]]
      if (is.null(x) || length(x) != 1L) .g3_an_na(ty) else x
    })
    .g3_an_typed(unlist(v, use.names = FALSE), ty)
  })
  .g3_an_new_df(cols, names(schema))
}

# Parts given as lists of column vectors; length-one fields (game id, ply)
# are recycled to the part's row count.
#' @noRd
.g3_an_cols_df <- function(parts, schema) {
  parts <- Filter(Negate(is.null), parts)
  parts <- lapply(parts, function(p) {
    len <- max(lengths(p))
    lapply(p, function(x) if (length(x) == 1L && len != 1L) rep(x, len) else x)
  })
  cols <- lapply(names(schema), function(nm) {
    .g3_an_typed(unlist(lapply(parts, `[[`, nm), use.names = FALSE), schema[[nm]])
  })
  .g3_an_new_df(cols, names(schema))
}

#' @noRd
.g3_an_new_df <- function(cols, nms) {
  n <- if (length(cols)) length(cols[[1L]]) else 0L
  structure(stats::setNames(cols, nms), class = "data.frame", row.names = if (n) c(NA_integer_, -n) else integer())
}
