# Agents: the public choose/analyse API over search, baselines and models.
#
# Built-in agents:
#   "search"  alpha-beta search with the transparent evaluation
#   "greedy"  one ply: best static evaluation after each legal action
#   "random"  uniform over legal actions (an explicitly named baseline)
# Model agents ("model:<id>" or a g3_model) are provided by the model
# registry when a validated checkpoint and runtime are available; otherwise
# they fail with a g3_incompatible_model condition, never a silent
# substitute.

.G3_BUILTIN_AGENTS <- c("search", "greedy", "random")

#' Choose an action
#'
#' Asks an agent for one action in a position. The state is not modified
#' and the action is not applied.
#'
#' @param state A `g3_state`.
#' @param agent `"search"` (default), `"greedy"`, `"random"`, or a model
#'   agent (`"model:<id>"`, or a `g3_model` from [g3_register_model()]).
#' @param budget For search: a list with any of `nodes` (default 2000,
#'   positions visited), `time_ms` and `depth` (maximum full-width depth,
#'   default 8). Only completed iterations are published.
#' @param seed Integer seed for agents that sample (`"random"`, model
#'   sampling). The caller's random-number stream is restored afterwards.
#' @return A `g3_decision` list: `action_id` (`NA` in a finished
#'   position), `label`, `status` (`"completed"`, `"budget"`, `"fallback"`,
#'   `"terminal"`), `score`, `score_kind` (`"heuristic"` or `"mate"`),
#'   `perspective` (`"side_to_act"`), `depth`, `nodes`, `elapsed_ms`, `pv`,
#'   `analysis` (root alternatives) and `provenance`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' d <- gambitR3D::g3_choose_action(game$state, budget = base::list(nodes = 300L))
#' d$action_id
#' d$status
#' @export
g3_choose_action <- function(state, agent = "search", budget = list(nodes = 2000L), seed = NULL) {
  .g3_check_state(state)
  if (inherits(agent, "g3_model") || (is.character(agent) && grepl("^model", agent[1L]))) {
    return(.g3_model_decision(state, agent, budget, seed))
  }
  if (!is.character(agent) || length(agent) != 1L || !agent %in% .G3_BUILTIN_AGENTS) {
    .g3_abort("g3_incompatible_model", paste0("Unknown agent: ", format(agent)[1L],
                                              ". Use search, greedy, random or a registered model."))
  }
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  b <- .g3_budget(budget)
  prov <- .g3_decision_provenance(state, agent, b, seed)
  if (identical(state$outcome$status, "terminal")) {
    return(.g3_decision(NA_character_, NA_character_, "terminal", NA_real_, 0L, 0, 0, character(), NULL, prov))
  }
  t0 <- proc.time()[["elapsed"]]
  if (agent == "random") {
    ctx <- .g3_ctx(core, tabs)
    legal <- .g3_legal(core, tabs, ctx)
    ids <- .g3_action_ids(core, legal, tabs)
    i <- .g3_with_seed(seed, sample.int(length(ids), 1L))
    lab <- .g3_action_labels(core, legal[i, , drop = FALSE], tabs, ctx)
    return(.g3_decision(ids[i], lab, "completed", NA_real_, 0L, 0, (proc.time()[["elapsed"]] - t0) * 1000,
                        ids[i], NULL, prov))
  }
  if (agent == "greedy") {
    r <- .g3_greedy(core, tabs)
    ctx <- .g3_ctx(core, tabs)
    lab <- .g3_action_labels(core, r$legal[r$best, , drop = FALSE], tabs, ctx)
    alt <- .g3_alternatives(core, r$legal, r$ids, r$scores, rep("exact", length(r$ids)), tabs, ctx)
    return(.g3_decision(r$ids[r$best], lab, "completed", r$scores[r$best], 1L, length(r$ids),
                        (proc.time()[["elapsed"]] - t0) * 1000, r$ids[r$best], alt, prov))
  }
  r <- .g3_root_search(core, tabs, b)
  ctx <- .g3_ctx(core, tabs)
  lab <- .g3_action_labels(core, r$legal[r$best, , drop = FALSE], tabs, ctx)
  alt <- .g3_alternatives(core, r$legal, r$ids, r$scores, r$bounds, tabs, ctx)
  .g3_decision(r$ids[r$best], lab, r$status, r$scores[r$best], r$depth, r$nodes, r$elapsed, r$pv, alt, prov)
}

#' Analyse a position
#'
#' Searches every root action (or a chosen subset) with a full window at
#' each completed depth, so the alternatives' scores are exact for that
#' depth and directly comparable. Scores are heuristic units from the side
#' to act's perspective; forced mates within the horizon are reported as
#' mate distances in plies.
#'
#' @param state A `g3_state`.
#' @param evaluator `"search"` (default) or a model agent; see
#'   [g3_choose_action()].
#' @param budget Search budget, as for [g3_choose_action()]. Nodes are
#'   counted for the whole root search.
#' @param actions Optional character vector of action ids to analyse; they
#'   must all be legal.
#' @return A `g3_position_analysis` list: `alternatives` (data frame with
#'   `action_id`, `label`, `type`, `score`, `score_kind`, `mate_in`,
#'   `bound`, `rank`), `root_score`, `score_kind`, `perspective`, `side`,
#'   `pv`, `pv_labels`, `depth`, `completeness` (`"complete"` when the
#'   requested depth was reached, `"partial"` when the budget stopped
#'   deepening, `"none"` when no iteration completed), `cost` and
#'   `provenance`.
#' @examples
#' st <- gambitR3D::g3_new_game()$state
#' an <- gambitR3D::g3_analyse_position(st, budget = base::list(nodes = 400L, depth = 1L))
#' utils::head(an$alternatives[, c("label", "score", "bound")])
#' @export
g3_analyse_position <- function(state, evaluator = "search", budget = list(nodes = 2000L), actions = NULL) {
  .g3_check_state(state)
  if (inherits(evaluator, "g3_model") || (is.character(evaluator) && grepl("^model", evaluator[1L]))) {
    return(.g3_model_position_analysis(state, evaluator, budget, actions))
  }
  if (!identical(evaluator, "search")) {
    .g3_abort("g3_incompatible_model", "Position analysis supports evaluator = 'search' or a model.")
  }
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  b <- .g3_budget(budget)
  prov <- .g3_decision_provenance(state, evaluator, b, NULL)
  if (!is.null(actions)) {
    legal_ids <- .g3_action_ids(core, .g3_legal(core, tabs, ctx), tabs)
    bad <- setdiff(actions, legal_ids)
    if (length(bad)) .g3_abort("g3_illegal_action", paste0("Not legal here: ", bad[1L]), action_id = bad[1L])
  }
  r <- .g3_root_search(core, tabs, b, exact_alternatives = TRUE, restrict = actions)
  alt <- .g3_alternatives(core, r$legal, r$ids, r$scores, r$bounds, tabs, ctx)
  pv_labels <- .g3_pv_labels(core, r$pv, tabs)
  root <- if (is.na(r$best)) NA_real_ else r$scores[r$best]
  structure(
    list(
      alternatives = alt,
      root_score = root,
      score_kind = .g3_score_kind(root),
      perspective = "side_to_act",
      side = state$side,
      pv = r$pv,
      pv_labels = pv_labels,
      depth = r$depth,
      completeness = if (r$status == "terminal") "terminal" else if (r$depth == 0L) "none" else
        if (r$depth >= b$depth || r$status == "completed") "complete" else "partial",
      cost = list(nodes = r$nodes, elapsed_ms = r$elapsed),
      provenance = prov
    ),
    class = "g3_position_analysis"
  )
}

#' @export
print.g3_position_analysis <- function(x, ...) {
  cat("<g3_position_analysis> ", x$side, " to act, depth ", x$depth, " (", x$completeness, "), ",
      x$cost$nodes, " nodes\n", sep = "")
  if (nrow(x$alternatives)) {
    top <- utils::head(x$alternatives[order(x$alternatives$rank), c("rank", "label", "score", "score_kind", "bound")], 8L)
    print(top, row.names = FALSE)
  }
  if (length(x$pv_labels)) cat("PV: ", paste(x$pv_labels, collapse = "  "), "\n", sep = "")
  invisible(x)
}

#' @export
print.g3_decision <- function(x, ...) {
  cat("<g3_decision> ", x$provenance$agent, ": ", x$label %||% NA, " [", x$status, "]",
      if (!is.na(x$score)) paste0(", score ", format(x$score), " (", x$score_kind, ")") else "",
      "\n", sep = "")
  invisible(x)
}

# One-ply greedy baseline.
#' @noRd
.g3_greedy <- function(core, tabs) {
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  ids <- .g3_action_ids(core, legal, tabs)
  scores <- vapply(seq_len(nrow(legal)), function(i) {
    child <- .g3_make(core, legal[i, ], tabs)
    cctx <- .g3_ctx(child, tabs)
    cl <- .g3_legal(child, tabs, cctx)
    if (!nrow(cl)) {
      return(if (.g3_in_check(child, child$side, cctx, tabs)) .G3_MATE - 1 else 0)
    }
    if (.g3_rule_draw(child)) return(0)
    -.g3_eval(child, cctx, tabs)
  }, 0)
  list(legal = legal, ids = ids, scores = scores, best = which.max(scores))
}

#' @noRd
.g3_score_kind <- function(s) {
  ifelse(is.na(s), NA_character_, ifelse(abs(s) > .G3_MATE_BOUND, "mate", "heuristic"))
}

#' @noRd
.g3_alternatives <- function(core, legal, ids, scores, bounds, tabs, ctx) {
  kind <- .g3_score_kind(scores)
  mate_in <- ifelse(!is.na(kind) & kind == "mate", sign(scores) * (.G3_MATE - abs(scores)), NA_real_)
  rk <- rank(-ifelse(is.na(scores), -Inf, scores), ties.method = "first")
  data.frame(
    action_id = ids,
    label = .g3_action_labels(core, legal, tabs, ctx),
    type = c("move", "platform", "castle")[legal[, "type"]],
    score = scores,
    score_kind = kind,
    mate_in = as.integer(mate_in),
    bound = bounds,
    rank = as.integer(rk),
    stringsAsFactors = FALSE
  )
}

#' @noRd
.g3_pv_labels <- function(core, pv, tabs) {
  out <- character()
  cur <- core
  for (id in pv) {
    ctx <- .g3_ctx(cur, tabs)
    legal <- .g3_legal(cur, tabs, ctx)
    i <- match(id, .g3_action_ids(cur, legal, tabs))
    if (is.na(i)) break
    out <- c(out, .g3_action_labels(cur, legal[i, , drop = FALSE], tabs, ctx))
    cur <- .g3_make(cur, legal[i, ], tabs)
  }
  out
}

#' @noRd
.g3_decision <- function(id, label, status, score, depth, nodes, elapsed, pv, analysis, prov) {
  structure(
    list(
      action_id = id, label = label, status = status, score = score,
      score_kind = .g3_score_kind(score), perspective = "side_to_act",
      depth = as.integer(depth), nodes = nodes, elapsed_ms = elapsed, pv = pv,
      analysis = analysis, provenance = prov
    ),
    class = "g3_decision"
  )
}

#' @noRd
.g3_decision_provenance <- function(state, agent, budget, seed) {
  list(
    agent = if (is.character(agent)) agent else "model",
    engine = .G3_SEARCH_VERSION,
    evaluator = .G3_EVAL_VERSION,
    rules_version = state$rules_version,
    rules_hash = state$rules_hash,
    budget = budget,
    seed = seed,
    state_hash = .g3_state_hash(state),
    package_version = as.character(utils::packageVersion("gambitR3D"))
  )
}

# Evaluate `expr` with a temporary seed, restoring the caller's RNG state.
#' @noRd
.g3_with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old, envir = globalenv())
    }
  })
  set.seed(seed)
  expr
}
