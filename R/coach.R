# Engine-grounded coaching: action explanations and post-game review.
#
# The coach works offline with no language model, no Python and no
# network. Its sentences come from the template catalogues in inst/coach/
# and its content from the rules engine and the bounded search:
#
#   g3_explain_action()  legal facts of one action (and heuristic notes
#                        with their definitions)
#   g3_coach_game()      replays a game, compares each played action with
#                        the best analysed alternative under one evaluator
#                        and one budget, and returns typed claims, their
#                        evidence and candidate exercises
#
# Claim strength vocabulary (column `strength` of a claims table):
#
#   exact           rules fact, true in every case (legal_fact)
#   exact_at_depth  search value of a completed iteration; exact for that
#                   depth and evaluator only, not a game-theoretic value
#   proven          checkmate established by exhaustive enumeration of
#                   every reply (all replies resolved)
#   analysed_line   holds in the replayed line only; other replies were not
#                   all examined ("in the analysed line")
#   heuristic       an interpretation with its definition attached
#   suggestion      a learning suggestion
#
# Scores are heuristic units from the side to act's perspective (or a
# model's own value kind). They are never called centipawns, ratings or
# accuracy. "Forces mate" is said only with strength `proven`.

.G3_REVIEW_SCHEMA <- "g3-coach-review/1"

#' Explain an action with engine facts
#'
#' Describes one legal action and its consequences using only facts
#' returned by the rules engine: the moving piece, origin and destination
#' named with their board identity (for example `N:c3`, never just `c3`),
#' captures, promotion, platform transport (carried piece, docks, rotation
#' and the carried piece's new coordinate), castling, check given, check
#' escaped, checkmate or stalemate, newly attacked pieces and changes in
#' attacked squares. King-zone and undefended-piece notes are labelled as
#' heuristics with their definitions.
#'
#' @param state A `g3_state`.
#' @param action An action id, a one-row `g3_actions` table or a list with
#'   `action_id`; it must be legal in `state`.
#' @param lang Language of the sentences: `"en"` (default) or `"de"`.
#' @return A `g3_explanation` list: `action_id`, `label`, `state_hash`,
#'   `rules_hash`, `lang`, `facts` (named list of engine facts), `claims`
#'   (data frame with `claim_id`, `type` (`"legal_fact"` or
#'   `"heuristic"`), `strength`, `topic`, `template`, `text`,
#'   `definition`) and `text` (the sentences in order).
#' @examples
#' # Two knight moves reach coordinate c3, on different boards.
#' st <- gambitR3D::g3_new_game()$state
#' gambitR3D::g3_explain_action(st, "m:W:0:0-N:1:0")$text
#' gambitR3D::g3_explain_action(st, "m:W:0:0-W:1:2", lang = "de")$text
#' @export
g3_explain_action <- function(state, action, lang = "en") {
  .g3_check_state(state)
  lang <- .g3_coach_lang(lang)
  id <- .g3_action_id_arg(action)
  if (identical(state$outcome$status, "terminal")) {
    .g3_abort("g3_illegal_action", .g3_tr("illegal.game_over", lang))
  }
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  i <- .g3_find_action(core, legal, id, tabs)
  if (is.na(i)) {
    d <- .g3_diagnose_illegal(core, ctx, id, tabs)
    .g3_abort("g3_illegal_action", .g3_illegal_text(d, lang), action_id = id, reason = d$code)
  }
  facts <- .g3_action_facts(core, ctx, legal[i, ], tabs)
  claims <- .g3_fact_claims(facts, lang)
  structure(
    list(
      action_id = facts$action_id, label = facts$label,
      state_hash = .g3_state_hash(state), rules_hash = state$rules_hash, lang = lang,
      facts = facts, claims = claims, text = claims$text
    ),
    class = "g3_explanation"
  )
}

#' @export
print.g3_explanation <- function(x, ...) {
  cat("<g3_explanation> ", x$label, "\n", sep = "")
  for (i in seq_len(nrow(x$claims))) {
    cat("  [", x$claims$type[i], "] ", x$claims$text[i], "\n", sep = "")
  }
  invisible(x)
}

# Text of an illegal-input diagnosis.
#' @noRd
.g3_illegal_text <- function(d, lang) {
  p <- d$params
  if (!is.null(p$piece)) p$piece <- .g3_piece_word(p$piece, lang)
  for (nm in c("owner", "side")) if (!is.null(p[[nm]])) p[[nm]] <- .g3_color_word(p[[nm]], lang)
  do.call(.g3_tr, c(list(paste0("illegal.", d$code), lang), p))
}

# Empty typed claims table.
#' @noRd
.g3_empty_claims <- function() {
  data.frame(
    claim_id = character(), ply = integer(), side = character(), type = character(),
    strength = character(), topic = character(), template = character(), text = character(),
    definition = character(), evidence_id = character(), stringsAsFactors = FALSE
  )
}

# Build a claims table from a list of claim lists.
#' @noRd
.g3_claims_df <- function(items, ply = NA_integer_, side = NA_character_, evidence_id = NA_character_,
                          prefix = "c") {
  if (!length(items)) return(.g3_empty_claims())
  get <- function(x, nm, default) if (is.null(x[[nm]])) default else x[[nm]]
  out <- data.frame(
    claim_id = paste0(prefix, seq_along(items)),
    ply = as.integer(ply),
    side = as.character(side),
    type = vapply(items, function(x) x$type, ""),
    strength = vapply(items, function(x) x$strength, ""),
    topic = vapply(items, function(x) x$topic, ""),
    template = vapply(items, function(x) x$template, ""),
    text = vapply(items, function(x) x$text, ""),
    definition = vapply(items, function(x) get(x, "definition", NA_character_), ""),
    evidence_id = vapply(items, function(x) get(x, "evidence_id", evidence_id), ""),
    stringsAsFactors = FALSE
  )
  stopifnot(all(out$type %in% .G3_CLAIM_TYPES))
  out
}

# One claim item.
#' @noRd
.g3_claim <- function(.type, .strength, .topic, .template, .lang, ..., definition = NULL, evidence_id = NULL) {
  # Dotted formals so that placeholders (`to`, `side`, ...) never partially
  # match an argument.
  list(type = .type, strength = .strength, topic = .topic, template = .template,
       text = .g3_tr(.template, .lang, ...), definition = definition, evidence_id = evidence_id)
}

# "Black's rook on N:b6" style list of pieces.
#' @noRd
.g3_piece_list <- function(colors, types, names, lang) {
  items <- vapply(seq_along(types), function(i) {
    .g3_tr("item.piece_on", lang, color = .g3_color_word(colors[i], lang),
           piece = .g3_piece_word(types[i], lang), square = names[i])
  }, "")
  .g3_join_words(items, lang)
}

# Claims (legal facts and labelled heuristics) from action facts.
#' @noRd
.g3_fact_claims <- function(f, lang, topics = NULL) {
  L <- list()
  add <- function(x) L[[length(L) + 1L]] <<- x
  col <- .g3_color_word(f$side, lang)
  opp <- .g3_color_word(f$opponent, lang)
  pw <- .g3_piece_word(f$piece, lang)
  if (f$type == "move") {
    add(.g3_claim("legal_fact", "exact", "action", "action.move", lang,
                  color = col, piece = pw, from = f$from_name, to = f$to_name))
  } else if (f$type == "platform") {
    key <- if (f$rotation == 180L) "action.platform_turn" else "action.platform_keep"
    add(.g3_claim("legal_fact", "exact", "action", key, lang, color = col, platform = f$platform,
                  piece = pw, from_dock = f$from_dock, to_dock = f$to_dock))
    add(.g3_claim("legal_fact", "exact", "platform", "fact.platform_coords", lang, piece = pw,
                  square_id = f$from_square, from_coord = f$coord_before, to_coord = f$coord_after,
                  square = f$to_name))
    if (f$rotation == 180L) {
      add(.g3_claim("legal_fact", "exact", "platform", "fact.platform_orientation", lang,
                    platform = f$platform, before = f$orient_before, after = f$orient_after))
    }
    if (isTRUE(f$pawn_double_lost)) {
      add(.g3_claim("legal_fact", "exact", "rights", "fact.double_step_lost", lang, square = f$to_name))
    }
  } else {
    key <- paste0("action.castle_", f$castle_side)
    add(.g3_claim("legal_fact", "exact", "action", key, lang, color = col, king_from = f$king_from,
                  king_to = f$king_to, rook_from = f$rook_from, rook_to = f$rook_to))
  }
  if (isTRUE(f$capture)) {
    if (isTRUE(f$en_passant)) {
      add(.g3_claim("legal_fact", "exact", "capture", "fact.capture_ep", lang, victim_color = opp,
                    square = f$victim_name, to = f$to_name))
    } else {
      add(.g3_claim("legal_fact", "exact", "capture", "fact.capture", lang, victim_color = opp,
                    victim = .g3_piece_word(f$victim, lang), square = f$victim_name))
    }
  }
  if (!is.na(f$promotion)) {
    add(.g3_claim("legal_fact", "exact", "promotion", "fact.promotion", lang,
                  promo = .g3_piece_word(f$promotion, lang)))
  }
  if (length(f$twin_names) && f$type != "castle") {
    add(.g3_claim("legal_fact", "exact", "coordinates", "fact.twin_squares", lang, coord = f$to_coord,
                  to = f$to_name, twins = .g3_join_words(f$twin_names, lang), board = f$to_board))
  }
  if (isTRUE(f$was_in_check)) {
    chk <- .g3_piece_list(rep(f$opponent, length(f$checkers)), f$checker_types, f$checker_names, lang)
    add(.g3_claim("legal_fact", "exact", "check_escape", paste0("fact.escape.", f$escape), lang,
                  color = col, checkers = chk))
  }
  oc <- f$outcome
  if (identical(oc$reason, "checkmate")) {
    add(.g3_claim("legal_fact", "exact", "outcome", "fact.checkmate", lang, opp = opp, king = f$enemy_king_name))
  } else if (identical(oc$reason, "stalemate")) {
    add(.g3_claim("legal_fact", "exact", "outcome", "fact.stalemate", lang, opp = opp))
  } else {
    if (isTRUE(f$gives_check)) {
      add(.g3_claim("legal_fact", "exact", "check", "fact.check_given", lang, opp = opp, king = f$enemy_king_name))
    }
    if (identical(oc$status, "terminal")) {
      add(.g3_claim("legal_fact", "exact", "outcome", "fact.draw_rule", lang,
                    reason = .g3_term(paste0("reason.", oc$reason), lang)))
    }
  }
  newt <- f$targets %in% f$new_targets & f$target_types != "K"
  if (any(newt)) {
    tl <- .g3_piece_list(rep(f$opponent, sum(newt)), f$target_types[newt], f$target_names[newt], lang)
    add(.g3_claim("legal_fact", "exact", "attacks", "fact.new_targets", lang, piece = pw,
                  square = f$to_name, targets = tl,
                  definition = .g3_tr("def.attack", lang)))
  }
  if (length(f$rights_lost)) {
    rl <- .g3_join_words(vapply(f$rights_lost, function(r) .g3_term(paste0("right.", r), lang), ""), lang)
    add(.g3_claim("legal_fact", "exact", "rights", "fact.rights_lost", lang, rights = rl))
  }
  add(.g3_claim("legal_fact", "exact", "attacks", "fact.attack_count", lang, color = col,
                before = f$attacked_before[1L], after = f$attacked_after[1L], opp = opp,
                obefore = f$attacked_before[2L], oafter = f$attacked_after[2L],
                definition = .g3_tr("def.attack", lang)))
  kz <- f$own_king_zone
  if (!anyNA(kz) && (kz[["after"]] != kz[["before"]] || kz[["after"]] > 0L)) {
    add(.g3_claim("heuristic", "heuristic", "king_safety", "heur.king_zone", lang, color = col,
                  king = f$own_king_name, before = kz[["before"]], after = kz[["after"]], opp = opp,
                  definition = .g3_tr("def.king_zone", lang)))
  }
  if (length(f$hanging)) {
    hl <- .g3_piece_list(rep(f$side, length(f$hanging)), f$hanging_types, f$hanging_names, lang)
    add(.g3_claim("heuristic", "heuristic", "undefended", "heur.hanging", lang, pieces = hl,
                  definition = .g3_tr("def.hanging", lang)))
  }
  out <- .g3_claims_df(L, prefix = "f")
  if (!is.null(topics)) out <- out[out$topic %in% topics, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Coach a game: post-game review with evidence
#'
#' Replays a game through the rules engine and, for each selected
#' decision, compares the played action with the best analysed
#' alternative under one evaluator and one budget. Candidates are the
#' played action plus the `max_candidates` best actions of a one-ply
#' static ranking; all candidates are then searched together with
#' [g3_analyse_position()] so their values come from the same completed
#' depth. A decision is critical when the best analysed alternative beats
#' the played action by at least `threshold` (heuristic units), when it
#' mates in the analysed line and the played action does not, or when the
#' played action allows mate in the analysed line. For critical decisions
#' the best line and the reply line after the played action are replayed
#' through the engine before they are shown, and checkmate is called
#' forced only after an exhaustive check of every reply (up to three
#' plies). Finally, candidate exercises are created from the reviewed
#' positions with [g3_create_exercises()].
#'
#' Values are heuristic units from the side to act's perspective (search)
#' or the model's declared value kind; they are not centipawns, accuracy
#' or ratings. A model evaluator is used only when a validated model
#' runtime is available; otherwise a `g3_incompatible_model` condition is
#' raised (no substitute is used).
#'
#' @param game A `g3_game`.
#' @param evaluator `"search"` (default) or a model agent (`"model:<id>"`
#'   or a `g3_model`), dispatched through [g3_analyse_position()].
#' @param budget Search budget per analysed position, as for
#'   [g3_analyse_position()]; default `list(nodes = 2000L)`. A critical
#'   decision also spends one reply search with the same budget.
#' @param sides Which sides' decisions to review.
#' @param plies Optional integer vector of action plies (1 = first
#'   action) to review; `NULL` reviews all.
#' @param max_candidates Number of statically best actions analysed next
#'   to the played one.
#' @param threshold Minimum value gap for a critical decision. `NULL`
#'   uses 100 heuristic units for search values and 0.1 for model values.
#' @param max_exercises Maximum number of exercises to create (0 for none).
#' @param exercise_budget Search budget used by exercise grading and
#'   continuations, see [g3_create_exercises()].
#' @param lang `"en"` (default) or `"de"`.
#' @return A `g3_coach_review` list: `schema`, `game_id`, `game_hash`,
#'   `rules_version`, `rules_hash`, `evaluator` (list with `id`, `kind`,
#'   `model_id`, `model_revision`), `budget`, `decisions` (data frame, one
#'   row per reviewed decision: `ply`, `side`, `played_action_id`,
#'   `played_label`, `n_legal`, `n_candidates`, `best_action_id`,
#'   `best_label`, `played_score`, `best_score`, `score_kind_played`,
#'   `score_kind_best`, `played_mate_in`, `best_mate_in`, `gap`,
#'   `perspective`, `depth`, `nodes`, `completeness`, `forced`, `critical`,
#'   `critical_reason`, `state_hash`, `position_key`), `claims` (data
#'   frame: `claim_id`, `ply`, `side`, `type`, `strength`, `topic`,
#'   `template`, `text`, `definition`, `evidence_id`), `evidence` (named
#'   list of evidence records), `exercises` (a `g3_exercise_set`),
#'   `game` (the reviewed game), `limitations`, `lang`, `stop_reason` and
#'   `provenance`.
#' @examples
#' # A short game reviewed with a small budget (well under a second).
#' g <- gambitR3D::g3_new_game()
#' for (id in c("m:W:2:1-W:2:3", "m:B:2:2-B:2:0", "m:W:1:1-W:1:3"))
#'   g <- gambitR3D::g3_game_apply(g, id)
#' rev <- gambitR3D::g3_coach_game(g, budget = base::list(nodes = 300L), max_exercises = 2L,
#'                                  exercise_budget = base::list(nodes = 100L))
#' rev$decisions[, c("ply", "played_label", "best_label", "gap", "critical")]
#' utils::head(rev$claims[, c("type", "strength", "text")])
#' @export
g3_coach_game <- function(game, evaluator = "search", budget = list(nodes = 2000L),
                          sides = c("white", "black"), plies = NULL, max_candidates = 4L,
                          threshold = NULL, max_exercises = 10L,
                          exercise_budget = list(nodes = 300L, depth = 2L), lang = "en") {
  .g3_coach_game_impl(game, evaluator, budget, sides, plies, max_candidates, threshold,
                      max_exercises, exercise_budget, lang)
}

# Implementation with optional progress/stop callbacks for background jobs.
#' @noRd
.g3_coach_game_impl <- function(game, evaluator, budget, sides, plies, max_candidates, threshold,
                                max_exercises, exercise_budget, lang, progress = NULL, should_stop = NULL) {
  .g3_check_game(game)
  g3_validate_game(game)
  lang <- .g3_coach_lang(lang)
  sides <- match.arg(sides, .G3_COLORS, several.ok = TRUE)
  max_candidates <- as.integer(max_candidates)
  if (is.na(max_candidates) || max_candidates < 1L) {
    .g3_abort("g3_budget_exceeded", "`max_candidates` must be at least 1.")
  }
  is_model <- inherits(evaluator, "g3_model") || (is.character(evaluator) && grepl("^model", evaluator[1L]))
  if (!is_model && !identical(evaluator, "search")) {
    .g3_abort("g3_incompatible_model", "Coaching supports evaluator = 'search' or a model agent.")
  }
  b <- .g3_budget(budget)
  tabs <- .g3_tables()
  line <- .g3_active_line(game$events)
  ids <- game$events$action_id[line]
  # States before each action, replayed through the public API.
  states <- vector("list", length(ids))
  st <- game$initial_state
  for (k in seq_along(ids)) {
    states[[k]] <- st
    st <- g3_apply_action(st, ids[[k]])$state
  }
  base_ply <- game$initial_state$ply
  sel <- seq_along(ids)
  if (!is.null(plies)) sel <- intersect(sel, as.integer(plies) - base_ply)
  sel <- sel[vapply(sel, function(k) states[[k]]$side %in% sides, TRUE)]

  claims <- list()
  evidence <- list()
  rows <- list()
  eval_info <- list(
    id = if (is.character(evaluator)) evaluator else paste0("model:", evaluator$model_id %||% "unknown"),
    kind = if (is_model) "model" else "search",
    model_id = if (!is_model) NA_character_ else if (is.character(evaluator)) sub("^model:", "", evaluator) else
      as.character(evaluator$model_id %||% NA_character_),
    model_revision = NA_character_
  )
  stop_reason <- "completed"
  done <- 0L
  for (k in sel) {
    if (!is.null(should_stop) && isTRUE(should_stop())) {
      stop_reason <- "stopped"
      break
    }
    res <- .g3_review_decision(states[[k]], ids[[k]], k + base_ply, evaluator, b, max_candidates,
                               threshold, lang, tabs)
    rows[[length(rows) + 1L]] <- res$row
    claims[[length(claims) + 1L]] <- res$claims
    evidence <- c(evidence, res$evidence)
    if (!is.null(res$evaluator_info)) eval_info <- utils::modifyList(eval_info, res$evaluator_info)
    done <- done + 1L
    if (!is.null(progress)) progress(done, length(sel), "decisions")
  }
  decisions <- if (length(rows)) do.call(rbind, rows) else .g3_empty_decisions()
  rownames(decisions) <- NULL
  claims_df <- if (length(claims)) do.call(rbind, claims) else .g3_empty_claims()
  rownames(claims_df) <- NULL
  limitations <- c(
    .g3_tr("limit.candidates", lang, k = max_candidates),
    .g3_tr("limit.search", lang, nodes = b$nodes, depth = b$depth),
    .g3_tr("limit.units", lang)
  )
  if (is_model) limitations <- c(limitations, .g3_tr("limit.model", lang))
  review <- structure(
    list(
      schema = .G3_REVIEW_SCHEMA,
      game_id = game$metadata$game_id,
      game_hash = g3_game_hash(game),
      rules_version = game$state$rules_version,
      rules_hash = game$state$rules_hash,
      evaluator = eval_info,
      budget = b,
      decisions = decisions,
      claims = claims_df,
      evidence = evidence,
      exercises = NULL,
      game = game,
      limitations = limitations,
      lang = lang,
      stop_reason = stop_reason,
      provenance = list(
        coach = .G3_COACH_VERSION, engine = .G3_SEARCH_VERSION, evaluator = .G3_EVAL_VERSION,
        package_version = as.character(utils::packageVersion("gambitR3D")), created = .g3_now(),
        threshold = threshold, max_candidates = max_candidates, sides = sides
      )
    ),
    class = "g3_coach_review"
  )
  max_exercises <- as.integer(max_exercises)
  if (!is.na(max_exercises) && max_exercises > 0L && stop_reason == "completed") {
    review$exercises <- g3_create_exercises(review, max_n = max_exercises, budget = exercise_budget)
    review <- .g3_review_link_exercises(review, lang)
  } else {
    review$exercises <- .g3_empty_exercise_set()
  }
  # Overall summary claim.
  n_crit <- sum(decisions$critical)
  summ_items <- list(.g3_claim("searched", "exact_at_depth", "summary", "review.summary", lang,
                               n = nrow(decisions), k = n_crit, nodes = b$nodes))
  if (is_model) {
    summ_items[[2L]] <- .g3_claim("searched", "exact_at_depth", "summary", "review.evaluator_model", lang,
                                  model = eval_info$model_id,
                                  revision = if (is.na(eval_info$model_revision)) "-" else eval_info$model_revision)
  }
  summ <- .g3_claims_df(summ_items, prefix = "summary", evidence_id = NA_character_)
  review$claims <- rbind(review$claims, summ)
  rownames(review$claims) <- NULL
  review
}

#' @noRd
.g3_empty_decisions <- function() {
  data.frame(
    ply = integer(), side = character(), played_action_id = character(), played_label = character(),
    n_legal = integer(), n_candidates = integer(), best_action_id = character(), best_label = character(),
    played_score = numeric(), best_score = numeric(), score_kind_played = character(),
    score_kind_best = character(), played_mate_in = integer(), best_mate_in = integer(), gap = numeric(),
    perspective = character(), depth = integer(), nodes = numeric(), completeness = character(),
    forced = logical(), critical = logical(), critical_reason = character(), state_hash = character(),
    position_key = character(), stringsAsFactors = FALSE
  )
}

# Review one decision: the position `state` before action `played` at
# action ply `ply`.
#' @noRd
.g3_review_decision <- function(state, played, ply, evaluator, b, max_candidates, threshold, lang, tabs) {
  side <- state$side
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  lids <- .g3_action_ids(core, legal, tabs)
  ip <- match(played, lids)
  shash <- .g3_state_hash(state)
  pkey <- core$rep[length(core$rep)]
  ev_id <- function(x) paste0("ply", ply, "_", x)
  evidence <- list()
  facts <- .g3_action_facts(core, ctx, legal[ip, ], tabs)
  evidence[[ev_id("facts")]] <- list(
    evidence_id = ev_id("facts"), kind = "legal_facts", ply = ply, state_hash = shash, position_key = pkey,
    rules_hash = state$rules_hash, action_id = played, label = facts$label
  )
  fc <- .g3_fact_claims(facts, lang, topics = c(
    "action", "platform", "capture", "promotion", "coordinates", "check_escape", "check", "outcome"
  ))
  items <- lapply(seq_len(nrow(fc)), function(i) {
    x <- as.list(fc[i, c("type", "strength", "topic", "template", "text", "definition")])
    x$evidence_id <- ev_id("facts")
    x
  })
  row <- data.frame(
    ply = as.integer(ply), side = side, played_action_id = played, played_label = facts$label,
    n_legal = nrow(legal), n_candidates = 1L, best_action_id = played, best_label = facts$label,
    played_score = NA_real_, best_score = NA_real_, score_kind_played = NA_character_,
    score_kind_best = NA_character_, played_mate_in = NA_integer_, best_mate_in = NA_integer_,
    gap = NA_real_, perspective = "side_to_act", depth = 0L, nodes = 0, completeness = "none",
    forced = nrow(legal) == 1L, critical = FALSE, critical_reason = NA_character_,
    state_hash = shash, position_key = pkey, stringsAsFactors = FALSE
  )
  if (nrow(legal) == 1L) {
    items[[length(items) + 1L]] <- .g3_claim("legal_fact", "exact", "forced", "review.forced", lang,
                                             evidence_id = ev_id("facts"))
    return(list(row = row, claims = .g3_claims_df(items, ply, side, prefix = paste0("p", ply, "_")),
                evidence = evidence))
  }
  # Candidate preselection: one-ply static ranking (g3-eval/1).
  gr <- .g3_greedy(core, tabs)
  ord <- order(-gr$scores, seq_along(gr$scores))
  cand <- unique(c(gr$ids[ord][seq_len(min(max_candidates, length(ord)))], played))
  an <- tryCatch(
    g3_analyse_position(state, evaluator = evaluator, budget = b, actions = cand),
    g3_incompatible_model = function(e) {
      .g3_abort("g3_incompatible_model", paste0(
        "Coaching with evaluator '", if (is.character(evaluator)) evaluator else "model",
        "' is unavailable: ", sub("([^.])$", "\\1.", conditionMessage(e)), " Use evaluator = 'search'."
      ))
    }
  )
  alt <- an$alternatives
  ei <- NULL
  if (!is.null(an$provenance$model_id)) {
    # The model registry identifies a checkpoint revision by its weights
    # digest; a later registry may add an explicit model_revision.
    ei <- list(model_id = as.character(an$provenance$model_id),
               model_revision = as.character(an$provenance$model_revision %||% an$provenance$weights_digest %||%
                                               NA_character_))
  }
  pr <- match(played, alt$action_id)
  bi <- if (is.na(an$root_score)) NA_integer_ else which(alt$rank == 1L)[1L]
  row$n_candidates <- nrow(alt)
  row$depth <- as.integer(an$depth)
  row$nodes <- as.numeric(an$cost$nodes)
  row$completeness <- an$completeness
  row$played_score <- alt$score[pr]
  row$score_kind_played <- alt$score_kind[pr]
  row$played_mate_in <- alt$mate_in[pr]
  if (!is.na(bi)) {
    row$best_action_id <- alt$action_id[bi]
    row$best_label <- alt$label[bi]
    row$best_score <- alt$score[bi]
    row$score_kind_best <- alt$score_kind[bi]
    row$best_mate_in <- alt$mate_in[bi]
  }
  evidence[[ev_id("analysis")]] <- list(
    evidence_id = ev_id("analysis"), kind = "search", ply = ply, state_hash = shash,
    rules_hash = state$rules_hash, evaluator = if (is.character(evaluator)) evaluator else "model",
    model_id = ei$model_id %||% NA_character_, model_revision = ei$model_revision %||% NA_character_,
    budget = b, depth = an$depth, completeness = an$completeness, bound = "exact_at_depth",
    nodes = an$cost$nodes, elapsed_ms = an$cost$elapsed_ms, perspective = "side_to_act", side = side,
    n_legal = nrow(legal), preselection = "one-ply static ranking (g3-eval/1), top candidates plus the played action",
    candidates = alt[, c("action_id", "label", "score", "score_kind", "mate_in", "bound", "rank")]
  )
  # Criticality.
  thr <- threshold %||% if (identical(row$score_kind_best, "heuristic") || identical(row$score_kind_best, "mate")) 100 else 0.1
  reason <- NA_character_
  if (!is.na(bi) && !is.na(row$played_score)) {
    bk <- row$score_kind_best
    pk <- row$score_kind_played
    bm <- row$best_mate_in
    pm <- row$played_mate_in
    if (identical(bk, "mate") && !is.na(bm) && bm > 0 && !(identical(pk, "mate") && !is.na(pm) && pm > 0)) {
      reason <- "missed_mate"
    } else if (identical(pk, "mate") && !is.na(pm) && pm < 0 && !(identical(bk, "mate") && !is.na(bm) && bm < 0)) {
      reason <- "allows_mate"
    } else if (!identical(bk, "mate") && !identical(pk, "mate")) {
      row$gap <- row$best_score - row$played_score
      if (row$gap >= thr) reason <- "value_gap"
    }
  }
  if (!is.na(bi) && is.na(row$gap) && !identical(row$score_kind_best, "mate") && !identical(row$score_kind_played, "mate")) {
    row$gap <- row$best_score - row$played_score
  }
  row$critical <- !is.na(reason)
  row$critical_reason <- reason
  if (!row$critical) {
    items[[length(items) + 1L]] <- .g3_claim(
      "searched", "exact_at_depth", "comparison", "review.not_critical", lang,
      n = nrow(alt), depth = an$depth, nodes = an$cost$nodes,
      completeness = .g3_term(paste0("completeness.", an$completeness), lang),
      evidence_id = ev_id("analysis")
    )
    return(list(row = row, claims = .g3_claims_df(items, ply, side, prefix = paste0("p", ply, "_")),
                evidence = evidence, evaluator_info = ei))
  }
  # Critical decision: comparison, verified lines, mate proofs, heuristics.
  sidew <- .g3_color_word(side, lang)
  items[[length(items) + 1L]] <- .g3_claim(
    "searched", "exact_at_depth", "comparison", "review.compare", lang,
    best = row$best_label, best_score = .g3_fmt_score(row$best_score, row$score_kind_best, row$best_mate_in, lang),
    played = row$played_label,
    played_score = .g3_fmt_score(row$played_score, row$score_kind_played, row$played_mate_in, lang),
    side = sidew, n = nrow(alt), depth = an$depth, nodes = an$cost$nodes,
    completeness = .g3_term(paste0("completeness.", an$completeness), lang),
    evidence_id = ev_id("analysis")
  )
  if (identical(reason, "value_gap")) {
    items[[length(items) + 1L]] <- .g3_claim(
      "searched", "exact_at_depth", "comparison", "review.gap", lang,
      gap = round(row$gap), side = sidew, depth = an$depth, evidence_id = ev_id("analysis")
    )
  }
  # Best line: the analysis PV, replayed through the engine before display.
  if (length(an$pv) && identical(an$pv[1L], row$best_action_id)) {
    rl <- .g3_replay_line(state, an$pv)
    if (rl$ok) {
      evidence[[ev_id("best_line")]] <- .g3_line_evidence(ev_id("best_line"), ply, state, an$pv, rl, side, b, an$depth, tabs)
      items[[length(items) + 1L]] <- .g3_claim(
        "searched", "analysed_line", "line", "review.best_line", lang,
        line = paste(rl$notation, collapse = "  "), depth = an$depth, evidence_id = ev_id("best_line")
      )
    }
  }
  # Reply line after the played action.
  after <- g3_apply_action(state, played)$state
  if (!identical(after$outcome$status, "terminal")) {
    rep <- tryCatch(g3_choose_action(after, agent = if (is.character(evaluator)) evaluator else evaluator, budget = b),
                    g3_error = function(e) NULL)
    if (!is.null(rep) && length(rep$pv)) {
      rl <- .g3_replay_line(after, rep$pv)
      if (rl$ok) {
        ev <- .g3_line_evidence(ev_id("reply_line"), ply, after, rep$pv, rl, side, b, rep$depth, tabs,
                                start_from = state)
        evidence[[ev_id("reply_line")]] <- ev
        items[[length(items) + 1L]] <- .g3_claim(
          "searched", "analysed_line", "line", "review.reply_line", lang,
          played = row$played_label, line = paste(rl$notation, collapse = "  "), depth = rep$depth,
          evidence_id = ev_id("reply_line")
        )
        if (!is.na(ev$material_delta) && ev$material_delta < 0) {
          items[[length(items) + 1L]] <- .g3_claim(
            "searched", "analysed_line", "material", "review.material_line", lang,
            side = sidew, delta = ev$material_delta, evidence_id = ev_id("reply_line"),
            definition = .g3_tr("def.material", lang)
          )
        }
        # Played action allows mate. Mate in one is a rules fact (the
        # replayed reply ends the game by checkmate); longer mates are
        # proven exhaustively when short enough.
        opp_word <- .g3_color_word(.G3_COLORS[3L - match(side, .G3_COLORS)], lang)
        if (identical(reason, "allows_mate") && identical(abs(row$played_mate_in) - 1L, 1L) &&
            identical(rl$final$outcome$reason, "checkmate") && length(rep$pv) == 1L) {
          items[[length(items) + 1L]] <- .g3_claim(
            "legal_fact", "exact", "mate", "review.allows_mate_in_one", lang,
            opp = opp_word, reply = rl$notation[1L], evidence_id = ev_id("reply_line")
          )
        } else if (identical(reason, "allows_mate") && !is.na(row$played_mate_in) && abs(row$played_mate_in) - 1L <= 3L) {
          acore <- .g3_core(after, tabs)
          alegal <- .g3_legal(acore, tabs)
          ai <- .g3_find_action(acore, alegal, rep$pv[1L], tabs)
          pf <- .g3_prove_mate(acore, alegal[ai, ], abs(row$played_mate_in) - 1L, tabs)
          evidence[[ev_id("mate_against")]] <- c(list(evidence_id = ev_id("mate_against"), kind = "mate_proof",
                                                      ply = ply, state_hash = .g3_state_hash(after),
                                                      action_id = rep$pv[1L]), pf)
          key <- if (pf$status == "proven") "review.mate_against_proven" else "review.mate_against_line"
          items[[length(items) + 1L]] <- .g3_claim(
            "searched", if (pf$status == "proven") "proven" else "analysed_line", "mate", key, lang,
            opp = opp_word, plies = abs(row$played_mate_in) - 1L, replies = pf$replies,
            evidence_id = ev_id("mate_against")
          )
        }
      }
    }
  }
  # Missed mate: prove the best alternative's mate exhaustively if short.
  if (identical(reason, "missed_mate") && identical(as.integer(row$best_mate_in), 1L)) {
    # Mate in one is a rules fact: the action's transition ends in checkmate.
    st1 <- g3_apply_action(state, row$best_action_id)$state
    if (identical(st1$outcome$reason, "checkmate")) {
      items[[length(items) + 1L]] <- .g3_claim(
        "legal_fact", "exact", "mate", "review.mate_in_one", lang, action = row$best_label,
        evidence_id = ev_id("analysis")
      )
    }
  } else if (identical(reason, "missed_mate")) {
    bm <- row$best_mate_in
    bi_l <- .g3_find_action(core, legal, row$best_action_id, tabs)
    pf <- if (bm <= 3L) .g3_prove_mate(core, legal[bi_l, ], bm, tabs) else
      list(status = "unresolved", max_plies = bm, replies = NA_integer_, transitions = 0, method = "not_attempted")
    evidence[[ev_id("mate")]] <- c(list(evidence_id = ev_id("mate"), kind = "mate_proof", ply = ply,
                                        state_hash = shash, action_id = row$best_action_id), pf)
    key <- if (pf$status == "proven") "review.mate_proven" else "review.mate_line"
    items[[length(items) + 1L]] <- .g3_claim(
      "searched", if (pf$status == "proven") "proven" else "analysed_line", "mate", key, lang,
      action = row$best_label, plies = bm, replies = pf$replies %||% NA, evidence_id = ev_id("mate")
    )
  }
  # Heuristic: pieces left attacked and undefended by the played action.
  if (length(facts$hanging)) {
    hl <- .g3_piece_list(rep(side, length(facts$hanging)), facts$hanging_types, facts$hanging_names, lang)
    items[[length(items) + 1L]] <- .g3_claim("heuristic", "heuristic", "undefended", "heur.hanging", lang,
                                             pieces = hl, definition = .g3_tr("def.hanging", lang),
                                             evidence_id = ev_id("facts"))
  }
  list(row = row, claims = .g3_claims_df(items, ply, side, prefix = paste0("p", ply, "_")),
       evidence = evidence, evaluator_info = ei)
}

# Evidence record of a verified line. The material change is measured
# for `side` from `start_from` (default `state`) to the end of the line.
#' @noRd
.g3_line_evidence <- function(id, ply, state, pv, rl, side, b, depth, tabs, start_from = state) {
  s <- match(side, .G3_COLORS)
  m0 <- .g3_material(.g3_core(start_from, tabs))
  m1 <- .g3_material(.g3_core(rl$final, tabs))
  delta <- (m1[s] - m1[3L - s]) - (m0[s] - m0[3L - s])
  list(
    evidence_id = id, kind = "line", ply = ply, start_state_hash = .g3_state_hash(state),
    actions = pv, notation = rl$notation, post_state_hashes = rl$hashes, verified = TRUE,
    verified_with = "g3_apply_action", final_state_hash = .g3_state_hash(rl$final),
    budget = b, depth = depth, material_delta = unname(delta), perspective = side
  )
}

# Add learning-suggestion claims linking critical decisions to exercises.
#' @noRd
.g3_review_link_exercises <- function(review, lang) {
  ex <- review$exercises
  if (is.null(ex) || !nrow(ex$summary)) return(review)
  crit <- review$decisions$ply[review$decisions$critical]
  items <- list()
  plys <- integer()
  sides <- character()
  for (i in seq_len(nrow(ex$summary))) {
    s <- ex$summary[i, ]
    if (is.na(s$source_ply)) next
    critical <- s$source_ply %in% crit
    key <- if (critical) "suggest.exercise_critical" else "suggest.exercise"
    items[[length(items) + 1L]] <- .g3_claim(
      "learning_suggestion", "suggestion", "practice", key, lang,
      theme = .g3_theme_word(s$theme, lang), exercise_id = substr(s$exercise_id, 1L, 12L),
      status = .g3_term(paste0("status.", s$status), lang), evidence_id = paste0("exercise_", s$exercise_id)
    )
    plys <- c(plys, s$source_ply)
    sides <- c(sides, s$side)
  }
  if (!length(items)) return(review)
  df <- .g3_claims_df(items, prefix = "suggest_")
  df$ply <- as.integer(plys)
  df$side <- sides
  df <- df[order(df$ply, seq_len(nrow(df))), , drop = FALSE]
  for (i in seq_len(nrow(ex$summary))) {
    s <- ex$summary[i, ]
    review$evidence[[paste0("exercise_", s$exercise_id)]] <- list(
      evidence_id = paste0("exercise_", s$exercise_id), kind = "exercise", exercise_id = s$exercise_id,
      theme = s$theme, status = s$status, start_state_hash = s$start_state_hash
    )
  }
  review$claims <- rbind(review$claims, df)
  rownames(review$claims) <- NULL
  review
}

#' @export
print.g3_coach_review <- function(x, ...) {
  d <- x$decisions
  cat("<g3_coach_review> game ", x$game_id, ": ", nrow(d), " decisions reviewed, ",
      sum(d$critical), " critical (evaluator ", x$evaluator$id, ", ", x$budget$nodes, " nodes per position)\n",
      sep = "")
  crit <- x$claims[x$claims$ply %in% d$ply[d$critical] & x$claims$type != "legal_fact", , drop = FALSE]
  for (i in seq_len(min(nrow(crit), 12L))) {
    cat("  ply ", crit$ply[i], " [", crit$type[i], "/", crit$strength[i], "] ", crit$text[i], "\n", sep = "")
  }
  if (!is.null(x$exercises)) cat("  exercises: ", nrow(x$exercises$summary), "\n", sep = "")
  invisible(x)
}

# Background job type for the app: a coaching review with progress and
# cooperative cancellation (register as `coach = ".g3_job_coach"` in
# .G3_JOB_TYPES). `args`: game, evaluator, budget, sides, plies,
# max_candidates, threshold, max_exercises, exercise_budget, lang.
#' @noRd
.g3_job_coach <- function(args, ctx) {
  op <- options(gambitR3D.stop_check = ctx$should_stop)
  on.exit(options(op), add = TRUE)
  out <- .g3_coach_game_impl(
    args$game, args$evaluator %||% "search", args$budget %||% list(nodes = 2000L),
    args$sides %||% .G3_COLORS, args$plies, args$max_candidates %||% 4L, args$threshold,
    args$max_exercises %||% 10L, args$exercise_budget %||% list(nodes = 300L, depth = 2L), args$lang %||% "en",
    progress = ctx$progress, should_stop = ctx$should_stop
  )
  if (!identical(out$stop_reason, "completed")) attr(out, "stop_reason") <- if (ctx$timed_out()) "timed_out" else "cancelled"
  out
}
