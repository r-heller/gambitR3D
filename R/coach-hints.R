# Graduated hints and attempt scoring.
#
# Hints reveal one layer at a time, always about the exercise's primary
# solution (one of the accepted actions):
#
#   level 1  area or threat     boards, the threatened piece, the enemy king
#   level 2  piece or platform  "use your knight on W:b1" / "use platform WK"
#   level 3  destination        square with board identity, or dock+rotation
#   level 4  full action        plus a short continuation replayed through
#                               the engine (labelled as an analysed line)
#
# A leak guard checks every rendered hint: text of level L may not contain
# the origin square, platform, destination, dock or action id that a
# higher level reveals (unless the objective itself already names it). If
# a template would leak, a generic sentence for that level is used. Hints
# are a pure function of the exercise and the level, so switching 2D/3D or
# Neutral height cannot change them; `highlight` lists physical square ids,
# board ids, the platform and the dock, identical in both views.
#
# Scoring separates illegal input (a malformed id, a piece that is not
# there, a move leaving the king in check) from strategic mistakes (a legal
# action that does not meet the objective). Grades are rechecked live with
# the criterion; a recorded solution table that disagrees with the engine
# is an error, never silently trusted.

#' Graduated hint for an exercise
#'
#' @param exercise A `g3_exercise`.
#' @param level Hint level 1 (area or threat), 2 (piece or platform), 3
#'   (destination) or 4 (full action and a short verified continuation).
#' @param lang `"en"` (default) or `"de"`.
#' @param tutor Optional offline paraphraser: `NULL` (default,
#'   deterministic text), a function taking the tutor context (see
#'   [g3_validate_tutor_output()]) and returning a proposal, or a model id.
#'   A proposal is shown only if it passes [g3_validate_tutor_output()];
#'   otherwise, or when no tutor runtime is installed, the deterministic
#'   hint is returned with `fallback_reason`.
#' @return A `g3_hint` list: `exercise_id`, `level`, `text` (sentences
#'   shown), `deterministic_text`, `source` (`"deterministic"` or
#'   `"tutor"`), `fallback_reason`, `reveals` (`"area"`, `"piece"`,
#'   `"destination"`, `"solution"`), `claim_types` (one per sentence),
#'   `highlight` (list: `boards`, `squares` (physical ids), `platform`,
#'   `dock`, `action_id`), `leak_guard` (TRUE when a generic sentence
#'   replaced a leaking one) and `lang`.
#' @examples
#' ex <- gambitR3D::g3_exercise_catalogue()$exercises[[1]]
#' gambitR3D::g3_exercise_hint(ex, 1L)$text
#' gambitR3D::g3_exercise_hint(ex, 3L)$highlight
#' gambitR3D::g3_exercise_hint(ex, 4L, lang = "de")$text
#' @export
g3_exercise_hint <- function(exercise, level, lang = "en", tutor = NULL) {
  .g3_check_exercise(exercise)
  lang <- .g3_coach_lang(lang)
  level <- suppressWarnings(as.integer(level))
  if (length(level) != 1L || is.na(level) || !level %in% 1:4) {
    .g3_abort("g3_invalid_state", "`level` must be 1, 2, 3 or 4.")
  }
  parts <- .g3_hint_parts(exercise, lang)
  h <- parts[[level]]
  text <- h$text
  guard <- FALSE
  if (level < 4L) {
    forbidden <- unlist(lapply(parts[(level + 1L):4], function(p) p$tokens))
    allowed <- .g3_objective_text(exercise, lang)
    if (.g3_hint_leaks(text, forbidden, allowed)) {
      text <- .g3_tr(paste0("hint.L", level, ".generic"), lang)
      guard <- TRUE
    }
  }
  hint <- structure(
    list(
      exercise_id = exercise$exercise_id, level = level, text = text, deterministic_text = text,
      source = "deterministic", fallback_reason = NA_character_,
      reveals = c("area", "piece", "destination", "solution")[seq_len(level)],
      claim_types = h$claim_types[seq_along(text)] %||% rep("legal_fact", length(text)),
      highlight = h$highlight, leak_guard = guard, lang = lang
    ),
    class = "g3_hint"
  )
  if (length(hint$claim_types) != length(text)) hint$claim_types <- rep("legal_fact", length(text))
  if (!is.null(tutor)) {
    ctxt <- g3_tutor_context(hint, exercise = exercise)
    res <- .g3_tutor_apply(paste(text, collapse = " "), ctxt, tutor, lang)
    hint$text <- res$text
    hint$source <- res$source
    hint$fallback_reason <- res$fallback_reason
  }
  hint
}

#' @export
print.g3_hint <- function(x, ...) {
  cat("<g3_hint> level ", x$level, " (", x$source, ")\n", sep = "")
  cat(paste0("  ", x$text), sep = "\n")
  invisible(x)
}

# Does text leak any forbidden token? Tokens already in `allowed` (the
# objective) are not leaks. Platform ids match only as whole words that
# are not part of a square name (WK vs WK:e0).
#' @noRd
.g3_hint_leaks <- function(text, forbidden, allowed = "") {
  txt <- paste(text, collapse = " ")
  for (tok in unique(forbidden[!is.na(forbidden) & nzchar(forbidden)])) {
    if (grepl(tok, allowed, fixed = TRUE)) next
    hit <- if (tok %in% .G3_PLATFORMS) {
      grepl(paste0("(?<![A-Za-z])", tok, "(?![A-Za-z:])"), txt, perl = TRUE)
    } else {
      grepl(tok, txt, fixed = TRUE)
    }
    if (hit) return(TRUE)
  }
  FALSE
}

# Text, tokens and highlights for all four levels.
#' @noRd
.g3_hint_parts <- function(ex, lang) {
  tabs <- .g3_tables()
  st <- ex$start_state
  core <- .g3_core(st, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  i <- .g3_find_action(core, legal, ex$primary_action_id, tabs)
  if (is.na(i)) .g3_abort("g3_invalid_state", "The exercise's primary solution is not legal in its position.")
  a <- legal[i, ]
  side <- core$side
  opp <- 3L - side
  oppw <- .g3_color_word(.G3_COLORS[opp], lang)
  sidew <- .g3_color_word(.G3_COLORS[side], lang)
  child <- .g3_make(core, a, tabs)
  cctx <- .g3_ctx(child, tabs)
  type <- a[["type"]]
  p <- a[["piece"]]
  label <- .g3_action_labels(core, legal[i, , drop = FALSE], tabs, ctx)
  from_name <- .g3_sq_name(a[["from"]], ctx, tabs)
  from_id <- tabs$sq_id[a[["from"]]]
  # Level 2 and 3 facts.
  if (type == 1L) {
    l2 <- .g3_tr("hint.L2.piece", lang, piece = .g3_piece_word(.G3_TYPES[core$ptype[p]], lang), square = from_name)
    to_name <- .g3_sq_name(a[["to"]], ctx, tabs)
    l3 <- .g3_tr("hint.L3.square", lang, square = to_name, board = tabs$board_ids[tabs$sq_board[a[["to"]]]],
                 coord = .g3_sq_coord(a[["to"]], ctx))
    tok2 <- c(from_name, from_id)
    tok3 <- c(to_name, tabs$sq_id[a[["to"]]])
    hl2 <- list(squares = from_id)
    hl3 <- list(squares = c(from_id, tabs$sq_id[a[["to"]]]))
  } else if (type == 2L) {
    pl <- .G3_PLATFORMS[a[["plat"]]]
    l2 <- .g3_tr("hint.L2.platform", lang, platform = pl, piece = .g3_piece_word(.G3_TYPES[core$ptype[p]], lang))
    dock <- tabs$dock_ids[a[["dock"]]]
    key <- if (a[["rot"]] == 180L) "hint.L3.dock_turn" else "hint.L3.dock_keep"
    l3 <- .g3_tr(key, lang, dock = dock, square = .g3_sq_name(a[["from"]], cctx, tabs))
    tok2 <- c(pl, from_name, from_id)
    tok3 <- c(dock, .g3_sq_name(a[["from"]], cctx, tabs))
    hl2 <- list(squares = from_id, platform = pl)
    hl3 <- list(squares = from_id, platform = pl, dock = dock)
  } else {
    def <- tabs$castle[[side]][[a[["cside"]]]]
    l2 <- .g3_tr("hint.L2.castle", lang)
    l3 <- .g3_tr(paste0("hint.L3.castle_", c("kingside", "queenside")[a[["cside"]]]), lang,
                 king_to = .g3_sq_name(def$king_to, ctx, tabs), rook_to = .g3_sq_name(def$rook_to, ctx, tabs))
    tok2 <- c(from_name, from_id)
    tok3 <- c(.g3_sq_name(def$king_to, ctx, tabs), tabs$sq_id[def$king_to])
    hl2 <- list(squares = c(from_id, tabs$sq_id[def$rook_from]))
    hl3 <- list(squares = c(tabs$sq_id[def$king_to], tabs$sq_id[def$rook_to]))
  }
  # Level 1: area or threat, by criterion.
  o <- ex$objective
  hl1 <- list()
  l1 <- switch(
    o$criterion,
    checkmate = ,
    platform_check = {
      ks <- .g3_king_sq(core, opp)
      hl1 <- list(squares = tabs$sq_id[ks], boards = tabs$board_ids[tabs$sq_board[ks]])
      .g3_tr(paste0("hint.L1.", o$criterion), lang, opp = oppw, king = .g3_sq_name(ks, ctx, tabs))
    },
    escape_safe = ,
    platform_escape = {
      ks <- .g3_king_sq(core, side)
      chk <- .g3_attackers_of(core, ctx, ks, opp)
      boards <- unique(tabs$board_ids[tabs$sq_board[core$psq[chk]]])
      hl1 <- list(boards = boards)
      .g3_tr(paste0("hint.L1.", if (o$criterion == "escape_safe") "escape_check" else "platform_escape"), lang,
             n = length(chk), boards = .g3_join_words(boards, lang))
    },
    safe_capture = {
      v <- a[["cap"]]
      b <- tabs$board_ids[tabs$sq_board[core$psq[v]]]
      hl1 <- list(boards = b)
      .g3_tr("hint.L1.capture", lang, opp = oppw, board = b)
    },
    fork = {
      tg <- .g3_targets_of(child, cctx, p)
      tg <- tg[child$ptype[tg] != 1L]
      items <- vapply(tg, function(q) .g3_tr("item.piece_on", lang, color = oppw,
                                             piece = .g3_piece_word(.G3_TYPES[child$ptype[q]], lang),
                                             square = .g3_sq_name(child$psq[q], cctx, tabs)), "")
      hl1 <- list(squares = tabs$sq_id[child$psq[tg]])
      .g3_tr("hint.L1.fork", lang, targets = .g3_join_words(items, lang))
    },
    keep_piece = {
      q <- match(o$params$piece_id, core$pid)
      att <- .g3_attackers_of(core, ctx, core$psq[q], opp)
      ab <- unique(tabs$board_ids[tabs$sq_board[core$psq[att]]])
      at <- unique(.G3_TYPES[core$ptype[att]])
      hl1 <- list(squares = tabs$sq_id[core$psq[q]], boards = ab)
      .g3_tr("hint.L1.defend", lang, piece = .g3_piece_word(.G3_TYPES[core$ptype[q]], lang),
             square = .g3_sq_name(core$psq[q], ctx, tabs),
             attackers = .g3_join_words(vapply(at, .g3_piece_word, "", lang = lang), lang),
             boards = .g3_join_words(ab, lang))
    },
    capture_target = {
      q <- match(o$params$target_id, core$pid)
      tw <- which(ctx$ci == ctx$ci[core$psq[q]])
      boards <- tabs$board_ids[tabs$sq_board[tw]]
      hl1 <- list(boards = boards, squares = tabs$sq_id[tw])
      .g3_tr("hint.L1.coordinate_levels", lang, coord = .g3_sq_coord(core$psq[q], ctx),
             boards = .g3_join_words(boards, lang))
    },
    improve_decision = {
      # The board where the action takes effect: destination board, the
      # destination dock's parent board, or the castling king's board.
      b <- switch(type,
                  tabs$board_ids[tabs$sq_board[a[["to"]]]],
                  substr(tabs$dock_ids[a[["dock"]]], 1L, 1L),
                  tabs$board_ids[tabs$sq_board[a[["from"]]]])
      hl1 <- list(boards = b)
      .g3_tr("hint.L1.improve_decision", lang, board = b)
    }
  )
  # Level 4: full action and verified continuation.
  cont <- ex$continuation
  l4 <- .g3_tr("hint.L4.solution", lang, label = label)
  ct4 <- "legal_fact"
  if (isTRUE(cont$verified) && length(cont$notation) > 1L) {
    l4 <- c(l4, .g3_tr("hint.L4.continuation", lang, line = paste(cont$notation, collapse = "  "),
                       depth = cont$depth %||% NA))
    ct4 <- c(ct4, "searched")
  } else if (!nrow(.g3_legal(child, tabs, cctx))) {
    oc <- .g3_outcome_core(child, tabs, cctx)
    l4 <- c(l4, .g3_tr("hint.L4.ends", lang, outcome = .g3_term(paste0("reason.", oc$reason), lang)))
    ct4 <- c(ct4, "legal_fact")
  }
  n_acc <- nrow(ex$solutions)
  if (n_acc > 1L) {
    l4 <- c(l4, .g3_tr("hint.L4.more", lang, n = sum(ex$solutions$grade == "solved"),
                       m = sum(ex$solutions$grade == "acceptable")))
    ct4 <- c(ct4, "legal_fact")
  }
  basis1 <- "legal_fact"
  list(
    list(text = l1, tokens = character(), highlight = .g3_highlight(hl1), claim_types = basis1),
    list(text = l2, tokens = tok2, highlight = .g3_highlight(hl2), claim_types = "legal_fact"),
    list(text = l3, tokens = tok3, highlight = .g3_highlight(hl3), claim_types = "legal_fact"),
    list(text = l4, tokens = c(ex$primary_action_id, label),
         highlight = .g3_highlight(utils::modifyList(hl3, list(action_id = ex$primary_action_id))),
         claim_types = ct4)
  )
}

#' @noRd
.g3_highlight <- function(x) {
  list(boards = as.character(x$boards %||% character()), squares = as.character(x$squares %||% character()),
       platform = as.character(x$platform %||% NA_character_), dock = as.character(x$dock %||% NA_character_),
       action_id = as.character(x$action_id %||% NA_character_))
}

#' Score an exercise attempt
#'
#' Checks an attempted action (and an optional continuation) against an
#' exercise. Illegal input is reported separately from strategic
#' mistakes. A legal action is graded with the exercise's exact criterion,
#' re-evaluated live with the rules engine: `"solved"` (an accepted
#' solution), `"acceptable"` (meets the objective in a lesser way, for
#' example a smaller safe capture, or rated lower by the recorded bounded
#' search) or `"incorrect"` (legal, but the objective is not met; the
#' feedback names the verified refuting reply where there is one).
#'
#' @param exercise A `g3_exercise`. Its rules hash must match the
#'   installed profile (`g3_unsupported_rules` otherwise) and its position
#'   its recorded state hash (`g3_stale_revision` otherwise).
#' @param actions Character vector of action ids: the attempted action,
#'   optionally followed by a continuation (replayed for legality only).
#' @param hints_used Number of hint levels revealed before the attempt
#'   (0 to 4).
#' @param lang `"en"` (default) or `"de"`.
#' @return A `g3_attempt` list: `exercise_id`, `theme`, `status`
#'   (`"solved"`, `"acceptable"`, `"incorrect"` or `"illegal"`),
#'   `feedback_kind` (`"objective"` or `"illegal_input"`), `grade_basis`
#'   (`"exact"` or `"searched"`), `action_id`, `actions`, `hints_used`,
#'   `feedback` (sentences), `claims` (typed claims table), `evidence`
#'   (list: criterion, reason, refuting reply with its replayed notation,
#'   illegal diagnosis), `continuation` (replay result or `NULL`),
#'   `start_state_hash`, `rules_hash`, `scored_at` and `lang`.
#' @examples
#' ex <- gambitR3D::g3_exercise_catalogue()$exercises[[1]]
#' gambitR3D::g3_score_attempt(ex, ex$primary_action_id)$status
#' # A legal action that misses the objective, and an illegal input.
#' gambitR3D::g3_score_attempt(ex, "m:B:0:0-B:0:1")$feedback
#' gambitR3D::g3_score_attempt(ex, "m:W:0:0-W:0:3")$feedback
#' @export
g3_score_attempt <- function(exercise, actions, hints_used = 0L, lang = "en") {
  .g3_check_exercise(exercise)
  lang <- .g3_coach_lang(lang)
  hints_used <- suppressWarnings(as.integer(hints_used))
  if (length(hints_used) != 1L || is.na(hints_used) || hints_used < 0L || hints_used > 4L) {
    .g3_abort("g3_invalid_state", "`hints_used` must be an integer from 0 to 4.")
  }
  ids <- if (is.data.frame(actions)) as.character(actions$action_id) else
    if (is.list(actions)) vapply(actions, .g3_action_id_arg, "") else as.character(actions)
  if (!length(ids)) .g3_abort("g3_illegal_action", "`actions` must contain at least one action id.")
  tabs <- .g3_tables()
  st <- exercise$start_state
  scan <- .g3_scan_position(st, tabs)
  first <- ids[[1L]]
  i <- match(first, scan$ids)
  base <- list(
    exercise_id = exercise$exercise_id, theme = exercise$theme, action_id = first, actions = ids,
    hints_used = hints_used, start_state_hash = exercise$start_state_hash, rules_hash = exercise$rules_hash,
    scored_at = .g3_now(), lang = lang
  )
  if (is.na(i)) {
    d <- .g3_diagnose_illegal(scan$core, scan$ctx, first, tabs)
    fb <- c(.g3_tr("attempt.illegal", lang), .g3_illegal_text(d, lang))
    claims <- .g3_claims_df(list(
      list(type = "legal_fact", strength = "exact", topic = "illegal_input", template = paste0("illegal.", d$code),
           text = fb[2L])
    ), prefix = "a")
    return(structure(c(base, list(
      status = "illegal", feedback_kind = "illegal_input", grade_basis = "exact", feedback = fb,
      claims = claims, evidence = list(legal = FALSE, diagnosis = d$code, diagnosis_params = d$params),
      continuation = NULL
    )), class = "g3_attempt"))
  }
  ev <- .g3_criterion_eval(scan, i, exercise$objective, tabs)
  si <- match(first, exercise$solutions$action_id)
  basis <- "exact"
  search_ev <- NULL
  if (exercise$objective$criterion != "improve_decision") {
    if (!identical(isTRUE(ev$meets), !is.na(si))) {
      .g3_abort("g3_invalid_state", paste0(
        "The exercise's recorded solutions disagree with the rules engine for ", first,
        "; re-validate the exercise with g3_validate_exercise()."
      ))
    }
    status <- if (is.na(si)) "incorrect" else exercise$solutions$grade[si]
    if (!is.na(si)) basis <- exercise$solutions$basis[si]
    if (!is.na(si) && ev$tier %in% c("solved", "acceptable") && basis == "exact" && ev$tier != status) {
      .g3_abort("g3_invalid_state", "The exercise's recorded grade disagrees with the rules engine; re-validate it.")
    }
    if (status == "acceptable" && basis == "searched") ev$reason <- "lower_rated"
  } else {
    basis <- "searched"
    if (!is.na(si)) {
      status <- exercise$solutions$grade[si]
    } else if (identical(first, exercise$objective$params$played)) {
      status <- "incorrect"
      ev$reason <- "played_move"
    } else {
      g <- exercise$grading
      an <- g3_analyse_position(st, budget = g$budget, actions = unique(c(exercise$primary_action_id, first)))
      sc <- an$alternatives$score
      names(sc) <- an$alternatives$action_id
      ok <- !is.na(sc[first]) && !is.na(sc[exercise$primary_action_id]) &&
        sc[first] >= sc[exercise$primary_action_id] - g$tolerance
      status <- if (ok) "acceptable" else "incorrect"
      ev$reason <- if (ok) "searched_similar" else "searched_lower"
      search_ev <- list(depth = an$depth, nodes = an$cost$nodes, completeness = an$completeness,
                        scores = as.list(sc), budget = g$budget)
    }
  }
  # Verified notation of the refuting reply, if any.
  reply <- NULL
  if (!is.na(ev$reply)) {
    rl <- .g3_replay_line(st, c(first, ev$reply))
    if (rl$ok) reply <- list(action_id = ev$reply, notation = rl$notation[2L], verified = TRUE)
  }
  label <- scan$labels[i]
  fb <- .g3_tr(paste0("attempt.", status), lang, label = label)
  params <- list(reply = reply$notation %||% "", n = ev$extra$n %||% NA, depth = exercise$grading$depth %||% NA,
                 to = label)
  if (ev$reason == "twin_square") {
    q <- match(exercise$objective$params$target_id, scan$core$pid)
    tsq <- scan$core$psq[q]
    to <- scan$legal[i, "to"]
    params$to <- .g3_sq_name(to, scan$ctx, tabs)
    params$target <- .g3_sq_name(tsq, scan$ctx, tabs)
    params$coord <- .g3_sq_coord(tsq, scan$ctx)
    params$board <- tabs$board_ids[tabs$sq_board[tsq]]
  }
  fb <- c(fb, do.call(.g3_tr, c(list(paste0("reason.", ev$reason), lang), params)))
  if (status %in% c("solved", "acceptable") && nrow(exercise$solutions) > 1L) {
    fb <- c(fb, .g3_tr("attempt.other_solutions", lang, n = nrow(exercise$solutions) - 1L))
  }
  if (hints_used > 0L) fb <- c(fb, .g3_tr("attempt.hints", lang, n = hints_used))
  facts <- .g3_action_facts(scan$core, scan$ctx, scan$legal[i, ], tabs, child = scan$kids[[i]]$child)
  claims <- .g3_fact_claims(facts, lang, topics = c("action", "platform", "capture", "promotion", "coordinates",
                                                    "check_escape", "check", "outcome"))
  obj_claim <- .g3_claims_df(list(list(
    type = if (basis == "searched") "searched" else "legal_fact",
    strength = if (basis == "searched") "exact_at_depth" else "exact", topic = "objective",
    template = paste0("reason.", ev$reason), text = fb[2L]
  )), prefix = "o")
  claims <- rbind(obj_claim, claims)
  rownames(claims) <- NULL
  cont <- NULL
  if (length(ids) > 1L) {
    rl <- .g3_replay_line(st, ids)
    cont <- list(ok = rl$ok, notation = rl$notation, failed_at = rl$failed_at,
                 feedback = if (rl$ok) NA_character_ else
                   .g3_tr("illegal.continuation", lang, n = rl$failed_at, input = ids[[rl$failed_at]]))
    if (!rl$ok) fb <- c(fb, cont$feedback)
  }
  structure(c(base, list(
    status = status, feedback_kind = "objective", grade_basis = basis, feedback = fb, claims = claims,
    evidence = list(legal = TRUE, label = label, criterion = exercise$objective$criterion, reason = ev$reason,
                    reply = reply, search = search_ev, exercise_status = exercise$status),
    continuation = cont
  )), class = "g3_attempt")
}

#' @export
print.g3_attempt <- function(x, ...) {
  cat("<g3_attempt> ", x$action_id, ": ", x$status, "\n", sep = "")
  cat(paste0("  ", x$feedback), sep = "\n")
  invisible(x)
}
