# Practice exercises from real positions.
#
# An exercise is a start position, an objective and an accepted solution
# set derived from an exact criterion. Every legal action of the start
# position is checked against the criterion with the rules engine, so the
# solution set is complete under the saved rules hash; several good
# answers are normal. Themes and criteria (definitions are part of the
# objective and of the hint/feedback texts):
#
#   checkmate          the action checkmates                      exact
#   escape_check       in check: no legal reply checkmates        exact; the
#                      best-rated escapes are "solved", the rest "acceptable"
#                      (searched tiering, budget recorded)
#   capture            the action captures and no legal reply captures the
#                      capturing piece ("safe capture"); the most valuable
#                      safe captures are solved, smaller ones acceptable, an
#                      unsafe capture of a more valuable piece acceptable
#   fork               after the action the moved piece attacks at least two
#                      enemy non-pawn pieces (king included), at least one of
#                      them a king, worth at least as much as the mover, or
#                      undefended; and no legal reply captures the mover
#                      (an unsafe fork is acceptable)
#   defend             a threatened piece cannot be captured with a net gain
#                      after the action (net gain: the captured piece is worth
#                      more than its capturer, or no legal recapture exists);
#                      searched tiering as for escape_check
#   coordinate_levels  capture a named target piece; a move to another
#                      physical square at the same coordinate is incorrect
#   platform           a platform action gives check (or escapes check
#                      safely); the same effect without a platform is
#                      acceptable
#   improve_decision   from a reviewed game: an action the search rates
#                      within a tolerance of the best analysed alternative.
#                      Search-based, so it always stays a "candidate"
#
# Status: "validated" only after the criterion has been checked for every
# legal action under the installed rules hash and at least one action is
# solved; otherwise "candidate". A truncated search never makes a solution
# "proven unique": `unique_solution_proven` is TRUE only when exactly one
# legal action meets an exhaustive criterion.
#
# Identity: `exercise_id` = SHA-256 of the canonical JSON of the schema,
# the start state's full hash and the objective (theme, criterion, side,
# parameters). Display settings (2D/3D, Neutral height, camera) are not
# part of a state and therefore cannot change an exercise, its answers or
# its hints.

.G3_EXERCISE_SCHEMA <- "g3-exercise/1"
.G3_EXERCISE_SET_SCHEMA <- "g3-exercise-set/1"
.G3_EXERCISE_THEMES <- c("checkmate", "escape_check", "platform", "fork", "capture", "defend",
                         "coordinate_levels", "improve_decision")
.G3_SEARCH_TIERED <- c("escape_safe", "keep_piece", "improve_decision")

#' Create practice exercises from analysed positions
#'
#' Detects teachable situations in real positions (escape check, deliver
#' checkmate, safe capture, fork, defend a threatened piece, distinguish
#' boards at the same coordinate, use a platform) and turns them into
#' versioned exercises whose accepted solutions are checked against every
#' legal action with the rules engine. Critical decisions of a coaching
#' review are used first.
#'
#' @param analysis A `g3_coach_review` from [g3_coach_game()], a
#'   `g3_analysis` from [g3_analyse_game()] (pass the analysed game as
#'   `game`), a `g3_game`, a `g3_state` or a list of `g3_state`.
#' @param max_n Maximum number of exercises.
#' @param themes Optional subset of themes: `"checkmate"`,
#'   `"escape_check"`, `"platform"`, `"fork"`, `"capture"`, `"defend"`,
#'   `"coordinate_levels"`, `"improve_decision"`. The exact criterion of
#'   each theme is stated in its objective text.
#' @param budget Search budget for grading (solved vs acceptable where a
#'   theme uses searched tiering) and for the short continuation shown by
#'   the last hint level. `list(nodes = 0L)` disables both searches.
#' @param tolerance Heuristic-unit tolerance for searched tiering.
#' @param game For a `g3_analysis` input: the analysed `g3_game`. A
#'   `g3_analysis` stores tables, not the game, so positions are rebuilt by
#'   replaying `game`; its content hash must equal
#'   `analysis$game$game_content_hash` and every replayed state must equal
#'   `analysis$positions$recorded_state_hash` (otherwise the analysis is
#'   stale and a `g3_stale_revision` condition is raised). Decisions whose
#'   `move_loss_raw` is at least 100 heuristic units, or where the best
#'   action mates and the played one does not, count as critical.
#' @return A `g3_exercise_set` list: `schema`, `exercises` (list of
#'   `g3_exercise`), `summary` (data frame: `exercise_id`, `theme`,
#'   `status`, `side`, `n_legal`, `n_solved`, `n_acceptable`,
#'   `discriminating`, `unique_solution_proven`, `source`, `game_id`,
#'   `source_ply`, `start_state_hash`) and `provenance`. Each
#'   `g3_exercise` holds `schema`, `exercise_id`, `version`, `theme`,
#'   `objective`, `grading`, `start_state`, `start_state_hash`,
#'   `position_key`, `rules_version`, `rules_hash`, `solutions` (data
#'   frame: `action_id`, `label`, `grade` (`"solved"`/`"acceptable"`),
#'   `basis` (`"exact"`/`"searched"`), `reason`, `score`),
#'   `primary_action_id`, `continuation`, `status`, `validation` and
#'   `provenance`.
#' @examples
#' # A position where Black can mate in one in several ways.
#' pcs <- base::data.frame(
#'   id = c("wK", "bQ", "bRd", "bRf", "bK"), type = c("K", "Q", "R", "R", "K"),
#'   color = c("white", "black", "black", "black", "black"),
#'   square_id = c("WK:0:0", "B:0:0", "N:2:3", "BK:1:1", "B:0:3")
#' )
#' st <- gambitR3D::g3_setup_position(pcs, side = "black")
#' ex <- gambitR3D::g3_create_exercises(st, max_n = 1L, budget = base::list(nodes = 0L))
#' ex$exercises[[1]]$solutions[, c("label", "grade", "basis")]
#' @export
g3_create_exercises <- function(analysis, max_n = 10L, themes = NULL,
                                budget = list(nodes = 300L, depth = 2L), tolerance = 50, game = NULL) {
  max_n <- as.integer(max_n)
  if (is.na(max_n) || max_n < 0L) .g3_abort("g3_budget_exceeded", "`max_n` must be a non-negative integer.")
  if (!is.null(themes)) themes <- match.arg(themes, .G3_EXERCISE_THEMES, several.ok = TRUE)
  b <- .g3_budget(budget)
  tabs <- .g3_tables()
  src <- .g3_exercise_sources(analysis, tabs, game)
  if (!max_n || !length(src$positions)) return(.g3_empty_exercise_set(src$provenance))
  # Detect themes per position; critical decisions first, then by ply.
  pos <- src$positions
  prio <- order(!vapply(pos, function(p) isTRUE(p$critical), TRUE), vapply(pos, function(p) p$ply %||% 0L, 0L))
  queue <- list()
  for (k in prio) {
    scan <- .g3_scan_position(pos[[k]]$state, tabs)
    det <- .g3_detect_themes(scan, pos[[k]], tabs)
    if (!is.null(themes)) det <- det[vapply(det, function(d) d$theme %in% themes, TRUE)]
    for (d in det) queue[[length(queue) + 1L]] <- list(pos = k, scan = scan, det = d)
  }
  # Round-robin over themes for variety, preserving priority within a theme.
  th <- vapply(queue, function(q) q$det$theme, "")
  order_in_theme <- stats::ave(seq_along(th), th, FUN = seq_along)
  queue <- queue[order(order_in_theme, match(th, .G3_EXERCISE_THEMES))]
  out <- list()
  seen <- character()
  for (q in queue) {
    if (length(out) >= max_n) break
    ex <- .g3_build_exercise(q$scan, q$det, pos[[q$pos]], b, tolerance, tabs)
    if (is.null(ex) || ex$exercise_id %in% seen) next
    seen <- c(seen, ex$exercise_id)
    out[[length(out) + 1L]] <- ex
  }
  .g3_exercise_set(out, src$provenance)
}

#' @noRd
.g3_exercise_set <- function(exercises, provenance = list()) {
  structure(
    list(schema = .G3_EXERCISE_SET_SCHEMA, exercises = exercises,
         summary = .g3_exercise_summary(exercises), provenance = provenance),
    class = "g3_exercise_set"
  )
}

#' @noRd
.g3_empty_exercise_set <- function(provenance = list()) .g3_exercise_set(list(), provenance)

#' @noRd
.g3_exercise_summary <- function(exercises) {
  if (!length(exercises)) {
    return(data.frame(
      exercise_id = character(), theme = character(), status = character(), side = character(),
      n_legal = integer(), n_solved = integer(), n_acceptable = integer(), discriminating = logical(),
      unique_solution_proven = logical(), source = character(), game_id = character(),
      source_ply = integer(), start_state_hash = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    exercise_id = vapply(exercises, function(e) e$exercise_id, ""),
    theme = vapply(exercises, function(e) e$theme, ""),
    status = vapply(exercises, function(e) e$status, ""),
    side = vapply(exercises, function(e) e$objective$side, ""),
    n_legal = vapply(exercises, function(e) as.integer(e$validation$n_legal), 0L),
    n_solved = vapply(exercises, function(e) sum(e$solutions$grade == "solved"), 0L),
    n_acceptable = vapply(exercises, function(e) sum(e$solutions$grade == "acceptable"), 0L),
    discriminating = vapply(exercises, function(e) isTRUE(e$validation$discriminating), TRUE),
    unique_solution_proven = vapply(exercises, function(e) isTRUE(e$validation$unique_solution_proven), TRUE),
    source = vapply(exercises, function(e) e$provenance$source, ""),
    game_id = vapply(exercises, function(e) e$provenance$game_id %||% NA_character_, ""),
    source_ply = vapply(exercises, function(e) as.integer(e$provenance$ply %||% NA_integer_), 0L),
    start_state_hash = vapply(exercises, function(e) e$start_state_hash, ""),
    stringsAsFactors = FALSE
  )
}

#' @export
print.g3_exercise_set <- function(x, ...) {
  cat("<g3_exercise_set> ", length(x$exercises), " exercise(s)\n", sep = "")
  if (nrow(x$summary)) {
    s <- x$summary
    s$exercise_id <- substr(s$exercise_id, 1L, 12L)
    print(s[, c("exercise_id", "theme", "status", "side", "n_solved", "n_acceptable", "source_ply")], row.names = FALSE)
  }
  invisible(x)
}

#' @export
print.g3_exercise <- function(x, ...) {
  cat("<g3_exercise> ", substr(x$exercise_id, 1L, 12L), " [", x$theme, ", ", x$status, "]\n", sep = "")
  cat("  ", .g3_objective_text(x, "en"), "\n", sep = "")
  cat("  accepted: ", sum(x$solutions$grade == "solved"), " solved, ",
      sum(x$solutions$grade == "acceptable"), " acceptable of ", x$validation$n_legal, " legal actions\n", sep = "")
  invisible(x)
}

# Positions to mine for exercises, from any supported input.
#' @noRd
.g3_exercise_sources <- function(analysis, tabs, game = NULL) {
  pos <- list()
  prov <- list(generator = .G3_COACH_VERSION, input = class(analysis)[1L], created = .g3_now())
  game_positions <- function(game, decisions = NULL) {
    line <- .g3_active_line(game$events)
    ids <- game$events$action_id[line]
    st <- game$initial_state
    base <- game$initial_state$ply
    gh <- g3_game_hash(game)
    out <- list()
    for (k in seq_along(ids)) {
      ply <- base + k
      info <- list(state = st, source = "game", game_id = game$metadata$game_id, game_hash = gh,
                   ply = ply, played = ids[[k]], critical = FALSE, best = NA_character_)
      if (!is.null(decisions)) {
        r <- match(ply, decisions$ply)
        if (is.na(r)) {
          info <- NULL
        } else {
          if (!identical(decisions$state_hash[r], .g3_state_hash(st))) {
            .g3_abort("g3_stale_revision", paste0(
              "The analysis is stale: its position at ply ", ply,
              " does not match the game's replayed state."
            ), ply = ply)
          }
          info$critical <- isTRUE(decisions$critical[r])
          info$best <- decisions$best_action_id[r]
          info$review <- decisions[r, , drop = FALSE]
        }
      }
      if (!is.null(info) && !identical(st$outcome$status, "terminal")) out[[length(out) + 1L]] <- info
      st <- g3_apply_action(st, ids[[k]])$state
    }
    out
  }
  if (inherits(analysis, "g3_coach_review")) {
    if (!identical(analysis$rules_hash, tabs$rules_hash)) {
      .g3_abort("g3_unsupported_rules", "The review was made under a different rules hash.",
                rules_hash = analysis$rules_hash)
    }
    if (!inherits(analysis$game, "g3_game") || !identical(g3_game_hash(analysis$game), analysis$game_hash)) {
      .g3_abort("g3_stale_revision", "The review's game changed after the analysis (game hash mismatch).")
    }
    pos <- game_positions(analysis$game, analysis$decisions)
    prov$game_id <- analysis$game_id
    prov$evaluator <- analysis$evaluator
    prov$review_budget <- analysis$budget
  } else if (inherits(analysis, "g3_analysis")) {
    # A g3_analysis holds tables only; the positions are rebuilt from the
    # analysed game, which must be exactly the game that was analysed.
    if (inherits(analysis$game, "g3_game")) game <- analysis$game
    if (!inherits(game, "g3_game")) {
      .g3_abort("g3_invalid_state", paste0(
        "A g3_analysis stores tables, not positions: pass the analysed game as ",
        "`game` so its positions can be rebuilt."
      ))
    }
    rh <- analysis$provenance$rules_hash %||% analysis$game$rules_hash
    if (!identical(rh, tabs$rules_hash) || !identical(game$state$rules_hash, tabs$rules_hash)) {
      .g3_abort("g3_unsupported_rules", "The analysis was made under a different rules hash.")
    }
    if (!identical(analysis$game$game_content_hash, g3_game_hash(game))) {
      .g3_abort("g3_stale_revision", "The analysis is stale: it was made for a different game (content hash mismatch).")
    }
    ev <- analysis$evaluations
    ptab <- analysis$positions
    # Decision table in the review's layout: `ply` is the action ply, the
    # analysis tables use the ply of the position before the action.
    dec <- data.frame(
      ply = as.integer(ev$ply) + 1L,
      state_hash = ptab$recorded_state_hash[match(ev$ply, ptab$ply)],
      best_action_id = ev$best_action_id,
      critical = (!is.na(ev$move_loss_raw) & ev$move_loss_raw >= 100) |
        (!is.na(ev$best_kind) & ev$best_kind == "mate" & !is.na(ev$best_mate_in) & ev$best_mate_in > 0 &
           !(!is.na(ev$played_kind) & ev$played_kind == "mate" & !is.na(ev$played_mate_in) & ev$played_mate_in > 0)),
      stringsAsFactors = FALSE
    )
    dec <- dec[!is.na(ev$played_action_id), , drop = FALSE]
    pos <- game_positions(game, dec)
    prov$game_id <- game$metadata$game_id
    prov$evaluator <- analysis$provenance$evaluator
    prov$analysis_id <- analysis$provenance$analysis_id
  } else if (inherits(analysis, "g3_game")) {
    g3_validate_game(analysis)
    pos <- game_positions(analysis)
    prov$game_id <- analysis$metadata$game_id
  } else if (inherits(analysis, "g3_state")) {
    .g3_check_state(analysis)
    if (!identical(analysis$outcome$status, "terminal")) {
      pos <- list(list(state = analysis, source = "position", ply = NA_integer_, played = NA_character_,
                       critical = FALSE, best = NA_character_))
    }
  } else if (is.list(analysis) && length(analysis) && all(vapply(analysis, inherits, TRUE, "g3_state"))) {
    for (s in analysis) {
      .g3_check_state(s)
      if (!identical(s$outcome$status, "terminal")) {
        pos[[length(pos) + 1L]] <- list(state = s, source = "position", ply = NA_integer_,
                                        played = NA_character_, critical = FALSE, best = NA_character_)
      }
    }
  } else {
    .g3_abort("g3_invalid_state", paste0(
      "`analysis` must be a g3_coach_review, g3_analysis, g3_game, g3_state or list of g3_state."
    ))
  }
  list(positions = pos, provenance = prov)
}

# Everything the criteria need about a position: every legal action with
# its child position, the child's legal replies and check status.
#' @noRd
.g3_scan_position <- function(state, tabs) {
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  ids <- .g3_action_ids(core, legal, tabs)
  kids <- lapply(seq_len(nrow(legal)), function(i) {
    child <- .g3_make(core, legal[i, ], tabs)
    cctx <- .g3_ctx(child, tabs)
    clegal <- .g3_legal(child, tabs, cctx)
    check <- .g3_in_check(child, child$side, cctx, tabs)
    list(child = child, ctx = cctx, legal = clegal, check = check, mate = check && !nrow(clegal),
         ids = .g3_action_ids(child, clegal, tabs))
  })
  env <- new.env(parent = emptyenv())
  list(
    state = state, state_hash = .g3_state_hash(state), core = core, ctx = ctx, legal = legal, ids = ids,
    labels = if (nrow(legal)) .g3_action_labels(core, legal, tabs, ctx) else character(),
    side = core$side, in_check = .g3_in_check(core, core$side, ctx, tabs), kids = kids, cache = env
  )
}

# Does any legal reply in kid `k` checkmate? Returns the first such reply
# id or NA. Cached per action.
#' @noRd
.g3_mate_reply <- function(scan, i, tabs) {
  key <- paste0("mate_reply_", i)
  if (!is.null(scan$cache[[key]])) return(scan$cache[[key]])
  k <- scan$kids[[i]]
  res <- NA_character_
  for (j in seq_len(nrow(k$legal))) {
    gc <- .g3_make(k$child, k$legal[j, ], tabs)
    gctx <- .g3_ctx(gc, tabs)
    if (.g3_in_check(gc, gc$side, gctx, tabs) && !nrow(.g3_legal(gc, tabs, gctx))) {
      res <- k$ids[j]
      break
    }
  }
  scan$cache[[key]] <- res
  res
}

# First legal reply in kid i that captures piece index q, or NA.
#' @noRd
.g3_capture_reply <- function(scan, i, q) {
  k <- scan$kids[[i]]
  j <- which(k$legal[, "cap"] == q)
  if (length(j)) k$ids[j[1L]] else NA_character_
}

# Net-gain captures of own piece q after action i: a reply capturing q
# where q is worth more than the capturer, or no legal recapture of the
# capturer exists. Returns the first such reply id or NA.
#' @noRd
.g3_net_gain_reply <- function(scan, i, q, tabs) {
  k <- scan$kids[[i]]
  for (j in which(k$legal[, "cap"] == q)) {
    cap_by <- k$legal[j, "piece"]
    if (.g3_piece_value(k$child$ptype[q]) > .g3_piece_value(k$child$ptype[cap_by])) return(k$ids[j])
    gc <- .g3_make(k$child, k$legal[j, ], tabs)
    gl <- .g3_legal(gc, tabs)
    if (!any(gl[, "cap"] == cap_by)) return(k$ids[j])
  }
  NA_character_
}

# Fork test for action i (definition in the file header).
#' @noRd
.g3_fork_info <- function(scan, i) {
  k <- scan$kids[[i]]
  p <- scan$legal[i, "piece"]
  tg <- .g3_targets_of(k$child, k$ctx, p)
  tg <- tg[k$child$ptype[tg] != 1L]
  if (length(tg) < 2L) return(list(fork = FALSE, n = length(tg), targets = tg))
  val_p <- .g3_piece_value(k$child$ptype[p])
  meaningful <- any(k$child$ptype[tg] == 6L) ||
    any(.g3_piece_value(k$child$ptype[tg]) >= val_p) ||
    any(!vapply(tg, function(q) .g3_defended(k$child, k$ctx, q), TRUE))
  list(fork = meaningful, n = length(tg), targets = tg, reply = .g3_capture_reply(scan, i, p))
}

# Own non-king, non-pawn pieces threatened in the position: with the
# opponent to act (a null move), some legal capture of the piece is a net
# gain. Returns piece indices with the threatening capturer.
#' @noRd
.g3_threatened <- function(scan, tabs) {
  core <- scan$core
  if (scan$in_check) return(list())
  c2 <- core
  c2$side <- 3L - core$side
  c2$ep <- integer(0L)
  ctx2 <- .g3_ctx(c2, tabs)
  l2 <- .g3_legal(c2, tabs, ctx2)
  out <- list()
  for (j in which(l2[, "cap"] > 0L)) {
    q <- l2[j, "cap"]
    if (core$ptype[q] %in% c(1L, 6L) || q %in% vapply(out, function(o) o$q, 0L)) next
    c_by <- l2[j, "piece"]
    gain <- .g3_piece_value(core$ptype[q]) > .g3_piece_value(core$ptype[c_by])
    if (!gain) {
      gc <- .g3_make(c2, l2[j, ], tabs)
      gain <- !any(.g3_legal(gc, tabs)[, "cap"] == c_by)
    }
    if (gain) out[[length(out) + 1L]] <- list(q = q, by = c_by)
  }
  out
}

# Theme detections for one scanned position.
#' @noRd
.g3_detect_themes <- function(scan, info, tabs) {
  n <- nrow(scan$legal)
  if (!n) return(list())
  core <- scan$core
  lg <- scan$legal
  side <- .G3_COLORS[scan$side]
  det <- list()
  add <- function(theme, criterion, params = list()) {
    det[[length(det) + 1L]] <<- list(theme = theme, criterion = criterion, params = params)
  }
  mates <- which(vapply(scan$kids, function(k) k$mate, TRUE))
  if (length(mates)) add("checkmate", "checkmate")
  if (scan$in_check) {
    add("escape_check", "escape_safe")
    plat <- which(lg[, "type"] == 2L)
    if (length(plat) && any(vapply(plat, function(i) is.na(.g3_mate_reply(scan, i, tabs)), TRUE))) {
      add("platform", "platform_escape")
    }
  } else {
    plat <- which(lg[, "type"] == 2L)
    if (length(plat) && any(vapply(scan$kids[plat], function(k) k$check, TRUE))) add("platform", "platform_check")
  }
  forks <- which(vapply(seq_len(n), function(i) {
    f <- .g3_fork_info(scan, i)
    isTRUE(f$fork) && is.na(f$reply)
  }, TRUE))
  if (length(forks)) add("fork", "fork")
  caps <- which(lg[, "cap"] > 0L)
  good_cap <- caps[vapply(caps, function(i) {
    v <- .g3_piece_value(core$ptype[lg[i, "cap"]])
    safe <- is.na(.g3_capture_reply(scan, i, lg[i, "piece"]))
    v >= 300L && (safe || v > .g3_piece_value(core$ptype[lg[i, "piece"]]))
  }, TRUE)]
  if (length(good_cap)) add("capture", "safe_capture")
  thr <- .g3_threatened(scan, tabs)
  if (length(thr)) {
    q <- thr[[1L]]$q
    keep <- vapply(seq_len(n), function(i) is.na(.g3_net_gain_reply(scan, i, q, tabs)), TRUE)
    if (any(keep) && !all(keep)) add("defend", "keep_piece", list(piece_id = core$pid[q]))
  }
  # Coordinate levels: a capture whose destination coordinate also exists
  # on another board that some legal move reaches.
  move_to <- ifelse(lg[, "type"] == 1L, lg[, "to"], 0L)
  for (i in caps) {
    if (lg[i, "type"] != 1L || lg[i, "flag"] == 2L) next
    to <- lg[i, "to"]
    twins <- setdiff(which(scan$ctx$ci == scan$ctx$ci[to]), to)
    v <- .g3_piece_value(core$ptype[lg[i, "cap"]])
    ok <- is.na(.g3_capture_reply(scan, i, lg[i, "piece"])) || v >= .g3_piece_value(core$ptype[lg[i, "piece"]])
    if (length(twins) && any(move_to %in% twins) && ok) {
      add("coordinate_levels", "capture_target", list(target_id = core$pid[lg[i, "cap"]]))
      break
    }
  }
  if (isTRUE(info$critical) && !is.na(info$best %||% NA_character_) && !identical(info$best, info$played)) {
    add("improve_decision", "improve_decision", list(played = info$played))
  }
  det
}

# Evaluate the criterion for legal action i. Returns meets (logical),
# tier ("solved", "acceptable", "incorrect", or "meets" for searched
# tiering), reason code and a reply id when a reply is the evidence.
#' @noRd
.g3_criterion_eval <- function(scan, i, objective, tabs) {
  lg <- scan$legal
  core <- scan$core
  k <- scan$kids[[i]]
  r <- function(meets, tier, reason, reply = NA_character_, ...) {
    list(meets = meets, tier = tier, reason = reason, reply = reply, extra = list(...))
  }
  p <- lg[i, "piece"]
  is_plat <- lg[i, "type"] == 2L
  switch(
    objective$criterion,
    checkmate = if (k$mate) r(TRUE, "solved", "checkmate") else
      if (k$check) r(FALSE, "incorrect", "check_not_mate", n = nrow(k$legal)) else
        r(FALSE, "incorrect", "no_mate", n = nrow(k$legal)),
    escape_safe = {
      mr <- .g3_mate_reply(scan, i, tabs)
      if (is.na(mr)) r(TRUE, "meets", "escape_safe") else r(FALSE, "incorrect", "mate_reply", mr)
    },
    platform_escape = {
      mr <- .g3_mate_reply(scan, i, tabs)
      if (!is.na(mr)) r(FALSE, "incorrect", "mate_reply", mr) else
        if (is_plat) r(TRUE, "solved", "platform_escape") else r(TRUE, "acceptable", "no_platform")
    },
    platform_check = if (k$check && is_plat) r(TRUE, "solved", "platform_check") else
      if (k$check) r(TRUE, "acceptable", "no_platform") else r(FALSE, "incorrect", "no_check"),
    safe_capture = {
      v <- lg[i, "cap"]
      if (v == 0L) {
        r(FALSE, "incorrect", "no_capture")
      } else {
        val <- .g3_piece_value(core$ptype[v])
        rep <- .g3_capture_reply(scan, i, p)
        best <- scan$cache$best_safe_value
        if (is.null(best)) {
          caps <- which(lg[, "cap"] > 0L)
          sv <- vapply(caps, function(j) {
            if (is.na(.g3_capture_reply(scan, j, lg[j, "piece"]))) .g3_piece_value(core$ptype[lg[j, "cap"]]) else 0L
          }, 0L)
          best <- if (length(sv)) max(sv) else 0L
          scan$cache$best_safe_value <- best
        }
        if (is.na(rep) && val >= best) r(TRUE, "solved", "safe_capture", value = val)
        else if (is.na(rep)) r(TRUE, "acceptable", "lesser_capture", value = val)
        else if (val > .g3_piece_value(core$ptype[p])) r(TRUE, "acceptable", "favourable_trade", rep, value = val)
        else r(FALSE, "incorrect", "recapture", rep)
      }
    },
    fork = {
      f <- .g3_fork_info(scan, i)
      if (!isTRUE(f$fork)) r(FALSE, "incorrect", "not_fork", n = f$n)
      else if (is.na(f$reply)) r(TRUE, "solved", "fork", n = f$n)
      else r(TRUE, "acceptable", "unsafe_fork", f$reply, n = f$n)
    },
    keep_piece = {
      q <- match(objective$params$piece_id, core$pid)
      rep <- .g3_net_gain_reply(scan, i, q, tabs)
      if (is.na(rep)) r(TRUE, "meets", "keeps_piece") else r(FALSE, "incorrect", "piece_lost", rep)
    },
    capture_target = {
      q <- match(objective$params$target_id, core$pid)
      tsq <- core$psq[q]
      if (lg[i, "cap"] == q) {
        r(TRUE, "solved", "captures_target")
      } else if (lg[i, "type"] == 1L && lg[i, "to"] != tsq && scan$ctx$ci[lg[i, "to"]] == scan$ctx$ci[tsq]) {
        r(FALSE, "incorrect", "twin_square")
      } else {
        r(FALSE, "incorrect", "not_target")
      }
    },
    improve_decision = r(NA, "meets", "searched"),
    .g3_abort("g3_invalid_state", paste0("Unknown exercise criterion: ", objective$criterion))
  )
}

# Build one exercise from a detection. Returns NULL if no action solves it.
#' @noRd
.g3_build_exercise <- function(scan, det, info, b, tolerance, tabs, provenance = NULL) {
  side <- .G3_COLORS[scan$side]
  objective <- list(theme = det$theme, criterion = det$criterion, side = side,
                    params = .g3_sort_keys(det$params))
  n <- nrow(scan$legal)
  ev <- lapply(seq_len(n), function(i) .g3_criterion_eval(scan, i, objective, tabs))
  tier <- vapply(ev, function(e) e$tier, "")
  basis <- rep("exact", n)
  score <- rep(NA_real_, n)
  grading <- list(basis = "exact", tolerance = tolerance, budget = b, depth = NA_integer_,
                  nodes = NA_real_, completeness = NA_character_, pv = character())
  method <- "exhaustive_criterion"
  if (det$criterion %in% .G3_SEARCH_TIERED) {
    if (det$criterion == "improve_decision") {
      cand <- info$review
      if (is.null(cand)) return(NULL)
      an_ids <- unique(c(info$best, info$played))
      method <- "searched_criterion"
    } else {
      an_ids <- scan$ids[tier == "meets"]
    }
    if (!length(an_ids)) return(NULL)
    if (b$nodes > 0 && length(an_ids) >= 1L) {
      an <- g3_analyse_position(scan$state, budget = b, actions = an_ids)
      grading$basis <- "searched"
      grading$depth <- an$depth
      grading$nodes <- an$cost$nodes
      grading$completeness <- an$completeness
      grading$pv <- an$pv
      sc <- an$alternatives$score[match(scan$ids, an$alternatives$action_id)]
      score <- sc
      top <- suppressWarnings(max(sc, na.rm = TRUE))
      if (det$criterion == "improve_decision") {
        if (!is.finite(top)) return(NULL)
        tier <- ifelse(!is.na(sc) & sc >= top - tolerance & scan$ids != info$played, "solved", "incorrect")
        basis[] <- "searched"
      } else if (is.finite(top)) {
        m <- tier == "meets"
        tier[m] <- ifelse(!is.na(sc[m]) & sc[m] >= top - tolerance, "solved", "acceptable")
        basis[m] <- "searched"
      } else {
        tier[tier == "meets"] <- "solved"
      }
    } else {
      if (det$criterion == "improve_decision") return(NULL)
      tier[tier == "meets"] <- "solved"
    }
  }
  if (!any(tier == "solved")) return(NULL)
  keep <- tier %in% c("solved", "acceptable")
  sol <- data.frame(
    action_id = scan$ids[keep], label = scan$labels[keep], grade = tier[keep], basis = basis[keep],
    reason = vapply(ev[keep], function(e) e$reason, ""), score = score[keep], stringsAsFactors = FALSE
  )
  rownames(sol) <- NULL
  # Primary solution (used by hints): best searched score, else the most
  # valuable capture, else the first in action-id order.
  solved <- which(sol$grade == "solved")
  primary <- solved[1L]
  if (any(!is.na(sol$score[solved]))) {
    primary <- solved[order(-ifelse(is.na(sol$score[solved]), -Inf, sol$score[solved]), solved)][1L]
  } else if (det$criterion == "safe_capture") {
    vals <- vapply(solved, function(j) ev[keep][[j]]$extra$value %||% 0, 0)
    primary <- solved[order(-vals, solved)][1L]
  }
  primary_id <- sol$action_id[primary]
  continuation <- .g3_exercise_continuation(scan, primary_id, grading, b, tabs)
  n_meets <- sum(vapply(ev, function(e) isTRUE(e$meets), TRUE))
  validation <- list(
    method = method, rules_hash = tabs$rules_hash, checked = .g3_now(), n_legal = n,
    n_solved = sum(sol$grade == "solved"), n_acceptable = sum(sol$grade == "acceptable"),
    n_incorrect = n - nrow(sol), discriminating = n > nrow(sol),
    unique_solution_proven = method == "exhaustive_criterion" && n_meets == 1L,
    grading_basis = grading$basis
  )
  status <- if (method == "exhaustive_criterion") "validated" else "candidate"
  validation$status_reason <- if (status == "validated") "criterion checked for every legal action" else
    "solutions rest on a bounded search; not a proven solution set"
  prov <- provenance %||% list(
    source = info$source %||% "position",
    label = if (identical(info$source, "game")) "generated from an analysed game" else "generated from a position",
    game_id = info$game_id %||% NA_character_, game_hash = info$game_hash %||% NA_character_,
    ply = info$ply %||% NA_integer_, played_action_id = info$played %||% NA_character_
  )
  prov$generator <- .G3_COACH_VERSION
  prov$engine <- .G3_SEARCH_VERSION
  prov$evaluator <- .G3_EVAL_VERSION
  prov$created <- .g3_now()
  st_hash <- scan$state_hash
  ex <- structure(
    list(
      schema = .G3_EXERCISE_SCHEMA,
      exercise_id = .g3_exercise_id(st_hash, objective),
      version = 1L,
      theme = det$theme,
      objective = objective,
      grading = grading,
      start_state = scan$state,
      start_state_hash = st_hash,
      position_key = scan$core$rep[length(scan$core$rep)],
      rules_version = tabs$rules_version,
      rules_hash = tabs$rules_hash,
      solutions = sol,
      primary_action_id = primary_id,
      continuation = continuation,
      status = status,
      validation = validation,
      provenance = prov
    ),
    class = "g3_exercise"
  )
  ex
}

#' @noRd
.g3_exercise_id <- function(state_hash, objective) {
  .g3_hash_object(list(schema = .G3_EXERCISE_SCHEMA, start_state_hash = state_hash,
                       objective = .g3_sort_keys(objective)))
}

# Short continuation after the primary solution: the bounded search line,
# replayed through the engine. Labelled as an analysed line, never forced.
#' @noRd
.g3_exercise_continuation <- function(scan, primary_id, grading, b, tabs) {
  empty <- list(actions = primary_id, notation = character(), verified = FALSE, kind = "analysed_line",
                depth = NA_integer_, budget = b, final_state_hash = NA_character_, ends_game = NA)
  i <- match(primary_id, scan$ids)
  ends <- length(scan$kids[[i]]$legal) == 0L || !nrow(scan$kids[[i]]$legal)
  pv <- primary_id
  depth <- NA_integer_
  if (!ends && b$nodes > 0) {
    if (length(grading$pv) && identical(grading$pv[1L], primary_id)) {
      pv <- grading$pv
      depth <- grading$depth
    } else {
      an <- g3_analyse_position(scan$state, budget = b, actions = primary_id)
      if (length(an$pv)) pv <- an$pv
      depth <- an$depth
    }
  }
  pv <- pv[seq_len(min(3L, length(pv)))]
  rl <- .g3_replay_line(scan$state, pv)
  if (!rl$ok) return(empty)
  list(actions = pv, notation = rl$notation, verified = TRUE, verified_with = "g3_apply_action",
       kind = if (length(pv) > 1L) "analysed_line" else "single_action", depth = depth, budget = b,
       final_state_hash = .g3_state_hash(rl$final),
       ends_game = identical(rl$final$outcome$status, "terminal") && length(pv) == 1L,
       outcome = rl$final$outcome)
}

# Structural and identity checks used before any exercise is shown or
# scored: rules hash (mismatch -> g3_unsupported_rules) and the start
# state hash (mismatch -> g3_stale_revision).
#' @noRd
.g3_check_exercise <- function(exercise) {
  if (!inherits(exercise, "g3_exercise") || !identical(exercise$schema, .G3_EXERCISE_SCHEMA)) {
    .g3_abort("g3_invalid_state", "Expected a `g3_exercise` (schema g3-exercise/1).")
  }
  tabs <- .g3_tables()
  if (!identical(exercise$rules_hash, tabs$rules_hash) || !identical(exercise$start_state$rules_hash, tabs$rules_hash)) {
    .g3_abort("g3_unsupported_rules", paste0(
      "Exercise ", substr(exercise$exercise_id, 1L, 12L), " was made under a different rules hash; ",
      "it cannot be used with the installed rules profile."
    ), rules_hash = exercise$rules_hash)
  }
  .g3_check_state(exercise$start_state)
  if (!identical(.g3_state_hash(exercise$start_state), exercise$start_state_hash)) {
    .g3_abort("g3_stale_revision", paste0(
      "Exercise ", substr(exercise$exercise_id, 1L, 12L), " does not match its recorded position ",
      "(state hash mismatch); it is stale or was edited."
    ))
  }
  if (!identical(.g3_exercise_id(exercise$start_state_hash, exercise$objective), exercise$exercise_id)) {
    .g3_abort("g3_invalid_state", "Exercise id does not match its position and objective.")
  }
  invisible(TRUE)
}

#' Validate an exercise against the installed rules
#'
#' Rechecks an exercise: rules hash, start-state hash, identity, and the
#' exact criterion for every legal action of the start position. The
#' recorded solution set must be reproduced exactly (grades of searched
#' tiers are not recomputed). An exercise that passes keeps its status;
#' a searched (`improve_decision`) exercise remains a candidate.
#'
#' @param exercise A `g3_exercise`.
#' @param error If `TRUE`, a failed check raises the typed condition
#'   (`g3_unsupported_rules`, `g3_stale_revision` or `g3_invalid_state`);
#'   if `FALSE`, a list is returned.
#' @return Invisibly `TRUE` when valid and `error = TRUE`; otherwise
#'   `list(valid, status, problems)`.
#' @examples
#' set <- gambitR3D::g3_exercise_catalogue(validate = FALSE)
#' gambitR3D::g3_validate_exercise(set$exercises[[1]], error = FALSE)$valid
#' @export
g3_validate_exercise <- function(exercise, error = TRUE) {
  res <- tryCatch({
    .g3_check_exercise(exercise)
    tabs <- .g3_tables()
    scan <- .g3_scan_position(exercise$start_state, tabs)
    p <- character()
    if (exercise$objective$criterion != "improve_decision") {
      ev <- lapply(seq_len(nrow(scan$legal)), function(i) .g3_criterion_eval(scan, i, exercise$objective, tabs))
      meets <- scan$ids[vapply(ev, function(e) isTRUE(e$meets), TRUE)]
      if (!setequal(meets, exercise$solutions$action_id)) {
        p <- c(p, "the accepted solution set differs from the engine's recheck")
      }
      exact <- exercise$solutions$basis == "exact"
      tiers <- vapply(ev, function(e) e$tier, "")[match(exercise$solutions$action_id[exact], scan$ids)]
      if (any(tiers != exercise$solutions$grade[exact], na.rm = TRUE)) p <- c(p, "an exact grade differs from the recheck")
      if (!any(exercise$solutions$grade == "solved")) p <- c(p, "no solution")
    } else {
      bad <- setdiff(exercise$solutions$action_id, scan$ids)
      if (length(bad)) p <- c(p, paste("solution no longer legal:", bad[1L]))
    }
    list(valid = !length(p), status = exercise$status, problems = p)
  }, g3_error = function(e) {
    if (error) stop(e)
    list(valid = FALSE, status = "invalid", problems = conditionMessage(e))
  })
  if (error) {
    if (!res$valid) .g3_abort("g3_invalid_state", paste0("Invalid exercise: ", paste(res$problems, collapse = "; ")))
    return(invisible(TRUE))
  }
  res
}

#' Objective of an exercise
#'
#' The task sentence shown to the learner, rendered from the template
#' catalogue with engine facts (board-qualified squares). It names what
#' must be achieved, never the solution.
#'
#' @param exercise A `g3_exercise`.
#' @param lang `"en"` (default) or `"de"`.
#' @return One character string.
#' @examples
#' ex <- gambitR3D::g3_exercise_catalogue(validate = FALSE)$exercises[[1]]
#' gambitR3D::g3_exercise_objective(ex)
#' gambitR3D::g3_exercise_objective(ex, lang = "de")
#' @export
g3_exercise_objective <- function(exercise, lang = "en") {
  .g3_check_exercise(exercise)
  .g3_objective_text(exercise, .g3_coach_lang(lang))
}

# Objective sentence of an exercise.
#' @noRd
.g3_objective_text <- function(exercise, lang = "en") {
  o <- exercise$objective
  st <- exercise$start_state
  tabs <- .g3_tables()
  side <- .g3_color_word(o$side, lang)
  opp <- .g3_color_word(.G3_COLORS[3L - match(o$side, .G3_COLORS)], lang)
  switch(
    o$criterion,
    checkmate = .g3_tr("objective.checkmate", lang, side = side),
    escape_safe = .g3_tr("objective.escape_check", lang, side = side),
    platform_escape = .g3_tr("objective.platform_escape", lang, side = side),
    platform_check = .g3_tr("objective.platform_check", lang, side = side, opp = opp),
    safe_capture = .g3_tr("objective.capture", lang, side = side),
    fork = .g3_tr("objective.fork", lang, side = side, opp = opp),
    keep_piece = {
      core <- .g3_core(st, tabs)
      ctx <- .g3_ctx(core, tabs)
      q <- match(o$params$piece_id, core$pid)
      .g3_tr("objective.defend", lang, side = side, piece = .g3_piece_word(.G3_TYPES[core$ptype[q]], lang),
             square = .g3_sq_name(core$psq[q], ctx, tabs))
    },
    capture_target = {
      core <- .g3_core(st, tabs)
      ctx <- .g3_ctx(core, tabs)
      q <- match(o$params$target_id, core$pid)
      .g3_tr("objective.coordinate_levels", lang, side = side, opp = opp,
             piece = .g3_piece_word(.G3_TYPES[core$ptype[q]], lang), coord = .g3_sq_coord(core$psq[q], ctx))
    },
    improve_decision = {
      core <- .g3_core(st, tabs)
      ctx <- .g3_ctx(core, tabs)
      legal <- .g3_legal(core, tabs, ctx)
      i <- .g3_find_action(core, legal, o$params$played, tabs)
      lab <- if (is.na(i)) o$params$played else .g3_action_labels(core, legal[i, , drop = FALSE], tabs, ctx)
      .g3_tr("objective.improve_decision", lang, side = side, played = lab,
             tolerance = exercise$grading$tolerance)
    }
  )
}

# ---- Serialisation -------------------------------------------------------

# Plain list of an exercise for JSON.
#' @noRd
.g3_exercise_plain <- function(ex) {
  x <- unclass(ex)
  x$start_state <- .g3_state_plain(ex$start_state)
  x$solutions <- .g3_rows(ex$solutions)
  x
}

# Rebuild an exercise from its parsed JSON form, with validation.
#' @noRd
.g3_exercise_from_list <- function(x) {
  if (!is.list(x) || !identical(x$schema, .G3_EXERCISE_SCHEMA)) {
    .g3_abort("g3_invalid_state", "Not a g3-exercise/1 record.")
  }
  tabs <- .g3_tables()
  if (!identical(x$rules_hash, tabs$rules_hash)) {
    .g3_abort("g3_unsupported_rules", paste0(
      "Exercise ", substr(format(x$exercise_id), 1L, 12L), " uses a different rules hash; it is rejected."
    ), rules_hash = x$rules_hash)
  }
  st <- .g3_state_from_list(x$start_state)
  sol <- x$solutions
  chr <- function(v) if (is.null(v)) NA_character_ else as.character(v)
  num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  solutions <- data.frame(
    action_id = vapply(sol, function(s) chr(s$action_id), ""),
    label = vapply(sol, function(s) chr(s$label), ""),
    grade = vapply(sol, function(s) chr(s$grade), ""),
    basis = vapply(sol, function(s) chr(s$basis), ""),
    reason = vapply(sol, function(s) chr(s$reason), ""),
    score = vapply(sol, function(s) num(s$score), 0),
    stringsAsFactors = FALSE
  )
  unl <- function(v) as.character(unlist(v))
  grading <- x$grading
  grading$pv <- unl(grading$pv)
  grading$budget <- .g3_budget(lapply(grading$budget, function(v) if (is.null(v)) Inf else v))
  cont <- x$continuation
  cont$actions <- unl(cont$actions)
  cont$notation <- unl(cont$notation)
  ex <- structure(
    list(
      schema = x$schema, exercise_id = chr(x$exercise_id), version = as.integer(x$version),
      theme = chr(x$theme),
      objective = list(theme = chr(x$objective$theme), criterion = chr(x$objective$criterion),
                       side = chr(x$objective$side), params = .g3_sort_keys(x$objective$params %||% list())),
      grading = grading, start_state = st, start_state_hash = chr(x$start_state_hash),
      position_key = chr(x$position_key), rules_version = chr(x$rules_version), rules_hash = chr(x$rules_hash),
      solutions = solutions, primary_action_id = chr(x$primary_action_id), continuation = cont,
      status = chr(x$status), validation = x$validation, provenance = x$provenance
    ),
    class = "g3_exercise"
  )
  if (length(ex$objective$params) == 0L) ex$objective$params <- .g3_sort_keys(list())
  .g3_check_exercise(ex)
  ex
}

#' Write and read exercises
#'
#' Exercises are stored as UTF-8 JSON (`g3-exercise-set/1`) with an atomic
#' write. Reading rebuilds every start state through the engine's state
#' validation, rejects exercises made under another rules hash
#' (`g3_unsupported_rules`) or whose position no longer matches its
#' recorded hash (`g3_stale_revision`), and rechecks each solution set.
#'
#' @param exercises A `g3_exercise_set` or a list of `g3_exercise`.
#' @param path File path.
#' @param overwrite Allow replacing an existing file.
#' @param validate Recheck each exercise's solutions with
#'   [g3_validate_exercise()] after reading (default `TRUE`).
#' @return `g3_write_exercises()` returns the path invisibly;
#'   `g3_read_exercises()` returns a `g3_exercise_set`.
#' @examples
#' set <- gambitR3D::g3_exercise_catalogue()
#' path <- base::tempfile(fileext = ".json")
#' gambitR3D::g3_write_exercises(set, path)
#' gambitR3D::g3_read_exercises(path)$summary[, c("theme", "status")]
#' @export
g3_write_exercises <- function(exercises, path, overwrite = FALSE) {
  exs <- if (inherits(exercises, "g3_exercise_set")) exercises$exercises else exercises
  if (inherits(exs, "g3_exercise")) exs <- list(exs)
  if (!is.list(exs) || !all(vapply(exs, inherits, TRUE, "g3_exercise"))) {
    .g3_abort("g3_invalid_state", "`exercises` must be a g3_exercise_set or a list of g3_exercise.")
  }
  if (file.exists(path) && !overwrite) .g3_abort("g3_io_error", paste0("File exists: ", path, " (use overwrite = TRUE)."))
  x <- list(schema = .G3_EXERCISE_SET_SCHEMA, rules_hash = .g3_tables()$rules_hash,
            exercises = lapply(exs, .g3_exercise_plain))
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
  .g3_atomic_write(as.character(txt), path)
}

#' @rdname g3_write_exercises
#' @export
g3_read_exercises <- function(path, validate = TRUE) {
  raw <- .g3_read_json_file(path)
  exs <- if (identical(raw$schema, .G3_EXERCISE_SET_SCHEMA)) raw$exercises else
    if (identical(raw$schema, .G3_EXERCISE_SCHEMA)) list(raw) else
      .g3_abort("g3_invalid_state", "Not a gambitR3D exercise file.")
  out <- lapply(exs, .g3_exercise_from_list)
  if (validate) for (e in out) g3_validate_exercise(e)
  .g3_exercise_set(out, list(source = basename(path), sha256 = digest::digest(file = path, algo = "sha256")))
}

#' Authored exercise catalogue
#'
#' Loads the small set of authored exercises shipped in
#' `inst/extdata/exercises/`. Each was built from an explicit position
#' definition with the rules engine, carries the provenance label
#' "authored, engine-verified" and the rules hash it was verified under,
#' and is rechecked on load.
#'
#' @param validate Recheck every exercise (default `TRUE`).
#' @return A `g3_exercise_set`.
#' @examples
#' set <- gambitR3D::g3_exercise_catalogue(validate = FALSE)
#' set$summary[, c("theme", "status", "n_solved")]
#' @export
g3_exercise_catalogue <- function(validate = TRUE) {
  dir <- system.file("extdata", "exercises", package = "gambitR3D")
  idx_path <- file.path(dir, "index.json")
  if (!nzchar(dir) || !file.exists(idx_path)) {
    .g3_abort("g3_io_error", "The authored exercise catalogue is missing from this installation.")
  }
  idx <- jsonlite::fromJSON(idx_path, simplifyVector = FALSE)
  if (!identical(idx$rules_hash, .g3_tables()$rules_hash)) {
    .g3_abort("g3_unsupported_rules", "The authored exercises were verified under a different rules hash.")
  }
  exs <- lapply(idx$files, function(f) {
    p <- file.path(dir, f$file)
    if (!identical(digest::digest(file = p, algo = "sha256"), f$sha256)) {
      .g3_abort("g3_io_error", paste0("Checksum mismatch for authored exercise file ", f$file, "."))
    }
    e <- .g3_exercise_from_list(.g3_read_json_file(p))
    if (!identical(e$exercise_id, f$exercise_id)) {
      .g3_abort("g3_invalid_state", paste0("Authored exercise file ", f$file, " does not match its index entry."))
    }
    if (validate) g3_validate_exercise(e)
    e
  })
  .g3_exercise_set(exs, list(source = "authored", label = "authored, engine-verified",
                             generator = idx$generator, rules_hash = idx$rules_hash))
}

# Build an authored exercise from a definition (pieces, docks, side,
# objective). Used to create and to re-verify the shipped catalogue.
#' @noRd
.g3_authored_exercise <- function(def, budget = list(nodes = 400L, depth = 2L), tolerance = 50) {
  pcs <- def$pieces
  pieces <- data.frame(
    id = vapply(pcs, function(p) p$id, ""), type = vapply(pcs, function(p) p$type, ""),
    color = vapply(pcs, function(p) p$color, ""), square_id = vapply(pcs, function(p) p$square_id, ""),
    has_moved = vapply(pcs, function(p) isTRUE(p$has_moved %||% TRUE), TRUE),
    stringsAsFactors = FALSE
  )
  docks <- if (length(def$docks)) unlist(def$docks) else NULL
  orient <- if (length(def$orientation)) unlist(def$orientation) else NULL
  castling <- if (length(def$castling)) unlist(def$castling) else NULL
  st <- g3_setup_position(pieces, docks = docks, orientation = orient, side = def$side, castling = castling)
  tabs <- .g3_tables()
  scan <- .g3_scan_position(st, tabs)
  det <- list(theme = def$theme, criterion = def$criterion, params = def$params %||% list())
  prov <- list(source = "authored", label = "authored, engine-verified", game_id = NA_character_,
               game_hash = NA_character_, ply = NA_integer_, played_action_id = NA_character_,
               definition_id = def$id, title = def$title, note = def$note %||% NA_character_)
  ex <- .g3_build_exercise(scan, det, list(source = "authored"), .g3_budget(budget), tolerance, tabs, provenance = prov)
  if (is.null(ex)) .g3_abort("g3_invalid_state", paste0("Authored exercise ", def$id, " has no solution."))
  ex
}

# Build the authored catalogue: one JSON file per exercise plus index.json
# holding each definition, its exercise id and the file checksum. Rerun
# after a rules change; the test suite rebuilds every definition and
# compares it with the shipped file.
#' @noRd
.g3_write_authored_catalogue <- function(defs, dir, budget = list(nodes = 2000L, depth = 3L), tolerance = 50) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  tabs <- .g3_tables()
  files <- lapply(defs, function(def) {
    ex <- .g3_authored_exercise(def, budget = budget, tolerance = tolerance)
    fn <- paste0(def$id, ".json")
    txt <- jsonlite::toJSON(.g3_exercise_plain(ex), auto_unbox = TRUE, pretty = TRUE, digits = NA,
                            null = "null", na = "null")
    .g3_atomic_write(as.character(txt), file.path(dir, fn))
    list(file = fn, exercise_id = ex$exercise_id, theme = ex$theme, status = ex$status,
         sha256 = digest::digest(file = file.path(dir, fn), algo = "sha256"), definition = def)
  })
  idx <- list(
    schema = "g3-exercise-catalogue/1", provenance = "authored, engine-verified",
    generator = .G3_COACH_VERSION, engine = .G3_SEARCH_VERSION, evaluator = .G3_EVAL_VERSION,
    rules_version = tabs$rules_version, rules_hash = tabs$rules_hash,
    build = list(budget = budget, tolerance = tolerance), files = files
  )
  txt <- jsonlite::toJSON(idx, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
  .g3_atomic_write(as.character(txt), file.path(dir, "index.json"))
  invisible(idx)
}

# Definitions from index.json in the form .g3_authored_exercise() takes.
#' @noRd
.g3_authored_definitions <- function() {
  dir <- system.file("extdata", "exercises", package = "gambitR3D")
  idx <- jsonlite::fromJSON(file.path(dir, "index.json"), simplifyVector = FALSE)
  lapply(idx$files, function(f) f$definition)
}
