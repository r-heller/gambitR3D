# Coaching facts: what the rules engine says about an action.
#
# Everything here is computed with the engine's own primitives
# (`.g3_legal()`, `.g3_make()`, `.g3_attacked()`, `.g3_in_check()`,
# `.g3_outcome_core()`); nothing re-implements a rule. Facts are plain
# lists of ids, names and numbers. Sentences are built later from the
# template catalogue (R/coach-text.R) and every sentence carries a claim
# type:
#
#   legal_fact           exact consequence of the rules for this action:
#                        moving piece, squares, capture, promotion, check,
#                        check escaped, checkmate, platform transport,
#                        attack sets under the engine's attack function
#   searched             a value or line from a bounded search, always with
#                        evaluator, budget, depth, completeness and bound
#   heuristic            an interpretation with a stated definition, e.g.
#                        "attacked and not defended"
#   learning_suggestion  what to practise next
#
# Squares are always named with their board identity, `N:c3`, because the
# same coordinate can exist on two or three boards. The physical square id
# (`N:1:0`) travels alongside in the evidence.

.G3_COACH_VERSION <- "g3-coach/1"
.G3_CLAIM_TYPES <- c("legal_fact", "searched", "heuristic", "learning_suggestion")

# Board-qualified display name of square index `s`, e.g. "N:c3".
#' @noRd
.g3_sq_name <- function(s, ctx, tabs) {
  paste0(tabs$board_ids[tabs$sq_board[s]], ":", .g3_coord_label(ctx$file[s], ctx$rank[s]))
}

#' @noRd
.g3_sq_coord <- function(s, ctx) .g3_coord_label(ctx$file[s], ctx$rank[s])

# Does piece `p` alone attack projected coordinate (f, r)? Blockers are
# all occupants (the full occupancy count), attackers only `p`.
#' @noRd
.g3_piece_attacks <- function(core, ctx, p, f, r) {
  psq <- integer(length(core$psq))
  psq[p] <- core$psq[p]
  .g3_attacked(f, r, core$pcolor[p], ctx$file, ctx$rank, ctx$cnt, psq, core$ptype, core$pcolor)
}

# Logical[64]: is each physical square's coordinate attacked by colour `by`
# (engine attack semantics; squares holding `by`'s own pieces included)?
#' @noRd
.g3_attack_map <- function(core, ctx, by) {
  ucis <- unique(ctx$ci)
  hit <- vapply(ucis, function(ci) {
    .g3_attacked((ci - 1L) %/% 10L, (ci - 1L) %% 10L, by, ctx$file, ctx$rank, ctx$cnt,
                 core$psq, core$ptype, core$pcolor)
  }, TRUE)
  hit[match(ctx$ci, ucis)]
}

# Pieces of colour `by` that attack the coordinate of square `s`.
#' @noRd
.g3_attackers_of <- function(core, ctx, s, by) {
  cand <- which(core$pcolor == by & core$psq > 0L)
  cand[vapply(cand, function(q) .g3_piece_attacks(core, ctx, q, ctx$file[s], ctx$rank[s]), TRUE)]
}

# Enemy pieces attacked by piece `p` (indices).
#' @noRd
.g3_targets_of <- function(core, ctx, p) {
  if (core$psq[p] == 0L) return(integer())
  enemy <- which(core$pcolor == 3L - core$pcolor[p] & core$psq > 0L)
  enemy[vapply(enemy, function(q) {
    s <- core$psq[q]
    .g3_piece_attacks(core, ctx, p, ctx$file[s], ctx$rank[s])
  }, TRUE)]
}

# Is the piece on square `s` defended, i.e. is its coordinate attacked by
# its own colour? A piece never attacks its own coordinate.
#' @noRd
.g3_defended <- function(core, ctx, q) {
  s <- core$psq[q]
  .g3_attacked(ctx$file[s], ctx$rank[s], core$pcolor[q], ctx$file, ctx$rank, ctx$cnt,
               core$psq, core$ptype, core$pcolor)
}

# Squares next to the king of `color`: physical squares at projected
# Chebyshev distance 1 (a square at the king's own coordinate on another
# board is not adjacent under the profile).
#' @noRd
.g3_king_zone <- function(core, ctx, color) {
  ks <- .g3_king_sq(core, color)
  if (is.na(ks)) return(integer())
  d <- pmax(abs(ctx$file - ctx$file[ks]), abs(ctx$rank - ctx$rank[ks]))
  which(d == 1L)
}

# Material by colour with the provisional piece values of the evaluation.
#' @noRd
.g3_material <- function(core) {
  alive <- core$psq > 0L
  c(sum(.G3_PIECE_VALUE[core$ptype[alive & core$pcolor == 1L]]),
    sum(.G3_PIECE_VALUE[core$ptype[alive & core$pcolor == 2L]]))
}

#' @noRd
.g3_piece_value <- function(type) .G3_PIECE_VALUE[type]

# Index of the legal action with id `id` in matrix `legal`, or NA.
#' @noRd
.g3_find_action <- function(core, legal, id, tabs) {
  match(id, .g3_action_ids(core, legal, tabs))
}

# Facts about one legal action row `a` of `core`. `child` may be supplied
# when already computed. Everything is exact engine output.
#' @noRd
.g3_action_facts <- function(core, ctx, a, tabs, child = NULL) {
  a <- a[.G3_ACOLS]
  side <- core$side
  opp <- 3L - side
  p <- a[["piece"]]
  m1 <- matrix(a, nrow = 1L, dimnames = list(NULL, .G3_ACOLS))
  if (is.null(child)) child <- .g3_make(core, a, tabs)
  cctx <- .g3_ctx(child, tabs)
  clegal <- .g3_legal(child, tabs, cctx)
  outcome <- .g3_outcome_core(child, tabs, cctx, clegal)
  type <- c("move", "platform", "castle")[a[["type"]]]
  f <- list(
    action_id = .g3_action_ids(core, m1, tabs),
    label = .g3_action_labels(core, m1, tabs, ctx),
    type = type,
    side = .G3_COLORS[side],
    opponent = .G3_COLORS[opp],
    piece_index = p,
    piece_id = core$pid[p],
    piece = .G3_TYPES[core$ptype[p]],
    from_sq = a[["from"]],
    from_square = tabs$sq_id[a[["from"]]],
    from_name = .g3_sq_name(a[["from"]], ctx, tabs),
    outcome = outcome,
    n_replies = nrow(clegal)
  )
  # Destination: for moves the target square; for a platform action the
  # carried piece's (unchanged) physical square at its new coordinate.
  if (type == "platform") {
    f$to_sq <- a[["from"]]
    f$to_square <- tabs$sq_id[a[["from"]]]
    f$to_name <- .g3_sq_name(a[["from"]], cctx, tabs)
  } else {
    f$to_sq <- a[["to"]]
    f$to_square <- tabs$sq_id[a[["to"]]]
    f$to_name <- .g3_sq_name(a[["to"]], cctx, tabs)
  }
  f$to_board <- tabs$board_ids[tabs$sq_board[f$to_sq]]
  f$to_coord <- .g3_sq_coord(f$to_sq, cctx)
  # Other physical squares sharing the destination coordinate.
  tw <- setdiff(which(cctx$ci == cctx$ci[f$to_sq]), f$to_sq)
  f$twins <- tabs$sq_id[tw]
  f$twin_names <- vapply(tw, .g3_sq_name, "", ctx = cctx, tabs = tabs)
  # Capture.
  f$capture <- a[["cap"]] > 0L
  if (f$capture) {
    v <- a[["cap"]]
    f$victim_id <- core$pid[v]
    f$victim <- .G3_TYPES[core$ptype[v]]
    f$victim_square <- tabs$sq_id[core$psq[v]]
    f$victim_name <- .g3_sq_name(core$psq[v], ctx, tabs)
    f$victim_value <- .g3_piece_value(core$ptype[v])
  }
  f$en_passant <- a[["flag"]] == 2L
  f$promotion <- if (a[["promo"]] > 0L) .G3_TYPES[a[["promo"]]] else NA_character_
  # Platform transport details.
  if (type == "platform") {
    pl <- a[["plat"]]
    f$platform <- .G3_PLATFORMS[pl]
    f$from_dock <- tabs$dock_ids[core$dock[pl]]
    f$to_dock <- tabs$dock_ids[a[["dock"]]]
    f$rotation <- as.integer(a[["rot"]])
    f$orient_before <- as.integer(core$orient[pl])
    f$orient_after <- as.integer(child$orient[pl])
    f$coord_before <- .g3_sq_coord(a[["from"]], ctx)
    f$coord_after <- .g3_sq_coord(a[["from"]], cctx)
    f$pawn_double_lost <- core$ptype[p] == 1L && !core$pmoved[p]
  }
  if (type == "castle") {
    def <- tabs$castle[[side]][[a[["cside"]]]]
    f$castle_side <- c("kingside", "queenside")[a[["cside"]]]
    f$king_from <- .g3_sq_name(def$king_from, ctx, tabs)
    f$king_to <- .g3_sq_name(def$king_to, cctx, tabs)
    f$rook_from <- .g3_sq_name(def$rook_from, ctx, tabs)
    f$rook_to <- .g3_sq_name(def$rook_to, cctx, tabs)
  }
  # Castling rights given up by this action.
  lost <- which(core$castle & !child$castle)
  f$rights_lost <- .G3_CASTLE_NAMES[lost]
  # Check before (escape) and after (given).
  f$was_in_check <- .g3_in_check(core, side, ctx, tabs)
  if (f$was_in_check) {
    ks <- .g3_king_sq(core, side)
    chk <- .g3_attackers_of(core, ctx, ks, opp)
    f$checkers <- core$pid[chk]
    f$checker_names <- vapply(chk, function(q) .g3_sq_name(core$psq[q], ctx, tabs), "")
    f$checker_types <- .G3_TYPES[core$ptype[chk]]
    f$escape <- if (type == "platform" && core$ptype[p] == 6L) {
      "platform_king"
    } else if (type == "platform") {
      "platform_block"
    } else if (core$ptype[p] == 6L) {
      "king_moves"
    } else if (f$capture && a[["cap"]] %in% chk) {
      "capture_checker"
    } else {
      "block"
    }
  }
  f$gives_check <- .g3_in_check(child, opp, cctx, tabs)
  oks <- .g3_king_sq(child, opp)
  f$enemy_king_name <- if (is.na(oks)) NA_character_ else .g3_sq_name(oks, cctx, tabs)
  # Attack facts for the moved piece after the action (engine attack
  # function, physical targets).
  tg <- .g3_targets_of(child, cctx, p)
  f$targets <- child$pid[tg]
  f$target_types <- .G3_TYPES[child$ptype[tg]]
  f$target_names <- vapply(tg, function(q) .g3_sq_name(child$psq[q], cctx, tabs), "")
  before_tg <- .g3_targets_of(core, ctx, p)
  f$new_targets <- setdiff(f$targets, core$pid[before_tg])
  # Attacked-square counts (physical squares whose coordinate is attacked).
  f$attacked_before <- c(sum(.g3_attack_map(core, ctx, side)), sum(.g3_attack_map(core, ctx, opp)))
  f$attacked_after <- c(sum(.g3_attack_map(child, cctx, side)), sum(.g3_attack_map(child, cctx, opp)))
  # King-zone indicators (heuristic interpretation of exact attack facts).
  kz <- function(cr, cx, color, by) {
    z <- .g3_king_zone(cr, cx, color)
    if (!length(z)) return(NA_integer_)
    am <- .g3_attack_map(cr, cx, by)
    sum(am[z])
  }
  f$own_king_zone <- c(before = kz(core, ctx, side, opp), after = kz(child, cctx, side, opp))
  f$enemy_king_zone <- c(before = kz(core, ctx, opp, side), after = kz(child, cctx, opp, side))
  ks_after <- .g3_king_sq(child, side)
  f$own_king_name <- if (is.na(ks_after)) NA_character_ else .g3_sq_name(ks_after, cctx, tabs)
  # Own pieces left attacked and undefended after the action (heuristic).
  own <- which(child$pcolor == side & child$psq > 0L & child$ptype != 6L)
  hang <- own[vapply(own, function(q) {
    s <- child$psq[q]
    .g3_attacked(cctx$file[s], cctx$rank[s], opp, cctx$file, cctx$rank, cctx$cnt,
                 child$psq, child$ptype, child$pcolor) && !.g3_defended(child, cctx, q)
  }, TRUE)]
  f$hanging <- child$pid[hang]
  f$hanging_types <- .G3_TYPES[child$ptype[hang]]
  f$hanging_names <- vapply(hang, function(q) .g3_sq_name(child$psq[q], cctx, tabs), "")
  f$material_before <- .g3_material(core)
  f$material_after <- .g3_material(child)
  f
}

# Exhaustive mate proof (AND-OR search over every legal reply). Starting
# with action row `a` of the side to act, is checkmate forced within
# `max_plies` plies (counting `a`)? Returns status "proven", "refuted" or
# "unresolved" (transition limit reached), the number of replies to `a`
# and the transitions examined. Independent of alpha-beta and quiescence.
#' @noRd
.g3_prove_mate <- function(core, a, max_plies, tabs, limit = 20000L) {
  env <- new.env(parent = emptyenv())
  env$n <- 0
  env$limit <- limit
  env$over <- FALSE
  child <- .g3_make(core, a, tabs)
  cctx <- .g3_ctx(child, tabs)
  env$replies <- nrow(.g3_legal(child, tabs, cctx))
  res <- .g3_mate_and(env, child, max_plies - 1L, tabs)
  list(
    status = if (env$over) "unresolved" else if (res) "proven" else "refuted",
    max_plies = as.integer(max_plies), replies = env$replies, transitions = env$n,
    method = "exhaustive_and_or"
  )
}

# Defender to act: TRUE if every legal reply leads to mate within n plies.
#' @noRd
.g3_mate_and <- function(env, core, n, tabs) {
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  if (!nrow(legal)) return(.g3_in_check(core, core$side, ctx, tabs))
  if (.g3_rule_draw(core) || n <= 0L) return(FALSE)
  for (i in seq_len(nrow(legal))) {
    env$n <- env$n + 1
    if (env$n > env$limit) {
      env$over <- TRUE
      return(FALSE)
    }
    if (!.g3_mate_or(env, .g3_make(core, legal[i, ], tabs), n - 1L, tabs)) return(FALSE)
  }
  TRUE
}

# Attacker to act: TRUE if some legal action leads to mate within n plies.
#' @noRd
.g3_mate_or <- function(env, core, n, tabs) {
  if (n <= 0L) return(FALSE)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  if (!nrow(legal) || .g3_rule_draw(core)) return(FALSE)
  for (i in seq_len(nrow(legal))) {
    env$n <- env$n + 1
    if (env$n > env$limit) {
      env$over <- TRUE
      return(FALSE)
    }
    child <- .g3_make(core, legal[i, ], tabs)
    # With one ply left only a checking action can mate.
    if (n == 1L && !.g3_in_check(child, child$side, .g3_ctx(child, tabs), tabs)) next
    if (.g3_mate_and(env, child, n - 1L, tabs)) return(TRUE)
  }
  FALSE
}

# Replay a line of action ids from `state` through the public rules API.
# Returns ok, the board-qualified notation of each applied action, the
# state hashes after each action, the final state and, on failure, the
# index and message of the first illegal action.
#' @noRd
.g3_replay_line <- function(state, ids) {
  st <- state
  notation <- character()
  hashes <- character()
  for (k in seq_along(ids)) {
    step <- tryCatch(g3_apply_action(st, ids[[k]]), g3_error = function(e) e)
    if (inherits(step, "error")) {
      return(list(ok = FALSE, notation = notation, hashes = hashes, final = st,
                  failed_at = k, message = conditionMessage(step)))
    }
    notation <- c(notation, step$event$notation)
    hashes <- c(hashes, step$event$post_state_hash)
    st <- step$state
  }
  list(ok = TRUE, notation = notation, hashes = hashes, final = st, failed_at = NA_integer_,
       message = NA_character_)
}

# Diagnose why an action id is not legal, for feedback that is kept
# separate from strategic mistakes. Returns list(code, params).
#' @noRd
.g3_diagnose_illegal <- function(core, ctx, id, tabs) {
  sq_pat <- "(W|N|B|WQ|WK|BQ|BK):[0-3]:[0-3]"
  side <- core$side
  out <- function(code, ...) list(code = code, params = list(...))
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    return(out("malformed", input = format(id)[1L]))
  }
  pseudo <- .g3_pseudo(core, ctx, tabs)
  pseudo_ids <- .g3_action_ids(core, pseudo, tabs)
  side_word <- .G3_COLORS[side]
  if (grepl(paste0("^m:", sq_pat, "-", sq_pat, "(=[QRBN])?$"), id)) {
    parts <- regmatches(id, regexec(paste0("^m:(", sq_pat, ")-(", sq_pat, ")(=[QRBN])?$"), id))[[1L]]
    from <- match(parts[2L], tabs$sq_id)
    to <- match(parts[4L], tabs$sq_id)
    if (is.na(from) || is.na(to)) return(out("malformed", input = id))
    p <- core$occ[from]
    if (p == 0L) return(out("no_piece", square = .g3_sq_name(from, ctx, tabs)))
    if (core$pcolor[p] != side) {
      return(out("wrong_side", square = .g3_sq_name(from, ctx, tabs), owner = .G3_COLORS[core$pcolor[p]],
                 side = side_word))
    }
    if (id %in% pseudo_ids) return(out("self_check"))
    base <- sub("=[QRBN]$", "", id)
    if (any(startsWith(pseudo_ids, paste0(base, "=")))) return(out("promotion_required"))
    if (base %in% pseudo_ids) return(out("no_promotion"))
    return(out("not_reachable", piece = .G3_TYPES[core$ptype[p]], from = .g3_sq_name(from, ctx, tabs),
               to = .g3_sq_name(to, ctx, tabs)))
  }
  if (grepl("^t:(WQ|WK|BQ|BK)>[WNB]:[LR]:(lo|hi)@(0|180)(=[QRBN])?$", id)) {
    parts <- regmatches(id, regexec("^t:(WQ|WK|BQ|BK)>([WNB]:[LR]:(lo|hi))@(0|180)", id))[[1L]]
    pl <- match(parts[2L], .G3_PLATFORMS)
    dock <- match(parts[3L], tabs$dock_ids)
    sqs <- tabs$plat_squares[[pl]]
    occp <- core$occ[sqs][core$occ[sqs] > 0L]
    if (length(occp) != 1L) return(out("platform_occupancy", platform = parts[2L], n = length(occp)))
    if (core$pcolor[occp] != side) return(out("platform_wrong_side", platform = parts[2L], side = side_word))
    if (!dock %in% tabs$dock_adj[[core$dock[pl]]]) {
      return(out("dock_not_adjacent", platform = parts[2L], dock = parts[3L],
                 from_dock = tabs$dock_ids[core$dock[pl]]))
    }
    if (dock %in% core$dock[-pl]) return(out("dock_occupied", dock = parts[3L]))
    if (id %in% pseudo_ids) return(out("self_check"))
    base <- sub("=[QRBN]$", "", id)
    if (any(startsWith(pseudo_ids, paste0(base, "=")))) return(out("promotion_required"))
    return(out("not_legal", input = id))
  }
  if (grepl("^c:(white|black):(kingside|queenside)$", id)) {
    col <- sub("^c:(white|black):.*$", "\\1", id)
    if (col != side_word) return(out("castle_wrong_side", side = side_word))
    return(out("castle_unavailable"))
  }
  out("malformed", input = id)
}
