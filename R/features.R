# Engine-derived position features and tactical labels.
#
# Every feature here is computed from the engine's own primitives: the
# replay supplies the core, its context and the side to act's legal action
# matrix; attack maps call `.g3_attacked()` (the same geometric threat test
# the engine uses for check), never a second movement interpreter and never
# legal-move lists. Display settings (Neutral height, camera, 2D/3D) are not
# inputs to any function in this file.
#
# Attack semantics, as in the engine: a projected coordinate (file, rank) is
# attacked by a colour when one of its pieces could capture there
# geometrically, with any occupant on any board blocking intermediate ray
# coordinates. Attack status therefore belongs to a coordinate; every
# physical square currently projected onto an attacked coordinate counts as
# attacked. Per-piece attacks reuse `.g3_attacked()` with the other pieces of
# the attacking colour lifted from `psq` while the occupancy counts (the
# blockers) stay unchanged.
#
# Mobility distinguishes actions from distinct destinations: promotion
# choices and platform orientations inflate action counts. The side to act
# uses its legal actions (none in a finished position); the side not to act
# uses "null-move legal" actions (side flipped, en-passant entitlement
# cleared, legality by the engine), a counterfactual labelled as such.
#
# Tactical labels carry a claim strength:
#   exact           check, checkmate, capture, pin (king-safety
#                   counterfactual under the engine's attack semantics)
#   bounded_search  forced mates found by a completed full-width search
#                   iteration within the stated budget
#   heuristic       fork, hanging piece, consequential platform action

.G3_FEATURE_SCHEMA <- "g3-features/1"
.G3_FEAT_LEVELS <- c("W", "N", "B")

# Packed coordinates (file * 10 + rank + 1) attacked by colour `by`.
#' @noRd
.g3_feat_attack_map <- function(core, ctx, by) {
  out <- logical(60L)
  for (f in 0:5) {
    for (r in 0:9) {
      out[f * 10L + r + 1L] <- .g3_attacked(f, r, by, ctx$file, ctx$rank, ctx$cnt, core$psq,
                                            core$ptype, core$pcolor)
    }
  }
  out
}

# Does piece `p` alone attack coordinate (f, r)? Other pieces of its colour
# are lifted from `psq` but still block rays through `cnt`.
#' @noRd
.g3_feat_piece_attacks <- function(core, ctx, p, f, r, cnt = ctx$cnt) {
  psq <- core$psq
  col <- core$pcolor[p]
  psq[core$pcolor == col & seq_along(psq) != p] <- 0L
  .g3_attacked(f, r, col, ctx$file, ctx$rank, cnt, psq, core$ptype, core$pcolor)
}

# Parent level (W, N, B) of every physical square for the current docks.
#' @noRd
.g3_feat_levels <- function(core, tabs) {
  b <- tabs$sq_board
  out <- b
  plat <- b >= 4L
  out[plat] <- tabs$dock_parent[core$dock[b[plat] - 3L]]
  .G3_FEAT_LEVELS[out]
}

# Destination keys of an action matrix: square for piece moves and castling,
# platform/dock/rotation for platform actions (orientation decides where the
# carried piece ends up).
#' @noRd
.g3_feat_dest_keys <- function(m) {
  ifelse(m[, "type"] == 2L, paste0("t", m[, "plat"], ".", m[, "dock"], ".", m[, "rot"]),
         paste0("s", m[, "to"]))
}

# Mobility rows (long format) for one action matrix.
#' @noRd
.g3_feat_mobility <- function(core, m, color, basis, tabs) {
  if (!nrow(m)) {
    return(NULL)
  }
  ptype <- .G3_TYPES[core$ptype[m[, "piece"]]]
  board <- tabs$board_ids[tabs$sq_board[m[, "from"]]]
  dest <- .g3_feat_dest_keys(m)
  grp <- paste(ptype, board, sep = "|")
  u <- unique(grp)
  list(
    color = rep(.G3_COLORS[color], length(u)),
    basis = rep(basis, length(u)),
    piece_type = sub("\\|.*$", "", u),
    board = sub("^.*\\|", "", u),
    n_actions = vapply(u, function(g) sum(grp == g), 0L, USE.NAMES = FALSE),
    n_destinations = vapply(u, function(g) length(unique(dest[grp == g])), 0L, USE.NAMES = FALSE)
  )
}

# All features of one position.
#
# `legal` is the side to act's legal action matrix (zero rows when the
# position is finished). Returns the wide `positions` row, long rows for
# mobility, platforms, pieces and squares, and piece-level sets used by the
# tactical labels.
#' @noRd
.g3_feat_position <- function(core, ctx, legal, terminal, tabs) {
  side <- core$side
  alive <- core$psq > 0L
  sq_level <- .g3_feat_levels(core, tabs)
  coord_exists <- logical(60L)
  coord_exists[unique(ctx$ci)] <- TRUE

  # Attack maps for both colours (coordinates).
  att <- list(.g3_feat_attack_map(core, ctx, 1L), .g3_feat_attack_map(core, ctx, 2L))

  # Mobility: legal for the side to act, null-move legal for the other.
  other <- 3L - side
  c2 <- core
  c2$side <- other
  c2$ep <- integer(0L)
  legal_other <- .g3_legal(c2, tabs, ctx)
  mats <- vector("list", 2L)
  mats[[side]] <- legal
  mats[[other]] <- legal_other
  basis <- character(2L)
  basis[side] <- if (terminal) "legal_finished" else "legal"
  basis[other] <- "null_move_legal"

  # Piece-level threats.
  n <- length(core$psq)
  attacked <- logical(n)
  defended <- logical(n)
  attacked[alive] <- vapply(which(alive), function(p) att[[3L - core$pcolor[p]]][ctx$ci[core$psq[p]]], TRUE)
  defended[alive] <- vapply(which(alive), function(p) att[[core$pcolor[p]]][ctx$ci[core$psq[p]]], TRUE)
  hanging <- alive & attacked & !defended & core$ptype != 6L

  # Fork targets: enemy non-pawn pieces (king included) attacked by one
  # piece. A target whose coordinate the attacking colour does not attack at
  # all cannot be attacked by any single piece, so only attacked targets are
  # tested piece by piece.
  hits <- vector("list", n)
  for (e in which(alive & core$ptype != 1L)) {
    ce <- core$pcolor[e]
    s <- core$psq[e]
    if (!att[[3L - ce]][ctx$ci[s]]) next
    for (p in which(alive & core$pcolor == 3L - ce)) {
      if (.g3_feat_piece_attacks(core, ctx, p, ctx$file[s], ctx$rank[s])) hits[[p]] <- c(hits[[p]], e)
    }
  }
  n_fork <- lengths(hits)
  fork_targets <- vapply(hits, function(h) paste(core$pid[h], collapse = ","), "")

  # Pins: removing a piece makes its king attacked by an enemy line piece
  # that does not attack the king with the piece in place. When the king is
  # not attacked, a colour-level test with the piece removed screens the
  # candidates; the pinning pieces are then identified one by one.
  pinned_by <- character(n)
  for (col in 1:2) {
    ks <- .g3_king_sq(core, col)
    if (is.na(ks)) next
    kf <- ctx$file[ks]
    kr <- ctx$rank[ks]
    sliders <- which(alive & core$pcolor == 3L - col & core$ptype %in% 3:5)
    if (!length(sliders)) next
    king_attacked <- att[[3L - col]][ctx$ci[ks]]
    direct <- NULL
    for (x in which(alive & core$pcolor == col & core$ptype != 6L)) {
      cnt <- ctx$cnt
      cx <- ctx$ci[core$psq[x]]
      cnt[cx] <- cnt[cx] - 1L
      cx_core <- core
      cx_core$psq[x] <- 0L
      if (!king_attacked && !.g3_attacked(kf, kr, 3L - col, ctx$file, ctx$rank, cnt, cx_core$psq,
                                          core$ptype, core$pcolor)) next
      if (is.null(direct)) direct <- vapply(sliders, function(s) .g3_feat_piece_attacks(core, ctx, s, kf, kr), TRUE)
      after <- vapply(sliders, function(s) .g3_feat_piece_attacks(cx_core, ctx, s, kf, kr, cnt), TRUE)
      pinners <- sliders[after & !direct]
      if (length(pinners)) pinned_by[x] <- paste(core$pid[pinners], collapse = ",")
    }
  }

  # King escape options and the attacked king zone.
  king_actions <- integer(2L)
  king_dest <- integer(2L)
  king_zone <- integer(2L)
  in_check <- logical(2L)
  for (col in 1:2) {
    m <- mats[[col]]
    kp <- which(core$ptype == 6L & core$pcolor == col & alive)
    ks <- .g3_king_sq(core, col)
    if (!is.na(ks)) {
      in_check[col] <- att[[3L - col]][ctx$ci[ks]]
      zone <- 0L
      for (df in -1:1) {
        for (dr in -1:1) {
          if (df == 0L && dr == 0L) next
          f <- ctx$file[ks] + df
          r <- ctx$rank[ks] + dr
          if (!.g3_in_grid(f, r)) next
          ci <- f * 10L + r + 1L
          if (coord_exists[ci] && att[[3L - col]][ci]) zone <- zone + 1L
        }
      }
      king_zone[col] <- zone
    }
    if (length(kp) && nrow(m)) {
      km <- m[m[, "piece"] %in% kp, , drop = FALSE]
      king_actions[col] <- nrow(km)
      king_dest[col] <- length(unique(.g3_feat_dest_keys(km)))
    }
  }

  # Material and piece counts.
  vals <- .G3_PIECE_VALUE
  counts <- matrix(0L, 2L, 6L)
  for (col in 1:2) for (t in 1:6) counts[col, t] <- sum(alive & core$pcolor == col & core$ptype == t)
  material <- c(sum(vals[core$ptype[alive & core$pcolor == 1L]]), sum(vals[core$ptype[alive & core$pcolor == 2L]]))

  # Pawn progress in ranks from the colour's starting rank (white 1, black 8).
  pw <- which(alive & core$ptype == 1L & core$pcolor == 1L)
  pb <- which(alive & core$ptype == 1L & core$pcolor == 2L)
  prog_w <- pmax(ctx$rank[core$psq[pw]] - 1L, 0L)
  prog_b <- pmax(8L - ctx$rank[core$psq[pb]], 0L)

  # Platforms: occupancy, controller (exactly one occupant), free docks.
  plat_rows <- list(
    platform_id = .G3_PLATFORMS, dock = tabs$dock_ids[core$dock],
    level = .G3_FEAT_LEVELS[tabs$dock_parent[core$dock]], orientation = as.integer(core$orient),
    n_occupants = integer(4L), controller = character(4L), occupant_ids = character(4L),
    free_adjacent_docks = integer(4L), legal_actions_side = integer(4L)
  )
  plat_ctrl <- integer(2L)
  plat_free <- integer(2L)
  for (pl in 1:4) {
    sqs <- tabs$plat_squares[[pl]]
    occp <- core$occ[sqs][core$occ[sqs] > 0L]
    free <- length(setdiff(tabs$dock_adj[[core$dock[pl]]], core$dock[-pl]))
    plat_rows$n_occupants[pl] <- length(occp)
    plat_rows$occupant_ids[pl] <- paste(core$pid[occp], collapse = ",")
    plat_rows$free_adjacent_docks[pl] <- free
    plat_rows$controller[pl] <- if (length(occp) == 1L) .G3_COLORS[core$pcolor[occp]] else if (!length(occp)) "none" else "multiple"
    if (length(occp) == 1L) {
      plat_ctrl[core$pcolor[occp]] <- plat_ctrl[core$pcolor[occp]] + 1L
      plat_free[core$pcolor[occp]] <- plat_free[core$pcolor[occp]] + free
    }
    if (nrow(legal)) plat_rows$legal_actions_side[pl] <- sum(legal[, "type"] == 2L & legal[, "plat"] == pl)
  }

  # Attacked physical squares versus attacked coordinates.
  att_sq <- c(sum(att[[1L]][ctx$ci]), sum(att[[2L]][ctx$ci]))
  att_co <- c(sum(att[[1L]] & coord_exists), sum(att[[2L]] & coord_exists))

  key <- core$rep[length(core$rep)]
  row <- list(
    side = .G3_COLORS[side],
    position_key = key,
    halfmove = as.integer(core$halfmove),
    repetition_count = sum(core$rep == key),
    repetition_record_length = length(core$rep),
    in_check = in_check[side],
    white_material = as.integer(material[1L]), black_material = as.integer(material[2L]),
    material_balance_white = as.integer(material[1L] - material[2L])
  )
  for (col in 1:2) {
    for (t in 1:6) row[[paste0(.G3_COLORS[col], "_n_", .G3_TYPES[t])]] <- counts[col, t]
  }
  for (col in 1:2) {
    cn <- .G3_COLORS[col]
    m <- mats[[col]]
    row[[paste0(cn, "_mobility_basis")]] <- basis[col]
    row[[paste0(cn, "_actions")]] <- nrow(m)
    row[[paste0(cn, "_destinations")]] <- length(unique(.g3_feat_dest_keys(m)))
    row[[paste0(cn, "_platform_actions")]] <- sum(m[, "type"] == 2L)
    row[[paste0(cn, "_king_actions")]] <- king_actions[col]
    row[[paste0(cn, "_king_destinations")]] <- king_dest[col]
    row[[paste0(cn, "_king_zone_attacked")]] <- king_zone[col]
    row[[paste0(cn, "_in_check")]] <- in_check[col]
    row[[paste0(cn, "_pawn_progress")]] <- as.integer(sum(if (col == 1L) prog_w else prog_b))
    mx <- if (col == 1L) prog_w else prog_b
    row[[paste0(cn, "_pawn_max_progress")]] <- if (length(mx)) as.integer(max(mx)) else NA_integer_
    row[[paste0(cn, "_attacked_squares")]] <- as.integer(att_sq[col])
    row[[paste0(cn, "_attacked_coordinates")]] <- as.integer(att_co[col])
    row[[paste0(cn, "_hanging")]] <- sum(hanging & core$pcolor == col)
    row[[paste0(cn, "_pinned")]] <- sum(nzchar(pinned_by) & core$pcolor == col)
    row[[paste0(cn, "_forking_pieces")]] <- sum(n_fork >= 2L & core$pcolor == col & alive)
    row[[paste0(cn, "_platforms_controlled")]] <- plat_ctrl[col]
    row[[paste0(cn, "_platform_free_docks")]] <- plat_free[col]
  }

  # Long tables.
  mob <- list(
    .g3_feat_mobility(core, mats[[1L]], 1L, basis[1L], tabs),
    .g3_feat_mobility(core, mats[[2L]], 2L, basis[2L], tabs)
  )
  ap <- which(alive)
  pieces <- list(
    piece_id = core$pid[ap], type = .G3_TYPES[core$ptype[ap]], origin_type = .G3_TYPES[core$porigin[ap]],
    color = .G3_COLORS[core$pcolor[ap]], square_id = tabs$sq_id[core$psq[ap]],
    board = tabs$board_ids[tabs$sq_board[core$psq[ap]]], level = sq_level[core$psq[ap]],
    u = tabs$sq_u[core$psq[ap]], v = tabs$sq_v[core$psq[ap]],
    file = as.integer(ctx$file[core$psq[ap]]), rank = as.integer(ctx$rank[core$psq[ap]]),
    has_moved = core$pmoved[ap], attacked = attacked[ap], defended = defended[ap],
    hanging = hanging[ap], pinned_by = ifelse(nzchar(pinned_by[ap]), pinned_by[ap], NA_character_),
    fork_targets = ifelse(n_fork[ap] >= 2L, fork_targets[ap], NA_character_),
    n_attacked_non_pawns = n_fork[ap]
  )
  occ <- core$occ
  has <- occ > 0L
  squares <- list(
    square_id = tabs$sq_id, board = tabs$board_ids[tabs$sq_board], level = sq_level,
    u = tabs$sq_u, v = tabs$sq_v, file = as.integer(ctx$file), rank = as.integer(ctx$rank),
    occupant_id = ifelse(has, core$pid[pmax(occ, 1L)], NA_character_),
    occupant_type = ifelse(has, .G3_TYPES[core$ptype[pmax(occ, 1L)]], NA_character_),
    occupant_color = ifelse(has, .G3_COLORS[core$pcolor[pmax(occ, 1L)]], NA_character_),
    attacked_by_white = att[[1L]][ctx$ci], attacked_by_black = att[[2L]][ctx$ci]
  )
  # Sets used by tactical labels (piece ids).
  sets <- list(
    hanging = lapply(1:2, function(col) core$pid[hanging & core$pcolor == col]),
    attacked_enemy = lapply(1:2, function(col) core$pid[attacked & alive & core$pcolor == 3L - col]),
    pins = lapply(1:2, function(col) {
      x <- which(nzchar(pinned_by) & core$pcolor == col)
      paste0(core$pid[x], "<", pinned_by[x])
    }),
    fork_targets = stats::setNames(fork_targets, core$pid),
    n_fork = stats::setNames(n_fork, core$pid)
  )
  list(row = row, mobility = mob, platforms = plat_rows, pieces = pieces, squares = squares, sets = sets)
}

# Tactical labels for the action leading from position `k` to `k + 1`.
# `mv` is the move row (list), `fk`/`fk1` the feature sets before/after,
# `ev`/`ev1` the evaluation rows before/after (or NULL).
#' @noRd
.g3_feat_tactics <- function(mv, fk, fk1, ev, ev1, budget_text) {
  out <- list()
  add <- function(label, strength, evidence, pieces) {
    out[[length(out) + 1L]] <<- list(
      label = label, claim_strength = strength, definition_id = label,
      evidence = evidence, pieces = pieces
    )
  }
  actor <- match(mv$actor, .G3_COLORS)
  enemy <- 3L - actor
  if (isTRUE(mv$checkmate)) {
    add("checkmate", "exact", "The action leaves the opponent in check with no legal action.", mv$piece_id)
  } else if (isTRUE(mv$check)) {
    add("check", "exact", "The opponent's king is attacked after the action.", mv$piece_id)
  }
  if (isTRUE(mv$capture)) {
    add("capture", "exact", paste0("Captures ", mv$captured_piece_id, " (", mv$captured_type, ")."),
        paste(mv$piece_id, mv$captured_piece_id, sep = ","))
  }
  # Pins created by the action (on the opponent's pieces).
  new_pins <- setdiff(fk1$sets$pins[[enemy]], fk$sets$pins[[enemy]])
  if (length(new_pins)) {
    add("pin", "exact",
        paste0("Removing the pinned piece would expose its king: ", paste(sub("<", " pinned by ", new_pins), collapse = "; "), "."),
        paste(sub("<.*$", "", new_pins), collapse = ","))
  }
  # Fork by the moving piece after the action.
  if (!is.na(mv$piece_id) && mv$piece_id %in% names(fk1$sets$n_fork) && fk1$sets$n_fork[[mv$piece_id]] >= 2L) {
    add("fork", "heuristic",
        paste0(mv$piece_id, " attacks ", fk1$sets$n_fork[[mv$piece_id]], " enemy non-pawn pieces: ",
               fk1$sets$fork_targets[[mv$piece_id]], "."),
        paste(mv$piece_id, fk1$sets$fork_targets[[mv$piece_id]], sep = ","))
  }
  # Pieces of the actor newly left attacked and undefended.
  newly <- setdiff(fk1$sets$hanging[[actor]], fk$sets$hanging[[actor]])
  moved_hanging <- !is.na(mv$piece_id) && mv$piece_id %in% fk1$sets$hanging[[actor]]
  hang <- unique(c(newly, if (moved_hanging) mv$piece_id))
  if (length(hang)) {
    add("hanging_piece", "heuristic",
        paste0("After the action, attacked and undefended: ", paste(hang, collapse = ","), "."),
        paste(hang, collapse = ","))
  }
  # Platform actions with a demonstrable tactical change.
  if (identical(mv$engine_type, "platform")) {
    why <- character()
    if (isTRUE(mv$check)) why <- c(why, "gives check")
    if (isTRUE(mv$in_check_before)) why <- c(why, "answers a check")
    if (!is.na(mv$promotion)) why <- c(why, "promotes")
    gained <- setdiff(fk1$sets$attacked_enemy[[actor]], fk$sets$attacked_enemy[[actor]])
    lost <- setdiff(fk$sets$attacked_enemy[[actor]], fk1$sets$attacked_enemy[[actor]])
    if (length(gained) || length(lost)) {
      why <- c(why, paste0("changes attacked enemy pieces (",
                           paste(c(if (length(gained)) paste0("+", gained), if (length(lost)) paste0("-", lost)), collapse = " "),
                           ")"))
    }
    if (length(why)) add("consequential_platform_action", "heuristic", paste0(paste(why, collapse = "; "), "."), mv$piece_id)
  }
  # Bounded-search labels from completed full-width iterations.
  if (!is.null(ev) && identical(ev$best_kind, "mate") && isTRUE(ev$best_score > 0)) {
    if (!(identical(ev$played_kind, "mate") && isTRUE(ev$played_score > 0))) {
      add("missed_forced_mate", "bounded_search",
          paste0("A forced mate in ", ev$best_mate_in, " plies was found (", budget_text, ", depth ", ev$depth,
                 ") but the played action does not keep a forced mate within that search."),
          mv$piece_id)
    }
  }
  if (!is.null(ev) && identical(ev$played_kind, "mate") && isTRUE(ev$played_score > 0) && !isTRUE(mv$checkmate)) {
    add("forced_mate_line", "bounded_search",
        paste0("The action keeps a forced mate in ", ev$played_mate_in, " plies against every reply searched (",
               budget_text, ", depth ", ev$depth, ")."),
        mv$piece_id)
  }
  if (!is.null(ev1) && identical(ev1$score_kind, "mate") && isTRUE(ev1$raw_score > 0)) {
    add("allows_forced_mate", "bounded_search",
        paste0("After the action the opponent has a forced mate in ", ev1$mate_in_mover, " plies within the search (",
               budget_text, ", depth ", ev1$depth, ")."),
        mv$piece_id)
  }
  out
}

# Definitions shared by the feature dictionary and reports.
#' @noRd
.g3_feat_tactic_defs <- function() {
  data.frame(
    label = c("check", "checkmate", "capture", "pin", "fork", "hanging_piece",
              "consequential_platform_action", "forced_mate_line", "missed_forced_mate", "allows_forced_mate"),
    claim_strength = c("exact", "exact", "exact", "exact", "heuristic", "heuristic", "heuristic",
                       "bounded_search", "bounded_search", "bounded_search"),
    definition = c(
      "After the action the opponent's king is attacked (engine attack semantics); excludes checkmate, which is labelled separately.",
      "After the action the opponent is in check and has no legal action (engine terminal rule).",
      "The action removes an enemy piece from its physical square (including en passant).",
      "After the action an enemy piece X is pinned: X's king is not attacked by a bishop, rook or queen S with X in place, but would be attacked by S if X were removed from its physical square (king-safety counterfactual with the engine's cross-board blockers). Only pins that did not exist before the action are listed.",
      "After the action the moving piece alone attacks at least two enemy non-pawn pieces (king included) under the engine's attack semantics. Attacking is not proof of winning material.",
      "After the action at least one of the actor's non-king pieces is attacked by the opponent and not defended by the actor (engine attack semantics), and was not already hanging before the action (or is the moved piece). Exchanges, checks and platform moves can change the consequence.",
      "A platform action that gives check, answers a check, promotes, or changes the set of enemy pieces attacked by the actor. Platform actions without such a change are not labelled.",
      "The played action keeps a mate score: within the completed full-width search iteration (with quiescence) at the stated budget, every reply searched leads to mate. Beyond that horizon nothing is claimed.",
      "The search found a forced mate for the side to act within its completed iteration, but the played action's score in the same root search is not a forced mate.",
      "After the action the next position's completed search iteration finds a forced mate for the opponent within the stated budget."
    ),
    stringsAsFactors = FALSE
  )
}

#' Feature dictionary
#'
#' Documents every column of the tables returned by [g3_analyse_game()]
#' (`moves`, `positions`, `evaluations`, `tactics`, `mobility`, `platforms`,
#' `pieces`, `squares`, `summary`) and every tactical label, with its unit,
#' denominator, missing-value policy and definition. Features are derived
#' from the rules engine (legal actions and the engine's geometric attack
#' test), never from rendered geometry; display settings never enter them.
#'
#' @return A data frame with columns `table`, `name`, `type`, `unit`,
#'   `denominator`, `missing_policy`, `claim_strength` (for tactical labels
#'   and other claims: `"exact"`, `"bounded_search"`, `"heuristic"`, or
#'   `"descriptive"`) and `definition`.
#' @examples
#' # Look up how mobility and attack counts are defined.
#' dict <- gambitR3D::g3_feature_dictionary()
#' dict[dict$table == "positions" & base::grepl("attacked", dict$name), c("name", "unit", "definition")]
#' @export
g3_feature_dictionary <- function() {
  d <- .g3_feat_dictionary()
  tac <- .g3_feat_tactic_defs()
  tac_rows <- data.frame(
    table = "tactics", name = paste0("label:", tac$label), type = "label", unit = "event",
    denominator = "actions in the game (one label per qualifying action)",
    missing_policy = "absent label means not detected under this definition, not proven absent",
    claim_strength = tac$claim_strength, definition = tac$definition, stringsAsFactors = FALSE
  )
  out <- rbind(d, tac_rows)
  rownames(out) <- NULL
  out
}

#' @noRd
.g3_feat_dictionary <- function() {
  rows <- list()
  add <- function(table, name, type, unit, denominator, missing, claim, definition) {
    rows[[length(rows) + 1L]] <<- data.frame(
      table = table, name = name, type = type, unit = unit, denominator = denominator,
      missing_policy = missing, claim_strength = claim, definition = definition, stringsAsFactors = FALSE
    )
  }
  never <- "never missing"
  # Shared identifiers.
  for (tb in c("moves", "positions", "evaluations", "tactics", "events", "mobility", "platforms", "pieces", "squares", "summary")) {
    add(tb, "game_id", "character", "identifier", "none", never, "exact", "Persistent game identity from the game's metadata.")
  }
  for (tb in c("positions", "evaluations", "mobility", "platforms", "pieces", "squares")) {
    add(tb, "ply", "integer", "plies", "none", never, "exact", "Board ply of the position (0 is the game's initial position); the position before action ply + 1.")
  }
  # moves
  mv <- list(
    c("ply", "integer", "plies", "Board ply after the action (1 for the first action)."),
    c("move_number", "integer", "moves", "Full-move number, ceiling(ply / 2) counted from the game's initial ply."),
    c("actor", "character", "colour", "Side that played the action."),
    c("action_id", "character", "identifier", "Canonical action id (schema g3-action/1)."),
    c("label", "character", "text", "Readable action label with board-qualified squares."),
    c("notation", "character", "text", "Label with + for check and # for checkmate."),
    c("engine_type", "character", "category", "Engine action type: move, platform or castle."),
    c("action_type", "character", "category", "piece_move, promotion, en_passant, castling, platform_relocation (orientation kept) or platform_rotation (relocated and turned by 180 degrees; rotation without relocation is not legal)."),
    c("piece_id", "character", "identifier", "Moving piece (the carried piece for platform actions, the king for castling)."),
    c("piece_type", "character", "category", "Type of the moving piece before the action."),
    c("from_square", "character", "physical square", "Physical square of the moving piece before the action."),
    c("to_square", "character", "physical square", "Destination square for piece moves and castling; NA for platform actions (the piece keeps its physical square)."),
    c("from_board", "character", "board", "Board holding the moving piece before the action."),
    c("to_board", "character", "board", "Board holding the moving piece after the action."),
    c("from_level", "character", "level", "Main level (W, N, B) of the piece before the action; a platform square belongs to the level of its current dock."),
    c("to_level", "character", "level", "Main level (W, N, B) of the piece after the action."),
    c("from_file", "integer", "file index 0-5", "Projected file before the action."),
    c("from_rank", "integer", "rank 0-9", "Projected rank before the action."),
    c("to_file", "integer", "file index 0-5", "Projected file of the moving piece after the action (changes under transport)."),
    c("to_rank", "integer", "rank 0-9", "Projected rank of the moving piece after the action."),
    c("platform_id", "character", "platform", "Platform moved by a platform action, else NA."),
    c("from_dock", "character", "dock", "Dock of the platform before a platform action, else NA."),
    c("to_dock", "character", "dock", "Destination dock of a platform action, else NA."),
    c("rotation_delta", "integer", "degrees", "0 or 180 for platform actions, else NA."),
    c("promotion", "character", "piece type", "Promotion piece (piece moves or platform transport), else NA."),
    c("castle_side", "character", "category", "kingside or queenside for castling, else NA."),
    c("en_passant", "logical", "flag", "Action is an en-passant capture."),
    c("capture", "logical", "flag", "Action captures a piece."),
    c("captured_piece_id", "character", "identifier", "Captured piece, else NA."),
    c("captured_type", "character", "piece type", "Type of the captured piece, else NA."),
    c("check", "logical", "flag", "The opponent's king is attacked after the action (includes checkmate)."),
    c("checkmate", "logical", "flag", "The action ends the game by checkmate."),
    c("in_check_before", "logical", "flag", "The actor was in check before the action."),
    c("platform_action", "logical", "flag", "Action is a platform relocation/rotation."),
    c("platform_square", "logical", "flag", "Moving piece starts or ends on a platform square (includes transport)."),
    c("board_transfer", "logical", "flag", "A piece move whose source and destination are on different physical boards."),
    c("level_transfer", "logical", "flag", "The moving piece's level (W, N, B) changed, by its own move or by transport."),
    c("transfer_mode", "character", "category", "'piece' for a piece move between boards, 'transport' for any platform action, NA otherwise."),
    c("pre_position_key", "character", "identifier", "Repetition key before the action."),
    c("position_key", "character", "identifier", "Repetition key after the action (verified against the game log)."),
    c("outcome_after", "character", "category", "ongoing, or the rule reason ending the game after the action.")
  )
  for (x in mv) add("moves", x[1], x[2], x[3], "one row per action of the active line", if (grepl("else NA|NA for", x[4])) "NA when not applicable" else never, "exact", x[4])

  # positions
  pos <- list(
    c("side", "character", "colour", "Side to act."),
    c("status", "character", "category", "ongoing or terminal (rule outcome)."),
    c("result", "character", "category", "Rule result of a terminal position (white, black, draw), else NA."),
    c("reason", "character", "category", "Rule reason of a terminal position, else NA."),
    c("position_key", "character", "identifier", "Engine repetition key (g3_position_key)."),
    c("analysis_context_hash", "character", "identifier", "Hash of the position with its full repetition record and halfmove counter, rules hash, evaluator identity/version/model revision, feature and action schema, budget and numeric precision. Evaluation cache key."),
    c("recorded_state_hash", "character", "identifier", "Full-state checksum recorded in the game log (computed for ply 0)."),
    c("halfmove", "integer", "plies", "Rule counter towards the 100-ply draw."),
    c("repetition_count", "integer", "occurrences", "Occurrences of the current key in the repetition record (3 ends the game)."),
    c("repetition_record_length", "integer", "positions", "Positions since the last irreversible action, including this one."),
    c("in_check", "logical", "flag", "Side to act is in check."),
    c("white_material", "integer", "provisional piece values", "Sum of White piece values P100 N300 B300 R500 Q900 (ordinary chess values used as provisional, unmeasured weights; not centipawns)."),
    c("black_material", "integer", "provisional piece values", "Sum of Black piece values (as white_material)."),
    c("material_balance_white", "integer", "provisional piece values", "white_material - black_material.")
  )
  for (x in pos) add("positions", x[1], x[2], x[3], "one row per position (plies 0..n)", if (grepl("else NA", x[4])) "NA when not applicable" else never, "exact", x[4])
  for (cn in .G3_COLORS) {
    cap <- if (cn == "white") "White" else "Black"
    for (t in .G3_TYPES) add("positions", paste0(cn, "_n_", t), "integer", "pieces", "one row per position", never, "exact", paste0("Number of ", cap, " pieces of type ", t, "."))
    pc <- list(
      c("mobility_basis", "character", "category", "legal (side to act), legal_finished (side to act in a finished position: no legal action) or null_move_legal (side not to act: its legal actions if it were to act, en passant cleared; a counterfactual)."),
      c("actions", "integer", "actions", "Number of actions on the stated mobility basis; promotion choices and platform orientations count separately."),
      c("destinations", "integer", "destinations", "Distinct destinations of those actions: physical squares for piece moves and castling, platform-dock-orientation triples for platform actions."),
      c("platform_actions", "integer", "actions", "Platform actions on the stated mobility basis."),
      c("king_actions", "integer", "actions", "King escape options: actions moving the king (king moves, castling, platform actions carrying the king) on the stated basis."),
      c("king_destinations", "integer", "destinations", "Distinct destinations of the king actions."),
      c("king_zone_attacked", "integer", "coordinates", "Coordinates at Chebyshev distance 1 from the king that have at least one physical square and are attacked by the opponent (engine attack semantics; threats, not legal moves)."),
      c("in_check", "logical", "flag", "This side's king is attacked."),
      c("pawn_progress", "integer", "ranks", "Sum over pawns of ranks advanced from the starting rank (White rank 1, Black rank 8)."),
      c("pawn_max_progress", "integer", "ranks", "Largest pawn progress; NA without pawns."),
      c("attacked_squares", "integer", "physical squares", "Physical squares whose current projected coordinate this side attacks (engine attack semantics; overlapping squares on different boards each count)."),
      c("attacked_coordinates", "integer", "coordinates", "Distinct projected coordinates with at least one physical square that this side attacks."),
      c("hanging", "integer", "pieces", "This side's non-king pieces attacked by the opponent and not defended by this side."),
      c("pinned", "integer", "pieces", "This side's pieces pinned under the king-safety counterfactual (see tactics label pin)."),
      c("forking_pieces", "integer", "pieces", "This side's pieces that alone attack at least two enemy non-pawn pieces."),
      c("platforms_controlled", "integer", "platforms", "Platforms whose single occupant belongs to this side."),
      c("platform_free_docks", "integer", "docks", "Sum of free adjacent docks over the platforms this side controls (legal transport options before king safety).")
    )
    for (x in pc) add("positions", paste0(cn, "_", x[1]), x[2], x[3], "one row per position", if (grepl("NA without", x[4])) "NA without pawns" else never, "exact", paste0(cap, ": ", x[4]))
  }
  # evaluations
  ev <- list(
    c("side", "character", "colour", "Side to act (the mover for this position).", never),
    c("evaluator", "character", "identifier", "Evaluator id: search, none or a model.", never),
    c("evaluator_version", "character", "identifier", "Search and evaluation version, or the model revision.", "NA for evaluator none"),
    c("model_revision", "character", "identifier", "Model revision/hash for model evaluators.", "NA for search and none"),
    c("scale_id", "character", "identifier", "Score scale identity: evaluator, version and unit. Rows with different scale ids are never joined into one series.", "NA when no score"),
    c("score_unit", "character", "category", "heuristic_units for the search evaluator (not centipawns), or the model's declared unit.", "NA when no score"),
    c("raw_score", "double", "evaluator units", "Evaluator output from the side to act's perspective; mates are encoded as +/-(1e6 - plies).", "NA with missing_reason"),
    c("score_kind", "character", "category", "heuristic, mate (forced mate within the completed search) or terminal (rule outcome).", "NA with missing_reason"),
    c("raw_perspective", "character", "category", "Perspective of raw_score: side_to_act.", never),
    c("mate_in_mover", "integer", "plies", "Signed mate distance for the mover (positive: the mover mates).", "NA unless score_kind is mate"),
    c("mate_in_white", "integer", "plies", "Signed mate distance from White's perspective.", "NA unless score_kind is mate"),
    c("score_white", "double", "heuristic units", "Heuristic score from White's perspective (raw_score, negated when Black acts).", "NA unless score_kind is heuristic"),
    c("score_mover", "double", "heuristic units", "Heuristic score from the mover's perspective (equals raw_score).", "NA unless score_kind is heuristic"),
    c("expected_score_white", "double", "expected score 0-1", "For heuristic scores: documented logistic display map 1 / (1 + exp(-score / 400)), uncalibrated and not a probability. Mates map to 1/0, terminal positions to their exact result (1, 0.5, 0). Model WDL: p_win + 0.5 p_draw.", "NA with missing_reason"),
    c("expected_score_mover", "double", "expected score 0-1", "As expected_score_white, from the mover's perspective (1 - White's value when Black acts).", "NA with missing_reason"),
    c("expected_score_kind", "character", "category", "heuristic_logistic_map, mate_within_search, terminal_exact or model_wdl.", "NA with missing_reason"),
    c("calibration", "character", "category", "uncalibrated (heuristic map or unvalidated model) or exact (terminal).", "NA with missing_reason"),
    c("depth", "integer", "plies", "Completed full-width depth of the root search (0: no iteration completed).", never),
    c("completeness", "character", "category", "complete, partial, none, terminal or not_evaluated.", never),
    c("nodes", "double", "nodes", "Positions visited by the root search (per-position budget).", "0 when not evaluated"),
    c("elapsed_ms", "double", "milliseconds", "Wall time of the evaluation (from the cache entry when cached).", "0 when not evaluated"),
    c("cache_hit", "logical", "flag", "Evaluation reused from the cache under an identical analysis context.", never),
    c("analysis_context_hash", "character", "identifier", "Cache key; see positions.analysis_context_hash.", never),
    c("best_action_id", "character", "identifier", "Best root action in the completed iteration.", "NA when no completed iteration or finished"),
    c("best_score", "double", "evaluator units", "Raw score of the best action (side to act).", "NA when missing"),
    c("best_kind", "character", "category", "Score kind of the best action.", "NA when missing"),
    c("best_mate_in", "integer", "plies", "Mate distance of the best action when it is a mate score.", "NA unless mate"),
    c("played_action_id", "character", "identifier", "Action actually played from this position.", "NA for the final position"),
    c("played_score", "double", "evaluator units", "Raw score of the played action in the same root search (identical settings).", "NA when missing or final"),
    c("played_kind", "character", "category", "Score kind of the played action.", "NA when missing or final"),
    c("played_mate_in", "integer", "plies", "Mate distance of the played action when it is a mate score.", "NA unless mate"),
    c("played_rank", "integer", "rank", "Rank of the played action among the root alternatives (1 best).", "NA when missing or final"),
    c("n_alternatives", "integer", "actions", "Root actions searched.", never),
    c("move_loss_raw", "double", "heuristic units", "best_score - played_score from the same completed root iteration (matched settings). Stored unclipped; it is non-negative by construction because both scores come from one search.", "NA when either score is missing or a mate score"),
    c("move_loss_sequential", "double", "heuristic units", "Mover's score before the action minus the mover's score after it (negated root score of the next position). Can be negative: finite-search horizon effects are an uncertainty, not hidden.", "NA when either side is missing or not heuristic"),
    c("move_loss_display_clipped", "double", "heuristic units", "max(move_loss_raw, 0), a display field only.", "NA when move_loss_raw is NA"),
    c("expected_loss_mover", "double", "expected score 0-1", "Expected score of the best action minus that of the played action (mover's perspective, same map as expected_score). Defined for mate scores too.", "NA when missing or final"),
    c("missing_reason", "character", "category", "Why the score is missing: not_requested, no_completed_iteration_within_budget, evaluator_unavailable: <message>, or NA when present.", "NA when the score is present")
  )
  for (x in ev) add("evaluations", x[1], x[2], x[3], "one row per position (plies 0..n)", x[5], if (x[1] %in% c("expected_score_white", "expected_score_mover")) "heuristic" else "exact", x[4])
  # tactics
  tac <- list(
    c("ply", "integer", "plies", "Ply of the labelled action."),
    c("actor", "character", "colour", "Side that played the action."),
    c("action_id", "character", "identifier", "Labelled action."),
    c("label", "character", "category", "Tactical label (see the label rows)."),
    c("claim_strength", "character", "category", "exact, bounded_search or heuristic."),
    c("definition_id", "character", "identifier", "Key of the definition in this dictionary."),
    c("evidence", "character", "text", "Engine evidence supporting the label."),
    c("pieces", "character", "identifiers", "Pieces involved, comma-separated.")
  )
  for (x in tac) add("tactics", x[1], x[2], x[3], "one row per label", never, "exact", x[4])
  # events (session events of the game log)
  for (x in list(
    c("sequence", "integer", "events", "Position of the event in the game's event log."),
    c("type", "character", "category", "resign, draw_agreed, truncated (a computational cutoff, never a rule draw) or takeback."),
    c("actor", "character", "category", "white, black or system."),
    c("ply", "integer", "plies", "Board ply at which the event was recorded."),
    c("reason", "character", "text", "Free-text reason recorded with the event (shown escaped in reports, never executed); NA when absent.")
  )) add("events", x[1], x[2], x[3], "one row per session event", if (x[1] == "reason") "NA when absent" else never, "exact", x[4])
  # mobility, platforms, pieces, squares
  for (x in list(
    c("color", "character", "colour", "Side whose actions are counted."),
    c("basis", "character", "category", "legal, legal_finished or null_move_legal (see positions.*_mobility_basis)."),
    c("piece_type", "character", "piece type", "Type of the moving piece (castling counts for K)."),
    c("board", "character", "board", "Board of the moving piece's square before the action."),
    c("n_actions", "integer", "actions", "Actions in this group."),
    c("n_destinations", "integer", "destinations", "Distinct destinations in this group.")
  )) add("mobility", x[1], x[2], x[3], "groups with at least one action; absent groups have zero actions", never, "exact", x[4])
  for (x in list(
    c("platform_id", "character", "platform", "Platform identity (WQ, WK, BQ, BK)."),
    c("dock", "character", "dock", "Current dock."),
    c("level", "character", "level", "Parent level of the dock."),
    c("orientation", "integer", "degrees", "0 or 180."),
    c("n_occupants", "integer", "pieces", "Pieces on the platform's four squares."),
    c("controller", "character", "category", "Colour of the single occupant; none when empty; multiple when more than one piece (no platform action is then legal)."),
    c("occupant_ids", "character", "identifiers", "Occupying pieces, comma-separated."),
    c("free_adjacent_docks", "integer", "docks", "Adjacent docks not occupied by another platform (legal free docks, not total graph degree)."),
    c("legal_actions_side", "integer", "actions", "Legal platform actions of this platform for the side to act.")
  )) add("platforms", x[1], x[2], x[3], "four rows per position", never, "exact", x[4])
  for (x in list(
    c("piece_id", "character", "identifier", "Piece identity."), c("type", "character", "piece type", "Current type."),
    c("origin_type", "character", "piece type", "Type at the start (P for promoted pawns)."), c("color", "character", "colour", "Piece colour."),
    c("square_id", "character", "physical square", "Physical square."), c("board", "character", "board", "Board of the square."),
    c("level", "character", "level", "Main level of the square (platforms: level of the current dock)."),
    c("u", "integer", "local index", "Local column in the board's own frame."), c("v", "integer", "local index", "Local row in the board's own frame."),
    c("file", "integer", "file index 0-5", "Projected file."), c("rank", "integer", "rank 0-9", "Projected rank."),
    c("has_moved", "logical", "flag", "Engine movement flag."),
    c("attacked", "logical", "flag", "The piece's coordinate is attacked by the opponent."),
    c("defended", "logical", "flag", "The piece's coordinate is attacked by its own side."),
    c("hanging", "logical", "flag", "Attacked, not defended, not a king."),
    c("pinned_by", "character", "identifiers", "Pinning pieces under the king-safety counterfactual, else NA."),
    c("fork_targets", "character", "identifiers", "Enemy non-pawn pieces attacked by this piece when at least two, else NA."),
    c("n_attacked_non_pawns", "integer", "pieces", "Enemy non-pawn pieces this piece alone attacks.")
  )) add("pieces", x[1], x[2], x[3], "one row per piece on the board per position", if (grepl("else NA", x[4])) "NA when not applicable" else never, "exact", x[4])
  for (x in list(
    c("square_id", "character", "physical square", "Physical square board:u:v."), c("board", "character", "board", "Board."),
    c("level", "character", "level", "Main level (platforms: level of the current dock)."),
    c("u", "integer", "local index", "Local column."), c("v", "integer", "local index", "Local row."),
    c("file", "integer", "file index 0-5", "Projected file at this position."), c("rank", "integer", "rank 0-9", "Projected rank at this position."),
    c("occupant_id", "character", "identifier", "Occupying piece, else NA."), c("occupant_type", "character", "piece type", "Occupant type, else NA."),
    c("occupant_color", "character", "colour", "Occupant colour, else NA."),
    c("attacked_by_white", "logical", "flag", "The square's coordinate is attacked by White."),
    c("attacked_by_black", "logical", "flag", "The square's coordinate is attacked by Black.")
  )) add("squares", x[1], x[2], x[3], "64 rows per position", if (grepl("else NA", x[4])) "NA when empty" else never, "exact", x[4])
  # summary
  for (x in .g3_feat_summary_dict()) add("summary", x[1], x[2], x[3], x[4], x[5], "descriptive", x[6])
  do.call(rbind, rows)
}

#' @noRd
.g3_feat_summary_dict <- function() {
  list(
    c("game_content_hash", "character", "identifier", "one row per game", "never missing", "g3_game_hash() of the game."),
    c("n_plies", "integer", "plies", "one row per game", "never missing", "Actions in the active line."),
    c("n_events", "integer", "events", "one row per game", "never missing", "Rows in the game's event log (actions and session events)."),
    c("result", "character", "category", "one row per game", "never missing", "white, black, draw (rule or agreed), truncated (computational cutoff, never a draw) or unfinished."),
    c("termination", "character", "category", "one row per game", "never missing", "checkmate, stalemate, repetition, halfmove, bare_kings, resign, draw_agreed, truncated or unfinished."),
    c("outcome_class", "character", "category", "one row per game", "never missing", "rule_decisive, rule_draw, resignation, agreed_draw, truncated or unfinished."),
    c("start_position_key", "character", "identifier", "one row per game", "never missing", "Repetition key of the game's initial position."),
    c("standard_start", "logical", "flag", "one row per game", "never missing", "The initial position equals the profile's starting position."),
    c("n_captures", "integer", "actions", "actions in the game", "never missing", "Capturing actions."),
    c("n_checks", "integer", "actions", "actions in the game", "never missing", "Checking actions (including mate)."),
    c("n_platform_actions", "integer", "actions", "actions in the game", "never missing", "Platform actions."),
    c("n_platform_rotations", "integer", "actions", "actions in the game", "never missing", "Platform actions with a 180-degree turn."),
    c("n_promotions", "integer", "actions", "actions in the game", "never missing", "Promotions (piece moves and transport)."),
    c("n_castles", "integer", "actions", "actions in the game", "never missing", "Castling actions."),
    c("n_en_passant", "integer", "actions", "actions in the game", "never missing", "En-passant captures."),
    c("n_board_transfers", "integer", "actions", "actions in the game", "never missing", "Piece moves between physical boards."),
    c("n_level_transfers", "integer", "actions", "actions in the game", "never missing", "Actions changing the moving piece's level."),
    c("n_positions", "integer", "positions", "one row per game", "never missing", "Positions analysed (plies + 1)."),
    c("n_evaluated", "integer", "positions", "n_positions", "never missing", "Positions with a score."),
    c("n_eval_missing", "integer", "positions", "n_positions", "never missing", "Positions without a score (see evaluations.missing_reason)."),
    c("max_repetition_count", "integer", "occurrences", "one row per game", "never missing", "Largest repetition count reached."),
    c("white_mean_move_loss", "double", "heuristic units", "white_n_move_loss", "NA when no heuristic loss", "Mean move_loss_raw of White's actions."),
    c("black_mean_move_loss", "double", "heuristic units", "black_n_move_loss", "NA when no heuristic loss", "Mean move_loss_raw of Black's actions."),
    c("white_n_move_loss", "integer", "actions", "White's actions", "never missing", "White actions with a heuristic move loss (denominator of the mean)."),
    c("black_n_move_loss", "integer", "actions", "Black's actions", "never missing", "Black actions with a heuristic move loss."),
    c("white_mean_expected_loss", "double", "expected score 0-1", "White actions with expected_loss_mover", "NA when none", "Mean expected_loss_mover of White's actions."),
    c("black_mean_expected_loss", "double", "expected score 0-1", "Black actions with expected_loss_mover", "NA when none", "Mean expected_loss_mover of Black's actions."),
    c("n_tactic_labels", "integer", "labels", "one row per game", "never missing", "Rows in the tactics table.")
  )
}
