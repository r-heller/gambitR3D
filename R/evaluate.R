# Static evaluation.
#
# A transparent heuristic, scored in "heuristic units" from the side to
# act's perspective (positive is good for the side to act). These are not
# centipawns: the piece values are ordinary chess values used as a
# provisional starting point, not measured values for this variant.
#
#   material   P 100, N 300, B 300, R 500, Q 900
#   mobility   4 per pseudo-legal action (both sides; actions, not
#              distinct destinations)
#   king       -25 per enemy non-pawn piece within two coordinates of the
#              king; -40 when in check
#   pawns      +6 per rank advanced towards the promotion end
#   platforms  +8 per free adjacent dock of a platform whose single
#              occupant belongs to that side (legal transport options)
#
# Display settings (height, camera, view) never enter the evaluation.

.G3_EVAL_VERSION <- "g3-eval/1"
.G3_PIECE_VALUE <- c(100L, 300L, 300L, 500L, 900L, 0L)
.G3_EVAL_WEIGHTS <- list(mobility = 4L, king_near = -25L, in_check = -40L, pawn_rank = 6L, platform_dock = 8L)

# Evaluation terms for both colours (white, black), unsigned.
#' @noRd
.g3_eval_terms <- function(core, ctx, tabs) {
  alive <- core$psq > 0L
  col <- core$pcolor
  typ <- core$ptype
  material <- c(
    sum(.G3_PIECE_VALUE[typ[alive & col == 1L]]),
    sum(.G3_PIECE_VALUE[typ[alive & col == 2L]])
  )
  # Mobility: pseudo-legal action counts for both sides.
  mob <- c(.g3_count_pseudo(core, ctx, tabs, 1L), .g3_count_pseudo(core, ctx, tabs, 2L))
  # King neighbourhood and check.
  king_near <- integer(2L)
  check <- logical(2L)
  for (s in 1:2) {
    ks <- .g3_king_sq(core, s)
    if (is.na(ks)) next
    enemy <- which(alive & col == 3L - s & typ != 1L & typ != 6L)
    es <- core$psq[enemy]
    d <- pmax(abs(ctx$file[es] - ctx$file[ks]), abs(ctx$rank[es] - ctx$rank[ks]))
    king_near[s] <- sum(d <= 2L)
    check[s] <- .g3_attacked(ctx$file[ks], ctx$rank[ks], 3L - s, ctx$file, ctx$rank, ctx$cnt,
                             core$psq, typ, col)
  }
  # Pawn progress in ranks from the colour's start rank (white 1, black 8).
  pw <- which(alive & typ == 1L & col == 1L)
  pb <- which(alive & typ == 1L & col == 2L)
  pawn <- c(sum(pmax(ctx$rank[core$psq[pw]] - 1L, 0L)), sum(pmax(8L - ctx$rank[core$psq[pb]], 0L)))
  # Platform transport options.
  plat <- integer(2L)
  for (pl in 1:4) {
    sqs <- tabs$plat_squares[[pl]]
    occp <- core$occ[sqs][core$occ[sqs] > 0L]
    if (length(occp) != 1L) next
    free <- length(setdiff(tabs$dock_adj[[core$dock[pl]]], core$dock[-pl]))
    plat[col[occp]] <- plat[col[occp]] + free
  }
  list(material = material, mobility = mob, king_near = king_near, check = check, pawn = pawn, platform = plat)
}

# Scalar score from the side to act's perspective.
#' @noRd
.g3_eval <- function(core, ctx, tabs, terms = .g3_eval_terms(core, ctx, tabs)) {
  w <- .G3_EVAL_WEIGHTS
  white <- terms$material[1L] - terms$material[2L] +
    w$mobility * (terms$mobility[1L] - terms$mobility[2L]) +
    w$king_near * (terms$king_near[1L] - terms$king_near[2L]) +
    w$in_check * (terms$check[1L] - terms$check[2L]) +
    w$pawn_rank * (terms$pawn[1L] - terms$pawn[2L]) +
    w$platform_dock * (terms$platform[1L] - terms$platform[2L])
  if (core$side == 1L) white else -white
}
