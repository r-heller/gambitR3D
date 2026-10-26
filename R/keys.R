# Action ids, repetition keys and state checksums.
#
# Action ids are readable canonical strings, scoped to action schema
# `g3-action/1` and the rules hash recorded with every game:
#
#   piece move   m:<from square>-<to square>[=<promotion>]
#   platform     t:<platform>><dock>@<rotation delta>[=<promotion>]
#   castling     c:<colour>:<kingside|queenside>
#
# They depend only on game-significant fields, never on a row number.

.G3_ACTION_SCHEMA <- "g3-action/1"

#' @noRd
.g3_action_ids <- function(core, m, tabs) {
  if (!nrow(m)) {
    return(character())
  }
  promo <- ifelse(m[, "promo"] > 0L, paste0("=", .G3_TYPES[pmax(m[, "promo"], 1L)]), "")
  ids <- character(nrow(m))
  mv <- m[, "type"] == 1L
  ids[mv] <- paste0("m:", tabs$sq_id[m[mv, "from"]], "-", tabs$sq_id[m[mv, "to"]], promo[mv])
  pl <- m[, "type"] == 2L
  ids[pl] <- paste0(
    "t:", .G3_PLATFORMS[m[pl, "plat"]], ">", tabs$dock_ids[pmax(m[pl, "dock"], 1L)],
    "@", m[pl, "rot"], promo[pl]
  )
  cs <- m[, "type"] == 3L
  ids[cs] <- paste0(
    "c:", .G3_COLORS[core$pcolor[m[cs, "piece"]]], ":",
    c("kingside", "queenside")[pmax(m[cs, "cside"], 1L)]
  )
  ids
}

# Repetition key of a core: rules hash, side, occupancy by type and colour
# (unmoved pawns marked, because they keep the double step), platform docks
# and orientations, castling rights, first-turn eligibility while a colour
# still has a castling right, and an en-passant right only if it can be
# used. Piece ids, ply and counters are excluded.
#' @noRd
.g3_key_core <- function(core, tabs, ctx = NULL) {
  .g3_sha256(.g3_key_text(core, tabs, ctx))
}

# The canonical text behind the repetition key (one string).
#' @noRd
.g3_key_text <- function(core, tabs, ctx = NULL) {
  occ <- core$occ
  code <- integer(64L)
  o <- occ > 0L
  pi <- occ[o]
  code[o] <- (core$pcolor[pi] - 1L) * 7L + core$ptype[pi] + ifelse(core$ptype[pi] == 1L & !core$pmoved[pi], 6L, 0L)
  # Codes: white P..K = 1..6, unmoved white pawn = 7; black adds 7.
  cr <- core$castle
  first_rel <- c(core$first[1L] && any(cr[1:2]), core$first[2L] && any(cr[3:4]))
  ep <- "-"
  if (length(core$ep)) {
    if (is.null(ctx)) ctx <- .g3_ctx(core, tabs)
    # Crossed file, crossed rank and victim square, as one string.
    if (.g3_ep_effective(core, ctx, tabs)) ep <- paste(core$ep[3L], core$ep[4L], core$ep[2L], sep = ".")
  }
  paste(
    tabs$rules_hash, core$side, paste(code, collapse = "."),
    paste(core$dock, collapse = "."), paste(core$orient, collapse = "."),
    paste(as.integer(cr), collapse = ""), paste(as.integer(first_rel), collapse = ""), ep,
    sep = "|"
  )
}

#' Repetition key of a position
#'
#' A canonical hexadecimal key identifying the position for repetition
#' purposes: side to act, occupancy of all 64 physical squares by piece type
#' and colour, platform docks and orientations, castling rights,
#' first-turn eligibility while relevant, pawn double-step rights and an
#' en-passant right that can actually be used. Piece ids, ply, counters,
#' annotations and every display preference are excluded.
#'
#' @param state A `g3_state`.
#' @return One lowercase hexadecimal character string (SHA-256).
#' @examples
#' # The key of the starting position.
#' game <- gambitR3D::g3_new_game()
#' gambitR3D::g3_position_key(game$state)
#' @export
g3_position_key <- function(state) {
  .g3_check_state(state)
  tabs <- .g3_tables()
  .g3_key_core(.g3_core(state, tabs), tabs)
}

# Full-state checksum: canonical JSON of the whole logical state, including
# ply, counters, repetition history, piece ids and outcome.
#' @noRd
.g3_state_hash <- function(state) {
  .g3_hash_object(unclass(.g3_state_plain(state)))
}

# A state as plain lists (data frames as row records), for JSON.
#' @noRd
.g3_state_plain <- function(state) {
  x <- unclass(state)
  x$boards <- .g3_rows(x$boards)
  x$pieces <- .g3_rows(x$pieces)
  x$repetition$keys <- as.list(x$repetition$keys)
  x
}

#' @noRd
.g3_rows <- function(df) {
  if (!nrow(df)) {
    return(list())
  }
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

# Convert an internal action matrix to the public action table.
#' @noRd
.g3_action_table <- function(core, m, tabs, ctx = .g3_ctx(core, tabs)) {
  n <- nrow(m)
  ids <- .g3_action_ids(core, m, tabs)
  type <- c("move", "platform", "castle")[m[, "type"]]
  is_mv <- m[, "type"] != 2L
  is_pl <- m[, "type"] == 2L
  is_cs <- m[, "type"] == 3L
  out <- data.frame(
    action_id = ids,
    type = type,
    piece_id = core$pid[m[, "piece"]],
    from_square = tabs$sq_id[m[, "from"]],
    to_square = ifelse(is_mv, tabs$sq_id[pmax(m[, "to"], 1L)], NA_character_),
    platform_id = ifelse(is_pl, .G3_PLATFORMS[pmax(m[, "plat"], 1L)], NA_character_),
    to_dock = ifelse(is_pl, tabs$dock_ids[pmax(m[, "dock"], 1L)], NA_character_),
    rotation_delta = ifelse(is_pl, m[, "rot"], NA_integer_),
    promotion = ifelse(m[, "promo"] > 0L, .G3_TYPES[pmax(m[, "promo"], 1L)], NA_character_),
    castle_side = ifelse(is_cs, c("kingside", "queenside")[pmax(m[, "cside"], 1L)], NA_character_),
    capture_id = ifelse(m[, "cap"] > 0L, core$pid[pmax(m[, "cap"], 1L)], NA_character_),
    en_passant = m[, "flag"] == 2L,
    label = if (n) .g3_action_labels(core, m, tabs, ctx) else character(),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  class(out) <- c("g3_actions", "data.frame")
  out
}

#' @noRd
.g3_empty_action_table <- function() {
  out <- data.frame(
    action_id = character(), type = character(), piece_id = character(),
    from_square = character(), to_square = character(), platform_id = character(),
    to_dock = character(), rotation_delta = integer(), promotion = character(),
    castle_side = character(), capture_id = character(), en_passant = logical(),
    label = character(), stringsAsFactors = FALSE
  )
  class(out) <- c("g3_actions", "data.frame")
  out
}
