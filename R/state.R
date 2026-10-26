# Game state.
#
# The public `g3_state` is a list of plain fields and two data frames
# (`boards`, `pieces`), easy to inspect and serialise. The engine works on a
# compact internal "core" made of atomic vectors, converted at the API
# boundary:
#
#   occ      integer[64]  piece index on each square, 0 when empty
#   pid, ptype, porigin, pcolor, psq, pmoved
#            per-piece vectors; psq is 0 for a captured piece
#   dock, orient
#            integer[4]   dock (1..12) and orientation (0/180) of WQ WK BQ BK
#   side     1 white, 2 black
#   castle   logical[4]   white king side, white queen side, black king side,
#                         black queen side
#   first    logical[2]   first turn completed, per colour
#   ep       integer[4]   victim piece, victim square, crossed file, crossed
#                         rank; integer(0) when there is no entitlement
#   halfmove, ply
#   rep      character    repetition keys since the last irreversible action,
#                         ending with the current key

.G3_STATE_SCHEMA <- "g3-state/1"
.G3_CASTLE_NAMES <- c("white_kingside", "white_queenside", "black_kingside", "black_queenside")

#' @noRd
.g3_initial_core <- function(tabs = .g3_tables()) {
  st <- tabs$setup
  occ <- integer(64L)
  occ[st$sq] <- seq_len(nrow(st))
  core <- list(
    occ = occ,
    pid = st$id, ptype = st$type, porigin = st$type, pcolor = st$color,
    psq = st$sq, pmoved = rep(FALSE, nrow(st)),
    dock = tabs$init_dock, orient = integer(4L),
    side = 1L,
    castle = rep(TRUE, 4L), first = c(FALSE, FALSE),
    ep = integer(0L), halfmove = 0L, ply = 0L, rep = character(0L)
  )
  core$rep <- .g3_key_core(core, tabs)
  core
}

# Convert a public state into a core. Assumes the state is structurally
# valid (see g3_validate_state()).
#' @noRd
.g3_core <- function(state, tabs = .g3_tables()) {
  pc <- state$pieces
  n <- nrow(pc)
  sq <- match(pc$square_id, tabs$sq_id)
  occ <- integer(64L)
  occ[sq] <- seq_len(n)
  plat <- state$boards[match(.G3_PLATFORMS, state$boards$id), ]
  ep <- integer(0L)
  if (!is.null(state$ep)) {
    ep <- c(
      match(state$ep$victim_id, pc$id), match(state$ep$victim_square, tabs$sq_id),
      as.integer(state$ep$crossed_file), as.integer(state$ep$crossed_rank)
    )
  }
  cr <- state$rights$castling
  ft <- state$rights$first_turn_completed
  list(
    occ = occ,
    pid = pc$id, ptype = match(pc$type, .G3_TYPES), porigin = match(pc$origin_type, .G3_TYPES),
    pcolor = match(pc$color, .G3_COLORS), psq = sq, pmoved = as.logical(pc$has_moved),
    dock = match(plat$dock, tabs$dock_ids), orient = as.integer(plat$orientation),
    side = match(state$side, .G3_COLORS),
    castle = as.logical(unlist(cr[.G3_CASTLE_NAMES])),
    first = as.logical(unlist(ft[c("white", "black")])),
    ep = ep,
    halfmove = as.integer(state$clocks$halfmove), ply = as.integer(state$ply),
    rep = as.character(unlist(state$repetition$keys))
  )
}

# Convert a core back into a public state.
#' @noRd
.g3_state_from_core <- function(core, outcome = NULL, tabs = .g3_tables()) {
  alive <- which(core$psq > 0L)
  pieces <- data.frame(
    id = core$pid[alive],
    type = .G3_TYPES[core$ptype[alive]],
    origin_type = .G3_TYPES[core$porigin[alive]],
    color = .G3_COLORS[core$pcolor[alive]],
    square_id = tabs$sq_id[core$psq[alive]],
    has_moved = core$pmoved[alive],
    stringsAsFactors = FALSE
  )
  boards <- data.frame(
    id = tabs$board_ids,
    type = c(rep("main", 3L), rep("platform", 4L)),
    dock = c(rep(NA_character_, 3L), tabs$dock_ids[core$dock]),
    orientation = c(0L, 0L, 0L, as.integer(core$orient)),
    stringsAsFactors = FALSE
  )
  ep <- NULL
  if (length(core$ep)) {
    ep <- list(
      victim_id = core$pid[core$ep[1L]], victim_square = tabs$sq_id[core$ep[2L]],
      crossed_file = core$ep[3L], crossed_rank = core$ep[4L]
    )
  }
  structure(
    list(
      schema_version = .G3_STATE_SCHEMA,
      rules_version = tabs$rules_version,
      rules_hash = tabs$rules_hash,
      boards = boards,
      pieces = pieces,
      side = .G3_COLORS[core$side],
      rights = list(
        castling = as.list(stats::setNames(core$castle, .G3_CASTLE_NAMES)),
        first_turn_completed = list(white = core$first[1L], black = core$first[2L])
      ),
      ep = ep,
      ply = core$ply,
      clocks = list(halfmove = core$halfmove),
      repetition = list(keys = core$rep),
      outcome = outcome %||% list(status = "ongoing", result = NA_character_, reason = NA_character_)
    ),
    class = "g3_state"
  )
}

#' Start a new game
#'
#' Creates a game container holding the initial position of the rules
#' profile, an empty event log and session metadata. State-level functions
#' such as [g3_legal_actions()] take `game$state`.
#'
#' @param profile Rules profile id; only `"experimental-v0.1"` is supported.
#' @param players Optional named list, for example
#'   `list(white = "Ada", black = "search")`. Stored as metadata only.
#' @return A `g3_game` list with `initial_state`, `state` (both `g3_state`),
#'   `events` (a data frame, zero rows) and `metadata`.
#' @examples
#' # Create a game; the rules API consumes its logical state.
#' game <- gambitR3D::g3_new_game()
#' actions <- gambitR3D::g3_legal_actions(game$state)
#' # A pure transition returns both the next state and the auditable event.
#' step <- gambitR3D::g3_apply_action(game$state, actions$action_id[[1L]])
#' base::stopifnot(gambitR3D::g3_validate_state(step$state))
#' @export
g3_new_game <- function(profile = "experimental-v0.1", players = NULL) {
  g3_rules_profile(profile)
  tabs <- .g3_tables()
  state <- .g3_state_from_core(.g3_initial_core(tabs), tabs = tabs)
  if (!is.null(players) && (!is.list(players) || is.null(names(players)))) {
    .g3_abort("g3_invalid_state", "`players` must be a named list or NULL.")
  }
  .g3_game_new(state, players = players)
}

#' @noRd
.g3_game_new <- function(state, players = NULL, game_id = NULL, metadata = list()) {
  meta <- utils::modifyList(
    list(
      game_id = game_id %||% .g3_new_id("game"),
      created = .g3_now(),
      players = players %||% list(white = "White", black = "Black"),
      rules_version = state$rules_version,
      rules_hash = state$rules_hash,
      action_schema = .G3_ACTION_SCHEMA,
      result = NA_character_,
      termination = NA_character_
    ),
    metadata
  )
  structure(
    list(initial_state = state, state = state, events = .g3_empty_events(), metadata = meta),
    class = "g3_game"
  )
}

#' @noRd
.g3_empty_events <- function() {
  data.frame(
    event_id = character(), sequence = integer(), type = character(),
    actor = character(), ply = integer(), action_id = character(),
    pre_state_hash = character(), post_state_hash = character(),
    position_key = character(), payload = character(),
    stringsAsFactors = FALSE
  )
}

#' Set up a position
#'
#' Builds a `g3_state` from an explicit piece list, for analysis positions,
#' puzzles and tests. Such a position is an edited position, not a recorded
#' game's reachable position: it has no history before it, so its
#' repetition record starts with itself.
#'
#' @param pieces Data frame with columns `id`, `type` (`"P"`, `"N"`, `"B"`,
#'   `"R"`, `"Q"`, `"K"`), `color` (`"white"`, `"black"`) and `square_id`.
#'   Optional columns `has_moved` (default `TRUE`) and `origin_type`
#'   (default `type`).
#' @param docks Named character vector of platform docks, for example
#'   `c(WQ = "W:L:lo", WK = "W:R:lo", BQ = "B:L:hi", BK = "B:R:hi")`.
#'   Missing platforms keep their starting dock.
#' @param orientation Named integer vector of platform orientations (0 or
#'   180); missing platforms get 0.
#' @param side `"white"` or `"black"` to act.
#' @param castling Named logical vector with any of `white_kingside`,
#'   `white_queenside`, `black_kingside`, `black_queenside`; missing rights
#'   are `FALSE`.
#' @param first_turn_completed Named logical vector for `white` and
#'   `black`; defaults to `TRUE` for both.
#' @param ep Optional en-passant record: a list with `victim_id`,
#'   `victim_square`, `crossed_file` (0..5) and `crossed_rank` (0..9).
#' @param halfmove,ply Rule counter and ply number.
#' @param mode Validation mode, see [g3_validate_state()].
#' @return A validated `g3_state` whose `outcome` is already evaluated.
#' @examples
#' # King and rook against king. A position with kings only would already
#' # be a finished game (bare kings): set-up positions are judged by the
#' # same rules as played ones.
#' pcs <- base::data.frame(
#'   id = c("wK", "wR", "bK"), type = c("K", "R", "K"),
#'   color = c("white", "white", "black"),
#'   square_id = c("W:0:0", "W:2:0", "B:3:3")
#' )
#' st <- gambitR3D::g3_setup_position(pcs)
#' base::nrow(gambitR3D::g3_legal_actions(st))
#' @export
g3_setup_position <- function(pieces, docks = NULL, orientation = NULL, side = "white",
                              castling = NULL, first_turn_completed = c(white = TRUE, black = TRUE),
                              ep = NULL, halfmove = 0L, ply = 0L, mode = c("playable", "editor")) {
  mode <- match.arg(mode)
  tabs <- .g3_tables()
  if (!is.data.frame(pieces) || !all(c("id", "type", "color", "square_id") %in% names(pieces))) {
    .g3_abort("g3_invalid_state", "`pieces` needs columns id, type, color and square_id.")
  }
  pieces <- as.data.frame(pieces, stringsAsFactors = FALSE)
  if (is.null(pieces$has_moved)) pieces$has_moved <- TRUE
  if (is.null(pieces$origin_type)) pieces$origin_type <- pieces$type
  pieces <- pieces[, c("id", "type", "origin_type", "color", "square_id", "has_moved")]
  pieces$has_moved <- as.logical(pieces$has_moved)
  dk <- stats::setNames(tabs$dock_ids[tabs$init_dock], .G3_PLATFORMS)
  if (!is.null(docks)) dk[names(docks)] <- docks
  ori <- stats::setNames(integer(4L), .G3_PLATFORMS)
  if (!is.null(orientation)) ori[names(orientation)] <- as.integer(orientation)
  cr <- stats::setNames(rep(FALSE, 4L), .G3_CASTLE_NAMES)
  if (!is.null(castling)) cr[names(castling)] <- as.logical(castling)
  ft <- c(white = TRUE, black = TRUE)
  ft[names(first_turn_completed)] <- as.logical(first_turn_completed)
  state <- structure(
    list(
      schema_version = .G3_STATE_SCHEMA,
      rules_version = tabs$rules_version,
      rules_hash = tabs$rules_hash,
      boards = data.frame(
        id = tabs$board_ids, type = c(rep("main", 3L), rep("platform", 4L)),
        dock = c(rep(NA_character_, 3L), unname(dk)), orientation = c(0L, 0L, 0L, unname(ori)),
        stringsAsFactors = FALSE
      ),
      pieces = pieces,
      side = side,
      rights = list(
        castling = as.list(cr),
        first_turn_completed = list(white = unname(ft["white"]), black = unname(ft["black"]))
      ),
      ep = ep,
      ply = as.integer(ply),
      clocks = list(halfmove = as.integer(halfmove)),
      repetition = list(keys = character()),
      outcome = list(status = "ongoing", result = NA_character_, reason = NA_character_)
    ),
    class = "g3_state"
  )
  g3_validate_state(state, mode = mode)
  if (mode == "editor") {
    return(state)
  }
  core <- .g3_core(state, tabs)
  core$rep <- .g3_key_core(core, tabs)
  .g3_state_from_core(core, .g3_outcome_core(core, tabs), tabs)
}

#' @export
print.g3_state <- function(x, ...) {
  cat("<g3_state> ", x$rules_version, ", ply ", x$ply, ", ", x$side, " to act", sep = "")
  if (identical(x$outcome$status, "terminal")) {
    cat(" | finished: ", x$outcome$result, " (", x$outcome$reason, ")", sep = "")
  }
  cat("\n")
  cat(.g3_state_text(x), sep = "\n")
  invisible(x)
}

#' @export
print.g3_game <- function(x, ...) {
  cat("<g3_game> ", x$metadata$game_id, ": ", nrow(x$events), " events, ply ",
      x$state$ply, sep = "")
  if (!is.na(x$metadata$result)) cat(" | result ", x$metadata$result, " (", x$metadata$termination, ")", sep = "")
  cat("\n")
  print(x$state)
  invisible(x)
}

# Compact text picture: one line per board, squares listed by coordinate.
#' @noRd
.g3_state_text <- function(state) {
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  co <- .g3_square_coords(core$dock, core$orient, tabs)
  lines <- character()
  for (b in seq_along(tabs$board_ids)) {
    idx <- which(tabs$sq_board == b)
    lab <- tabs$board_ids[b]
    if (b >= 4L) lab <- paste0(lab, "@", tabs$dock_ids[core$dock[b - 3L]], if (core$orient[b - 3L] == 180L) " (turned)" else "")
    cells <- vapply(idx, function(s) {
      p <- core$occ[s]
      tok <- if (p == 0L) "." else {
        ch <- .G3_TYPES[core$ptype[p]]
        if (core$pcolor[p] == 2L) tolower(ch) else ch
      }
      paste0(.g3_coord_label(co$file[s], co$rank[s]), "=", tok)
    }, "")
    lines <- c(lines, paste0(formatC(lab, width = -16), paste(cells, collapse = " ")))
  }
  lines
}
