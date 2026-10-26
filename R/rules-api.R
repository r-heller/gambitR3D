# Public rules API: legal actions, transitions, validation and termination.

#' Legal actions in a position
#'
#' Lists every legal action for the side to act: piece moves (including
#' promotions and en passant), platform actions (relocation to an adjacent
#' free dock, keeping or turning the platform) and castling. A finished
#' position returns a typed table with zero rows.
#'
#' @param state A `g3_state`, for example `g3_new_game()$state`.
#' @return A `g3_actions` data frame sorted by `action_id`, with columns
#'   `action_id`, `type` (`"move"`, `"platform"`, `"castle"`), `piece_id`,
#'   `from_square`, `to_square`, `platform_id`, `to_dock`,
#'   `rotation_delta` (0 or 180), `promotion`, `castle_side`, `capture_id`,
#'   `en_passant` and a human-readable `label`. Inapplicable fields are
#'   typed `NA`.
#' @examples
#' # The twenty-odd opening actions; the four-piece platforms cannot move.
#' game <- gambitR3D::g3_new_game()
#' acts <- gambitR3D::g3_legal_actions(game$state)
#' base::table(acts$type)
#' @export
g3_legal_actions <- function(state) {
  .g3_check_state(state)
  if (identical(state$outcome$status, "terminal")) {
    return(.g3_empty_action_table())
  }
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  m <- .g3_legal(core, tabs, ctx)
  if (!nrow(m)) {
    return(.g3_empty_action_table())
  }
  .g3_action_table(core, m, tabs, ctx)
}

#' Apply a legal action
#'
#' Validates `action` against the legal actions of `state` and returns the
#' next state together with an auditable event. The input state is never
#' modified. Only the action id is trusted; any other fields of a supplied
#' action row are ignored and recomputed by the engine.
#'
#' @param state A `g3_state`.
#' @param action An action id (character), a one-row `g3_actions` data
#'   frame, or a list with an `action_id` element.
#' @param expected_state_hash Optional full-state checksum the caller
#'   believes is current (see [g3_state_hash()]). A mismatch raises a
#'   `g3_stale_revision` condition, so a delayed request cannot act on a
#'   later position.
#' @return A `g3_transition` list with `state` (the next `g3_state`) and
#'   `event` (a list: `event_id`, `type`, `actor`, `ply`, `action_id`,
#'   `action`, `notation`, `check`, `pre_state_hash`, `post_state_hash`,
#'   `pre_position_key`, `position_key`, `outcome`).
#' @examples
#' # Play the first legal action and inspect the event.
#' game <- gambitR3D::g3_new_game()
#' acts <- gambitR3D::g3_legal_actions(game$state)
#' step <- gambitR3D::g3_apply_action(game$state, acts$action_id[[1L]])
#' step$event$notation
#' @export
g3_apply_action <- function(state, action, expected_state_hash = NULL) {
  .g3_check_state(state)
  pre_hash <- .g3_state_hash(state)
  if (!is.null(expected_state_hash) && !identical(as.character(expected_state_hash), pre_hash)) {
    .g3_abort(
      "g3_stale_revision", "The position changed since this action was requested.",
      expected = expected_state_hash, actual = pre_hash
    )
  }
  if (identical(state$outcome$status, "terminal")) {
    .g3_abort("g3_illegal_action", "The game is already finished; no action is legal.")
  }
  id <- .g3_action_id_arg(action)
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  ids <- .g3_action_ids(core, legal, tabs)
  i <- match(id, ids)
  if (is.na(i)) {
    .g3_abort(
      "g3_illegal_action",
      paste0("Action '", id, "' is not legal in this position (", state$side, " to act)."),
      action_id = id, ply = state$ply
    )
  }
  .g3_transition(state, core, ctx, legal[i, ], tabs, pre_hash)
}

# Shared by g3_apply_action() and replay: build the transition for a known
# legal action row `a`.
#' @noRd
.g3_transition <- function(state, core, ctx, a, tabs, pre_hash = .g3_state_hash(state)) {
  row <- .g3_action_table(core, matrix(a, nrow = 1L, dimnames = list(NULL, .G3_ACOLS)), tabs, ctx)
  new <- .g3_make(core, a, tabs)
  ctx2 <- .g3_ctx(new, tabs)
  legal2 <- .g3_legal(new, tabs, ctx2)
  outcome <- .g3_outcome_core(new, tabs, ctx2, legal2)
  check <- .g3_in_check(new, new$side, ctx2, tabs)
  st <- .g3_state_from_core(new, outcome, tabs)
  post_hash <- .g3_state_hash(st)
  action <- as.list(row)
  action$label <- NULL
  notation <- paste0(
    row$label,
    if (identical(outcome$reason, "checkmate")) "#" else if (check) "+" else ""
  )
  event <- list(
    event_id = substr(.g3_sha256(paste(pre_hash, row$action_id, sep = "|")), 1L, 20L),
    type = "action",
    actor = state$side,
    ply = st$ply,
    action_id = row$action_id,
    action = action,
    notation = notation,
    check = check,
    pre_state_hash = pre_hash,
    post_state_hash = post_hash,
    pre_position_key = core$rep[length(core$rep)],
    position_key = new$rep[length(new$rep)],
    outcome = outcome
  )
  structure(list(state = st, event = event), class = "g3_transition")
}

#' @noRd
.g3_action_id_arg <- function(action) {
  id <- if (is.character(action)) {
    action
  } else if (is.data.frame(action)) {
    if (nrow(action) != 1L || is.null(action$action_id)) {
      .g3_abort("g3_illegal_action", "An action data frame must have exactly one row with an `action_id`.")
    }
    action$action_id
  } else if (is.list(action) && !is.null(action$action_id)) {
    action$action_id
  } else {
    .g3_abort("g3_illegal_action", "`action` must be an action id, a one-row action table or a list with `action_id`.")
  }
  if (length(id) != 1L || is.na(id) || !nzchar(id)) {
    .g3_abort("g3_illegal_action", "`action` must identify exactly one action.")
  }
  as.character(id)
}

# Rule outcome of a core, in the profile's precedence order.
#' @noRd
.g3_outcome_core <- function(core, tabs, ctx = .g3_ctx(core, tabs), legal = .g3_legal(core, tabs, ctx)) {
  side <- core$side
  if (!nrow(legal)) {
    if (.g3_in_check(core, side, ctx, tabs)) {
      return(list(status = "terminal", result = .G3_COLORS[3L - side], reason = "checkmate"))
    }
    return(list(status = "terminal", result = "draw", reason = "stalemate"))
  }
  key <- core$rep[length(core$rep)]
  if (sum(core$rep == key) >= 3L) {
    return(list(status = "terminal", result = "draw", reason = "repetition"))
  }
  if (core$halfmove >= 100L) {
    return(list(status = "terminal", result = "draw", reason = "halfmove"))
  }
  alive <- core$psq > 0L
  if (sum(alive) == 2L && all(core$ptype[alive] == 6L)) {
    return(list(status = "terminal", result = "draw", reason = "bare_kings"))
  }
  list(status = "ongoing", result = NA_character_, reason = NA_character_)
}

#' Is the game over?
#'
#' @param state A `g3_state`.
#' @return `TRUE` when the position is a rule outcome (checkmate, stalemate
#'   or an automatic draw), otherwise `FALSE`. The result and reason are in
#'   `state$outcome`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' gambitR3D::g3_is_terminal(game$state)
#' @export
g3_is_terminal <- function(state) {
  .g3_check_state(state)
  identical(state$outcome$status, "terminal")
}

#' Full-state checksum
#'
#' SHA-256 of the canonical JSON of the complete logical state, including
#' ply, counters, repetition history, piece ids and outcome. Use it as the
#' revision token for [g3_apply_action()]'s `expected_state_hash`. Display
#' preferences are not part of the state and cannot change it.
#'
#' @param state A `g3_state`.
#' @return One lowercase hexadecimal string.
#' @examples
#' gambitR3D::g3_state_hash(gambitR3D::g3_new_game()$state)
#' @export
g3_state_hash <- function(state) {
  .g3_check_state(state)
  .g3_state_hash(state)
}

# Cheap structural guard used by every state-level API.
#' @noRd
.g3_check_state <- function(state) {
  if (!inherits(state, "g3_state")) {
    .g3_abort("g3_invalid_state", "Expected a `g3_state` (for example `g3_new_game()$state`).")
  }
  if (!identical(state$rules_version, "experimental-v0.1")) {
    .g3_abort("g3_unsupported_rules", paste0("Unsupported rules version: ", format(state$rules_version)))
  }
  if (!identical(state$rules_hash, .g3_tables()$rules_hash)) {
    .g3_abort("g3_unsupported_rules", "The state's rules hash does not match the installed profile.",
              rules_hash = state$rules_hash)
  }
  if (!is.data.frame(state$pieces) || !is.data.frame(state$boards)) {
    .g3_abort("g3_invalid_state", "State boards and pieces must be data frames.")
  }
  invisible(TRUE)
}

#' Validate a state
#'
#' Checks the structure and logical consistency of a state: schema and
#' rules identity, the seven boards and their docks, unique piece ids and
#' squares, rights, en-passant record, counters and, in `"playable"` mode,
#' exactly one king per colour and no king of the side that just moved left
#' in check.
#'
#' @param state A `g3_state`.
#' @param mode `"playable"` (default) or `"editor"`. Editor mode allows a
#'   missing king for position setup; such a state cannot be played.
#' @param error If `TRUE` (default), an invalid state raises a
#'   `g3_invalid_state` condition. If `FALSE`, a list with `valid` and
#'   `problems` is returned instead.
#' @return Invisibly `TRUE` for a valid state when `error = TRUE`;
#'   otherwise a list `list(valid = <logical>, problems = <character>)`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' gambitR3D::g3_validate_state(game$state)
#' gambitR3D::g3_validate_state(game$state, error = FALSE)$valid
#' @export
g3_validate_state <- function(state, mode = c("playable", "editor"), error = TRUE) {
  mode <- match.arg(mode)
  problems <- .g3_state_problems(state, mode)
  if (error) {
    if (length(problems)) {
      .g3_abort("g3_invalid_state", paste0("Invalid state: ", paste(problems, collapse = "; ")),
                problems = problems)
    }
    return(invisible(TRUE))
  }
  list(valid = !length(problems), problems = problems)
}

#' @noRd
.g3_state_problems <- function(state, mode) {
  p <- character()
  if (!inherits(state, "g3_state")) {
    return("not a g3_state")
  }
  need <- c("schema_version", "rules_version", "rules_hash", "boards", "pieces", "side",
            "rights", "ply", "clocks", "repetition", "outcome")
  miss <- setdiff(need, names(state))
  if (length(miss)) {
    return(paste("missing fields:", paste(miss, collapse = ", ")))
  }
  if (!identical(state$schema_version, .G3_STATE_SCHEMA)) p <- c(p, "unknown schema version")
  tabs <- .g3_tables()
  if (!identical(state$rules_version, tabs$rules_version)) p <- c(p, "unsupported rules version")
  if (!identical(state$rules_hash, tabs$rules_hash)) p <- c(p, "rules hash mismatch")
  b <- state$boards
  if (!is.data.frame(b) || !identical(b$id, tabs$board_ids)) {
    p <- c(p, "boards must be W, N, B, WQ, WK, BQ, BK in order")
  } else {
    if (any(!is.na(b$dock[1:3])) || any(b$orientation[1:3] != 0L)) p <- c(p, "main boards are fixed")
    d <- b$dock[4:7]
    if (any(is.na(d)) || !all(d %in% tabs$dock_ids)) p <- c(p, "invalid platform dock")
    if (anyDuplicated(d)) p <- c(p, "two platforms on one dock")
    if (!all(b$orientation[4:7] %in% c(0L, 180L))) p <- c(p, "orientation must be 0 or 180")
  }
  pc <- state$pieces
  if (!is.data.frame(pc) || !all(c("id", "type", "origin_type", "color", "square_id", "has_moved") %in% names(pc))) {
    return(c(p, "pieces table lacks required columns"))
  }
  if (anyNA(pc$id) || any(!nzchar(pc$id)) || anyDuplicated(pc$id)) p <- c(p, "piece ids must be unique and non-empty")
  if (!all(pc$type %in% .G3_TYPES) || !all(pc$origin_type %in% .G3_TYPES)) p <- c(p, "invalid piece type")
  if (!all(pc$color %in% .G3_COLORS)) p <- c(p, "invalid piece colour")
  if (!all(pc$square_id %in% tabs$sq_id)) p <- c(p, "piece on a nonexistent square")
  if (anyDuplicated(pc$square_id)) p <- c(p, "two pieces on one square")
  if (!is.logical(pc$has_moved) || anyNA(pc$has_moved)) p <- c(p, "has_moved must be TRUE/FALSE")
  if (any(pc$origin_type != pc$type & pc$origin_type != "P")) p <- c(p, "only pawns change type")
  nk <- c(sum(pc$type == "K" & pc$color == "white"), sum(pc$type == "K" & pc$color == "black"))
  if (any(nk > 1L)) p <- c(p, "more than one king of a colour")
  if (mode == "playable" && any(nk != 1L)) p <- c(p, "a playable state needs exactly one king per colour")
  if (!is.character(state$side) || length(state$side) != 1L || !state$side %in% .G3_COLORS) p <- c(p, "invalid side")
  if (!is.numeric(state$ply) || length(state$ply) != 1L || is.na(state$ply) || state$ply < 0) p <- c(p, "invalid ply")
  hm <- state$clocks$halfmove
  if (!is.numeric(hm) || length(hm) != 1L || is.na(hm) || hm < 0) p <- c(p, "invalid halfmove counter")
  cr <- state$rights$castling
  ft <- state$rights$first_turn_completed
  if (!is.list(cr) || !setequal(names(cr), .G3_CASTLE_NAMES) || !all(vapply(cr, function(x) is.logical(x) && length(x) == 1L && !is.na(x), TRUE))) {
    p <- c(p, "invalid castling rights")
  }
  if (!is.list(ft) || !setequal(names(ft), .G3_COLORS)) p <- c(p, "invalid first-turn flags")
  if (!is.null(state$ep)) {
    e <- state$ep
    v <- match(e$victim_id, pc$id)
    if (is.na(v) || pc$type[v] != "P" || pc$square_id[v] != e$victim_square || pc$color[v] == state$side) {
      p <- c(p, "en-passant record does not match a pawn of the side that just moved")
    }
    if (!is.numeric(e$crossed_file) || !is.numeric(e$crossed_rank) ||
        e$crossed_file < 0 || e$crossed_file > 5 || e$crossed_rank < 0 || e$crossed_rank > 9) {
      p <- c(p, "en-passant coordinate out of range")
    }
  }
  if (!is.character(state$repetition$keys %||% character())) p <- c(p, "repetition keys must be character")
  oc <- state$outcome
  if (!is.list(oc) || !oc$status %in% c("ongoing", "terminal")) p <- c(p, "invalid outcome")
  if (!length(p) && mode == "playable") {
    core <- .g3_core(state, tabs)
    ctx <- .g3_ctx(core, tabs)
    if (.g3_in_check(core, 3L - core$side, ctx, tabs)) p <- c(p, "the side that just moved is in check")
  }
  p
}

# Human-readable action labels. Squares are named board:coordinate so that
# overlapping coordinates on different boards cannot be confused.
#' @noRd
.g3_action_labels <- function(core, m, tabs, ctx) {
  sqname <- function(s) paste0(tabs$board_ids[tabs$sq_board[s]], ":", .g3_coord_label(ctx$file[s], ctx$rank[s]))
  vapply(seq_len(nrow(m)), function(i) {
    a <- m[i, ]
    promo <- if (a[["promo"]] > 0L) paste0("=", .G3_TYPES[a[["promo"]]]) else ""
    if (a[["type"]] == 1L) {
      pc <- .G3_TYPES[core$ptype[a[["piece"]]]]
      sep <- if (a[["cap"]] > 0L) "x" else "-"
      paste0(pc, " ", sqname(a[["from"]]), sep, sqname(a[["to"]]), if (a[["flag"]] == 2L) " e.p." else "", promo)
    } else if (a[["type"]] == 2L) {
      pc <- .G3_TYPES[core$ptype[a[["piece"]]]]
      paste0(
        .G3_PLATFORMS[a[["plat"]]], "(", pc, ") to ", tabs$dock_ids[a[["dock"]]],
        if (a[["rot"]] == 180L) " turned" else "", promo
      )
    } else {
      if (a[["cside"]] == 1L) "O-O" else "O-O-O"
    }
  }, "")
}
