# Games: an initial state plus an ordered event log.
#
# Events are either board actions (`action`, which advance the ply) or
# session events that never move a piece: `resign`, `draw_agreed`,
# `truncated` (a computational cutoff, never a rule draw) and `takeback`
# (casual undo; it retracts the last action(s) of the active line but keeps
# them in the log). The event sequence number and the board ply are
# different counters. Replaying the initial state with the ordered events
# reconstructs the current state exactly.

.G3_SESSION_TYPES <- c("resign", "draw_agreed", "truncated", "takeback")

#' Record an action in a game
#'
#' Applies a legal action to the game's current state through
#' [g3_apply_action()] and appends the event to the game's log.
#'
#' @param game A `g3_game`.
#' @param action An action id, a one-row `g3_actions` table or a list with
#'   `action_id`.
#' @param expected_state_hash Optional revision token, see
#'   [g3_apply_action()].
#' @return The updated `g3_game`. The input is not modified.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' game$events[, c("sequence", "type", "ply", "action_id")]
#' @export
g3_game_apply <- function(game, action, expected_state_hash = NULL) {
  .g3_check_game(game)
  if (!is.na(game$metadata$result)) {
    .g3_abort("g3_illegal_action", paste0("The game has ended (", game$metadata$termination, ")."))
  }
  step <- g3_apply_action(game$state, action, expected_state_hash = expected_state_hash)
  ev <- step$event
  payload <- list(
    action = ev$action, notation = ev$notation, check = ev$check,
    pre_position_key = ev$pre_position_key, outcome = ev$outcome
  )
  game <- .g3_append_event(game, ev$event_id, "action", ev$actor, step$state$ply, ev$action_id,
                           ev$pre_state_hash, ev$post_state_hash, ev$position_key, payload)
  game$state <- step$state
  if (identical(step$state$outcome$status, "terminal")) {
    game$metadata$result <- step$state$outcome$result
    game$metadata$termination <- step$state$outcome$reason
  }
  game
}

#' Record a session event
#'
#' Session events change the game's status without moving a piece:
#' resignation, an agreed draw, a computational truncation (for example a
#' simulation ply cap, which is never a rule draw) or a casual takeback.
#'
#' @param game A `g3_game`.
#' @param type One of `"resign"`, `"draw_agreed"`, `"truncated"`,
#'   `"takeback"`.
#' @param actor `"white"`, `"black"` or `"system"`. Defaults to the side to
#'   act for `resign`, otherwise `"system"`.
#' @param reason Optional free-text reason (for example `"max_plies"`).
#' @param n For `takeback`, how many actions to retract (default 1).
#' @return The updated `g3_game`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' game <- gambitR3D::g3_game_event(game, "takeback")
#' game$state$ply
#' game <- gambitR3D::g3_game_event(game, "resign")
#' game$metadata$result
#' @export
g3_game_event <- function(game, type = .G3_SESSION_TYPES, actor = NULL, reason = NULL, n = 1L) {
  .g3_check_game(game)
  type <- match.arg(type)
  if (type != "takeback" && !is.na(game$metadata$result)) {
    .g3_abort("g3_illegal_action", "The game has already ended.")
  }
  actor <- actor %||% if (type == "resign") game$state$side else "system"
  if (!actor %in% c(.G3_COLORS, "system")) {
    .g3_abort("g3_illegal_action", "`actor` must be white, black or system.")
  }
  pre <- .g3_state_hash(game$state)
  payload <- list(reason = reason %||% NA_character_)
  if (type == "takeback") {
    n <- as.integer(n)
    active <- .g3_active_line(game$events)
    if (n < 1L || n > length(active)) {
      .g3_abort("g3_illegal_action", "Nothing to take back.")
    }
    payload$n <- n
    game$metadata$result <- NA_character_
    game$metadata$termination <- NA_character_
  }
  id <- substr(.g3_sha256(paste(pre, type, actor, nrow(game$events), sep = "|")), 1L, 20L)
  game <- .g3_append_event(game, id, type, actor, game$state$ply, NA_character_, pre, NA_character_,
                           NA_character_, payload)
  if (type == "takeback") {
    game$state <- g3_replay_game(game)
    post <- .g3_state_hash(game$state)
    game$events$post_state_hash[nrow(game$events)] <- post
    game$events$ply[nrow(game$events)] <- game$state$ply
    game$events$position_key[nrow(game$events)] <- game$state$repetition$keys[length(game$state$repetition$keys)]
  } else {
    game$events$post_state_hash[nrow(game$events)] <- pre
    game$metadata$result <- switch(
      type,
      resign = if (actor == "white") "black" else if (actor == "black") "white" else NA_character_,
      draw_agreed = "draw",
      truncated = "truncated"
    )
    game$metadata$termination <- type
  }
  game
}

#' @noRd
.g3_append_event <- function(game, id, type, actor, ply, action_id, pre, post, key, payload) {
  ev <- game$events
  row <- data.frame(
    event_id = id, sequence = nrow(ev) + 1L, type = type, actor = actor,
    ply = as.integer(ply), action_id = action_id, pre_state_hash = pre,
    post_state_hash = post, position_key = key,
    payload = .g3_canonical_json(payload), stringsAsFactors = FALSE
  )
  game$events <- rbind(ev, row)
  rownames(game$events) <- NULL
  game
}

# Indices of the action events in the active line, honouring takebacks.
#' @noRd
.g3_active_line <- function(events) {
  line <- integer()
  for (i in seq_len(nrow(events))) {
    if (events$type[i] == "action") {
      line <- c(line, i)
    } else if (events$type[i] == "takeback") {
      n <- jsonlite::fromJSON(events$payload[i])$n
      line <- line[seq_len(max(0L, length(line) - n))]
    }
  }
  line
}

#' Replay a game
#'
#' Reconstructs the state at a given ply by replaying the game's active
#' line of actions from its initial state through [g3_apply_action()].
#'
#' @param game A `g3_game`.
#' @param ply Board ply to stop at (0 is the initial position). `NULL`
#'   means after the last committed action.
#' @return A `g3_state`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' gambitR3D::g3_replay_game(game, ply = 0L)$ply
#' @export
g3_replay_game <- function(game, ply = NULL) {
  .g3_check_game(game)
  line <- .g3_active_line(game$events)
  target <- if (is.null(ply)) length(line) else as.integer(ply) - game$initial_state$ply
  if (length(target) != 1L || is.na(target) || target < 0L || target > length(line)) {
    .g3_abort("g3_invalid_state", paste0("Ply ", ply, " is outside the game (0..", length(line), ")."))
  }
  st <- game$initial_state
  for (i in line[seq_len(target)]) {
    st <- g3_apply_action(st, game$events$action_id[i])$state
  }
  st
}

#' Validate a game
#'
#' Checks the initial state, replays every action event, and compares the
#' recorded state checksums and repetition keys with the replay. The first
#' mismatch is reported with its event number.
#'
#' @param game A `g3_game`.
#' @param error If `TRUE` (default) raise a `g3_invalid_state` or
#'   `g3_illegal_action` condition on the first problem; if `FALSE` return
#'   `list(valid, problems, first_invalid_event)`.
#' @return Invisibly `TRUE`, or the diagnostic list when `error = FALSE`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' gambitR3D::g3_validate_game(game)
#' @export
g3_validate_game <- function(game, error = TRUE) {
  res <- .g3_game_problems(game)
  if (error) {
    if (length(res$problems)) {
      .g3_abort("g3_invalid_state", paste0("Invalid game: ", paste(res$problems, collapse = "; ")),
                first_invalid_event = res$first_invalid_event)
    }
    return(invisible(TRUE))
  }
  list(valid = !length(res$problems), problems = res$problems, first_invalid_event = res$first_invalid_event)
}

#' @noRd
.g3_game_problems <- function(game) {
  out <- function(p, i = NA_integer_) list(problems = p, first_invalid_event = i)
  if (!inherits(game, "g3_game")) return(out("not a g3_game"))
  v <- g3_validate_state(game$initial_state, error = FALSE)
  if (!v$valid) return(out(paste("initial state:", v$problems)))
  ev <- game$events
  if (!is.data.frame(ev)) return(out("events must be a data frame"))
  if (nrow(ev) && !identical(ev$sequence, seq_len(nrow(ev)))) return(out("event sequence must be 1..n"))
  bad_type <- !ev$type %in% c("action", .G3_SESSION_TYPES)
  if (any(bad_type)) return(out(paste("unknown event type", ev$type[which(bad_type)[1L]]), which(bad_type)[1L]))
  stack <- list(game$initial_state)
  ended <- FALSE
  for (i in seq_len(nrow(ev))) {
    cur <- stack[[length(stack)]]
    if (ev$type[i] == "action") {
      if (ended) return(out(paste0("event ", i, ": action after the game ended"), i))
      if (!identical(ev$pre_state_hash[i], .g3_state_hash(cur))) {
        return(out(paste0("event ", i, ": pre-state checksum does not match the replay"), i))
      }
      step <- tryCatch(g3_apply_action(cur, ev$action_id[i]), g3_error = function(e) e)
      if (inherits(step, "error")) return(out(paste0("event ", i, ": ", conditionMessage(step)), i))
      if (!identical(ev$post_state_hash[i], step$event$post_state_hash) ||
          !identical(ev$position_key[i], step$event$position_key)) {
        return(out(paste0("event ", i, ": post-state checksum or key does not match the replay"), i))
      }
      if (!identical(ev$ply[i], step$state$ply)) return(out(paste0("event ", i, ": ply mismatch"), i))
      stack[[length(stack) + 1L]] <- step$state
      ended <- identical(step$state$outcome$status, "terminal")
    } else if (ev$type[i] == "takeback") {
      n <- tryCatch(jsonlite::fromJSON(ev$payload[i])$n, error = function(e) NA)
      if (!is.numeric(n) || is.na(n) || n < 1L || n >= length(stack)) {
        return(out(paste0("event ", i, ": invalid takeback"), i))
      }
      stack <- stack[seq_len(length(stack) - n)]
      ended <- FALSE
    } else {
      if (ended) return(out(paste0("event ", i, ": session event after the game ended"), i))
      ended <- TRUE
    }
  }
  if (!identical(.g3_state_hash(stack[[length(stack)]]), .g3_state_hash(game$state))) {
    return(out("current state does not match the replayed events"))
  }
  out(character())
}

#' @noRd
.g3_check_game <- function(game) {
  if (!inherits(game, "g3_game")) {
    .g3_abort("g3_invalid_state", "Expected a `g3_game` (see `g3_new_game()`).")
  }
  invisible(TRUE)
}

#' Moves of a game as a table
#'
#' @param game A `g3_game`.
#' @return A data frame with one row per action of the active line:
#'   `ply`, `actor`, `action_id`, `type`, `notation`, `piece_id`,
#'   `capture_id`, `promotion`, `check`, `position_key`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' gambitR3D::g3_game_moves(game)
#' @export
g3_game_moves <- function(game) {
  .g3_check_game(game)
  line <- .g3_active_line(game$events)
  if (!length(line)) {
    return(data.frame(
      ply = integer(), actor = character(), action_id = character(), type = character(),
      notation = character(), piece_id = character(), capture_id = character(),
      promotion = character(), check = logical(), position_key = character(),
      stringsAsFactors = FALSE
    ))
  }
  ev <- game$events[line, ]
  pl <- lapply(ev$payload, jsonlite::fromJSON)
  na_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  data.frame(
    ply = ev$ply, actor = ev$actor, action_id = ev$action_id,
    type = vapply(pl, function(p) na_chr(p$action$type), ""),
    notation = vapply(pl, function(p) na_chr(p$notation), ""),
    piece_id = vapply(pl, function(p) na_chr(p$action$piece_id), ""),
    capture_id = vapply(pl, function(p) na_chr(p$action$capture_id), ""),
    promotion = vapply(pl, function(p) na_chr(p$action$promotion), ""),
    check = vapply(pl, function(p) isTRUE(p$check), TRUE),
    position_key = ev$position_key,
    stringsAsFactors = FALSE
  )
}
