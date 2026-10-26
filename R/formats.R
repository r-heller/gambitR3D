# Native interchange format.
#
# A game file is UTF-8 JSON with the envelope `gambitR3D-game`, version 1:
#
#   format, format_version, schema_version, action_schema,
#   rules {version, hash}, game_id, metadata, initial_state, events,
#   final {ply, state_hash, position_key}, provenance, annotations, ui
#
# `ui` holds presentation preferences (2D/3D, Neutral height, camera). It is
# optional and never read by the engine: stripping it leaves every key,
# legal action and result unchanged. Hashes are always computed from the
# canonical JSON form (sorted keys, no whitespace), so indentation and key
# order in a file never matter. This is not FEN or PGN: those formats cannot
# describe platforms or overlapping physical squares.

.G3_GAME_FORMAT <- "gambitR3D-game"
.G3_GAME_FORMAT_VERSION <- 1L
.G3_POSITION_FORMAT <- "gambitR3D-position"
.G3_MAX_FILE_BYTES <- 50 * 1024^2
.G3_MAX_EVENTS <- 20000L
.G3_MAX_DEPTH <- 32L

# Plain-list form of a game for JSON.
#' @noRd
.g3_game_to_list <- function(game, ui = NULL) {
  ev <- game$events
  events <- lapply(seq_len(nrow(ev)), function(i) {
    e <- as.list(ev[i, , drop = FALSE])
    e$payload <- jsonlite::fromJSON(e$payload, simplifyVector = FALSE)
    e
  })
  meta <- game$metadata
  list(
    format = .G3_GAME_FORMAT,
    format_version = .G3_GAME_FORMAT_VERSION,
    schema_version = game$state$schema_version,
    action_schema = .G3_ACTION_SCHEMA,
    rules = list(version = game$state$rules_version, hash = game$state$rules_hash),
    game_id = meta$game_id,
    metadata = meta[setdiff(names(meta), c("game_id", "rules_version", "rules_hash", "action_schema"))],
    initial_state = .g3_state_plain(game$initial_state),
    events = events,
    final = list(
      ply = game$state$ply,
      state_hash = .g3_state_hash(game$state),
      position_key = game$state$repetition$keys[length(game$state$repetition$keys)]
    ),
    provenance = game$provenance %||% list(),
    annotations = game$annotations %||% list(),
    ui = ui %||% game$ui %||% NULL
  )
}

#' Canonical content hash of a game
#'
#' SHA-256 over the rules identity, the canonical initial state and the
#' ordered events (actions and session events, with their payloads).
#' Metadata, annotations, provenance, presentation preferences and file
#' formatting are excluded, so the same game saved twice has the same
#' hash, while a resignation and an agreed draw after identical moves
#' differ.
#'
#' @param game A `g3_game`.
#' @return One lowercase hexadecimal string.
#' @examples
#' gambitR3D::g3_game_hash(gambitR3D::g3_new_game())
#' @export
g3_game_hash <- function(game) {
  .g3_check_game(game)
  ev <- game$events
  core_ev <- lapply(seq_len(nrow(ev)), function(i) {
    list(type = ev$type[i], actor = ev$actor[i], action_id = ev$action_id[i],
         post_state_hash = ev$post_state_hash[i])
  })
  .g3_hash_object(list(
    rules_hash = game$state$rules_hash,
    initial_state_hash = .g3_state_hash(game$initial_state),
    events = core_ev
  ))
}

#' Write a game file
#'
#' Saves a game in the native JSON format with an atomic write (a
#' temporary file renamed into place), so an interrupted save never leaves
#' a partial file at `path`.
#'
#' @param game A `g3_game`.
#' @param path Output file path (conventionally `.g3.json`).
#' @param overwrite Allow replacing an existing file.
#' @param ui Optional presentation preferences to store in the separate
#'   `ui` envelope, for example `list(view = "3d", neutral_height =
#'   "lowest")`.
#' @return Invisibly, the output path.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' path <- base::tempfile(fileext = ".g3.json")
#' gambitR3D::g3_write_game(game, path)
#' back <- gambitR3D::g3_read_game(path)
#' base::identical(gambitR3D::g3_game_hash(back), gambitR3D::g3_game_hash(game))
#' @export
g3_write_game <- function(game, path, overwrite = FALSE, ui = NULL) {
  .g3_check_game(game)
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    .g3_abort("g3_io_error", "`path` must be one file path.")
  }
  if (file.exists(path) && !overwrite) {
    .g3_abort("g3_io_error", paste0("File exists: ", path, " (use overwrite = TRUE)."))
  }
  txt <- jsonlite::toJSON(
    .g3_game_to_list(game, ui), auto_unbox = TRUE, pretty = TRUE, digits = NA,
    null = "null", na = "null"
  )
  .g3_atomic_write(as.character(txt), path)
}

#' Read a game file
#'
#' Reads and validates a native game file: bounded size and nesting,
#' format and rules identity, the initial state, and a full replay of every
#' action through the rules engine with checksum comparison. Nothing in the
#' file is evaluated as code.
#'
#' @param path Path to a `.g3.json` file.
#' @param strict If `TRUE` (default), any checksum mismatch is an error.
#'   If `FALSE`, recorded checksums are ignored but every action must still
#'   be legal.
#' @return A validated `g3_game`. The file's SHA-256 and path are recorded
#'   in `game$provenance$source`.
#' @examples
#' game <- gambitR3D::g3_new_game()
#' path <- base::tempfile(fileext = ".g3.json")
#' gambitR3D::g3_write_game(game, path)
#' gambitR3D::g3_read_game(path)$state$ply
#' @export
g3_read_game <- function(path, strict = TRUE) {
  raw <- .g3_read_json_file(path)
  game <- .g3_game_from_list(raw, strict = strict)
  game$provenance$source <- list(
    sha256 = digest::digest(file = path, algo = "sha256"),
    file = basename(path)
  )
  game
}

#' @noRd
.g3_read_json_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .g3_abort("g3_io_error", paste0("File not found: ", path))
  }
  size <- file.info(path)$size
  if (is.na(size) || size > .G3_MAX_FILE_BYTES) {
    .g3_abort("g3_io_error", "File is larger than the 50 MB limit.", bytes = size)
  }
  txt <- readChar(path, size, useBytes = TRUE)
  .g3_parse_json(txt)
}

#' @noRd
.g3_parse_json <- function(txt) {
  if (!validUTF8(txt)) .g3_abort("g3_io_error", "File is not valid UTF-8.")
  raw <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) .g3_abort("g3_io_error", paste0("Not valid JSON: ", conditionMessage(e)))
  )
  if (.g3_depth(raw) > .G3_MAX_DEPTH) .g3_abort("g3_io_error", "JSON nesting is too deep.")
  raw
}

#' @noRd
.g3_depth <- function(x) {
  if (!is.list(x) || !length(x)) return(1L)
  1L + max(vapply(x, .g3_depth, 1L))
}

# Build a validated game from a parsed list.
#' @noRd
.g3_game_from_list <- function(raw, strict = TRUE) {
  bad <- function(msg, ...) .g3_abort("g3_invalid_state", paste0("Invalid game record: ", msg), ...)
  if (!is.list(raw) || !identical(raw$format, .G3_GAME_FORMAT)) bad("not a gambitR3D game file.")
  raw <- .g3_migrate_game(raw)
  tabs <- .g3_tables()
  if (!identical(raw$rules$version, tabs$rules_version) || !identical(raw$rules$hash, tabs$rules_hash)) {
    .g3_abort(
      "g3_unsupported_rules",
      paste0("The game uses rules ", format(raw$rules$version), " with a different hash; ",
             "it cannot be replayed under the installed profile."),
      rules_hash = raw$rules$hash
    )
  }
  if (!identical(raw$action_schema, .G3_ACTION_SCHEMA)) bad("unknown action schema.")
  if (length(raw$events) > .G3_MAX_EVENTS) bad("too many events.")
  init <- .g3_state_from_list(raw$initial_state)
  meta <- raw$metadata %||% list()
  meta$players <- meta$players %||% list(white = "White", black = "Black")
  game <- .g3_game_new(init, players = meta$players, game_id = raw$game_id %||% .g3_new_id("game"),
                       metadata = meta[setdiff(names(meta), "players")])
  game$metadata$result <- NA_character_
  game$metadata$termination <- NA_character_
  for (i in seq_along(raw$events)) {
    e <- raw$events[[i]]
    if (!is.list(e) || is.null(e$type)) bad(paste0("event ", i, " is malformed."), first_invalid_event = i)
    if (e$type == "action") {
      if (strict && !identical(e$pre_state_hash, .g3_state_hash(game$state))) {
        bad(paste0("event ", i, ": pre-state checksum mismatch."), first_invalid_event = i,
            expected_key = game$state$repetition$keys[length(game$state$repetition$keys)])
      }
      game <- tryCatch(
        g3_game_apply(game, e$action_id),
        g3_error = function(err) {
          bad(paste0("event ", i, " (ply ", game$state$ply + 1L, "): ", conditionMessage(err)),
              first_invalid_event = i)
        }
      )
      last <- nrow(game$events)
      if (strict && (!identical(e$post_state_hash, game$events$post_state_hash[last]) ||
                     !identical(e$position_key, game$events$position_key[last]))) {
        bad(paste0("event ", i, ": post-state checksum mismatch."), first_invalid_event = i)
      }
    } else if (e$type %in% .G3_SESSION_TYPES) {
      pl <- e$payload %||% list()
      reason <- if (is.null(pl$reason)) NULL else as.character(pl$reason)
      game <- tryCatch(
        g3_game_event(game, e$type, actor = e$actor, reason = reason, n = pl$n %||% 1L),
        g3_error = function(err) bad(paste0("event ", i, ": ", conditionMessage(err)), first_invalid_event = i)
      )
    } else {
      bad(paste0("event ", i, " has unknown type '", e$type, "'."), first_invalid_event = i)
    }
  }
  if (strict && !is.null(raw$final$state_hash) && !identical(raw$final$state_hash, .g3_state_hash(game$state))) {
    bad("final state checksum mismatch.")
  }
  game$provenance <- raw$provenance %||% list()
  game$annotations <- raw$annotations %||% list()
  game$ui <- raw$ui
  game
}

# Schema migrations: a list of functions keyed by the version they upgrade
# from. Version 1 is current; a newer file is refused with a clear message.
#' @noRd
.g3_migrate_game <- function(raw) {
  v <- raw$format_version
  if (!is.numeric(v) || length(v) != 1L) {
    .g3_abort("g3_invalid_state", "Invalid game record: missing format_version.")
  }
  if (v > .G3_GAME_FORMAT_VERSION) {
    .g3_abort("g3_unsupported_rules",
              paste0("This file uses game format version ", v, "; this version of gambitR3D reads up to ",
                     .G3_GAME_FORMAT_VERSION, ". Update the package to read it."))
  }
  migrations <- list()
  while (raw$format_version < .G3_GAME_FORMAT_VERSION) {
    f <- migrations[[as.character(raw$format_version)]]
    if (is.null(f)) .g3_abort("g3_invalid_state", "No migration path for this game format version.")
    raw <- f(raw)
  }
  raw
}

# Rebuild a g3_state from its plain-list JSON form, with validation.
#' @noRd
.g3_state_from_list <- function(x) {
  bad <- function(msg) .g3_abort("g3_invalid_state", paste0("Invalid state record: ", msg))
  if (!is.list(x)) bad("not an object.")
  chr <- function(v) if (is.null(v)) NA_character_ else as.character(v)
  num_ok <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v)
  boards <- x$boards
  pieces <- x$pieces
  if (!is.list(boards) || length(boards) != 7L) bad("seven boards required.")
  if (!is.list(pieces) || length(pieces) > 64L) bad("pieces must be a list of at most 64.")
  bdf <- data.frame(
    id = vapply(boards, function(b) chr(b$id), ""),
    type = vapply(boards, function(b) chr(b$type), ""),
    dock = vapply(boards, function(b) chr(b$dock), ""),
    orientation = vapply(boards, function(b) {
      if (!num_ok(b$orientation)) bad("orientation must be a finite number.")
      as.integer(b$orientation)
    }, 0L),
    stringsAsFactors = FALSE
  )
  pdf <- data.frame(
    id = vapply(pieces, function(p) chr(p$id), ""),
    type = vapply(pieces, function(p) chr(p$type), ""),
    origin_type = vapply(pieces, function(p) chr(p$origin_type), ""),
    color = vapply(pieces, function(p) chr(p$color), ""),
    square_id = vapply(pieces, function(p) chr(p$square_id), ""),
    has_moved = vapply(pieces, function(p) isTRUE(p$has_moved), TRUE),
    stringsAsFactors = FALSE
  )
  for (f in c("ply")) if (!num_ok(x[[f]])) bad(paste(f, "must be a finite number."))
  if (!num_ok(x$clocks$halfmove)) bad("halfmove must be a finite number.")
  ep <- x$ep
  if (!is.null(ep)) {
    if (!num_ok(ep$crossed_file) || !num_ok(ep$crossed_rank)) bad("en-passant coordinates.")
    ep <- list(victim_id = chr(ep$victim_id), victim_square = chr(ep$victim_square),
               crossed_file = as.integer(ep$crossed_file), crossed_rank = as.integer(ep$crossed_rank))
  }
  cr <- x$rights$castling
  ft <- x$rights$first_turn_completed
  oc <- x$outcome %||% list()
  state <- structure(
    list(
      schema_version = chr(x$schema_version),
      rules_version = chr(x$rules_version),
      rules_hash = chr(x$rules_hash),
      boards = bdf,
      pieces = pdf,
      side = chr(x$side),
      rights = list(
        castling = lapply(cr[.G3_CASTLE_NAMES], isTRUE),
        first_turn_completed = list(white = isTRUE(ft$white), black = isTRUE(ft$black))
      ),
      ep = ep,
      ply = as.integer(x$ply),
      clocks = list(halfmove = as.integer(x$clocks$halfmove)),
      repetition = list(keys = as.character(unlist(x$repetition$keys))),
      outcome = list(status = chr(oc$status), result = chr(oc$result), reason = chr(oc$reason))
    ),
    class = "g3_state"
  )
  if (!setequal(names(cr), .G3_CASTLE_NAMES)) bad("castling rights.")
  g3_validate_state(state)
  # The recorded outcome must agree with the rules.
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  if (!length(core$rep) || !identical(core$rep[length(core$rep)], .g3_key_core(core, tabs))) {
    bad("repetition record does not end with this position's key.")
  }
  expect <- .g3_outcome_core(core, tabs)
  if (!identical(expect$status, state$outcome$status) || !identical(expect$reason, state$outcome$reason)) {
    bad("recorded outcome disagrees with the rules.")
  }
  state
}

#' Write and read single positions
#'
#' A position file (`gambitR3D-position`, version 1) stores one state for
#' analysis or puzzles. It carries the state's repetition record; a
#' position set up by hand has no earlier history, which the file marks
#' with `history = "edited"`.
#'
#' @param state A `g3_state`.
#' @param path File path.
#' @param overwrite Allow replacing an existing file.
#' @param history `"recorded"` if the state comes from a played game,
#'   `"edited"` if it was set up by hand.
#' @return `g3_write_position()` returns the path invisibly;
#'   `g3_read_position()` returns a validated `g3_state` with attribute
#'   `history`.
#' @examples
#' st <- gambitR3D::g3_new_game()$state
#' path <- base::tempfile(fileext = ".g3pos.json")
#' gambitR3D::g3_write_position(st, path)
#' gambitR3D::g3_position_key(gambitR3D::g3_read_position(path)) ==
#'   gambitR3D::g3_position_key(st)
#' @export
g3_write_position <- function(state, path, overwrite = FALSE, history = c("recorded", "edited")) {
  .g3_check_state(state)
  history <- match.arg(history)
  if (file.exists(path) && !overwrite) {
    .g3_abort("g3_io_error", paste0("File exists: ", path, " (use overwrite = TRUE)."))
  }
  x <- list(
    format = .G3_POSITION_FORMAT, format_version = 1L, history = history,
    state = .g3_state_plain(state), state_hash = .g3_state_hash(state)
  )
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
  .g3_atomic_write(as.character(txt), path)
}

#' @rdname g3_write_position
#' @export
g3_read_position <- function(path) {
  raw <- .g3_read_json_file(path)
  if (!identical(raw$format, .G3_POSITION_FORMAT) || !identical(raw$format_version, 1L)) {
    .g3_abort("g3_invalid_state", "Not a gambitR3D position file (version 1).")
  }
  st <- .g3_state_from_list(raw$state)
  if (!identical(raw$state_hash, .g3_state_hash(st))) {
    .g3_abort("g3_invalid_state", "Position checksum mismatch.")
  }
  attr(st, "history") <- raw$history %||% "recorded"
  st
}
