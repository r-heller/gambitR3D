# Session game store, the single action dispatcher and the storage service.
#
# Every Shiny session owns one live store (and, separately, at most one
# analysis store). A store is a plain environment holding a `g3_game`, a
# session revision counter, the acknowledgements of recent requests and the
# last save receipt. Nothing else may assign `store$game`: every change -
# a board action, a session event (resign, agreed draw, takeback), a new
# game or a loaded game - goes through `.g3_dispatch()`:
#
#   1. a request id seen before returns its original acknowledgement and
#      changes nothing (idempotent duplicates);
#   2. a request for another game id or an older revision, or whose expected
#      state hash differs from the current one, is rejected unchanged;
#   3. the engine computes the next game (`g3_game_apply()`,
#      `g3_game_event()`), and the result is validated;
#   4. the next game is saved durably (atomic file write, or the game
#      database when it is configured and available);
#   5. only then is it committed: the revision increments and the
#      acknowledgement is recorded and returned.
#
# If step 3 or 4 fails the store keeps its previous game and revision, so a
# move that was not saved is never acknowledged. Analysis stores have no
# saver: they are explicit unsaved branches. Shiny is single-threaded per
# process, so calls for one store are serialised; the revision check makes
# delayed requests (for example a late computer reply) harmless.

.G3_ACK_MEMORY <- 500L

#' @noRd
.g3_new_store <- function(game, kind = c("live", "analysis"), saver = NULL) {
  kind <- match.arg(kind)
  store <- new.env(parent = emptyenv())
  store$kind <- kind
  store$game <- game
  store$revision <- 0L
  store$saver <- saver
  store$acks <- new.env(hash = TRUE, parent = emptyenv())
  store$ack_order <- character()
  store$receipt <- NULL
  store$last <- NULL
  store$commits <- 0L
  store
}

# Current revision token sent to the browser with every state.
#' @noRd
.g3_store_token <- function(store) {
  list(
    game_id = store$game$metadata$game_id,
    revision = store$revision,
    state_hash = .g3_state_hash(store$game$state)
  )
}

#' @noRd
.g3_ack <- function(req, status, reason = NA_character_, message = NA_character_, store = NULL) {
  list(
    request_id = as.character(req$request_id %||% NA_character_),
    status = status,
    reason = reason,
    message = message,
    revision = if (is.null(store)) NA_integer_ else store$revision,
    game_id = if (is.null(store)) NA_character_ else store$game$metadata$game_id
  )
}

#' @noRd
.g3_remember_ack <- function(store, ack) {
  id <- ack$request_id
  if (is.na(id) || !nzchar(id)) return(invisible(ack))
  assign(id, ack, envir = store$acks)
  store$ack_order <- c(store$ack_order, id)
  if (length(store$ack_order) > .G3_ACK_MEMORY) {
    drop <- store$ack_order[seq_len(length(store$ack_order) - .G3_ACK_MEMORY)]
    rm(list = drop, envir = store$acks)
    store$ack_order <- store$ack_order[-seq_along(drop)]
  }
  invisible(ack)
}

# Normalise a request from the browser (a JSON object) or from R.
#' @noRd
.g3_request <- function(req) {
  if (!is.list(req)) return(NULL)
  scalar <- function(x) if (is.null(x) || length(x) != 1L) NULL else x
  out <- list(
    kind = as.character(scalar(req$kind) %||% "action"),
    game_id = scalar(req$game_id),
    revision = suppressWarnings(as.integer(scalar(req$revision) %||% NA)),
    request_id = as.character(scalar(req$request_id) %||% NA_character_),
    expected_state_hash = scalar(req$expected_state_hash),
    action_id = scalar(req$action_id),
    event = scalar(req$event),
    n = suppressWarnings(as.integer(scalar(req$n) %||% 1L)),
    actor = scalar(req$actor),
    game = req$game
  )
  out
}

#' Dispatch one request against a store
#'
#' @param store A store from `.g3_new_store()`.
#' @param req A list: `kind` (`"action"`, `"event"` or `"replace"`),
#'   `game_id`, `revision`, `request_id`, `expected_state_hash`, and
#'   `action_id` (actions), `event` and `n` (events: `"resign"`,
#'   `"draw_agreed"`, `"takeback"`), `actor`, or `game` (replace: a
#'   validated `g3_game` built by the server, never by the browser).
#' @return An acknowledgement list: `request_id`, `status` (`"committed"`,
#'   `"duplicate"` or `"rejected"`), `reason` (`NA` or one of
#'   `"invalid_request"`, `"wrong_game"`, `"stale_revision"`,
#'   `"stale_state"`, `"illegal_action"`, `"game_over"`, `"save_failed"`),
#'   `message`, `revision` and `game_id` after the call.
#' @noRd
.g3_dispatch <- function(store, req) {
  req <- .g3_request(req)
  if (is.null(req) || is.na(req$request_id) || !nzchar(req$request_id)) {
    return(.g3_ack(req %||% list(), "rejected", "invalid_request", "A request needs a request id.", store))
  }
  seen <- get0(req$request_id, envir = store$acks, inherits = FALSE)
  if (!is.null(seen)) {
    # Idempotent: report the original outcome, change nothing.
    seen$original_status <- seen$status
    seen$status <- "duplicate"
    return(seen)
  }
  reject <- function(reason, message) {
    .g3_remember_ack(store, .g3_ack(req, "rejected", reason, message, store))
  }
  game <- store$game
  if (!identical(as.character(req$game_id), game$metadata$game_id)) {
    return(reject("wrong_game", "This request belongs to another game."))
  }
  if (is.na(req$revision) || req$revision != store$revision) {
    return(reject("stale_revision", "The game changed since this request was made."))
  }
  if (!is.null(req$expected_state_hash) && !identical(req$expected_state_hash, .g3_state_hash(game$state))) {
    return(reject("stale_state", "The position changed since this request was made."))
  }
  next_game <- tryCatch(
    switch(req$kind,
      action = {
        if (is.null(req$action_id)) .g3_abort("g3_illegal_action", "No action was given.")
        g3_game_apply(game, as.character(req$action_id), expected_state_hash = .g3_state_hash(game$state))
      },
      event = {
        type <- as.character(req$event %||% "")
        if (!type %in% c("resign", "draw_agreed", "takeback")) {
          .g3_abort("g3_invalid_state", "Unknown session event.")
        }
        g3_game_event(game, type, actor = req$actor, n = if (type == "takeback") req$n else 1L)
      },
      replace = {
        if (!inherits(req$game, "g3_game")) .g3_abort("g3_invalid_state", "No game to load.")
        req$game
      },
      .g3_abort("g3_invalid_state", "Unknown request kind.")
    ),
    g3_error = function(e) e
  )
  if (inherits(next_game, "error")) {
    reason <- if (inherits(next_game, "g3_illegal_action")) {
      if (!is.na(game$metadata$result)) "game_over" else "illegal_action"
    } else if (inherits(next_game, "g3_stale_revision")) {
      "stale_state"
    } else {
      "invalid_request"
    }
    return(reject(reason, conditionMessage(next_game)))
  }
  # The engine built `next_game`; validating its state is cheap and catches a
  # broken invariant before anything is saved. (Loaded games were already
  # fully replayed by g3_read_game(); replaying every game on every move
  # would make each commit cost grow with the game's length.)
  valid <- g3_validate_state(next_game$state, error = FALSE)
  if (!isTRUE(valid$valid)) {
    return(reject("invalid_request", paste(valid$problems, collapse = "; ")))
  }
  # Durable save before commit. A failed save leaves the store untouched.
  receipt <- NULL
  if (is.function(store$saver)) {
    receipt <- tryCatch(store$saver(next_game), error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    })
    if (!isTRUE(receipt$ok)) {
      return(reject("save_failed", paste0("The game could not be saved, so the change was not made: ",
                                          receipt$message %||% "unknown error")))
    }
  }
  store$game <- next_game
  store$revision <- store$revision + 1L
  store$commits <- store$commits + 1L
  store$receipt <- receipt
  store$last <- list(kind = req$kind, request_id = req$request_id, event = req$event %||% NA_character_)
  .g3_remember_ack(store, .g3_ack(req, "committed", store = store))
}

# A server-side request built from the store's current token.
#' @noRd
.g3_store_request <- function(store, kind, ..., request_id = .g3_new_id("srv")) {
  tok <- .g3_store_token(store)
  c(list(kind = kind, game_id = tok$game_id, revision = tok$revision,
         request_id = request_id, expected_state_hash = tok$state_hash), list(...))
}

# ---------------------------------------------------------------- storage

#' Storage service for app games
#'
#' Keeps file input/output behind one small interface so UI modules never
#' invent storage formats. Games are saved in the native format with
#' `g3_write_game()` (atomic temporary write and rename) under
#' `<root>/games/<game_id>.g3.json`. When `db_path` is given and the game
#' database API (`g3_open_game_db()`, `g3_save_game()`) is present in this
#' build, games are saved to the database instead.
#'
#' @param root Storage folder; default `tools::R_user_dir("gambitR3D",
#'   "data")`.
#' @param db_path Optional SQLite database path.
#' @return A list of functions and fields: `root`, `games_dir`, `backend`
#'   (`"file"` or `"db"`), `note`, `save(game, ui = NULL)` (returns a
#'   receipt `list(ok, backend, path, time, message)`), `list_games()`
#'   (newest first; database games have paths `"db:<game_id>"`),
#'   `read(path)` (a file path or a `"db:"` path) and `close()`.
#' @noRd
.g3_storage_service <- function(root = NULL, db_path = NULL) {
  root <- normalizePath(root %||% tools::R_user_dir("gambitR3D", which = "data"), mustWork = FALSE)
  games_dir <- file.path(root, "games")
  ns <- asNamespace("gambitR3D")
  db_open <- get0("g3_open_game_db", envir = ns, inherits = FALSE)
  db_save <- get0("g3_save_game", envir = ns, inherits = FALSE)
  db_close <- get0("g3_close_game_db", envir = ns, inherits = FALSE)
  backend <- "file"
  note <- NA_character_
  db <- NULL
  if (!is.null(db_path)) {
    if (is.function(db_open) && is.function(db_save)) {
      db <- tryCatch(db_open(db_path), error = function(e) {
        note <<- paste0("The game database could not be opened (", conditionMessage(e), "); saving to files.")
        NULL
      })
      if (!is.null(db)) backend <- "db"
    } else {
      note <- "The game database API is not available in this build; games are saved as files."
    }
  }
  save_file <- function(game, ui = NULL) {
    if (!dir.exists(games_dir) && !dir.create(games_dir, recursive = TRUE, showWarnings = FALSE)) {
      .g3_abort("g3_io_error", paste0("Cannot create the storage folder ", games_dir))
    }
    path <- file.path(games_dir, paste0(game$metadata$game_id, ".g3.json"))
    g3_write_game(game, path, overwrite = TRUE, ui = ui)
    list(ok = TRUE, backend = "file", path = path, time = .g3_now(), message = NA_character_)
  }
  save <- function(game, ui = NULL) {
    if (backend == "db") {
      # g3_save_game() returns only after its transaction has committed.
      res <- db_save(db, game, source = "app")
      return(list(ok = TRUE, backend = "db", path = db_path, time = res$saved_at %||% .g3_now(),
                  message = NA_character_, db_status = res$status))
    }
    save_file(game, ui)
  }
  list_games_empty <- function() {
    data.frame(game_id = character(), path = character(), modified = as.POSIXct(character()),
               ply = integer(), result = character(), mode = character(), white = character(),
               black = character(), stringsAsFactors = FALSE)
  }
  list_db <- function() {
    q <- get("g3_query_games", envir = ns)(db, filters = list(origin = "app"), limit = 50L)
    if (!nrow(q)) return(list_games_empty())
    data.frame(
      game_id = q$game_id, path = paste0("db:", q$game_id),
      modified = as.POSIXct(sub("Z$", "", q$updated_at), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      ply = as.integer(q$final_ply), result = as.character(q$result), mode = NA_character_,
      white = as.character(q$white), black = as.character(q$black), stringsAsFactors = FALSE
    )
  }
  read <- function(path) {
    if (backend == "db" && startsWith(path, "db:")) {
      return(get("g3_load_game", envir = ns)(db, substring(path, 4L)))
    }
    .g3_app_read_file(path)
  }
  list_games <- function() {
    if (backend == "db") return(list_db())
    files <- if (dir.exists(games_dir)) list.files(games_dir, pattern = "\\.g3\\.json$", full.names = TRUE) else character()
    if (!length(files)) return(list_games_empty())
    rows <- lapply(files, function(f) {
      x <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(x) || !identical(x$format, .G3_GAME_FORMAT)) return(NULL)
      m <- x$metadata %||% list()
      data.frame(
        game_id = as.character(x$game_id %||% NA), path = f, modified = file.info(f)$mtime,
        ply = as.integer(x$final$ply %||% NA), result = as.character(m$result %||% NA),
        mode = as.character(m$mode %||% "hotseat"),
        white = as.character(m$players$white %||% "White"), black = as.character(m$players$black %||% "Black"),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows[!vapply(rows, is.null, TRUE)])
    if (is.null(out)) return(list_games_empty())
    out <- out[order(out$modified, decreasing = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    out
  }
  close <- function() {
    if (!is.null(db) && is.function(db_close)) try(db_close(db), silent = TRUE)
    invisible(TRUE)
  }
  list(root = root, games_dir = games_dir, backend = backend, note = note,
       save = save, list_games = list_games, read = read, close = close)
}

#' Read a game or position file for the app
#'
#' Detects the native format and validates the content: a game file is read
#' with `g3_read_game()` (full replay and checksums); a position file with
#' `g3_read_position()` and wrapped in a new game starting from it.
#'
#' @param path File path.
#' @return A validated `g3_game`. Errors are `g3_*` conditions; an
#'   unsupported format or rules version raises `g3_unsupported_rules`.
#' @noRd
.g3_app_read_file <- function(path) {
  raw <- .g3_read_json_file(path)
  fmt <- raw$format %||% ""
  if (identical(fmt, .G3_GAME_FORMAT)) {
    return(g3_read_game(path, strict = TRUE))
  }
  if (identical(fmt, .G3_POSITION_FORMAT)) {
    if (!identical(raw$format_version, 1L)) {
      .g3_abort("g3_unsupported_rules", paste0("Position format version ", format(raw$format_version),
                                               " is not supported by this version of gambitR3D."))
    }
    st <- g3_read_position(path)
    game <- .g3_game_new(st, metadata = list(source = "position file", mode = "hotseat"))
    attr(game$initial_state, "history") <- NULL
    attr(game$state, "history") <- NULL
    return(game)
  }
  .g3_abort("g3_invalid_state", "This is not a gambitR3D game or position file.")
}
