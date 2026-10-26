# Game database: saving, loading, annotating and soft deletion.
#
# A game row is derived from a validated `g3_game`: its event log goes to
# `actions` (one row per event, event sequence distinct from board ply), its
# initial and current states to `snapshots`, its players to `game_players`.
# Loading never trusts stored states: the game is rebuilt by replaying the
# stored events through the rules engine and the replay must reproduce the
# stored checksums.
#
# Saving from the app (`g3_save_game()`) is keyed by the game's persistent
# `game_id`. A live game that gained events is extended in place: the
# stored events must be an exact prefix of the new log (takebacks are
# appended events, so history is never rewritten), only the new events are
# replayed from the stored current state, and the update is guarded by the
# stored revision so a stale writer cannot overwrite a newer save. A new
# finished game whose content already exists is recorded as another
# provenance link to the existing game instead of a copy.
#
# Player metadata convention (read when present): `metadata$players` holds
# display names (`list(white = "Ada", black = "Ben")`); `metadata$agents`
# may name the agent per colour, either as a string (`"search"`) or a list
# with `agent` and `model_revision`. Names are stored verbatim and returned
# as data; they are never interpreted as SQL or markup.

.G3_DB_STATUS <- c("live", "finished")

#' @noRd
.g3_chr1 <- function(x) {
  if (is.null(x) || !is.atomic(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  as.character(x)
}

# A valid identifier (game, run) or NA.
#' @noRd
.g3_db_valid_id <- function(x) {
  x <- .g3_chr1(x)
  if (is.na(x) || !grepl(.G3_ID_PATTERN, x)) NA_character_ else x
}

#' @noRd
.g3_db_label <- function(x, what = "source") {
  x <- .g3_chr1(x)
  if (is.na(x) || !nzchar(x) || nchar(x) > 200L) {
    .g3_abort("g3_io_error", paste0("`", what, "` must be one non-empty label of at most 200 characters."))
  }
  x
}

# Result implied by the event log and the final state (never trusted from
# metadata).
#' @noRd
.g3_db_derived_result <- function(game) {
  ev <- game$events
  n <- nrow(ev)
  last <- if (n) ev$type[n] else ""
  if (last %in% c("resign", "draw_agreed", "truncated")) {
    res <- switch(last,
      resign = if (ev$actor[n] == "white") "black" else if (ev$actor[n] == "black") "white" else NA_character_,
      draw_agreed = "draw", truncated = "truncated")
    return(list(result = res, termination = last))
  }
  oc <- game$state$outcome
  if (identical(oc$status, "terminal")) return(list(result = oc$result, termination = oc$reason))
  list(result = NA_character_, termination = NA_character_)
}

#' @noRd
.g3_db_players <- function(meta) {
  pl <- if (is.list(meta[["players"]])) meta[["players"]] else list()
  ag <- if (is.list(meta[["agents"]])) meta[["agents"]] else list()
  one <- function(color) {
    nm <- .g3_chr1(pl[[color]])
    if (is.na(nm)) nm <- if (color == "white") "White" else "Black"
    a <- ag[[color]]
    agent <- NA_character_
    model <- NA_character_
    if (is.list(a)) {
      agent <- .g3_chr1(a[["agent"]] %||% a[["type"]] %||% a[["id"]])
      model <- .g3_chr1(a[["model_revision"]] %||% a[["model_hash"]] %||% a[["revision"]])
    } else {
      agent <- .g3_chr1(a)
    }
    data.frame(color = color, name = nm, agent = agent, model_revision = model, stringsAsFactors = FALSE)
  }
  rbind(one("white"), one("black"))
}

# All rows describing one validated game. `content_hash` may be passed when
# already computed; `rows` limits the event rows built (an extension only
# inserts its new events).
#' @noRd
.g3_db_game_record <- function(game, content_hash = NULL, rows = NULL) {
  ev <- game$events
  init <- game$initial_state
  fin <- game$state
  meta <- game$metadata
  line <- .g3_active_line(ev)
  derived <- .g3_db_derived_result(game)
  if (!identical(derived$result, .g3_chr1(meta$result))) {
    .g3_abort("g3_invalid_state", "The game's recorded result disagrees with its events.",
              game_id = meta$game_id)
  }
  key_of <- function(st) st$repetition$keys[length(st$repetition$keys)]
  # The state hash is the SHA-256 of exactly this canonical JSON (see
  # .g3_state_hash()), so each state is serialised once.
  init_json <- .g3_canonical_json(.g3_state_plain(init))
  fin_json <- .g3_canonical_json(.g3_state_plain(fin))
  init_hash <- .g3_sha256(init_json)
  fin_hash <- .g3_sha256(fin_json)
  rows <- rows %||% seq_len(nrow(ev))
  er <- ev[rows, , drop = FALSE]
  pl <- lapply(er$payload, jsonlite::fromJSON, simplifyVector = FALSE)
  date <- .g3_chr1(meta[["created"]])
  if (!is.na(date) && !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", date)) date <- NA_character_
  keep <- setdiff(names(meta), c("game_id", "rules_version", "rules_hash", "action_schema"))
  json_or_na <- function(x) if (is.null(x) || !length(x)) NA_character_ else .g3_canonical_json(x)
  list(
    game = list(
      game_id = meta$game_id,
      game_content_hash = content_hash %||% g3_game_hash(game),
      trajectory_hash = .g3_hash_object(list(rules_hash = init$rules_hash, initial_state_hash = init_hash,
                                             actions = as.list(ev$action_id[line]))),
      rules_hash = init$rules_hash,
      rules_version = init$rules_version,
      initial_state_hash = init_hash,
      initial_ply = init$ply,
      final_ply = fin$ply,
      plies = length(line),
      n_events = nrow(ev),
      final_state_hash = fin_hash,
      final_position_key = key_of(fin),
      status = if (is.na(derived$result)) "live" else "finished",
      result = derived$result,
      termination = derived$termination,
      game_date = date,
      metadata_json = .g3_canonical_json(meta[keep]),
      provenance_json = json_or_na(game$provenance),
      ui_json = json_or_na(game$ui)
    ),
    players = .g3_db_players(meta),
    actions = data.frame(
      event_seq = er$sequence, event_id = er$event_id, event_type = er$type, actor = er$actor,
      ply = er$ply, action_id = er$action_id,
      action_type = vapply(pl, function(p) .g3_chr1(p$action$type), ""),
      notation = vapply(pl, function(p) .g3_chr1(p$notation), ""),
      pre_state_hash = er$pre_state_hash, post_state_hash = er$post_state_hash,
      position_key = er$position_key, payload_json = er$payload,
      stringsAsFactors = FALSE
    ),
    snapshots = data.frame(
      kind = c("initial", "final"), event_seq = c(0L, nrow(ev)), ply = c(init$ply, fin$ply),
      state_hash = c(init_hash, fin_hash), position_key = c(key_of(init), key_of(fin)),
      state_json = c(init_json, fin_json),
      stringsAsFactors = FALSE
    )
  )
}

# ------------------------------------------------------------- insert path

#' @noRd
.g3_db_register_rules <- function(con, rules_hash, rules_version) {
  .g3_db_exec(con, paste(
    "INSERT OR IGNORE INTO rules_profiles (rules_hash, rules_version, action_schema, state_schema, registered_at)",
    "VALUES (?, ?, ?, ?, ?)"
  ), list(rules_hash, rules_version, .G3_ACTION_SCHEMA, .G3_STATE_SCHEMA, .g3_now()))
}

#' @noRd
.g3_db_insert_game <- function(con, rec, game_id, origin, origin_game_id = NA_character_, now = .g3_now()) {
  g <- rec$game
  .g3_db_register_rules(con, g$rules_hash, g$rules_version)
  .g3_db_exec(con, paste(
    "INSERT INTO games (game_id, origin_game_id, origin, game_content_hash, trajectory_hash, rules_hash,",
    "initial_state_hash, initial_ply, final_ply, plies, n_events, final_state_hash, final_position_key,",
    "status, result, termination, game_date, created_at, updated_at, revision, metadata_json,",
    "provenance_json, ui_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?, ?)"
  ), list(game_id, origin_game_id, origin, g$game_content_hash, g$trajectory_hash, g$rules_hash,
          g$initial_state_hash, g$initial_ply, g$final_ply, g$plies, g$n_events, g$final_state_hash,
          g$final_position_key, g$status, g$result, g$termination, g$game_date %|na|% now, now, now,
          g$metadata_json, g$provenance_json, g$ui_json))
  .g3_db_insert_players(con, game_id, rec$players)
  .g3_db_insert_actions(con, game_id, rec$actions)
  .g3_db_insert_snapshots(con, game_id, rec$snapshots)
  invisible(game_id)
}

#' @noRd
`%|na|%` <- function(x, y) if (length(x) != 1L || is.na(x)) y else x

#' @noRd
.g3_db_insert_players <- function(con, game_id, players) {
  .g3_db_exec(con, "INSERT INTO game_players (game_id, color, name, agent, model_revision) VALUES (?, ?, ?, ?, ?)",
              list(rep(game_id, nrow(players)), players$color, players$name, players$agent, players$model_revision))
}

#' @noRd
.g3_db_insert_actions <- function(con, game_id, actions) {
  if (!nrow(actions)) return(invisible(0L))
  .g3_db_exec(con, paste(
    "INSERT INTO actions (game_id, event_seq, event_id, event_type, actor, ply, action_id, action_type,",
    "notation, pre_state_hash, post_state_hash, position_key, payload_json)",
    "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  ), c(list(rep(game_id, nrow(actions))), unname(as.list(actions))))
}

#' @noRd
.g3_db_insert_snapshots <- function(con, game_id, snaps) {
  .g3_db_exec(con, paste(
    "INSERT INTO snapshots (game_id, kind, event_seq, ply, state_hash, position_key, state_json)",
    "VALUES (?, ?, ?, ?, ?, ?, ?)"
  ), c(list(rep(game_id, nrow(snaps))), unname(as.list(snaps))))
}

# Named app source (no bytes), created on first use.
#' @noRd
.g3_db_app_source <- function(con, label) {
  now <- .g3_now()
  .g3_db_exec(con, paste(
    "INSERT INTO sources (kind, label, format, parser_version, status, first_seen_at, last_import_at)",
    "VALUES ('app', ?, 'app', ?, 'active', ?, ?) ON CONFLICT (label) WHERE kind = 'app' DO NOTHING"
  ), list(label, .G3_DB_PARSER_VERSION, now, now))
  DBI::dbGetQuery(con, "SELECT source_id FROM sources WHERE kind = 'app' AND label = ?", params = list(label))$source_id
}

#' @noRd
.g3_db_link <- function(con, game_id, source_id, record_index, relation, content_hash) {
  .g3_db_exec(con, paste(
    "INSERT INTO game_sources (game_id, source_id, record_index, relation, content_hash, linked_at)",
    "VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (source_id, record_index, game_id)",
    "DO UPDATE SET content_hash = excluded.content_hash, linked_at = excluded.linked_at"
  ), list(game_id, source_id, as.integer(record_index), relation, content_hash, .g3_now()))
}

#' @noRd
.g3_db_game_row <- function(db, game_id) {
  .g3_db_get(db, paste(
    "SELECT g.*, r.rules_version FROM games g JOIN rules_profiles r ON r.rules_hash = g.rules_hash",
    "WHERE g.game_id = ?"
  ), list(game_id))
}

# ---------------------------------------------------------------- saving

#' Save a game to the database
#'
#' Persists a played or unfinished game in one short transaction and
#' returns only after the commit. Saving is idempotent: saving the same
#' content again changes nothing. A live game that gained events since its
#' last save is extended in place (only the new events are replayed and
#' appended); a save whose stored history is not a prefix of the game's
#' events is refused as stale. A new finished game whose content is already
#' in the database is linked to the existing record instead of duplicated.
#'
#' @param db A writable `g3_db`.
#' @param game A `g3_game`. Its `metadata$game_id` is the persistent key.
#' @param source Label of the saving application or workflow, recorded as
#'   provenance (default `"app"`).
#' @return A list with `game_id` (the stored id; for a duplicate, the id of
#'   the existing game), `status` (`"inserted"`, `"updated"`,
#'   `"unchanged"` or `"duplicate"`), `game_content_hash`, `revision`
#'   (increments with every update), `n_events`, `plies`,
#'   `catalogue_status` (`"live"` for an unfinished, resumable game,
#'   `"finished"` otherwise) and `saved_at`.
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' game <- gambitR3D::g3_new_game(players = base::list(white = "Ada", black = "Ben"))
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' gambitR3D::g3_save_game(db, game)$status
#' game <- gambitR3D::g3_game_apply(game, "m:B:0:2-B:0:1")
#' gambitR3D::g3_save_game(db, game)$status
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_save_game <- function(db, game, source = "app") {
  .g3_db_con(db, write = TRUE)
  .g3_check_game(game)
  label <- .g3_db_label(source)
  gid <- .g3_db_valid_id(game$metadata$game_id)
  if (is.na(gid)) {
    .g3_abort("g3_invalid_state", "The game's metadata$game_id must be 1-128 letters, digits or _ . : - characters.")
  }
  h <- g3_game_hash(game)
  row <- .g3_db_game_row(db, gid)
  if (nrow(row)) {
    if (!is.na(row$deleted_at)) {
      .g3_abort("g3_io_error", paste0("Game ", gid, " is deleted in this database; restore it with g3_restore_games() first."))
    }
    if (identical(row$game_content_hash, h)) {
      .g3_db_write(db, function(con) {
        sid <- .g3_db_app_source(con, label)
        .g3_db_link(con, gid, sid, 0L, "save", h)
      })
      return(.g3_save_result(row$game_id, "unchanged", h, row$revision, row$n_events, row$plies, row$status, row$updated_at))
    }
    return(.g3_db_extend_game(db, row, game, h, label))
  }
  g3_validate_game(game)
  rec <- .g3_db_game_record(game, content_hash = h)
  if (rec$game$status == "finished") {
    dup <- .g3_db_get(db, paste(
      "SELECT game_id, revision, n_events, plies, status, updated_at FROM games",
      "WHERE game_content_hash = ? AND status = 'finished' ORDER BY created_at, game_id LIMIT 1"
    ), list(h))
    if (nrow(dup)) {
      .g3_db_write(db, function(con) {
        sid <- .g3_db_app_source(con, label)
        .g3_db_link(con, dup$game_id, sid, 0L, "duplicate", h)
      })
      return(.g3_save_result(dup$game_id, "duplicate", h, dup$revision, dup$n_events, dup$plies, dup$status, dup$updated_at))
    }
  }
  now <- .g3_now()
  .g3_db_write(db, function(con) {
    taken <- DBI::dbGetQuery(con, "SELECT 1 FROM games WHERE game_id = ?", params = list(gid))
    if (nrow(taken)) {
      .g3_abort("g3_stale_revision", paste0("Game ", gid, " was saved by another writer meanwhile; reload it."), game_id = gid)
    }
    .g3_db_insert_game(con, rec, gid, "app", now = now)
    sid <- .g3_db_app_source(con, label)
    .g3_db_link(con, gid, sid, 0L, "save", h)
  }, what = "save")
  .g3_save_result(gid, "inserted", h, 1L, rec$game$n_events, rec$game$plies, rec$game$status, now)
}

#' @noRd
.g3_save_result <- function(game_id, status, hash, revision, n_events, plies, cat_status, time) {
  list(game_id = game_id, status = status, game_content_hash = hash, revision = as.integer(revision),
       n_events = as.integer(n_events), plies = as.integer(plies), catalogue_status = cat_status,
       saved_at = time)
}

# Append the new events of a game whose stored log is a prefix of `game`.
#' @noRd
.g3_db_extend_game <- function(db, row, game, h, label) {
  gid <- row$game_id
  stored <- .g3_db_get(db, paste(
    "SELECT event_seq, event_id, event_type, action_id, post_state_hash FROM actions",
    "WHERE game_id = ? ORDER BY event_seq"
  ), list(gid))
  ev <- game$events
  k <- nrow(stored)
  idx <- seq_len(k)
  new <- if (nrow(ev) > k) (k + 1L):nrow(ev) else integer()
  rec <- .g3_db_game_record(game, content_hash = h, rows = new)
  prefix <- length(new) > 0L &&
    identical(rec$game$initial_state_hash, row$initial_state_hash) &&
    identical(ev$event_id[idx], stored$event_id) && identical(ev$type[idx], stored$event_type) &&
    identical(ev$action_id[idx], stored$action_id) && identical(ev$post_state_hash[idx], stored$post_state_hash)
  if (!prefix) {
    .g3_abort("g3_stale_revision", paste0(
      "Game ", gid, " in the database (revision ", row$revision, ", ", k, " events) has a different history; ",
      "reload it before saving."
    ), game_id = gid, revision = row$revision)
  }
  if (row$status == "finished" || any(ev$type[new] == "takeback")) {
    g3_validate_game(game)
  } else {
    fin <- .g3_db_snapshot_state(db, gid, "final")
    tail <- ev[new, , drop = FALSE]
    tail$sequence <- seq_len(nrow(tail))
    rownames(tail) <- NULL
    part <- structure(list(initial_state = fin, state = game$state, events = tail, metadata = game$metadata),
                      class = "g3_game")
    res <- .g3_game_problems(part)
    if (length(res$problems)) {
      .g3_abort("g3_invalid_state", paste0("Invalid game: ", paste(res$problems, collapse = "; ")),
                first_invalid_event = k + res$first_invalid_event)
    }
  }
  g <- rec$game
  now <- .g3_now()
  .g3_db_write(db, function(con) {
    n <- .g3_db_exec(con, paste(
      "UPDATE games SET game_content_hash = ?, trajectory_hash = ?, final_ply = ?, plies = ?, n_events = ?,",
      "final_state_hash = ?, final_position_key = ?, status = ?, result = ?, termination = ?,",
      "metadata_json = ?, provenance_json = ?, ui_json = ?, updated_at = ?, revision = revision + 1",
      "WHERE game_id = ? AND revision = ? AND n_events = ? AND deleted_at IS NULL"
    ), list(g$game_content_hash, g$trajectory_hash, g$final_ply, g$plies, g$n_events, g$final_state_hash,
            g$final_position_key, g$status, g$result, g$termination, g$metadata_json, g$provenance_json,
            g$ui_json, now, gid, row$revision, k))
    if (n != 1L) {
      .g3_abort("g3_stale_revision", paste0("Game ", gid, " was changed by another writer meanwhile; reload it."),
                game_id = gid)
    }
    .g3_db_insert_actions(con, gid, rec$actions)
    .g3_db_exec(con, "DELETE FROM snapshots WHERE game_id = ? AND kind = 'final'", list(gid))
    .g3_db_insert_snapshots(con, gid, rec$snapshots[rec$snapshots$kind == "final", , drop = FALSE])
    .g3_db_exec(con, "DELETE FROM game_players WHERE game_id = ?", list(gid))
    .g3_db_insert_players(con, gid, rec$players)
    sid <- .g3_db_app_source(con, label)
    .g3_db_link(con, gid, sid, 0L, "save", h)
  }, what = "save")
  .g3_save_result(gid, "updated", h, row$revision + 1L, g$n_events, g$plies, g$status, now)
}

# A stored snapshot as a validated state.
#' @noRd
.g3_db_snapshot_state <- function(db, game_id, kind) {
  s <- .g3_db_get(db, "SELECT state_json, state_hash FROM snapshots WHERE game_id = ? AND kind = ?", list(game_id, kind))
  if (nrow(s) != 1L) .g3_abort("g3_invalid_state", paste0("Game ", game_id, " has no ", kind, " snapshot."))
  st <- .g3_state_from_list(jsonlite::fromJSON(s$state_json, simplifyVector = FALSE))
  if (!identical(.g3_state_hash(st), s$state_hash)) {
    .g3_abort("g3_invalid_state", paste0("The stored ", kind, " snapshot of game ", game_id, " fails its checksum."))
  }
  st
}

# --------------------------------------------------------------- loading

#' Load a game from the database
#'
#' Rebuilds a stored game by replaying its event log from the stored
#' initial state through the rules engine; every recorded checksum must be
#' reproduced. The catalogue record is attached as `game$catalogue`, which
#' distinguishes a live unfinished game (which may be resumed) from a
#' finished game (which can be analysed but not continued).
#'
#' @param db A `g3_db`.
#' @param game_id One game id.
#' @return A validated `g3_game` with its stored `annotations` (list),
#'   `provenance` and `ui`, plus `catalogue`: a list with `game_id`,
#'   `status` (`"live"` or `"finished"`), `resumable` (logical), `result`,
#'   `termination`, `revision`, `origin` (`"app"` or `"import"`),
#'   `created_at`, `updated_at`, `n_sources` and `game_content_hash`.
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' game <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' id <- gambitR3D::g3_save_game(db, game)$game_id
#' back <- gambitR3D::g3_load_game(db, id)
#' back$catalogue$status
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_load_game <- function(db, game_id) {
  gid <- .g3_chr1(game_id)
  row <- if (is.na(gid)) data.frame() else .g3_db_game_row(db, gid)
  if (!nrow(row)) .g3_abort("g3_io_error", paste0("No game with id ", format(game_id)[1L], " in the database."))
  if (!is.na(row$deleted_at)) {
    .g3_abort("g3_io_error", paste0("Game ", gid, " is deleted; restore it with g3_restore_games() to load it."))
  }
  game <- .g3_db_rebuild(db, row)
  game$annotations <- .g3_db_annotations_export(db, gid)
  n_src <- .g3_db_get(db, "SELECT count(*) AS n FROM game_sources WHERE game_id = ?", list(gid))$n
  game$catalogue <- list(
    game_id = gid, status = row$status, resumable = identical(row$status, "live"),
    result = row$result, termination = row$termination, revision = as.integer(row$revision),
    origin = row$origin, created_at = row$created_at, updated_at = row$updated_at,
    n_sources = as.integer(n_src), game_content_hash = row$game_content_hash
  )
  game
}

# Replay-validated reconstruction of a stored game row.
#' @noRd
.g3_db_rebuild <- function(db, row) {
  gid <- row$game_id
  snap <- .g3_db_get(db, "SELECT state_json FROM snapshots WHERE game_id = ? AND kind = 'initial'", list(gid))
  ev <- .g3_db_get(db, "SELECT * FROM actions WHERE game_id = ? ORDER BY event_seq", list(gid))
  nul <- function(x) if (is.na(x)) NULL else x
  events <- lapply(seq_len(nrow(ev)), function(i) {
    list(event_id = ev$event_id[i], sequence = ev$event_seq[i], type = ev$event_type[i], actor = ev$actor[i],
         ply = ev$ply[i], action_id = nul(ev$action_id[i]), pre_state_hash = ev$pre_state_hash[i],
         post_state_hash = ev$post_state_hash[i], position_key = nul(ev$position_key[i]),
         payload = jsonlite::fromJSON(ev$payload_json[i], simplifyVector = FALSE))
  })
  parse <- function(x) if (is.na(x)) NULL else jsonlite::fromJSON(x, simplifyVector = FALSE)
  raw <- list(
    format = .G3_GAME_FORMAT, format_version = .G3_GAME_FORMAT_VERSION, schema_version = .G3_STATE_SCHEMA,
    action_schema = .G3_ACTION_SCHEMA,
    rules = list(version = row$rules_version, hash = row$rules_hash),
    game_id = gid, metadata = parse(row$metadata_json),
    initial_state = if (nrow(snap) == 1L) jsonlite::fromJSON(snap$state_json, simplifyVector = FALSE) else NULL,
    events = events,
    final = list(ply = row$final_ply, state_hash = row$final_state_hash, position_key = row$final_position_key),
    provenance = parse(row$provenance_json), annotations = list(), ui = parse(row$ui_json)
  )
  game <- tryCatch(.g3_game_from_list(raw, strict = TRUE), error = function(e) {
    .g3_abort("g3_invalid_state", paste0("Stored game ", gid, " failed replay validation (", conditionMessage(e),
                                         "). Run g3_check_game_db()."), game_id = gid)
  })
  if (!identical(g3_game_hash(game), row$game_content_hash) || nrow(game$events) != row$n_events) {
    .g3_abort("g3_invalid_state", paste0("Stored game ", gid, " does not reproduce its content hash. Run g3_check_game_db()."),
              game_id = gid)
  }
  game
}

# ------------------------------------------------- deletion and annotation

#' Delete and restore games
#'
#' Deletion is a reversible soft delete: the game, its events and its
#' provenance stay in the database but are hidden from queries and loading
#' until restored. Nothing happens without `confirm = TRUE`; without it the
#' function only reports what would be deleted. Imported source files are
#' never touched.
#'
#' @param db A writable `g3_db`.
#' @param game_ids Character vector of game ids.
#' @param confirm Must be `TRUE` to delete.
#' @param note Optional reason stored with the deletion.
#' @return A data frame with `game_id` and `status`: for
#'   `g3_delete_games()` one of `"would_delete"`, `"deleted"`,
#'   `"already_deleted"`, `"not_found"`; for `g3_restore_games()` one of
#'   `"restored"`, `"not_deleted"`, `"not_found"`.
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' id <- gambitR3D::g3_save_game(db, gambitR3D::g3_new_game())$game_id
#' gambitR3D::g3_delete_games(db, id)
#' gambitR3D::g3_delete_games(db, id, confirm = TRUE)
#' gambitR3D::g3_restore_games(db, id)
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_delete_games <- function(db, game_ids, confirm = FALSE, note = NULL) {
  .g3_db_con(db, write = TRUE)
  ids <- .g3_db_ids_arg(game_ids)
  cur <- .g3_db_id_state(db, ids)
  status <- ifelse(!cur$found, "not_found", ifelse(cur$deleted, "already_deleted", "would_delete"))
  if (!isTRUE(confirm)) {
    n <- sum(status == "would_delete")
    message("Nothing was deleted. ", n, " game(s) would be deleted (reversibly); call again with confirm = TRUE.")
    return(data.frame(game_id = ids, status = status, stringsAsFactors = FALSE))
  }
  note <- if (is.null(note)) NA_character_ else .g3_db_label(note, "note")
  todo <- ids[status == "would_delete"]
  if (length(todo)) {
    now <- .g3_now()
    .g3_db_write(db, function(con) {
      .g3_db_exec(con, "UPDATE games SET deleted_at = ?, deleted_note = ? WHERE game_id = ? AND deleted_at IS NULL",
                  list(rep(now, length(todo)), rep(note, length(todo)), todo))
    }, what = "delete")
  }
  status[status == "would_delete"] <- "deleted"
  data.frame(game_id = ids, status = status, stringsAsFactors = FALSE)
}

#' @rdname g3_delete_games
#' @export
g3_restore_games <- function(db, game_ids) {
  .g3_db_con(db, write = TRUE)
  ids <- .g3_db_ids_arg(game_ids)
  cur <- .g3_db_id_state(db, ids)
  status <- ifelse(!cur$found, "not_found", ifelse(cur$deleted, "restored", "not_deleted"))
  todo <- ids[status == "restored"]
  if (length(todo)) {
    .g3_db_write(db, function(con) {
      .g3_db_exec(con, "UPDATE games SET deleted_at = NULL, deleted_note = NULL WHERE game_id = ?", list(todo))
    }, what = "restore")
  }
  data.frame(game_id = ids, status = status, stringsAsFactors = FALSE)
}

#' @noRd
.g3_db_ids_arg <- function(game_ids) {
  if (!is.character(game_ids) || !length(game_ids) || anyNA(game_ids)) {
    .g3_abort("g3_io_error", "`game_ids` must be a character vector of game ids.")
  }
  unique(game_ids)
}

#' @noRd
.g3_db_id_state <- function(db, ids) {
  rows <- .g3_db_get(db, paste0("SELECT game_id, deleted_at FROM games WHERE game_id IN (",
                                paste(rep("?", length(ids)), collapse = ", "), ")"), as.list(ids))
  i <- match(ids, rows$game_id)
  list(found = !is.na(i), deleted = !is.na(i) & !is.na(rows$deleted_at[i]))
}

#' Annotate a stored game
#'
#' Adds a comment or a tag to a game, optionally tied to a board ply.
#' Annotations are stored as data and returned verbatim; escape them when
#' rendering (Shiny's text outputs do).
#'
#' @param db A writable `g3_db`.
#' @param game_id One game id.
#' @param text Comment text (up to 65536 characters) or tag (up to 200
#'   characters, one line).
#' @param ply Optional board ply the annotation refers to, between the
#'   game's initial and current ply.
#' @param kind `"comment"` or `"tag"`.
#' @param author Optional author label.
#' @return `g3_annotate_game()` returns the new annotation id invisibly.
#'   `g3_game_annotations()` returns a data frame with `annotation_id`,
#'   `game_id`, `kind` (`"comment"`, `"tag"` or `"imported"`), `ply`,
#'   `body`, `author`, `created_at` and `source` (label of the import
#'   source, `NA` for annotations added here).
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' id <- gambitR3D::g3_save_game(db, gambitR3D::g3_new_game())$game_id
#' gambitR3D::g3_annotate_game(db, id, "opening test", kind = "tag")
#' gambitR3D::g3_annotate_game(db, id, "A quiet start.", ply = 0L)
#' gambitR3D::g3_game_annotations(db, id)[, c("kind", "ply", "body")]
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_annotate_game <- function(db, game_id, text, ply = NULL, kind = c("comment", "tag"), author = NULL) {
  .g3_db_con(db, write = TRUE)
  kind <- match.arg(kind)
  gid <- .g3_chr1(game_id)
  row <- if (is.na(gid)) data.frame() else .g3_db_game_row(db, gid)
  if (!nrow(row) || !is.na(row$deleted_at)) .g3_abort("g3_io_error", paste0("No game with id ", format(game_id)[1L], "."))
  body <- .g3_chr1(text)
  max_chars <- if (kind == "tag") 200L else .g3_db_limits()$annotation_chars
  if (is.na(body) || !nzchar(body) || nchar(body) > max_chars || (kind == "tag" && grepl("[\r\n]", body))) {
    .g3_abort("g3_io_error", paste0("`text` must be one non-empty ", kind, " of at most ", max_chars, " characters",
                                    if (kind == "tag") " on one line" else "", "."))
  }
  p <- NA_integer_
  if (!is.null(ply)) {
    if (!is.numeric(ply) || length(ply) != 1L || is.na(ply) || ply != round(ply) ||
        ply < row$initial_ply || ply > row$final_ply) {
      .g3_abort("g3_io_error", paste0("`ply` must be a whole number from ", row$initial_ply, " to ", row$final_ply, "."))
    }
    p <- as.integer(ply)
  }
  who <- if (is.null(author)) NA_character_ else .g3_db_label(author, "author")
  id <- .g3_db_write(db, function(con) {
    .g3_db_exec(con, "INSERT INTO annotations (game_id, kind, ply, body, author, created_at) VALUES (?, ?, ?, ?, ?, ?)",
                list(gid, kind, p, body, who, .g3_now()))
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
  }, what = "annotation")
  invisible(as.integer(id))
}

#' @rdname g3_annotate_game
#' @export
g3_game_annotations <- function(db, game_id) {
  gid <- .g3_chr1(game_id)
  rows <- .g3_db_get(db, paste(
    "SELECT a.annotation_id, a.game_id, a.kind, a.ply, a.body, a.author, a.created_at, s.label AS source",
    "FROM annotations a LEFT JOIN sources s ON s.source_id = a.source_id",
    "WHERE a.game_id = ? AND a.deleted_at IS NULL ORDER BY a.annotation_id"
  ), list(gid))
  .g3_db_typed(rows, c(annotation_id = "integer", game_id = "character", kind = "character", ply = "integer",
                       body = "character", author = "character", created_at = "character", source = "character"))
}

# Annotations in the native file's form (a list of records).
#' @noRd
.g3_db_annotations_export <- function(db, game_id) {
  a <- g3_game_annotations(db, game_id)
  lapply(seq_len(nrow(a)), function(i) {
    x <- list(kind = a$kind[i], body = a$body[i], ply = a$ply[i], author = a$author[i], created_at = a$created_at[i])
    x[!vapply(x, function(v) length(v) == 1L && is.na(v), TRUE)]
  })
}
