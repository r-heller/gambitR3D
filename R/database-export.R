# Game database: export, backup, restore and integrity checks.
#
# Export writes the native versioned format, never PGN or FEN (they cannot
# express three boards, movable platforms or overlapping physical squares):
# either one JSONL collection (`*.jsonl`, one game envelope per line) or a
# directory with one `.g3.json` file per game. Every export also writes a
# human-readable move list (for reading only, not an import format) and a
# manifest with SHA-256 checksums, game identities and source provenance.
# Manifests and exported provenance name sources by file name only, so no
# absolute private path leaves the machine. Files are written atomically
# and the manifest last; a directory export is staged and renamed into
# place.
#
# Backups use `VACUUM INTO`: one read transaction produces a consistent,
# self-contained copy (rollback-journal mode, no -wal/-shm companions) even
# while other connections write to a WAL database. The copy is written to a
# temporary name, checked with `PRAGMA integrity_check` and renamed. SQLite
# does not fsync the VACUUM INTO output and R has no portable fsync, so a
# backup taken immediately before a power loss may be incomplete; the
# integrity check on restore detects this. A restore copies a backup to a
# new path, migrates it if older, replays every game and only then moves it
# into place; the active database is never overwritten.

.G3_GAMES_EXPORT_FORMAT <- "gambitR3D-export"

#' Export games from the database
#'
#' Writes selected games in the native versioned format with a checksum and
#' provenance manifest and a human-readable move list. A `path` ending in
#' `.jsonl` produces one collection file plus `<name>.manifest.json` and
#' `<name>.moves.txt` beside it; any other path is a new directory holding
#' one `.g3.json` file per game, `manifest.json` and `moves.txt`. Existing
#' files are never overwritten. PGN and FEN are refused because they cannot
#' represent this variant without loss.
#'
#' @param db A `g3_db`.
#' @param game_ids Character vector of game ids (deleted games cannot be
#'   exported until restored).
#' @param path Output `.jsonl` file or new directory.
#' @return The manifest as a list: `format`, `format_version`, `layout`
#'   (`"jsonl"` or `"directory"`), `created_at`, `package_version`,
#'   `n_games`, `files` (data frame `file`, `role`, `sha256`, `bytes`,
#'   `records`; file names are relative to the manifest), `games` (data frame
#'   `game_id`, `file`, `record`, `game_content_hash`, `final_state_hash`,
#'   `status`, `result`, `termination`, `plies`, `n_sources`) and
#'   `manifest_path`.
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' game <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' id <- gambitR3D::g3_save_game(db, game)$game_id
#' out <- base::tempfile(fileext = ".jsonl")
#' man <- gambitR3D::g3_export_games(db, id, out)
#' man$files[, c("file", "role")]
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_export_games <- function(db, game_ids, path) {
  .g3_db_con(db)
  ids <- .g3_db_ids_arg(game_ids)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .g3_abort("g3_io_error", "`path` must be one output path.")
  }
  if (grepl("\\.(pgn|fen|epd)$", tolower(path))) {
    .g3_abort("g3_io_error", paste(
      "PGN and FEN cannot describe this variant: they have one 8x8 board, while gambitR3D has three boards,",
      "four movable and rotatable platforms and physical squares that can share a projected file and rank.",
      "Export to a .jsonl collection or a directory of native .g3.json files instead; g3_export_games() also",
      "writes a human-readable moves.txt."
    ))
  }
  if (grepl("\\.json$", tolower(path))) {
    .g3_abort("g3_io_error", "Use a path ending in .jsonl for a collection, or a new directory for one .g3.json file per game.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  layout <- if (grepl("\\.jsonl$", tolower(path))) "jsonl" else "directory"
  if (!dir.exists(dirname(path))) .g3_abort("g3_io_error", paste0("Directory does not exist: ", dirname(path)))
  stem <- sub("\\.jsonl$", "", path, ignore.case = TRUE)
  targets <- if (layout == "jsonl") c(path, paste0(stem, ".manifest.json"), paste0(stem, ".moves.txt")) else path
  if (any(file.exists(targets))) {
    .g3_abort("g3_io_error", paste0("Refusing to overwrite: ", paste(targets[file.exists(targets)], collapse = ", ")))
  }
  cur <- .g3_db_id_state(db, ids)
  if (!all(cur$found)) .g3_abort("g3_io_error", paste0("No game with id: ", paste(ids[!cur$found], collapse = ", ")))
  if (any(cur$deleted)) .g3_abort("g3_io_error", paste0("Deleted game(s) cannot be exported: ", paste(ids[cur$deleted], collapse = ", ")))
  now <- .g3_now()
  games <- lapply(ids, function(id) g3_load_game(db, id))
  srcs <- .g3_db_get(db, paste0(
    "SELECT gs.game_id, s.label, s.kind, s.source_sha256, gs.relation, s.first_seen_at FROM game_sources gs ",
    "JOIN sources s ON s.source_id = gs.source_id WHERE gs.game_id IN (", paste(rep("?", length(ids)), collapse = ", "),
    ") ORDER BY gs.game_id, s.source_id"
  ), as.list(ids))
  envelopes <- lapply(games, function(g) {
    s <- srcs[srcs$game_id == g$metadata$game_id, , drop = FALSE]
    g$provenance$catalogue <- list(
      exported_at = now, package_version = .g3_pkg_version(),
      sources = lapply(seq_len(nrow(s)), function(i) {
        x <- list(label = s$label[i], kind = s$kind[i], sha256 = s$source_sha256[i], relation = s$relation[i],
                  first_seen_at = s$first_seen_at[i])
        x[!vapply(x, function(v) is.na(v), TRUE)]
      })
    )
    .g3_game_to_list(g)
  })
  lines <- vapply(envelopes, function(e) {
    as.character(jsonlite::toJSON(e, auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  }, "")
  moves <- paste(c(
    "gambitR3D move list - a human-readable summary for inspection, not an import format.",
    "Squares are physical squares board:u:v (boards W, N, B and platforms WQ, WK, BQ, BK).",
    "", unlist(lapply(games, .g3_moves_text))
  ), collapse = "\n")
  game_rows <- data.frame(
    game_id = ids, file = NA_character_, record = NA_integer_,
    game_content_hash = vapply(games, g3_game_hash, ""),
    final_state_hash = vapply(games, function(g) .g3_state_hash(g$state), ""),
    status = vapply(games, function(g) g$catalogue$status, ""),
    result = vapply(games, function(g) .g3_chr1(g$catalogue$result), ""),
    termination = vapply(games, function(g) .g3_chr1(g$catalogue$termination), ""),
    plies = vapply(games, function(g) length(.g3_active_line(g$events)), 1L),
    n_sources = vapply(games, function(g) g$catalogue$n_sources, 1L),
    stringsAsFactors = FALSE
  )
  file_row <- function(file, role, text, records) {
    data.frame(file = file, role = role, sha256 = .g3_sha256(text), bytes = nchar(text, type = "bytes"),
               records = as.integer(records), stringsAsFactors = FALSE)
  }
  if (layout == "jsonl") {
    body <- paste0(paste(lines, collapse = "\n"), "\n")
    game_rows$file <- basename(path)
    game_rows$record <- seq_along(ids)
    files <- rbind(file_row(basename(path), "games", body, length(ids)),
                   file_row(basename(paste0(stem, ".moves.txt")), "moves", moves, length(ids)))
    .g3_atomic_write(body, path)
    .g3_atomic_write(moves, paste0(stem, ".moves.txt"))
    manifest_path <- paste0(stem, ".manifest.json")
    manifest <- .g3_export_manifest(layout, now, files, game_rows)
    .g3_atomic_write(.g3_manifest_json(manifest), manifest_path)
  } else {
    stage <- tempfile(pattern = paste0(".", basename(path), "-partial-"), tmpdir = dirname(path))
    dir.create(stage)
    staged_ok <- FALSE
    on.exit(if (!staged_ok) unlink(stage, recursive = TRUE), add = TRUE)
    safe <- substr(gsub("[^A-Za-z0-9_-]", "_", ids), 1L, 64L)
    names_out <- sprintf("%04d_%s.g3.json", seq_along(ids), safe)
    pretty <- vapply(envelopes, function(e) {
      as.character(jsonlite::toJSON(e, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null"))
    }, "")
    for (i in seq_along(ids)) .g3_atomic_write(pretty[i], file.path(stage, names_out[i]))
    .g3_atomic_write(moves, file.path(stage, "moves.txt"))
    game_rows$file <- names_out
    game_rows$record <- 1L
    files <- rbind(
      do.call(rbind, lapply(seq_along(ids), function(i) file_row(names_out[i], "game", pretty[i], 1L))),
      file_row("moves.txt", "moves", moves, length(ids))
    )
    manifest <- .g3_export_manifest(layout, now, files, game_rows)
    .g3_atomic_write(.g3_manifest_json(manifest), file.path(stage, "manifest.json"))
    if (!file.rename(stage, path)) .g3_abort("g3_io_error", paste0("Could not move the export into place: ", path))
    staged_ok <- TRUE
    manifest_path <- file.path(path, "manifest.json")
  }
  manifest$manifest_path <- manifest_path
  manifest
}

#' @noRd
.g3_export_manifest <- function(layout, now, files, games) {
  list(
    format = .G3_GAMES_EXPORT_FORMAT, format_version = 1L, layout = layout, created_at = now,
    package_version = .g3_pkg_version(),
    game_format = .G3_GAME_FORMAT, game_format_version = .G3_GAME_FORMAT_VERSION,
    rules = list(version = .g3_tables()$rules_version, hash = .g3_tables()$rules_hash),
    n_games = nrow(games),
    note = paste("File names are relative to this manifest; no absolute paths are recorded.",
                 "The moves file is a human-readable summary, not an import format."),
    files = files, games = games
  )
}

#' @noRd
.g3_manifest_json <- function(m) {
  as.character(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null",
                                dataframe = "rows"))
}

# Human-readable move list of one game.
#' @noRd
.g3_moves_text <- function(game) {
  m <- game$metadata
  cat_ <- game$catalogue %||% list()
  pl <- if (is.list(m$players)) m$players else list()
  ev <- game$events
  head <- c(
    paste0("Game ", m$game_id),
    paste0("  White: ", .g3_chr1(pl$white) %|na|% "White", "    Black: ", .g3_chr1(pl$black) %|na|% "Black"),
    paste0("  Status: ", cat_$status %||% (if (is.na(m$result)) "live" else "finished"),
           "    Result: ", m$result %|na|% "-", "    Termination: ", m$termination %|na|% "-"),
    paste0("  Rules: ", game$state$rules_version, " (", substr(game$state$rules_hash, 1L, 12L), ")",
           "    Plies: ", length(.g3_active_line(ev)), "    Events: ", nrow(ev))
  )
  body <- vapply(seq_len(nrow(ev)), function(i) {
    p <- jsonlite::fromJSON(ev$payload[i], simplifyVector = FALSE)
    what <- if (ev$type[i] == "action") {
      .g3_chr1(p$notation) %|na|% ev$action_id[i]
    } else {
      paste0("[", ev$type[i], if (!is.null(p$n)) paste0(" ", p$n) else "",
             if (!is.na(.g3_chr1(p$reason))) paste0(": ", p$reason) else "", "]")
    }
    sprintf("  %4d  ply %3d  %-6s %s", ev$sequence[i], ev$ply[i], ev$actor[i], what)
  }, "")
  c(head, if (length(body)) body else "  (no events)", "")
}

# ----------------------------------------------------------------- backup

#' Back up, restore and check a game database
#'
#' `g3_backup_game_db()` writes a consistent copy of a live database with
#' SQLite's `VACUUM INTO` (safe while other connections write; not a file
#' copy), checks it and moves it into place. `g3_restore_game_db()` copies a
#' backup to a new path, migrates it if needed, replays every game and only
#' then moves it into place; it never overwrites an existing database.
#' `g3_check_game_db()` runs SQLite's integrity and foreign-key checks,
#' verifies the recorded schema migrations and replays every stored game to
#' its stored final state.
#'
#' @param db A `g3_db` (read-only handles can be backed up and checked).
#' @param path Destination file; it must not exist.
#' @param backup Path of a backup file to restore.
#' @param replay For `g3_check_game_db()`: replay every game (default
#'   `TRUE`); `FALSE` runs only the SQLite-level checks.
#' @return `g3_backup_game_db()`: a list with `path`, `bytes`, `sha256`,
#'   `method` (`"VACUUM INTO"`), `schema_version`, `n_games`, `integrity`
#'   and `created_at`. `g3_check_game_db()`: a list with `ok` (logical),
#'   `integrity` (character, `"ok"` when sound), `foreign_key_violations`
#'   (data frame), `schema` (list `version`, `ok`, `message`), `games`
#'   (data frame `game_id`, `ok`, `problem` for every stored game, deleted
#'   or not; empty when `replay = FALSE`), `n_games`, `n_failed`,
#'   `elapsed_s` and `checked_at`. `g3_restore_game_db()`: the check
#'   result of the restored database with an added `path`.
#' @examples
#' dir <- base::tempfile()
#' base::dir.create(dir)
#' db <- gambitR3D::g3_open_game_db(base::file.path(dir, "games.sqlite"))
#' gambitR3D::g3_save_game(db, gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2"))
#' gambitR3D::g3_check_game_db(db)$ok
#' b <- gambitR3D::g3_backup_game_db(db, base::file.path(dir, "backup.sqlite"))
#' gambitR3D::g3_close_game_db(db)
#' r <- gambitR3D::g3_restore_game_db(b$path, base::file.path(dir, "restored.sqlite"))
#' r$ok
#' @export
g3_backup_game_db <- function(db, path) {
  con <- .g3_db_con(db)
  if (isTRUE(db$in_tx)) .g3_abort("g3_io_error", "Cannot back up inside a write transaction.")
  path <- .g3_new_db_path(path)
  if (identical(path, db$path)) .g3_abort("g3_io_error", "The backup path is the database itself.")
  tmp <- tempfile(pattern = ".g3backup-", tmpdir = dirname(path), fileext = ".sqlite")
  ok <- FALSE
  on.exit(if (!ok) unlink(paste0(tmp, c("", "-journal", "-wal", "-shm"))), add = TRUE)
  tryCatch(
    DBI::dbExecute(con, "VACUUM INTO ?", params = list(tmp)),
    error = function(e) .g3_abort("g3_io_error", paste0("Backup failed: ", conditionMessage(e)))
  )
  chk <- .g3_db_quick_validate(tmp)
  if (!file.rename(tmp, path)) .g3_abort("g3_io_error", paste0("Could not move the backup into place: ", path))
  ok <- TRUE
  list(path = path, bytes = file.info(path)$size, sha256 = digest::digest(file = path, algo = "sha256"),
       method = "VACUUM INTO", schema_version = chk$schema_version, n_games = chk$n_games,
       integrity = chk$integrity, created_at = .g3_now())
}

#' @rdname g3_backup_game_db
#' @export
g3_restore_game_db <- function(backup, path) {
  .g3_db_require()
  if (!is.character(backup) || length(backup) != 1L || is.na(backup) || !file.exists(backup) || dir.exists(backup)) {
    .g3_abort("g3_io_error", "`backup` must be an existing backup file.")
  }
  backup <- normalizePath(backup)
  path <- .g3_new_db_path(path)
  if (identical(path, backup)) .g3_abort("g3_io_error", "Restore to a new path, not onto the backup itself.")
  .g3_sqlite_header(backup, file.info(backup)$size)
  pre <- .g3_db_quick_validate(backup, allow_older = TRUE)
  tmp <- tempfile(pattern = ".g3restore-", tmpdir = dirname(path), fileext = ".sqlite")
  ok <- FALSE
  on.exit(if (!ok) unlink(paste0(tmp, c("", "-journal", "-wal", "-shm"))), add = TRUE)
  ro <- .g3_db_raw_ro(backup)
  tryCatch(DBI::dbExecute(ro, "VACUUM INTO ?", params = list(tmp)),
           error = function(e) .g3_abort("g3_io_error", paste0("Could not copy the backup: ", conditionMessage(e))),
           finally = DBI::dbDisconnect(ro))
  db <- g3_open_game_db(tmp)
  report <- tryCatch(g3_check_game_db(db), finally = g3_close_game_db(db))
  if (!isTRUE(report$ok)) {
    .g3_abort("g3_invalid_state", paste0(
      "The backup failed validation (integrity: ", paste(report$integrity, collapse = "; "), ", ",
      report$n_failed, " game(s) failed replay); nothing was restored."
    ), report = report)
  }
  if (any(file.exists(paste0(tmp, c("-wal", "-shm"))))) {
    .g3_abort("g3_io_error", "The restored copy was not checkpointed cleanly; nothing was restored.")
  }
  if (file.exists(path) || !file.rename(tmp, path)) {
    .g3_abort("g3_io_error", paste0("Could not move the restored database into place: ", path))
  }
  ok <- TRUE
  report$path <- path
  report$restored_from <- list(sha256 = digest::digest(file = backup, algo = "sha256"),
                               schema_version = pre$schema_version)
  report
}

#' @rdname g3_backup_game_db
#' @export
g3_check_game_db <- function(db, replay = TRUE) {
  con <- .g3_db_con(db)
  t0 <- proc.time()[["elapsed"]]
  integrity <- tryCatch(DBI::dbGetQuery(con, "PRAGMA integrity_check(100)")[[1L]],
                        error = function(e) paste("integrity_check failed:", conditionMessage(e)))
  fk <- tryCatch(DBI::dbGetQuery(con, "PRAGMA foreign_key_check"), error = function(e) {
    data.frame(table = "?", rowid = NA_integer_, parent = conditionMessage(e), fkid = NA_integer_)
  })
  info <- .g3_db_schema_info(con)
  schema_msg <- tryCatch({
    .g3_db_check_readable(info, .g3_db_migrations())
    NA_character_
  }, g3_error = function(e) conditionMessage(e))
  games <- data.frame(game_id = character(), ok = logical(), problem = character(), stringsAsFactors = FALSE)
  ids <- .g3_db_get(db, "SELECT game_id FROM games ORDER BY game_id")$game_id
  if (replay && length(ids)) {
    res <- lapply(ids, function(id) {
      problem <- tryCatch({
        row <- .g3_db_game_row(db, id)
        game <- .g3_db_rebuild(db, row)
        fin <- .g3_db_snapshot_state(db, id, "final")
        rec <- .g3_db_game_record(game)$game
        if (!identical(.g3_state_hash(game$state), row$final_state_hash) ||
            !identical(.g3_state_hash(fin), row$final_state_hash)) {
          "replayed final state differs from the stored final state"
        } else if (!identical(rec$status, row$status) || !identical(rec$result, row$result) ||
                   !identical(rec$plies, row$plies)) {
          "stored summary (status, result or plies) differs from the replay"
        } else {
          NA_character_
        }
      }, error = function(e) .g3_bound_text(conditionMessage(e)))
      data.frame(game_id = id, ok = is.na(problem), problem = problem, stringsAsFactors = FALSE)
    })
    games <- do.call(rbind, res)
  }
  n_failed <- sum(!games$ok)
  list(
    ok = identical(integrity, "ok") && !nrow(fk) && is.na(schema_msg) && n_failed == 0L,
    integrity = integrity, foreign_key_violations = fk,
    schema = list(version = info$user_version, ok = is.na(schema_msg), message = schema_msg),
    games = games, n_games = length(ids), n_failed = n_failed,
    elapsed_s = round(proc.time()[["elapsed"]] - t0, 3), checked_at = .g3_now()
  )
}

#' @noRd
.g3_new_db_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .g3_abort("g3_io_error", "`path` must be one file path.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (file.exists(path)) .g3_abort("g3_io_error", paste0("Refusing to overwrite an existing file: ", path))
  if (!dir.exists(dirname(path))) .g3_abort("g3_io_error", paste0("Directory does not exist: ", dirname(path)))
  path
}

#' @noRd
.g3_db_raw_ro <- function(path) {
  tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO, synchronous = NULL,
                   loadable.extensions = FALSE, default.extensions = FALSE, bigint = "integer"),
    error = function(e) .g3_abort("g3_io_error", paste0("Could not open ", path, ": ", conditionMessage(e)))
  )
}

# Integrity and identity of a database file, through a read-only connection.
#' @noRd
.g3_db_quick_validate <- function(path, allow_older = FALSE) {
  con <- .g3_db_raw_ro(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  integrity <- tryCatch(DBI::dbGetQuery(con, "PRAGMA integrity_check(100)")[[1L]],
                        error = function(e) paste("integrity_check failed:", conditionMessage(e)))
  if (!identical(integrity, "ok")) {
    .g3_abort("g3_io_error", paste0("The database file failed SQLite's integrity check: ",
                                    .g3_bound_text(paste(utils::head(integrity, 3L), collapse = "; "))))
  }
  info <- tryCatch(.g3_db_schema_info(con),
                   error = function(e) .g3_abort("g3_io_error", paste0("Unreadable database: ", conditionMessage(e))))
  migs <- .g3_db_migrations()
  latest <- max(vapply(migs, function(m) m$version, 1L))
  if (info$application_id != .G3_DB_APPLICATION_ID || info$user_version < 1L) {
    .g3_abort("g3_io_error", "The file is not a gambitR3D game database.")
  }
  if (info$user_version > latest) .g3_abort("g3_io_error", "The database is newer than this gambitR3D; update the package.")
  if (!allow_older && info$user_version != latest) .g3_abort("g3_io_error", "The copy has an unexpected schema version.")
  .g3_db_check_applied(info, migs)
  list(integrity = integrity, schema_version = info$user_version,
       n_games = DBI::dbGetQuery(con, "SELECT count(*) AS n FROM games")$n)
}
