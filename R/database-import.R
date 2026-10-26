# Game database: the staged import pipeline.
#
# Stages, per input path:
#   1. read      the file must exist and be a regular file with a supported
#                extension; its size is checked before reading and gzip
#                input is decompressed in bounded chunks, so a decompression
#                bomb stops at the limit.
#   2. hash      SHA-256 of the bytes as stored. Byte-identical files share
#                one `sources` row.
#   3. split     `.g3.json` is one record; `.jsonl` holds one game envelope
#                per line (blank lines are skipped, a record keeps its line
#                number as `record`). Each line is capped in size.
#   4. validate  JSON syntax and nesting depth, envelope and schema
#                versions, the exact rules identity (an unknown profile is
#                unsupported, never reinterpreted), then a full replay of
#                every event through the rules engine with checksum
#                comparison (`.g3_game_from_list()`, as g3_read_game()).
#                Nothing in a record is evaluated, and no path inside a
#                record is ever opened.
#   5. dedup     a record already linked from this source is a duplicate;
#                otherwise a stored game with the same content hash gains a
#                provenance link (and any new annotations) instead of a
#                copy. Different adjudications have different content
#                hashes and stay separate games. An id clash with different
#                content gets a fresh id; the claimed id is kept as
#                `origin_game_id`.
#   6. insert    each record commits in its own short transaction: game,
#                players, events, snapshots, provenance link and
#                annotations, or nothing.
#   7. report    one row per record (or per unreadable path).
#
# Invalid records go to `quarantine` with the condition class, a bounded
# diagnostic, the first invalid event and ply, and (when small enough) the
# record text; the source row is kept. Imports never write to input files.
# An interrupted import leaves every committed record intact and no partial
# record; running it again completes the rest and reports duplicates for
# what is already there.

#' Import game files into the database
#'
#' Imports native game files (`.g3.json`) and collections (`.jsonl`, one game
#' envelope per line), optionally gzip-compressed (`.gz`), through a staged
#' pipeline: bounded read, SHA-256 of the source bytes, JSON and schema
#' checks, the exact rules profile, replay of every action through the
#' rules engine, deduplication and an atomic insert per record. Invalid
#' records are quarantined with a diagnostic; a batch can mix accepted and
#' rejected records, and every input is accounted for in the report. Input
#' files are only read.
#'
#' @param db A writable `g3_db`.
#' @param paths Character vector of file paths.
#' @return A data frame with one row per record (or per unreadable path):
#'   `path` (as given), `source` (file name), `record` (1 for a single game
#'   file, the line number in a collection; `NA` when the file could not be
#'   read), `status` (`"accepted"`, `"duplicate"`, `"quarantined"` or
#'   `"error"`), `game_id` (stored id for accepted and duplicate records),
#'   `game_content_hash`, `source_sha256`, `condition` (condition class for
#'   rejected records), `message` (bounded), `first_invalid_event` and
#'   `first_invalid_ply`.
#' @examples
#' game <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' file <- base::tempfile(fileext = ".g3.json")
#' gambitR3D::g3_write_game(game, file)
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' gambitR3D::g3_import_games(db, file)[, c("record", "status", "game_id")]
#' gambitR3D::g3_import_games(db, file)$status
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_import_games <- function(db, paths) {
  .g3_db_con(db, write = TRUE)
  if (!is.character(paths) || anyNA(paths)) {
    .g3_abort("g3_io_error", "`paths` must be a character vector of file paths.")
  }
  rows <- lapply(paths, function(p) .g3_import_path(db, p))
  out <- do.call(rbind, c(list(.g3_import_report_empty()), rows))
  rownames(out) <- NULL
  out
}

#' @noRd
.g3_import_report_empty <- function() {
  data.frame(
    path = character(), source = character(), record = integer(), status = character(),
    game_id = character(), game_content_hash = character(), source_sha256 = character(),
    condition = character(), message = character(), first_invalid_event = integer(),
    first_invalid_ply = integer(), stringsAsFactors = FALSE
  )
}

#' @noRd
.g3_import_row <- function(path, record = NA_integer_, status, game_id = NA_character_, hash = NA_character_,
                           sha = NA_character_, condition = NA_character_, message = NA_character_,
                           event = NA_integer_, ply = NA_integer_) {
  data.frame(
    path = path, source = basename(path), record = as.integer(record), status = status,
    game_id = game_id, game_content_hash = hash, source_sha256 = sha, condition = condition,
    message = .g3_bound_text(message), first_invalid_event = as.integer(event),
    first_invalid_ply = as.integer(ply), stringsAsFactors = FALSE
  )
}

#' @noRd
.g3_bound_text <- function(x, n = .g3_db_limits()$diagnostic_chars) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  x <- gsub("[[:cntrl:]]+", " ", enc2utf8(as.character(x)))
  if (nchar(x) > n) paste0(substr(x, 1L, n - 3L), "...") else x
}

# Kind of an input file from its name.
#' @noRd
.g3_import_kind <- function(path) {
  name <- tolower(basename(path))
  gz <- grepl("\\.gz$", name)
  base <- sub("\\.gz$", "", name)
  format <- if (grepl("\\.jsonl$", base)) "g3-jsonl" else if (grepl("\\.json$", base)) "g3-json" else NA_character_
  list(format = format, gzip = gz, pgn = grepl("\\.(pgn|fen|epd)$", base))
}

# One input path: read, hash, split, then each record.
#' @noRd
.g3_import_path <- function(db, path) {
  fail <- function(msg, cond = "g3_io_error") .g3_import_row(path, status = "error", condition = cond, message = msg)
  kind <- .g3_import_kind(path)
  if (!file.exists(path)) return(fail("File not found."))
  if (dir.exists(path)) return(fail("This is a directory; pass the game files inside it."))
  if (kind$pgn) {
    return(fail(paste("PGN/FEN/EPD cannot describe this variant's three boards, movable platforms or overlapping",
                      "squares; import native .g3.json or .jsonl files.")))
  }
  if (is.na(kind$format)) return(fail("Unsupported file type; expected .g3.json, .json, .jsonl or their .gz forms."))
  read <- tryCatch(.g3_import_read(path, kind), g3_error = function(e) e)
  if (inherits(read, "g3_error")) return(fail(conditionMessage(read), class(read)[1L]))
  lim <- .g3_db_limits()
  records <- if (kind$format == "g3-json") {
    list(list(index = 1L, text = read$text, cap = lim$file_bytes))
  } else {
    lines <- strsplit(read$text, "\n", fixed = TRUE)[[1L]]
    keep <- which(nzchar(trimws(lines)))
    if (length(keep) > lim$records) return(fail(paste0("The collection has more than ", lim$records, " records.")))
    lapply(keep, function(i) list(index = i, text = sub("\r$", "", lines[[i]]), cap = lim$line_bytes))
  }
  if (!length(records)) return(fail("The collection contains no game records."))
  sid <- tryCatch(
    .g3_db_write(db, function(con) .g3_import_source(con, path, read, kind, length(records)), what = "import"),
    g3_error = function(e) e
  )
  if (inherits(sid, "g3_error")) return(fail(conditionMessage(sid), class(sid)[1L]))
  rows <- lapply(records, function(r) .g3_import_record(db, path, sid, read$sha256, r))
  .g3_db_write(db, function(con) {
    .g3_db_exec(con, "UPDATE sources SET status = 'complete' WHERE source_id = ?", list(sid))
  }, what = "import")
  out <- do.call(rbind, rows) %||% .g3_import_report_empty()
  out$source_sha256 <- rep(read$sha256, nrow(out))
  out
}

# Bounded read of a file (and bounded gunzip).
#' @noRd
.g3_import_read <- function(path, kind) {
  lim <- .g3_db_limits()
  size <- file.info(path)$size
  cap <- if (kind$gzip) lim$compressed_bytes else if (kind$format == "g3-json") lim$file_bytes else lim$collection_bytes
  if (is.na(size) || size > cap) {
    .g3_abort("g3_io_error", paste0("The file is larger than the ", format(cap / 1024^2), " MB limit."), bytes = size)
  }
  bytes <- readBin(path, "raw", n = size)
  sha <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  if (kind$gzip) {
    out_cap <- if (kind$format == "g3-json") lim$file_bytes else lim$collection_bytes
    bytes <- .g3_gunzip_bounded(bytes, out_cap)
  }
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) bytes <- bytes[-(1:3)]
  if (any(bytes == as.raw(0L))) .g3_abort("g3_io_error", "The file contains NUL bytes; it is not a text file.")
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) .g3_abort("g3_io_error", "The file is not valid UTF-8.")
  list(text = text, sha256 = sha, bytes = size, compression = if (kind$gzip) "gzip" else NA_character_)
}

#' @noRd
.g3_gunzip_bounded <- function(bytes, cap) {
  con <- gzcon(rawConnection(bytes))
  on.exit(close(con), add = TRUE)
  chunks <- list()
  total <- 0
  repeat {
    chunk <- tryCatch(readBin(con, "raw", n = 1024^2),
                      error = function(e) .g3_abort("g3_io_error", paste0("Corrupt gzip data: ", conditionMessage(e))))
    if (!length(chunk)) break
    total <- total + length(chunk)
    if (total > cap) {
      .g3_abort("g3_io_error", paste0("The decompressed data exceed the ", format(cap / 1024^2), " MB limit."))
    }
    chunks[[length(chunks) + 1L]] <- chunk
  }
  if (!length(chunks)) raw() else do.call(c, chunks)
}

# Find or create the source row for these bytes.
#' @noRd
.g3_import_source <- function(con, path, read, kind, n_records) {
  now <- .g3_now()
  .g3_db_exec(con, paste(
    "INSERT INTO sources (kind, source_sha256, label, location, format, compression, bytes, record_count,",
    "parser_version, status, import_count, first_seen_at, last_import_at)",
    "VALUES ('file', ?, ?, ?, ?, ?, ?, ?, ?, 'importing', 0, ?, ?) ON CONFLICT (source_sha256) DO NOTHING"
  ), list(read$sha256, basename(path), normalizePath(path, mustWork = FALSE), kind$format, read$compression,
          read$bytes, n_records, .G3_DB_PARSER_VERSION, now, now))
  .g3_db_exec(con, paste(
    "UPDATE sources SET import_count = import_count + 1, last_import_at = ?, status = 'importing'",
    "WHERE source_sha256 = ?"
  ), list(now, read$sha256))
  DBI::dbGetQuery(con, "SELECT source_id FROM sources WHERE source_sha256 = ?", params = list(read$sha256))$source_id
}

# One record: validate, deduplicate, insert or quarantine.
#' @noRd
.g3_import_record <- function(db, path, sid, sha, rec) {
  ok_row <- function(status, gid, hash) .g3_import_row(path, rec$index, status, gid, hash, sha)
  linked <- .g3_db_get(db, paste(
    "SELECT gs.game_id, g.game_content_hash FROM game_sources gs JOIN games g ON g.game_id = gs.game_id",
    "WHERE gs.source_id = ? AND gs.record_index = ?"
  ), list(sid, rec$index))
  if (nrow(linked)) return(ok_row("duplicate", linked$game_id, linked$game_content_hash))
  lim <- .g3_db_limits()
  parsed <- tryCatch({
    if (nchar(rec$text, type = "bytes") > rec$cap) {
      .g3_abort("g3_io_error", paste0("The record is larger than the ", format(rec$cap / 1024^2), " MB record limit."))
    }
    raw <- .g3_parse_json(rec$text)
    .g3_import_validate(raw)
  }, error = function(e) e)
  if (inherits(parsed, "error")) return(.g3_import_quarantine(db, path, sid, sha, rec, parsed))
  game <- parsed$game
  h <- g3_game_hash(game)
  rec_row <- .g3_db_game_record(game, content_hash = h)
  notes <- parsed$annotations
  out <- tryCatch(.g3_db_write(db, function(con) {
    # Re-check inside the transaction: another writer may have linked this
    # record since the check above.
    again <- DBI::dbGetQuery(con, "SELECT game_id FROM game_sources WHERE source_id = ? AND record_index = ?",
                             params = list(sid, rec$index))
    if (nrow(again)) return(list(status = "duplicate", game_id = again$game_id))
    dup <- DBI::dbGetQuery(con, paste(
      "SELECT game_id FROM games WHERE game_content_hash = ? ORDER BY (status = 'finished') DESC, created_at, game_id LIMIT 1"
    ), params = list(h))
    if (nrow(dup)) {
      .g3_db_link(con, dup$game_id, sid, rec$index, "duplicate", h)
      .g3_import_annotations(con, dup$game_id, notes, sid)
      .g3_db_exec(con, "DELETE FROM quarantine WHERE source_id = ? AND record_index = ?", list(sid, rec$index))
      return(list(status = "duplicate", game_id = dup$game_id))
    }
    gid <- game$metadata$game_id
    origin_id <- NA_character_
    note <- parsed$note
    if (nrow(DBI::dbGetQuery(con, "SELECT 1 FROM games WHERE game_id = ?", params = list(gid)))) {
      origin_id <- gid
      gid <- .g3_new_id("game")
      note <- paste0("The record's game_id ", origin_id, " belongs to a different stored game; stored as ", gid, ".")
    }
    .g3_db_insert_game(con, rec_row, gid, "import", origin_game_id = origin_id)
    .g3_db_link(con, gid, sid, rec$index, "original", h)
    .g3_import_annotations(con, gid, notes, sid)
    .g3_db_exec(con, "DELETE FROM quarantine WHERE source_id = ? AND record_index = ?", list(sid, rec$index))
    list(status = "accepted", game_id = gid, note = note)
  }, what = "import"), error = function(e) e)
  if (inherits(out, "error")) {
    # The transaction was rolled back; nothing of this record was stored.
    return(.g3_import_row(path, rec$index, "error", hash = h, sha = sha,
                          condition = intersect(class(out), .G3_CONDITION_CLASSES)[1L] %|na|% "g3_io_error",
                          message = conditionMessage(out)))
  }
  .g3_import_row(path, rec$index, out$status, out$game_id, h, sha, message = out$note %||% NA_character_)
}

# Validate a parsed record and return the game and its annotations.
#' @noRd
.g3_import_validate <- function(raw) {
  if (!is.list(raw) || is.null(names(raw))) {
    .g3_abort("g3_invalid_state", "Invalid game record: not a JSON object.")
  }
  note <- NA_character_
  if (!is.null(raw[["game_id"]]) && is.na(.g3_db_valid_id(raw[["game_id"]]))) {
    # An unusable id (wrong type, unsafe characters) is replaced; the game
    # content is unaffected.
    raw[["game_id"]] <- NULL
    note <- "The record's game_id was not a safe identifier and was replaced."
  }
  notes <- .g3_import_annotation_rows(raw[["annotations"]])
  raw[["annotations"]] <- list()
  game <- .g3_game_from_list(raw, strict = TRUE)
  list(game = game, annotations = notes, note = note)
}

# Normalise imported annotations to (kind, body, ply, author) rows.
#' @noRd
.g3_import_annotation_rows <- function(x) {
  empty <- data.frame(kind = character(), body = character(), ply = integer(), author = character(),
                      stringsAsFactors = FALSE)
  if (is.null(x) || !length(x)) return(empty)
  lim <- .g3_db_limits()
  items <- if (is.list(x) && is.null(names(x))) x else list(x)
  if (length(items) > lim$annotations) {
    .g3_abort("g3_invalid_state", paste0("Invalid game record: more than ", lim$annotations, " annotations."))
  }
  rows <- lapply(items, function(a) {
    kind <- if (is.list(a)) .g3_chr1(a[["kind"]]) else NA_character_
    body <- if (is.list(a)) .g3_chr1(a[["body"]]) else NA_character_
    if (!is.na(kind) && kind %in% c("comment", "tag", "imported") && !is.na(body) && is.character(a[["body"]]) && nzchar(body)) {
      ply <- a[["ply"]]
      ply <- if (is.numeric(ply) && length(ply) == 1L && is.finite(ply) && ply >= 0 && ply == round(ply)) as.integer(ply) else NA_integer_
      author <- .g3_chr1(a[["author"]])
    } else {
      kind <- "imported"
      body <- .g3_canonical_json(a)
      ply <- NA_integer_
      author <- NA_character_
    }
    if (nchar(body) > lim$annotation_chars) {
      .g3_abort("g3_invalid_state", paste0("Invalid game record: an annotation exceeds ", lim$annotation_chars, " characters."))
    }
    data.frame(kind = kind, body = body, ply = ply, author = author, stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(empty), rows))
}

# Add imported annotations that the game does not have yet.
#' @noRd
.g3_import_annotations <- function(con, game_id, notes, sid) {
  if (!nrow(notes)) return(invisible(0L))
  have <- DBI::dbGetQuery(con, "SELECT kind, body, ply FROM annotations WHERE game_id = ? AND deleted_at IS NULL",
                          params = list(game_id))
  key <- function(k, b, p) paste(k, ifelse(is.na(p), "", p), b, sep = "\r")
  new <- notes[!key(notes$kind, notes$body, notes$ply) %in% key(have$kind, have$body, have$ply), , drop = FALSE]
  new <- new[!duplicated(key(new$kind, new$body, new$ply)), , drop = FALSE]
  if (!nrow(new)) return(invisible(0L))
  n <- nrow(new)
  .g3_db_exec(con, paste(
    "INSERT INTO annotations (game_id, kind, ply, body, author, created_at, source_id) VALUES (?, ?, ?, ?, ?, ?, ?)"
  ), list(rep(game_id, n), new$kind, new$ply, new$body, new$author, rep(.g3_now(), n), rep(sid, n)))
  invisible(n)
}

# Record a rejected record and return its report row.
#' @noRd
.g3_import_quarantine <- function(db, path, sid, sha, rec, err) {
  cls <- intersect(class(err), .G3_CONDITION_CLASSES)[1L]
  if (is.na(cls)) cls <- "g3_invalid_state"
  ctx <- err$context %||% list()
  event <- ctx$first_invalid_event
  event <- if (is.numeric(event) && length(event) == 1L && !is.na(event)) as.integer(event) else NA_integer_
  ply <- if (!is.na(event)) .g3_import_invalid_ply(rec$text, event) else NA_integer_
  msg <- .g3_bound_text(conditionMessage(err))
  claimed <- tryCatch({
    j <- jsonlite::fromJSON(rec$text, simplifyVector = FALSE)
    list(id = .g3_bound_text(.g3_chr1(j[["game_id"]]), 128L), rules = .g3_bound_text(.g3_chr1(j[["rules"]][["hash"]]), 128L))
  }, error = function(e) list(id = NA_character_, rules = NA_character_))
  lim <- .g3_db_limits()
  keep_text <- if (nchar(rec$text, type = "bytes") <= lim$record_text_bytes) rec$text else NA_character_
  now <- .g3_now()
  .g3_db_write(db, function(con) {
    .g3_db_exec(con, paste(
      "INSERT INTO quarantine (source_id, record_index, record_sha256, condition, diagnostic, first_invalid_event,",
      "first_invalid_ply, claimed_game_id, claimed_rules_hash, record_text, first_seen_at, last_seen_at)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (source_id, record_index) DO UPDATE SET",
      "condition = excluded.condition, diagnostic = excluded.diagnostic,",
      "first_invalid_event = excluded.first_invalid_event, first_invalid_ply = excluded.first_invalid_ply,",
      "last_seen_at = excluded.last_seen_at"
    ), list(sid, rec$index, .g3_sha256(rec$text), cls, msg, event, ply, claimed$id, claimed$rules, keep_text, now, now))
  }, what = "quarantine")
  .g3_import_row(path, rec$index, "quarantined", sha = sha, condition = cls, message = msg, event = event, ply = ply)
}

# Board ply of the first invalid event: replay the valid events before it.
#' @noRd
.g3_import_invalid_ply <- function(text, event) {
  tryCatch({
    raw <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    if (!is.null(raw[["game_id"]]) && is.na(.g3_db_valid_id(raw[["game_id"]]))) raw[["game_id"]] <- NULL
    bad <- raw[["events"]][[event]]
    raw$events <- raw[["events"]][seq_len(event - 1L)]
    raw$final <- NULL
    raw[["annotations"]] <- list()
    g <- .g3_game_from_list(raw, strict = TRUE)
    as.integer(g$state$ply + if (identical(bad[["type"]], "action")) 1L else 0L)
  }, error = function(e) NA_integer_)
}

#' List quarantined import records
#'
#' Shows records that the importer rejected, with their source and
#' diagnostic, for inspection. The original files are untouched; small
#' rejected records are also kept verbatim in the database.
#'
#' @param db A `g3_db`.
#' @param limit,offset Paging, as for [g3_query_games()].
#' @return A data frame with `quarantine_id`, `source` (file name),
#'   `source_sha256`, `record`, `condition`, `diagnostic`,
#'   `first_invalid_event`, `first_invalid_ply`, `claimed_game_id`,
#'   `claimed_rules_hash`, `has_text` (whether the record text is kept),
#'   `first_seen_at` and `last_seen_at`, newest first.
#' @examples
#' bad <- base::tempfile(fileext = ".jsonl")
#' base::writeLines("{not json", bad)
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' gambitR3D::g3_import_games(db, bad)$status
#' gambitR3D::g3_quarantined_records(db)[, c("record", "condition")]
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_quarantined_records <- function(db, limit = 100L, offset = 0L) {
  lim <- .g3_db_limits()$page
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit < 1 || limit > lim) {
    .g3_abort("g3_io_error", paste0("`limit` must be a whole number from 1 to ", lim, "."))
  }
  if (!is.numeric(offset) || length(offset) != 1L || is.na(offset) || offset < 0) {
    .g3_abort("g3_io_error", "`offset` must be a non-negative whole number.")
  }
  rows <- .g3_db_get(db, paste(
    "SELECT q.quarantine_id, s.label AS source, s.source_sha256, q.record_index AS record, q.condition, q.diagnostic,",
    "q.first_invalid_event, q.first_invalid_ply, q.claimed_game_id, q.claimed_rules_hash,",
    "q.record_text IS NOT NULL AS has_text, q.first_seen_at, q.last_seen_at",
    "FROM quarantine q JOIN sources s ON s.source_id = q.source_id",
    "ORDER BY q.last_seen_at DESC, q.quarantine_id DESC LIMIT ? OFFSET ?"
  ), list(as.integer(limit), as.integer(offset)))
  .g3_db_typed(rows, c(
    quarantine_id = "integer", source = "character", source_sha256 = "character", record = "integer",
    condition = "character", diagnostic = "character", first_invalid_event = "integer",
    first_invalid_ply = "integer", claimed_game_id = "character", claimed_rules_hash = "character",
    has_text = "logical", first_seen_at = "character", last_seen_at = "character"
  ))
}
