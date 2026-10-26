# Phase 08: the versioned local game database.

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# Engine-generated fixtures (see R/database-fixtures.R), built once.
ex <- .g3_example_games(n_search = 1L, n_random = 1L, max_plies = 6L, nodes = 40L)

tmp_db <- function(env = parent.frame(), name = "games.sqlite") {
  dir <- withr::local_tempdir(.local_envir = env)
  db <- g3_open_game_db(file.path(dir, name))
  withr::defer(g3_close_game_db(db), envir = env)
  db
}

envelope_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
}

write_lines_file <- function(lines, path) {
  writeLines(lines, path, useBytes = TRUE)
  path
}

file_sha <- function(p) digest::digest(file = p, algo = "sha256")

db_count <- function(db, table) DBI::dbGetQuery(db$con, paste("SELECT count(*) AS n FROM", table))$n

test_that("opening creates a versioned schema with explicit pragmas", {
  db <- tmp_db()
  expect_s3_class(db, "g3_db")
  expect_identical(db$schema_version, 1L)
  expect_identical(db$journal_mode, "wal")
  expect_identical(db$synchronous, "full")
  expect_identical(db$sqlite_version, RSQLite::rsqliteVersion()[["library"]])
  q <- function(sql) DBI::dbGetQuery(db$con, sql)[[1L]]
  expect_identical(q("PRAGMA foreign_keys"), 1L)
  expect_identical(q("PRAGMA busy_timeout"), 5000L)
  expect_identical(q("PRAGMA user_version"), 1L)
  expect_identical(q("PRAGMA application_id"), .G3_DB_APPLICATION_ID)
  # Loadable extensions are disabled.
  expect_error(DBI::dbGetQuery(db$con, "SELECT load_extension('nope')"), "not authorized")
  mig <- DBI::dbGetQuery(db$con, "SELECT * FROM schema_migrations")
  expect_identical(mig$version, 1L)
  expect_identical(mig$checksum, .g3_db_migrations()[[1L]]$checksum)
  expect_match(mig$checksum, "^[0-9a-f]{64}$")
  tables <- DBI::dbListTables(db$con)
  expect_true(all(c("rules_profiles", "sources", "games", "game_sources", "actions", "snapshots", "annotations",
                    "analysis_runs", "evaluations", "quarantine", "schema_migrations") %in% tables))
  # Every table is STRICT, so integer ply columns refuse non-integers.
  strict <- DBI::dbGetQuery(db$con, "SELECT name, strict FROM pragma_table_list WHERE schema = 'main' AND name NOT LIKE 'sqlite_%'")
  expect_true(all(strict$strict == 1L))
  g3_save_game(db, ex$checkmate)
  expect_error(DBI::dbExecute(db$con, "UPDATE actions SET ply = 1.5 WHERE game_id = 'example-checkmate'"), "REAL")
  expect_error(DBI::dbExecute(db$con, "UPDATE actions SET ply = -1 WHERE game_id = 'example-checkmate'"), "CHECK")
  # Not a library location, not a directory, not an SQLite URI.
  lib_path <- file.path(.libPaths()[1L], "g3-should-not-exist.sqlite")
  expect_error(g3_open_game_db(lib_path), class = "g3_io_error")
  expect_false(file.exists(lib_path))
  expect_error(g3_open_game_db(tempdir()), class = "g3_io_error")
  expect_error(g3_open_game_db("file:x.sqlite?mode=rwc"), class = "g3_io_error")
})

test_that("closing is idempotent and disconnects cleanly", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "games.sqlite")
  db <- g3_open_game_db(path)
  g3_save_game(db, ex$live)
  con <- db$con
  expect_true(file.exists(paste0(path, "-wal")))
  expect_true(g3_close_game_db(db))
  expect_true(g3_close_game_db(db))
  expect_false(DBI::dbIsValid(con))
  expect_error(g3_query_games(db), class = "g3_io_error")
  # The last writer checkpoints, removes the WAL companions and leaves a
  # self-contained rollback-journal file.
  expect_identical(list.files(dir), "games.sqlite")
  expect_identical(.g3_sqlite_header(path, file.info(path)$size)$journal, "rollback")
  # With a reader still connected the file stays in WAL mode until later.
  db <- g3_open_game_db(path)
  ro <- g3_open_game_db(path, read_only = TRUE)
  g3_close_game_db(db)
  expect_identical(.g3_sqlite_header(path, file.info(path)$size)$journal, "wal")
  expect_identical(nrow(g3_query_games(ro)), 1L)
  g3_close_game_db(ro)
  g3_close_game_db(g3_open_game_db(path))
  expect_identical(list.files(dir), "games.sqlite")
})

test_that("read-only mode never creates, migrates or changes journalling", {
  dir <- withr::local_tempdir()
  missing <- file.path(dir, "missing.sqlite")
  expect_error(g3_open_game_db(missing, read_only = TRUE), class = "g3_io_error")
  expect_false(file.exists(missing))
  path <- file.path(dir, "games.sqlite")
  db <- g3_open_game_db(path)
  g3_save_game(db, ex$checkmate)
  g3_close_game_db(db)
  before <- file_sha(path)
  ro <- g3_open_game_db(path, read_only = TRUE)
  expect_true(ro$read_only)
  expect_identical(ro$journal_mode, "delete")
  expect_identical(nrow(g3_query_games(ro)), 1L)
  expect_s3_class(g3_load_game(ro, "example-checkmate"), "g3_game")
  expect_true(g3_check_game_db(ro)$ok)
  expect_error(g3_save_game(ro, ex$live), class = "g3_io_error")
  expect_error(g3_import_games(ro, path), class = "g3_io_error")
  expect_error(g3_annotate_game(ro, "example-checkmate", "x"), class = "g3_io_error")
  expect_error(g3_delete_games(ro, "example-checkmate", confirm = TRUE), class = "g3_io_error")
  expect_error(DBI::dbExecute(ro$con, "DELETE FROM games"), "readonly|read-only|query_only")
  g3_close_game_db(ro)
  expect_identical(file_sha(path), before)
  expect_identical(list.files(dir), "games.sqlite")
  # A rollback-journal file (a backup) opened read-only gains no companion files.
  db <- g3_open_game_db(path)
  b <- g3_backup_game_db(db, file.path(dir, "backup.sqlite"))
  g3_close_game_db(db)
  files_before <- list.files(dir)
  ro <- g3_open_game_db(b$path, read_only = TRUE)
  expect_identical(ro$journal_mode, "delete")
  g3_close_game_db(ro)
  expect_identical(list.files(dir), files_before)
  expect_identical(file_sha(b$path), b$sha256)
  # Foreign SQLite files and empty files are refused unchanged.
  other <- file.path(dir, "other.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), other)
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbDisconnect(con)
  sha <- file_sha(other)
  expect_error(g3_open_game_db(other, read_only = TRUE), "not a gambitR3D")
  expect_error(g3_open_game_db(other), "not a gambitR3D")
  expect_identical(file_sha(other), sha)
  empty <- file.path(dir, "empty.sqlite")
  file.create(empty)
  expect_error(g3_open_game_db(empty, read_only = TRUE), class = "g3_io_error")
  text <- write_lines_file("not a database", file.path(dir, "text.sqlite"))
  expect_error(g3_open_game_db(text), "Not an SQLite")
})

test_that("migrations roll back on failure and the database recovers", {
  dir <- withr::local_tempdir()
  good <- .g3_db_migrations()
  bad <- c(good, list(list(version = 2L, name = "broken", sql = c("CREATE TABLE extra (x INTEGER)", "THIS IS NOT SQL"),
                           checksum = strrep("c", 64))))
  # A failing first open removes the file it created.
  fresh <- file.path(dir, "fresh.sqlite")
  expect_error(.g3_db_open(fresh, migrations = bad), "rolled back")
  expect_false(file.exists(fresh))
  # A failing upgrade leaves version 1 and its data intact.
  path <- file.path(dir, "games.sqlite")
  db <- g3_open_game_db(path)
  g3_save_game(db, ex$resign)
  g3_close_game_db(db)
  err <- tryCatch(.g3_db_open(path, migrations = bad), g3_error = function(e) e)
  expect_s3_class(err, "g3_io_error")
  expect_match(conditionMessage(err), "stays at version 1")
  db <- g3_open_game_db(path)
  expect_identical(DBI::dbGetQuery(db$con, "PRAGMA user_version")[[1L]], 1L)
  expect_false("extra" %in% DBI::dbListTables(db$con))
  expect_identical(nrow(g3_query_games(db)), 1L)
  expect_true(g3_check_game_db(db)$ok)
  g3_close_game_db(db)
  # A newer schema is refused by this version in both modes, unchanged.
  v2 <- c(good, list(list(version = 2L, name = "extra table", sql = "CREATE TABLE extra (x INTEGER) STRICT")))
  v2[[2L]]$checksum <- .g3_sha256(v2[[2L]]$sql)
  db <- .g3_db_open(path, migrations = v2)
  expect_identical(db$schema_version, 2L)
  g3_close_game_db(db)
  sha <- file_sha(path)
  expect_error(g3_open_game_db(path), "Update the package")
  expect_error(g3_open_game_db(path, read_only = TRUE), "Update the package")
  expect_identical(file_sha(path), sha)
})

test_that("older schemas need a writable open and tampered migrations are detected", {
  dir <- withr::local_tempdir()
  good <- .g3_db_migrations()
  v2 <- c(good, list(list(version = 2L, name = "extra table", sql = "CREATE TABLE extra (x INTEGER) STRICT")))
  v2[[2L]]$checksum <- .g3_sha256(v2[[2L]]$sql)
  path <- file.path(dir, "games.sqlite")
  g3_close_game_db(g3_open_game_db(path))
  expect_error(.g3_db_open(path, read_only = TRUE, migrations = v2), "older schema")
  db <- .g3_db_open(path, migrations = v2)
  expect_identical(db$schema_version, 2L)
  g3_close_game_db(db)
  # Tampering with a recorded migration checksum is detected on open.
  p2 <- file.path(dir, "tamper.sqlite")
  g3_close_game_db(g3_open_game_db(p2))
  con <- DBI::dbConnect(RSQLite::SQLite(), p2)
  DBI::dbExecute(con, "UPDATE schema_migrations SET checksum = ?", params = list(strrep("0", 64)))
  DBI::dbDisconnect(con)
  expect_error(g3_open_game_db(p2), "checksum")
  expect_error(g3_open_game_db(p2, read_only = TRUE), "checksum")
})

test_that("network file systems never get a WAL database", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "games.sqlite")
  g3_close_game_db(g3_open_game_db(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), path) # leave the file in WAL mode
  DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  DBI::dbDisconnect(con)
  local_mocked_bindings(.g3_fs_type = function(dir) "nfs4")
  expect_error(g3_open_game_db(path), "network file system")
  expect_error(g3_open_game_db(path, read_only = TRUE), "network file system")
  p2 <- file.path(dir, "share.sqlite")
  expect_warning(db <- g3_open_game_db(p2), class = "g3_warning")
  expect_identical(db$journal_mode, "delete")
  expect_identical(db$filesystem, "nfs4")
  g3_close_game_db(db)
})

test_that("saving is idempotent, extends live games and refuses stale history", {
  db <- tmp_db()
  g <- ex$live
  r1 <- g3_save_game(db, g)
  expect_identical(r1$status, "inserted")
  expect_identical(r1$catalogue_status, "live")
  expect_identical(g3_save_game(db, g)$status, "unchanged")
  back <- g3_load_game(db, g$metadata$game_id)
  expect_identical(back$catalogue$status, "live")
  expect_true(back$catalogue$resumable)
  expect_identical(g3_game_hash(back), g3_game_hash(g))
  # Resume: the loaded game gains events and is extended in place.
  acts <- g3_legal_actions(back$state)$action_id
  g2 <- g3_game_apply(back, acts[1L])
  r2 <- g3_save_game(db, g2)
  expect_identical(r2$status, "updated")
  expect_identical(r2$revision, 2L)
  expect_identical(r2$n_events, nrow(g2$events))
  expect_identical(nrow(DBI::dbGetQuery(db$con, "SELECT * FROM actions WHERE game_id = ?", params = list(g$metadata$game_id))),
                   nrow(g2$events))
  # A different continuation of the old revision is stale.
  g_other <- g3_game_apply(back, acts[2L])
  expect_error(g3_save_game(db, g_other), class = "g3_stale_revision")
  # Takeback and resignation are appended events; event sequence and ply differ.
  g3 <- g3_game_event(g3_game_event(g2, "takeback"), "resign")
  r3 <- g3_save_game(db, g3)
  expect_identical(r3$status, "updated")
  expect_identical(r3$catalogue_status, "finished")
  ev <- DBI::dbGetQuery(db$con, "SELECT event_seq, event_type, ply, action_id FROM actions WHERE game_id = ? ORDER BY event_seq",
                        params = list(g$metadata$game_id))
  expect_identical(ev$event_seq, seq_len(nrow(g3$events)))
  expect_identical(ev$event_type[nrow(ev) - 0:1], c("resign", "takeback"))
  expect_identical(ev$ply[nrow(ev)], ev$ply[nrow(ev) - 1L])
  expect_true(all(is.na(ev$action_id[ev$event_type != "action"])))
  expect_false(identical(ev$event_seq, ev$ply))
  fin <- g3_load_game(db, g$metadata$game_id)
  expect_identical(fin$catalogue$status, "finished")
  expect_false(fin$catalogue$resumable)
  expect_identical(fin$metadata$result, g3$metadata$result)
  # A finished copy under another id is linked, not duplicated.
  copy <- ex$resign
  g3_save_game(db, copy)
  copy$metadata$game_id <- "another-id"
  dup <- g3_save_game(db, copy)
  expect_identical(dup$status, "duplicate")
  expect_identical(dup$game_id, ex$resign$metadata$game_id)
  expect_identical(nrow(g3_query_games(db, list(content_hash = g3_game_hash(copy)))), 1L)
  # Metadata that contradicts the events is refused.
  liar <- ex$live
  liar$metadata$game_id <- "liar"
  liar$metadata$result <- "white"
  expect_error(g3_save_game(db, liar), class = "g3_invalid_state")
  # Invalid ids are refused before anything is written.
  odd <- ex$live
  odd$metadata$game_id <- "../../etc/passwd"
  expect_error(g3_save_game(db, odd), class = "g3_invalid_state")
  expect_true(g3_check_game_db(db)$ok)
})

test_that("import accepts, deduplicates and keeps every provenance link", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  f1 <- file.path(dir, "resign.g3.json")
  g3_write_game(ex$resign, f1)
  coll <- file.path(dir, "games.jsonl")
  .g3_write_jsonl(list(ex$draw_agreed, ex$checkmate, ex$takeback), coll)
  shas <- c(file_sha(f1), file_sha(coll))
  rep <- g3_import_games(db, c(f1, coll))
  expect_identical(rep$status, rep("accepted", 4L))
  expect_identical(rep$record, c(1L, 1L, 2L, 3L))
  expect_identical(rep$game_id, vapply(list(ex$resign, ex$draw_agreed, ex$checkmate, ex$takeback),
                                       function(g) g$metadata$game_id, ""))
  # Resignation and agreed draw after the same moves stay different games.
  q <- DBI::dbGetQuery(db$con, "SELECT game_id, trajectory_hash, game_content_hash FROM games WHERE game_id IN (?, ?)",
                       params = list("example-resign", "example-draw-agreed"))
  expect_identical(length(unique(q$trajectory_hash)), 1L)
  expect_identical(length(unique(q$game_content_hash)), 2L)
  # Idempotent reimport: same bytes share the source row; nothing new.
  n_games <- db_count(db, "games")
  rep2 <- g3_import_games(db, c(f1, coll))
  expect_identical(rep2$status, rep("duplicate", 4L))
  expect_identical(rep2$game_id, rep$game_id)
  expect_identical(db_count(db, "games"), n_games)
  expect_identical(db_count(db, "sources"), 2L)
  expect_identical(DBI::dbGetQuery(db$con, "SELECT import_count FROM sources ORDER BY source_id")$import_count, c(2L, 2L))
  # Same game content in different bytes, with an annotation: one game, two
  # provenance links, the new annotation kept once.
  raw <- .g3_game_to_list(ex$resign)
  raw$metadata$event <- "club night"
  raw$annotations <- list(list(kind = "comment", body = "Resigned early.", ply = 2L), list(free = "form"))
  f2 <- write_lines_file(envelope_json(raw), file.path(dir, "resign-annotated.g3.json"))
  rep3 <- g3_import_games(db, f2)
  expect_identical(rep3$status, "duplicate")
  expect_identical(rep3$game_id, "example-resign")
  q <- g3_query_games(db, list(game_id = "example-resign"))
  expect_identical(q$n_sources, 2L)
  ann <- g3_game_annotations(db, "example-resign")
  expect_identical(ann$kind, c("comment", "imported"))
  expect_identical(ann$source, rep("resign-annotated.g3.json", 2L))
  g3_import_games(db, f2)
  expect_identical(nrow(g3_game_annotations(db, "example-resign")), 2L)
  # A different game reusing a stored id gets a fresh id; the claim is kept.
  clash <- ex$overlap
  clash$metadata$game_id <- "example-resign"
  f3 <- file.path(dir, "clash.g3.json")
  g3_write_game(clash, f3)
  rep4 <- g3_import_games(db, f3)
  expect_identical(rep4$status, "accepted")
  expect_false(identical(rep4$game_id, "example-resign"))
  expect_identical(DBI::dbGetQuery(db$con, "SELECT origin_game_id FROM games WHERE game_id = ?",
                                   params = list(rep4$game_id))$origin_game_id, "example-resign")
  expect_match(rep4$message, "different stored game")
  # An unsafe id in a record (path-like) is replaced, never used.
  unsafe <- .g3_game_to_list(ex$live)
  unsafe$game_id <- "../../etc/passwd"
  f4 <- write_lines_file(envelope_json(unsafe), file.path(dir, "unsafe.g3.json"))
  rep5 <- g3_import_games(db, f4)
  expect_identical(rep5$status, "accepted")
  expect_match(rep5$game_id, "^game_[0-9a-f]+$")
  expect_match(rep5$message, "not a safe identifier")
  # Input files are never modified.
  expect_identical(c(file_sha(f1), file_sha(coll)), shas)
  expect_true(g3_check_game_db(db)$ok)
})

test_that("invalid records are quarantined with the first invalid event and ply", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  good <- .g3_game_to_list(ex$takeback)
  base <- .g3_game_to_list(ex$random_1)
  illegal <- base
  illegal$events[[3L]]$action_id <- "m:W:0:0-W:3:3"
  wrong_rules <- base
  wrong_rules$rules$hash <- strrep("0", 64)
  future <- base
  future$format_version <- 9L
  schema <- base
  schema$action_schema <- "g3-action/99"
  checksum <- base
  checksum$events[[2L]]$post_state_hash <- strrep("a", 64)
  lines <- c(envelope_json(good), envelope_json(illegal), envelope_json(wrong_rules), "{not json",
             envelope_json(future), "[1, 2, 3]", "", envelope_json(schema), envelope_json(checksum))
  coll <- write_lines_file(lines, file.path(dir, "mixed.jsonl"))
  sha <- file_sha(coll)
  pgn <- write_lines_file("1. e4 e5", file.path(dir, "old.pgn"))
  rep <- g3_import_games(db, c(coll, file.path(dir, "missing.g3.json"), pgn))
  # Every input is accounted for: 8 records (the blank line is skipped) and 2 errors.
  expect_identical(nrow(rep), 10L)
  expect_identical(rep$record[1:8], c(1:6, 8:9))
  expect_identical(rep$status, c("accepted", rep("quarantined", 7L), "error", "error"))
  expect_identical(rep$condition[2:8], c("g3_invalid_state", "g3_unsupported_rules", "g3_io_error",
                                         "g3_unsupported_rules", "g3_invalid_state", "g3_invalid_state",
                                         "g3_invalid_state"))
  expect_identical(rep$first_invalid_event[2], 3L)
  expect_identical(rep$first_invalid_ply[2], 3L)
  expect_identical(rep$first_invalid_event[8], 2L)
  expect_match(rep$message[10], "PGN")
  expect_true(all(nchar(rep$message[!is.na(rep$message)]) <= 500L))
  # Nothing of a rejected record entered the game tables.
  expect_identical(db_count(db, "games"), 1L)
  expect_identical(db_count(db, "actions"), nrow(ex$takeback$events))
  qr <- g3_quarantined_records(db)
  expect_identical(sort(qr$record), c(2:6, 8:9))
  expect_true(all(qr$source == "mixed.jsonl"))
  expect_true(all(qr$has_text))
  expect_identical(qr$first_invalid_ply[qr$record == 2L], 3L)
  expect_identical(DBI::dbGetQuery(db$con, "SELECT status FROM sources")$status, "complete")
  expect_identical(file_sha(coll), sha)
  # Re-importing the same bytes keeps one quarantine row per record.
  rep2 <- g3_import_games(db, coll)
  expect_identical(rep2$status, c("duplicate", rep("quarantined", 7L)))
  expect_identical(db_count(db, "quarantine"), 7L)
})

test_that("oversized, compressed and deeply nested inputs are bounded", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  g1 <- file.path(dir, "one.g3.json")
  g3_write_game(ex$checkmate, g1)
  big_line <- envelope_json(.g3_game_to_list(ex$random_1))
  small_line <- envelope_json(.g3_game_to_list(ex$checkmate))
  coll <- write_lines_file(c(small_line, big_line), file.path(dir, "two.jsonl"))
  deep <- .g3_game_to_list(ex$checkmate)
  nest <- list()
  for (i in 1:40) nest <- list(nest)
  deep$provenance <- nest
  deep_f <- write_lines_file(envelope_json(deep), file.path(dir, "deep.jsonl"))
  # gzip input works and a decompression bomb stops at the limit.
  gz <- file.path(dir, "one.g3.json.gz")
  con <- gzfile(gz, "wb")
  writeBin(readBin(g1, "raw", file.info(g1)$size), con)
  close(con)
  bomb <- file.path(dir, "bomb.jsonl.gz")
  con <- gzfile(bomb, "wb")
  for (i in 1:8) writeBin(raw(1024^2), con)
  close(con)
  nul <- file.path(dir, "nul.g3.json")
  writeBin(as.raw(c(0x7b, 0x00, 0x7d)), nul)
  expect_gt(nchar(big_line), nchar(small_line) + 500L)
  local_mocked_bindings(.g3_db_limits = function() {
    list(file_bytes = 50 * 1024^2, collection_bytes = 4 * 1024^2, compressed_bytes = 1024^2,
         line_bytes = nchar(small_line) + 500, records = 100L, record_text_bytes = 1024^2,
         diagnostic_chars = 500L, annotation_chars = 65536L, annotations = 1000L, page = 10000L,
         text_chars = 10000L)
  })
  rep <- g3_import_games(db, c(coll, deep_f, gz, bomb, nul))
  expect_identical(rep$status, c("accepted", "quarantined", "quarantined", "duplicate", "error", "error"))
  expect_match(rep$message[2], "record limit")
  expect_match(rep$message[3], "nesting")
  expect_match(rep$message[5], "decompressed")
  expect_match(rep$message[6], "NUL")
  local_mocked_bindings(.g3_db_limits = function() {
    list(file_bytes = 1000, collection_bytes = 1000, compressed_bytes = 1000, line_bytes = 1000, records = 1L,
         record_text_bytes = 10, diagnostic_chars = 500L, annotation_chars = 65536L, annotations = 1000L,
         page = 10000L, text_chars = 10000L)
  })
  rep2 <- g3_import_games(db, c(g1, coll))
  expect_identical(rep2$status, c("error", "error"))
  expect_match(rep2$message[1], "larger than")
})

test_that("an injected failure rolls back exactly one record", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  coll <- file.path(dir, "three.jsonl")
  .g3_write_jsonl(list(ex$resign, ex$checkmate, ex$overlap), coll)
  calls <- 0L
  real <- .g3_db_insert_snapshots
  local_mocked_bindings(.g3_db_insert_snapshots = function(con, game_id, snaps) {
    calls <<- calls + 1L
    if (calls == 2L) stop("injected disk failure")
    real(con, game_id, snaps)
  })
  rep <- g3_import_games(db, coll)
  expect_identical(rep$status, c("accepted", "error", "accepted"))
  expect_match(rep$message[2], "injected disk failure")
  gid <- ex$checkmate$metadata$game_id
  for (tab in c("games", "game_players", "actions", "snapshots", "game_sources")) {
    n <- DBI::dbGetQuery(db$con, paste("SELECT count(*) AS n FROM", tab, "WHERE game_id = ?"), params = list(gid))$n
    expect_identical(n, 0L, label = tab)
  }
  expect_false(db$in_tx)
  # A failed save leaves nothing and reports a typed error.
  calls <- 1L
  expect_error(g3_save_game(db, ex$live), class = "g3_io_error")
  expect_identical(nrow(g3_query_games(db, list(game_id = ex$live$metadata$game_id))), 0L)
  # Re-running the import completes the missing record.
  local_mocked_bindings(.g3_db_insert_snapshots = real)
  rep2 <- g3_import_games(db, coll)
  expect_identical(rep2$status, c("duplicate", "accepted", "duplicate"))
  expect_true(g3_check_game_db(db)$ok)
})

test_that("an interrupted import keeps committed records and resumes", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  coll <- file.path(dir, "three.jsonl")
  .g3_write_jsonl(list(ex$resign, ex$checkmate, ex$overlap), coll)
  calls <- 0L
  real <- .g3_db_insert_actions
  local_mocked_bindings(.g3_db_insert_actions = function(con, game_id, actions) {
    calls <<- calls + 1L
    # A simulated user interrupt: an "interrupt" condition, not an error.
    if (calls == 2L) {
      stop(structure(class = c("g3_test_interrupt", "interrupt", "condition"), list(message = "interrupt", call = NULL)))
    }
    real(con, game_id, actions)
  })
  # Caught here (an exiting handler) so it never reaches testthat's own
  # interrupt handling.
  res <- tryCatch(g3_import_games(db, coll), g3_test_interrupt = function(e) "interrupted")
  expect_identical(res, "interrupted")
  expect_false(db$in_tx)
  expect_identical(g3_query_games(db)$game_id, ex$resign$metadata$game_id)
  expect_identical(db_count(db, "actions"), nrow(ex$resign$events))
  expect_identical(DBI::dbGetQuery(db$con, "SELECT status FROM sources")$status, "importing")
  local_mocked_bindings(.g3_db_insert_actions = real)
  rep <- g3_import_games(db, coll)
  expect_identical(rep$status, c("duplicate", "accepted", "accepted"))
  expect_identical(DBI::dbGetQuery(db$con, "SELECT status FROM sources")$status, "complete")
})

test_that("overlapping projected squares and platform rotation survive storage", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  for (g in list(ex$overlap, ex$platform_rotation)) g3_save_game(db, g)
  ov <- g3_load_game(db, "example-overlap")
  expect_identical(g3_state_hash(ov$state), g3_state_hash(ex$overlap$state))
  sq <- ov$state$pieces$square_id
  expect_true(all(c("W:1:2", "N:1:0") %in% sq))
  tabs <- .g3_tables()
  core <- .g3_core(ov$state, tabs)
  co <- .g3_square_coords(core$dock, core$orient, tabs)
  i <- match(c("W:1:2", "N:1:0"), tabs$sq_id)
  expect_identical(co$ci[i[1]], co$ci[i[2]]) # same projected file and rank
  rot <- g3_load_game(db, "example-platform-rotation")
  b <- rot$state$boards
  expect_identical(b$orientation[b$id == "WQ"], 180L)
  expect_identical(b$dock[b$id == "WQ"], "B:L:hi")
  snap <- DBI::dbGetQuery(db$con, "SELECT state_json FROM snapshots WHERE game_id = ? AND kind = 'final'",
                          params = list("example-platform-rotation"))$state_json
  expect_match(snap, "\"orientation\":180")
  expect_identical(rot$state$pieces$type[rot$state$pieces$id == "wP"], "Q")
  # The same through export and import into a fresh database.
  out <- file.path(dir, "pair.jsonl")
  g3_export_games(db, c("example-overlap", "example-platform-rotation"), out)
  db2 <- tmp_db(name = "second.sqlite")
  rep <- g3_import_games(db2, out)
  expect_identical(rep$status, c("accepted", "accepted"))
  for (id in rep$game_id) {
    expect_identical(g3_state_hash(g3_load_game(db2, id)$state), g3_state_hash(g3_load_game(db, id)$state))
  }
})

test_that("export round-trips losslessly with checksums and without private paths", {
  db <- tmp_db()
  dir <- withr::local_tempdir()
  for (g in ex) g3_save_game(db, g)
  g3_annotate_game(db, "example-checkmate", "mate", kind = "tag")
  g3_annotate_game(db, "example-checkmate", "Queen to e5 ends it.", ply = 1L, author = "coach")
  ids <- g3_query_games(db, list(order_by = "game_id", descending = FALSE), limit = 100L)$game_id
  out <- file.path(dir, "all.jsonl")
  man <- g3_export_games(db, ids, out)
  expect_identical(man$format, "gambitR3D-export")
  expect_identical(man$n_games, length(ids))
  files <- file.path(dir, man$files$file)
  expect_true(all(file.exists(c(files, man$manifest_path))))
  expect_identical(unname(vapply(files, file_sha, "")), man$files$sha256)
  disk <- jsonlite::fromJSON(man$manifest_path)
  expect_identical(disk$files$sha256, man$files$sha256)
  for (f in c(files, man$manifest_path)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_false(grepl(dir, txt, fixed = TRUE))
    expect_false(grepl(dirname(db$path), txt, fixed = TRUE))
  }
  expect_match(paste(readLines(file.path(dir, "all.moves.txt")), collapse = "\n"), "not an import format")
  # Re-import into a fresh database reproduces every game and annotation.
  db2 <- tmp_db(name = "copy.sqlite")
  rep <- g3_import_games(db2, out)
  expect_identical(rep$status, rep("accepted", length(ids)))
  expect_identical(rep$game_id, ids)
  a <- g3_query_games(db, list(order_by = "game_id", descending = FALSE))
  b <- g3_query_games(db2, list(order_by = "game_id", descending = FALSE))
  expect_identical(a$game_content_hash, b$game_content_hash)
  expect_identical(a[, c("status", "result", "termination", "plies", "white", "black")],
                   b[, c("status", "result", "termination", "plies", "white", "black")])
  expect_identical(g3_game_annotations(db2, "example-checkmate")$body,
                   g3_game_annotations(db, "example-checkmate")$body)
  expect_identical(g3_import_games(db, out)$status, rep("duplicate", length(ids)))
  # Directory layout: one native file per game, each readable on its own.
  d <- file.path(dir, "folder")
  man2 <- g3_export_games(db, ids[1:2], d)
  expect_identical(sort(list.files(d)), sort(c(man2$files$file, "manifest.json")))
  one <- g3_read_game(file.path(d, man2$games$file[1]))
  expect_identical(g3_game_hash(one), man2$games$game_content_hash[1])
  # Refusals: PGN/FEN, overwriting, unknown and deleted ids.
  expect_error(g3_export_games(db, ids, file.path(dir, "x.pgn")), "native")
  expect_error(g3_export_games(db, ids, file.path(dir, "x.fen")), "PGN and FEN")
  expect_error(g3_export_games(db, ids, out), "overwrite")
  expect_error(g3_export_games(db, "nope", file.path(dir, "y.jsonl")), "No game")
  g3_delete_games(db, ids[1], confirm = TRUE)
  expect_error(g3_export_games(db, ids[1], file.path(dir, "z.jsonl")), "Deleted")
  expect_false(file.exists(file.path(dir, "z.jsonl")))
})

test_that("queries filter with bound parameters, page stably and stay typed", {
  db <- tmp_db()
  hostile <- c("Robert'); DROP TABLE games;--", "<script>alert('x')</script>", "100% _wild\\card", "Zoë \"Q\"")
  for (i in seq_along(hostile)) {
    g <- ex$resign
    g$metadata$game_id <- paste0("hostile-", i)
    g$metadata$players <- list(white = hostile[i], black = "Ben")
    g$metadata$agents <- list(white = list(agent = "human"), black = list(agent = "search", model_revision = paste0("rev-", i)))
    g$events <- g$events[seq_len(i %% 3L), , drop = FALSE] # different content: 1, 2, 0 events
    g$state <- g3_replay_game(g)
    g$metadata$result <- NA_character_
    g$metadata$termination <- NA_character_
    g3_save_game(db, g)
  }
  for (g in ex) g3_save_game(db, g)
  g3_annotate_game(db, "example-checkmate", "mate", kind = "tag")
  expect_true("games" %in% DBI::dbListTables(db$con))
  for (h in hostile) {
    r <- g3_query_games(db, list(player = h))
    expect_identical(r$white, h)
  }
  expect_identical(g3_query_games(db, list(player = toupper(hostile[1])))$white, hostile[1])
  # A literal substring search: '%' and '_' are not wildcards.
  expect_identical(g3_query_games(db, list(player_contains = "% _"))$white, hostile[3])
  expect_identical(nrow(g3_query_games(db, list(player_contains = "%"))), 1L)
  expect_identical(g3_query_games(db, list(model_revision = "rev-2"))$game_id, "hostile-2")
  by_search <- g3_query_games(db, list(agent = "search"))
  expect_gte(nrow(by_search), 5L)
  expect_true(all(by_search$white_agent %in% "search" | by_search$black_agent %in% "search"))
  # Filters.
  q <- function(...) g3_query_games(db, list(...), limit = 1000L)
  expect_setequal(q(result = "draw")$game_id, "example-draw-agreed")
  expect_setequal(q(termination = "checkmate", tag = "mate")$game_id, "example-checkmate")
  expect_true(all(q(status = "live")$status == "live"))
  expect_true(all(q(min_plies = 3L)$plies >= 3L))
  expect_true(all(q(max_plies = 1L)$plies <= 1L))
  expect_identical(nrow(q(rules_hash = .g3_tables()$rules_hash)), nrow(q()))
  expect_identical(nrow(q(rules_version = "experimental-v0.1")), nrow(q()))
  expect_identical(nrow(q(rules_hash = strrep("0", 64))), 0L)
  expect_identical(nrow(q(date_from = as.Date("2026-10-26"), date_to = "2026-10-26")), nrow(q()))
  expect_identical(nrow(q(date_to = "2026-10-25")), 0L)
  expect_identical(nrow(q(source = "app")), nrow(q()))
  expect_identical(nrow(q(origin = "import")), 0L)
  # Rejected filter specifications.
  expect_error(q(nonsense = 1), "Unknown filter")
  expect_error(q(order_by = "g.game_id; DROP TABLE games"), "order_by")
  expect_error(q(result = "won"), "result")
  expect_error(q(min_plies = -1), "min_plies")
  expect_error(q(date_from = "26/10/2026"), "date_from")
  expect_error(g3_query_games(db, limit = 0L), "limit")
  # Stable paging covers every row exactly once.
  all <- g3_query_games(db, list(order_by = "result"), limit = 1000L)
  pages <- do.call(rbind, lapply(0:4, function(p) g3_query_games(db, list(order_by = "result"), limit = 3L, offset = p * 3L)))
  expect_identical(pages$game_id, all$game_id)
  expect_identical(attr(all, "total"), nrow(all))
  expect_identical(anyDuplicated(pages$game_id), 0L)
  # Typed empty result.
  empty <- q(player = "nobody")
  expect_identical(nrow(empty), 0L)
  expect_identical(vapply(empty, class, ""), vapply(all, class, ""))
  expect_identical(attr(empty, "total"), 0L)
  # Deterministic sampling: same seed, same games.
  s1 <- q(sample_n = 4L, sample_seed = 7L)
  s2 <- q(sample_n = 4L, sample_seed = 7L)
  expect_identical(s1$game_id, s2$game_id)
  expect_identical(nrow(s1), 4L)
  expect_error(q(sample_n = 4L), "sample_seed")
  # Soft deletion hides games until restored, after confirmation only.
  expect_message(prev <- g3_delete_games(db, c("example-live", "nope")), "Nothing was deleted")
  expect_identical(prev$status, c("would_delete", "not_found"))
  expect_identical(nrow(q(game_id = "example-live")), 1L)
  del <- g3_delete_games(db, "example-live", confirm = TRUE, note = "test")
  expect_identical(del$status, "deleted")
  expect_identical(nrow(q(game_id = "example-live")), 0L)
  expect_identical(q(deleted = "only")$game_id, "example-live")
  expect_error(g3_load_game(db, "example-live"), "deleted")
  expect_error(g3_save_game(db, ex$live), "deleted")
  expect_identical(g3_restore_games(db, c("example-live", "example-resign"))$status, c("restored", "not_deleted"))
  expect_identical(g3_load_game(db, "example-live")$catalogue$status, "live")
})

test_that("annotations are validated and returned verbatim", {
  db <- tmp_db()
  g3_save_game(db, ex$checkmate)
  xss <- "<img src=x onerror=alert(1)> & 'quotes'"
  id <- g3_annotate_game(db, "example-checkmate", xss, ply = 1L, author = "Eve <eve@example.org>")
  expect_type(id, "integer")
  a <- g3_game_annotations(db, "example-checkmate")
  expect_identical(a$body, xss)
  expect_identical(a$author, "Eve <eve@example.org>")
  expect_error(g3_annotate_game(db, "example-checkmate", "x", ply = 5L), "ply")
  expect_error(g3_annotate_game(db, "example-checkmate", "two\nlines", kind = "tag"), "one line")
  expect_error(g3_annotate_game(db, "nope", "x"), "No game")
  empty <- g3_game_annotations(db, "nope")
  expect_identical(nrow(empty), 0L)
  expect_type(empty$ply, "integer")
})

test_that("backups are consistent, verified and restore only to a new path", {
  dir <- withr::local_tempdir()
  db <- g3_open_game_db(file.path(dir, "games.sqlite"))
  withr::defer(g3_close_game_db(db))
  for (g in ex[c("resign", "checkmate", "live", "platform_rotation")]) g3_save_game(db, g)
  b <- g3_backup_game_db(db, file.path(dir, "b1.sqlite"))
  expect_identical(b$method, "VACUUM INTO")
  expect_identical(b$integrity, "ok")
  expect_identical(b$n_games, 4L)
  expect_identical(file_sha(b$path), b$sha256)
  expect_error(g3_backup_game_db(db, b$path), "overwrite")
  expect_length(list.files(dir, pattern = "^\\.g3backup"), 0L)
  r <- g3_restore_game_db(b$path, file.path(dir, "restored.sqlite"))
  expect_true(r$ok)
  expect_identical(r$n_games, 4L)
  expect_identical(r$restored_from$sha256, b$sha256)
  expect_error(g3_restore_game_db(b$path, r$path), "overwrite")
  db2 <- g3_open_game_db(r$path, read_only = TRUE)
  expect_identical(g3_query_games(db2, list(order_by = "game_id"))$game_content_hash,
                   g3_query_games(db, list(order_by = "game_id"))$game_content_hash)
  g3_close_game_db(db2)
  # A damaged backup is refused and nothing is created.
  bad <- file.path(dir, "bad.sqlite")
  file.copy(b$path, bad)
  bytes <- readBin(bad, "raw", file.info(bad)$size)
  bytes[seq(5000L, length(bytes) - 100L, by = 97L)] <- as.raw(0xff)
  writeBin(bytes, bad)
  expect_error(g3_restore_game_db(bad, file.path(dir, "never.sqlite")), class = "g3_error")
  expect_false(file.exists(file.path(dir, "never.sqlite")))
  expect_length(list.files(dir, pattern = "^\\.g3restore"), 0L)
  # The integrity check notices a stored game that no longer replays.
  DBI::dbExecute(db$con, "UPDATE actions SET action_id = 'm:W:0:0-W:3:3' WHERE game_id = ? AND event_seq = 1",
                 params = list("example-resign"))
  chk <- g3_check_game_db(db)
  expect_false(chk$ok)
  expect_identical(chk$games$game_id[!chk$games$ok], "example-resign")
  expect_error(g3_load_game(db, "example-resign"), class = "g3_invalid_state")
})

test_that("analysis runs and evaluations are recorded atomically", {
  db <- tmp_db()
  g3_save_game(db, ex$checkmate)
  st <- ex$checkmate$initial_state
  ev <- data.frame(
    game_id = "example-checkmate", event_seq = c(0L, 1L), ply = c(0L, 1L),
    state_hash = c(g3_state_hash(st), g3_state_hash(ex$checkmate$state)),
    analysis_context_hash = c(strrep("1", 64), strrep("2", 64)), perspective = "white",
    score = c(-3.5, NA), score_kind = c("heuristic", "none"), na_reason = c(NA, "terminal position"),
    stringsAsFactors = FALSE
  )
  run <- list(run_id = "run-1", kind = "game", evaluator = "search", settings = list(nodes = 50L), status = "completed")
  .g3_db_add_analysis(db, run, ev)
  got <- .g3_db_evaluations(db, game_id = "example-checkmate")
  expect_identical(got$score, c(-3.5, NA))
  expect_identical(got$na_reason[2], "terminal position")
  expect_identical(got$settings_hash[1], .g3_hash_object(list(nodes = 50L)))
  # A win/draw/loss triple that does not sum to one rolls the whole run back.
  bad <- ev
  bad$p_win <- c(0.5, NA)
  bad$p_draw <- c(0.5, NA)
  bad$p_loss <- c(0.5, NA)
  expect_error(.g3_db_add_analysis(db, utils::modifyList(run, list(run_id = "run-2")), bad), class = "g3_io_error")
  expect_identical(DBI::dbGetQuery(db$con, "SELECT count(*) AS n FROM analysis_runs WHERE run_id = 'run-2'")$n, 0L)
  missing <- ev
  missing$na_reason <- NA_character_
  expect_error(.g3_db_add_analysis(db, utils::modifyList(run, list(run_id = "run-3")), missing), "na_reason")
  expect_identical(nrow(.g3_db_evaluations(db, run_id = "nope")), 0L)
})

test_that("the example database replays completely", {
  dir <- withr::local_tempdir()
  path <- .g3_example_game_db(file.path(dir, "example.sqlite"), games = ex)
  db <- g3_open_game_db(path, read_only = TRUE)
  withr::defer(g3_close_game_db(db))
  chk <- g3_check_game_db(db)
  expect_true(chk$ok)
  expect_identical(chk$n_games, length(ex))
  q <- g3_query_games(db, list(tag = "generated example"), limit = 100L)
  expect_identical(nrow(q), length(ex))
  expect_true(all(c("live", "finished") %in% q$status))
})

test_that("simultaneous readers and serialized writers in separate processes", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "shared.sqlite")
  db <- g3_open_game_db(path)
  g3_save_game(db, ex$checkmate)
  pkg <- getNamespaceInfo(asNamespace("gambitR3D"), "path")
  dev <- pkgload::is_dev_package("gambitR3D")
  # Games for two writers: distinct ids, so every save is an insert.
  mk <- function(prefix, n) lapply(seq_len(n), function(i) {
    g <- if (i %% 2L) ex$live else ex$takeback
    g$metadata$game_id <- paste0(prefix, "-", i)
    g
  })
  child_games <- file.path(dir, "child.rds")
  saveRDS(mk("child", 6L), child_games)
  reader <- function(pkg, dev, path, seconds, foreign) {
    if (dev) pkgload::load_all(pkg, quiet = TRUE, helpers = FALSE) else library(gambitR3D)
    foreign_error <- tryCatch({ g3_query_games(foreign); "no error" }, error = function(e) conditionMessage(e))
    db <- g3_open_game_db(path, read_only = TRUE)
    on.exit(g3_close_game_db(db))
    n <- 0L
    errors <- character()
    sizes <- integer()
    end <- Sys.time() + seconds
    while (Sys.time() < end) {
      r <- tryCatch(g3_query_games(db, limit = 1000L), error = function(e) conditionMessage(e))
      if (is.character(r)) errors <- c(errors, r) else {
        n <- n + 1L
        sizes <- c(sizes, nrow(r))
      }
    }
    list(n = n, errors = errors, sizes = sizes, foreign_error = foreign_error)
  }
  writer <- function(pkg, dev, path, games_file) {
    if (dev) pkgload::load_all(pkg, quiet = TRUE, helpers = FALSE) else library(gambitR3D)
    db <- g3_open_game_db(path)
    on.exit(g3_close_game_db(db))
    vapply(readRDS(games_file), function(g) g3_save_game(db, g, source = "child")$status, "")
  }
  readers <- lapply(1:2, function(i) {
    callr::r_bg(reader, list(pkg, dev, path, 6, db), supervise = TRUE)
  })
  w <- callr::r_bg(writer, list(pkg, dev, path, child_games), supervise = TRUE)
  # The parent writes at the same time; SQLite serialises the writers.
  Sys.sleep(1.5)
  mine <- vapply(mk("parent", 6L), function(g) g3_save_game(db, g)$status, "")
  # A consistent backup while the other writer may still be writing.
  b <- g3_backup_game_db(db, file.path(dir, "live-backup.sqlite"))
  w$wait(60000)
  for (r in readers) r$wait(60000)
  expect_identical(mine, rep("inserted", 6L))
  expect_identical(w$get_result(), rep("inserted", 6L))
  for (r in readers) {
    res <- r$get_result()
    expect_identical(res$errors, character())
    expect_gt(res$n, 0L)
    expect_true(all(diff(res$sizes) >= 0L)) # readers only ever see committed growth
    expect_match(res$foreign_error, "another process")
  }
  expect_identical(nrow(g3_query_games(db, limit = 1000L)), 13L)
  expect_true(g3_check_game_db(db)$ok)
  restored <- g3_restore_game_db(b$path, file.path(dir, "from-live-backup.sqlite"))
  expect_true(restored$ok)
  expect_gte(restored$n_games, 7L)
  g3_close_game_db(db)
  expect_identical(sort(list.files(dir, pattern = "sqlite")),
                   sort(c("from-live-backup.sqlite", "live-backup.sqlite", "shared.sqlite")))
})
