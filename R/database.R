# Local game database: connection, pragmas, schema migrations, writes.
#
# SQLite (through DBI/RSQLite) is the authoritative local catalogue of
# games, their event logs, validated snapshots, provenance, annotations and
# analysis results. Files given to the importer are only read, never
# modified.
#
# Connections. `g3_open_game_db()` returns an owned `g3_db` handle (an
# environment, so closing it is visible to every copy). A handle belongs to
# the process that opened it; background jobs and Shiny sessions open their
# own connections and close them when they end. A finaliser closes a
# forgotten handle at garbage collection or session exit.
#
# Pragmas, set explicitly on every connection:
#   foreign_keys = ON        RSQLite leaves them off by default.
#   busy_timeout = 5000 ms   bounded wait for the single writer lock.
#   trusted_schema = OFF     schema expressions may use only innocuous
#                            functions.
#   synchronous = FULL       RSQLite's default is OFF. FULL syncs the log on
#                            every commit, so an acknowledged save survives
#                            an application crash and, as far as the
#                            operating system honours fsync, a power loss.
#   journal_mode = WAL       on a local file system with SQLite >= 3.51.3,
#                            which contains the fix for the WAL-reset
#                            corruption bug. Readers never block the
#                            writer, which suits a Shiny session saving
#                            moves while background jobs read. WAL needs
#                            shared memory on one host: on a detected
#                            network file system the catalogue uses a
#                            rollback journal (DELETE) and warns; an
#                            existing WAL database there is refused.
#                            When the last writer closes, the file returns to
#                            a rollback journal, so a catalogue at rest is one
#                            self-contained file.
#   query_only = ON          read-only handles, in addition to opening the
#                            file with SQLITE_OPEN_READONLY.
# Loadable extensions and RSQLite's bundled extension functions are
# disabled. Read-only mode never creates, migrates or changes the journal
# mode of a file. (If a file is still in WAL mode, for example after a
# crash or while a writer is open, SQLite may create its `-shm` and `-wal`
# companion files for a reader; the database file itself is not touched.)
#
# Writes go through `.g3_db_write()`: one short `BEGIN IMMEDIATE`
# transaction that takes the writer lock up front (so the busy timeout
# applies), commits, or rolls back on any error or interrupt. Driver errors
# are re-raised as `g3_io_error`.
#
# Migrations are an ordered list of SQL statement sets. Each applied
# migration is recorded in `schema_migrations` with its SHA-256; the schema
# version is also kept in `PRAGMA user_version` and the file is marked with
# `PRAGMA application_id`. Migrations run in one transaction and roll back
# completely on failure; a database file created by a failed first
# migration is removed again.

.G3_DB_APPLICATION_ID <- 1731413058L # 0x67334442, "g3DB"
.G3_DB_BUSY_TIMEOUT_MS <- 5000L
.G3_DB_MIN_WAL_SQLITE <- "3.51.3"
.G3_DB_PARSER_VERSION <- "g3-import/1"
.G3_ID_PATTERN <- "^[A-Za-z0-9][A-Za-z0-9_.:-]{0,127}$"
.G3_NETWORK_FS <- c(
  "nfs", "nfs4", "cifs", "smb3", "smb2", "smbfs", "ncpfs", "afs", "9p", "ceph",
  "glusterfs", "lustre", "gpfs", "beegfs", "davfs", "unc",
  "fuse.sshfs", "fuse.rclone", "fuse.s3fs", "fuse.glusterfs", "fuse.gcsfuse",
  "fuse.davfs2", "fuse.cephfs", "fuse.juicefs", "fuse.gvfsd-fuse"
)

# Size and count limits for imports and queries. A function so tests can
# shrink them.
#' @noRd
.g3_db_limits <- function() {
  list(
    file_bytes = .G3_MAX_FILE_BYTES,     # one .g3.json game file
    collection_bytes = 512 * 1024^2,     # one .jsonl collection, after decompression
    compressed_bytes = 128 * 1024^2,     # a .gz file as stored
    line_bytes = 8 * 1024^2,             # one JSONL record
    records = 100000L,                   # records per collection
    record_text_bytes = 1024^2,          # quarantined record text kept in the database
    diagnostic_chars = 500L,
    annotation_chars = 65536L,
    annotations = 1000L,
    page = 10000L,                       # maximum query page size
    text_chars = 10000L                  # one metadata text field (player name, label)
  )
}

#' @noRd
.g3_db_require <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    .g3_abort("g3_io_error", "The game database needs the 'DBI' and 'RSQLite' packages: install.packages(c('DBI', 'RSQLite')).")
  }
  invisible(TRUE)
}

#' Open a game database
#'
#' Opens (and, in writable mode, creates or migrates) a local SQLite game
#' catalogue. Keep databases in a folder you chose, for example under
#' `tools::R_user_dir("gambitR3D", "data")`; a database inside an R library
#' is refused. Each process (a Shiny session, a background job) opens its
#' own handle and closes it with [g3_close_game_db()]; handles cannot be
#' shared between processes.
#'
#' Writable handles use SQLite's write-ahead log on local disks (readers do
#' not block the writer), `synchronous = FULL` and a five-second busy
#' timeout for the single writer lock; when the last writer closes, the
#' file returns to a rollback journal, so a closed catalogue is one
#' self-contained file. On a detected network file system
#' (NFS, SMB, sshfs, rclone and similar) the write-ahead log is not used and
#' a warning is given: such a catalogue must not be used from several
#' computers at once.
#'
#' @param path Path to the database file (conventionally `.sqlite`).
#' @param read_only If `TRUE`, open an existing catalogue without any write:
#'   the file must exist and already have the current schema; it is never
#'   created, migrated or switched to another journal mode.
#' @return An owned `g3_db` handle (an environment) with fields `path`,
#'   `read_only`, `schema_version`, `journal_mode`, `synchronous`,
#'   `filesystem`, `sqlite_version`, `created` and `opened_at`. The
#'   connection inside is internal; use the `g3_*` database functions.
#' @examples
#' path <- base::tempfile(fileext = ".sqlite")
#' db <- gambitR3D::g3_open_game_db(path)
#' db$schema_version
#' gambitR3D::g3_close_game_db(db)
#' ro <- gambitR3D::g3_open_game_db(path, read_only = TRUE)
#' ro$read_only
#' gambitR3D::g3_close_game_db(ro)
#' @export
g3_open_game_db <- function(path, read_only = FALSE) {
  .g3_db_open(path, read_only = read_only)
}

# Internal opener; `migrations` can be replaced in tests.
#' @noRd
.g3_db_open <- function(path, read_only = FALSE, migrations = .g3_db_migrations()) {
  .g3_db_require()
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .g3_abort("g3_io_error", "`path` must be one database file path.")
  }
  if (path == ":memory:" || grepl("^file:", path)) {
    .g3_abort("g3_io_error", "`path` must be a file path, not an SQLite URI or in-memory name.")
  }
  if (!is.logical(read_only) || length(read_only) != 1L || is.na(read_only)) {
    .g3_abort("g3_io_error", "`read_only` must be TRUE or FALSE.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (dir.exists(path)) .g3_abort("g3_io_error", paste0("`path` is a directory: ", path))
  exists <- file.exists(path)
  if (read_only && !exists) {
    .g3_abort("g3_io_error", paste0("No database at ", path, "; read-only mode never creates one."))
  }
  if (!exists && !dir.exists(dirname(path))) {
    .g3_abort("g3_io_error", paste0("Directory does not exist: ", dirname(path)))
  }
  if (!read_only && .g3_path_in_library(path)) {
    .g3_abort("g3_io_error", "A writable game database cannot live inside an R package library; choose a data folder, e.g. tools::R_user_dir('gambitR3D', 'data').")
  }
  size <- if (exists) file.info(path)$size else 0
  hdr <- .g3_sqlite_header(path, size)
  if (read_only && size == 0) {
    .g3_abort("g3_io_error", paste0("The file is empty, not a game database: ", path))
  }
  fs <- .g3_fs_type(dirname(path))
  network <- !is.na(fs) && fs %in% .G3_NETWORK_FS
  if (network && identical(hdr$journal, "wal")) {
    .g3_abort("g3_io_error", paste0(
      "This WAL-mode database is on a network file system (", fs, "); SQLite's write-ahead log needs ",
      "shared memory on one computer. Copy it to a local disk, or open it once locally to convert it."
    ))
  }
  flags <- if (read_only) RSQLite::SQLITE_RO else RSQLite::SQLITE_RWC
  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path, flags = flags, synchronous = NULL,
                   loadable.extensions = FALSE, default.extensions = FALSE, bigint = "integer"),
    error = function(e) .g3_abort("g3_io_error", paste0("Could not open the database: ", conditionMessage(e)))
  )
  ok <- FALSE
  on.exit(if (!ok) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    if (!exists && !read_only) unlink(paste0(path, c("", "-wal", "-shm", "-journal")))
  }, add = TRUE)
  q1 <- function(sql) DBI::dbGetQuery(con, sql)[[1L]]
  # Driver errors on a damaged or locked file become typed conditions.
  tryCatch({
    sqlite_version <- q1("SELECT sqlite_version()")
    DBI::dbExecute(con, sprintf("PRAGMA busy_timeout = %d", .G3_DB_BUSY_TIMEOUT_MS))
    DBI::dbExecute(con, "PRAGMA trusted_schema = OFF")
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    if (read_only) {
      DBI::dbExecute(con, "PRAGMA query_only = ON")
      info <- .g3_db_schema_info(con)
      .g3_db_check_readable(info, migrations)
      journal <- q1("PRAGMA journal_mode")
      version <- info$user_version
    } else {
      # Refuse foreign, newer or tampered files before anything (including the
      # journal mode) is changed; the migration repeats these checks inside
      # its transaction.
      .g3_db_preflight(.g3_db_schema_info(con), migrations)
      want <- if (network) "delete" else if (utils::compareVersion(sqlite_version, .G3_DB_MIN_WAL_SQLITE) < 0) "delete" else "wal"
      journal <- tolower(q1(paste0("PRAGMA journal_mode = ", toupper(want))))
      if (!identical(journal, want)) {
        .g3_abort("g3_io_error", paste0("Could not set journal mode ", want, " (got ", journal, "); is another program using the database?"))
      }
      if (network) {
        .g3_warn(paste0("The game database is on a network file system (", fs, "): the write-ahead log is disabled. ",
                        "Do not use this database from several computers at the same time."), filesystem = fs)
      }
      DBI::dbExecute(con, "PRAGMA synchronous = FULL")
      DBI::dbExecute(con, "PRAGMA journal_size_limit = 67108864")
      mig <- .g3_db_migrate(con, migrations)
      version <- mig$to
    }
  }, error = function(e) {
    if (inherits(e, "g3_error")) stop(e)
    .g3_abort("g3_io_error", paste0("Could not open the database: ", conditionMessage(e)))
  })
  db <- new.env(parent = emptyenv())
  db$con <- con
  db$path <- path
  db$read_only <- read_only
  db$schema_version <- as.integer(version)
  db$journal_mode <- journal
  db$synchronous <- c("off", "normal", "full", "extra")[q1("PRAGMA synchronous") + 1L]
  db$filesystem <- fs
  db$sqlite_version <- sqlite_version
  db$created <- !exists
  db$opened_at <- .g3_now()
  db$owner_pid <- Sys.getpid()
  db$in_tx <- FALSE
  class(db) <- "g3_db"
  reg.finalizer(db, .g3_db_finalise, onexit = TRUE)
  ok <- TRUE
  db
}

#' Close a game database
#'
#' Closes the handle's connection. Closing twice, or closing a handle that
#' belongs to another process, is harmless.
#'
#' @param db A `g3_db` from [g3_open_game_db()].
#' @return Invisibly `TRUE`.
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' gambitR3D::g3_close_game_db(db)
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_close_game_db <- function(db) {
  if (!inherits(db, "g3_db")) .g3_abort("g3_io_error", "Expected a `g3_db` handle from g3_open_game_db().")
  .g3_db_finalise(db)
  invisible(TRUE)
}

#' @noRd
.g3_db_finalise <- function(db) {
  con <- db$con
  db$con <- NULL
  if (is.null(con)) return(invisible(FALSE))
  # A connection inherited by another process (fork) must not be closed
  # there; the owner closes it.
  if (!identical(db$owner_pid, Sys.getpid())) return(invisible(FALSE))
  if (isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
    if (!isTRUE(db$read_only) && identical(db$journal_mode, "wal") && !isTRUE(db$in_tx)) {
      # The last connection returns the file to a rollback journal, so a
      # closed catalogue is one self-contained file (read-only opens then
      # create no -wal/-shm companions). With other connections still open
      # this fails at once (no busy wait) and the file stays in WAL mode.
      try({
        DBI::dbExecute(con, "PRAGMA busy_timeout = 0")
        DBI::dbGetQuery(con, "PRAGMA journal_mode = DELETE")
      }, silent = TRUE)
    }
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
  invisible(TRUE)
}

#' @export
print.g3_db <- function(x, ...) {
  open <- !is.null(x$con) && isTRUE(tryCatch(DBI::dbIsValid(x$con), error = function(e) FALSE))
  cat("<g3_db> ", x$path, "\n", sep = "")
  cat("  ", if (open) "open" else "closed", ", ", if (isTRUE(x$read_only)) "read-only" else "writable",
      ", schema v", x$schema_version, ", journal ", x$journal_mode, ", synchronous ", x$synchronous,
      ", SQLite ", x$sqlite_version, "\n", sep = "")
  invisible(x)
}

# The live connection of a handle, with ownership and mode checks.
#' @noRd
.g3_db_con <- function(db, write = FALSE) {
  if (!inherits(db, "g3_db")) .g3_abort("g3_io_error", "Expected a `g3_db` handle from g3_open_game_db().")
  if (!identical(db$owner_pid, Sys.getpid())) {
    .g3_abort("g3_io_error", "This database handle belongs to another process; open a new handle in each process or job.")
  }
  con <- db$con
  if (is.null(con) || !isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
    .g3_abort("g3_io_error", "The game database handle is closed.")
  }
  if (write && isTRUE(db$read_only)) {
    .g3_abort("g3_io_error", "The game database is open read-only.")
  }
  con
}

# Read query with typed errors.
#' @noRd
.g3_db_get <- function(db, sql, params = NULL) {
  con <- .g3_db_con(db)
  tryCatch(
    if (length(params)) DBI::dbGetQuery(con, sql, params = unname(params)) else DBI::dbGetQuery(con, sql),
    error = function(e) .g3_abort("g3_io_error", paste0("Database read failed: ", conditionMessage(e)))
  )
}

# Run `fn(con)` in one short write transaction. Nested calls join the
# outer transaction. Any error or interrupt rolls back.
#' @noRd
.g3_db_write <- function(db, fn, what = "write") {
  con <- .g3_db_con(db, write = TRUE)
  if (isTRUE(db$in_tx)) return(fn(con))
  tryCatch(
    DBI::dbExecute(con, "BEGIN IMMEDIATE"),
    error = function(e) {
      .g3_abort("g3_io_error", paste0(
        "The game database is busy or not writable (", conditionMessage(e), "); nothing was saved. Try again."
      ))
    }
  )
  db$in_tx <- TRUE
  done <- FALSE
  on.exit({
    db$in_tx <- FALSE
    if (!done) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
  }, add = TRUE)
  out <- tryCatch(fn(con), error = function(e) {
    if (inherits(e, "g3_error")) stop(e)
    .g3_abort("g3_io_error", paste0("Database ", what, " failed and was rolled back: ", conditionMessage(e)))
  })
  tryCatch(
    DBI::dbExecute(con, "COMMIT"),
    error = function(e) .g3_abort("g3_io_error", paste0("Database commit failed and was rolled back: ", conditionMessage(e)))
  )
  done <- TRUE
  out
}

# Execute one statement (vectorised parameters run once per row).
#' @noRd
.g3_db_exec <- function(con, sql, params = NULL) {
  if (length(params)) DBI::dbExecute(con, sql, params = unname(params)) else DBI::dbExecute(con, sql)
}

# --------------------------------------------------------------- migrations

#' @noRd
.g3_db_migrations <- function() {
  migs <- list(list(version = 1L, name = "initial catalogue", sql = .g3_db_schema_v1()))
  lapply(migs, function(m) {
    m$checksum <- .g3_sha256(paste(m$sql, collapse = "\n;\n"))
    m
  })
}

#' @noRd
.g3_db_schema_info <- function(con) {
  q1 <- function(sql) DBI::dbGetQuery(con, sql)[[1L]]
  tables <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_schema WHERE type = 'table' AND name NOT LIKE 'sqlite_%'")$name
  applied <- if ("schema_migrations" %in% tables) {
    DBI::dbGetQuery(con, "SELECT version, name, checksum FROM schema_migrations ORDER BY version")
  } else {
    data.frame(version = integer(), name = character(), checksum = character())
  }
  list(user_version = as.integer(q1("PRAGMA user_version")),
       application_id = as.integer(q1("PRAGMA application_id")),
       tables = tables, applied = applied)
}

# Recorded migrations must match the code's migrations exactly.
#' @noRd
.g3_db_check_applied <- function(info, migrations) {
  ap <- info$applied
  codes <- vapply(migrations, function(m) m$version, 1L)
  if (!identical(as.integer(ap$version), seq_len(info$user_version))) {
    .g3_abort("g3_io_error", "The database's migration history is incomplete or inconsistent with its schema version.")
  }
  for (i in seq_len(nrow(ap))) {
    m <- migrations[[match(ap$version[i], codes)]]
    if (is.null(m) || !identical(m$checksum, ap$checksum[i])) {
      .g3_abort("g3_io_error", paste0(
        "Schema migration ", ap$version[i], " in the database has checksum ", substr(ap$checksum[i], 1L, 12L),
        "..., which does not match this version of gambitR3D. The database was changed outside the package ",
        "or by a different build; restore a backup or open it with the matching package version."
      ), version = ap$version[i])
    }
  }
  invisible(TRUE)
}

#' @noRd
.g3_db_check_readable <- function(info, migrations) {
  latest <- max(vapply(migrations, function(m) m$version, 1L))
  if (info$user_version == 0L || info$application_id != .G3_DB_APPLICATION_ID) {
    .g3_abort("g3_io_error", "This file is not a gambitR3D game database.")
  }
  if (info$user_version > latest) {
    .g3_abort("g3_io_error", paste0("The database has schema version ", info$user_version, "; this gambitR3D reads up to ",
                                    latest, ". Update the package."))
  }
  if (info$user_version < latest) {
    .g3_abort("g3_io_error", paste0("The database has the older schema version ", info$user_version,
                                    "; open it once writable (after a backup) to migrate it to version ", latest, "."))
  }
  .g3_db_check_applied(info, migrations)
}

# Checks before a writable open changes anything.
#' @noRd
.g3_db_preflight <- function(info, migrations) {
  latest <- max(vapply(migrations, function(m) m$version, 1L))
  if (info$user_version == 0L && length(info$tables)) {
    .g3_abort("g3_io_error", "This SQLite file already holds other tables and is not a gambitR3D game database; it was not changed.")
  }
  if (info$user_version > 0L && info$application_id != .G3_DB_APPLICATION_ID) {
    .g3_abort("g3_io_error", "This SQLite file is not a gambitR3D game database; it was not changed.")
  }
  if (info$user_version > latest) {
    .g3_abort("g3_io_error", paste0("The database has schema version ", info$user_version, "; this gambitR3D supports up to ",
                                    latest, ". Update the package."))
  }
  if (info$user_version > 0L) .g3_db_check_applied(info, migrations)
  invisible(TRUE)
}

# Bring the schema up to date in one transaction.
#' @noRd
.g3_db_migrate <- function(con, migrations) {
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE), add = TRUE)
  info <- .g3_db_schema_info(con)
  .g3_db_preflight(info, migrations)
  from <- info$user_version
  pending <- Filter(function(m) m$version > from, migrations)
  pending <- pending[order(vapply(pending, function(m) m$version, 1L))]
  for (m in pending) {
    tryCatch(
      for (stmt in m$sql) DBI::dbExecute(con, stmt),
      error = function(e) {
        .g3_abort("g3_io_error", paste0("Schema migration ", m$version, " (", m$name, ") failed and was rolled back; ",
                                        "the database stays at version ", from, ": ", conditionMessage(e)),
                  version = m$version)
      }
    )
    DBI::dbExecute(
      con,
      "INSERT INTO schema_migrations (version, name, checksum, applied_at, package_version, sqlite_version) VALUES (?, ?, ?, ?, ?, ?)",
      params = list(m$version, m$name, m$checksum, .g3_now(), .g3_pkg_version(),
                    DBI::dbGetQuery(con, "SELECT sqlite_version()")[[1L]])
    )
    DBI::dbExecute(con, sprintf("PRAGMA user_version = %d", as.integer(m$version)))
  }
  if (from == 0L) DBI::dbExecute(con, sprintf("PRAGMA application_id = %d", .G3_DB_APPLICATION_ID))
  DBI::dbExecute(con, "COMMIT")
  ok <- TRUE
  list(from = from, to = if (length(pending)) pending[[length(pending)]]$version else from,
       applied = vapply(pending, function(m) m$version, 1L))
}

#' @noRd
.g3_pkg_version <- function() {
  tryCatch(as.character(utils::packageVersion("gambitR3D")), error = function(e) "unknown")
}

# ------------------------------------------------------------ file checks

# First 100 bytes of an SQLite file: magic string and journal mode.
#' @noRd
.g3_sqlite_header <- function(path, size) {
  if (!file.exists(path) || is.na(size) || size == 0) return(list(sqlite = NA, journal = NA_character_))
  b <- readBin(path, "raw", 100L)
  magic <- charToRaw("SQLite format 3")
  if (length(b) < 100L || !identical(b[1:15], magic) || b[16] != as.raw(0L)) {
    .g3_abort("g3_io_error", paste0("Not an SQLite database file: ", path))
  }
  list(sqlite = TRUE, journal = if (as.integer(b[19]) == 2L) "wal" else "rollback")
}

#' @noRd
.g3_path_in_library <- function(path) {
  # Compare normalised forward-slash paths; the folder is normalised rather
  # than the (possibly new) file. Windows paths are case-insensitive.
  norm <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    if (.Platform$OS.type == "windows") tolower(p) else p
  }
  dir <- norm(dirname(path))
  libs <- sub("/+$", "", norm(.libPaths()))
  any(dir == libs | startsWith(dir, paste0(libs, "/")))
}

# File system type holding `dir` (Linux: /proc/self/mountinfo; Windows: UNC
# paths). NA when unknown.
#' @noRd
.g3_fs_type <- function(dir) {
  dir <- normalizePath(dir, mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    return(if (grepl("^(\\\\\\\\|//)", dir)) "unc" else NA_character_)
  }
  mi <- "/proc/self/mountinfo"
  if (!file.exists(mi)) return(NA_character_)
  lines <- tryCatch(readLines(mi, warn = FALSE), error = function(e) character())
  if (!length(lines)) return(NA_character_)
  halves <- strsplit(lines, " - ", fixed = TRUE)
  ok <- lengths(halves) >= 2L
  halves <- halves[ok]
  mp <- vapply(halves, function(h) strsplit(h[1L], " ", fixed = TRUE)[[1L]][5L], "")
  fst <- vapply(halves, function(h) strsplit(h[2L], " ", fixed = TRUE)[[1L]][1L], "")
  mp <- gsub("\\\\040", " ", gsub("\\\\011", "\t", gsub("\\\\012", "\n", gsub("\\\\134", "\\\\", mp))))
  inside <- mp == "/" | dir == mp | startsWith(dir, paste0(mp, "/"))
  if (!any(inside)) return(NA_character_)
  cand <- which(inside)
  fst[cand[which.max(nchar(mp[cand]))]]
}

# Default writable location for the app's catalogue.
#' @noRd
.g3_default_db_path <- function(create_dir = FALSE) {
  dir <- tools::R_user_dir("gambitR3D", which = "data")
  if (create_dir && !dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "games.sqlite")
}
