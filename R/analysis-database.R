# Batch analysis of game collections.
#
# `g3_analyse_database()` analyses every game selected from a game database
# (through the database API `g3_query_games()` / `g3_load_game()`), a list
# of `g3_game` objects or a vector of native game files. It works in
# chunks, polls an optional stop check between games (and inside each
# search), reports progress, writes only derived outputs, and can resume a
# run from its output directory:
#
#   <output_dir>/manifest.json      run identity, configuration hash, status
#   <output_dir>/completed.csv      one row per completed game
#   <output_dir>/errors.csv         one row per failed game (typed class)
#   <output_dir>/games/<id>.rds     the g3_analysis of each completed game
#
# A resumed run must have the same configuration hash (source, filters,
# evaluator, budget, rules hash, feature schema); completed games whose
# result file still matches its recorded checksum are not recomputed. Raw
# game records are never modified. Collection summaries keep denominators,
# missingness and filters, and keep truncated or unfinished games apart
# from rule-adjudicated draws.

.G3_DBRUN_SCHEMA <- "g3-analysis-run/1"

#' Analyse a game collection
#'
#' Analyses the selected games one at a time with [g3_analyse_game()],
#' sharing one evaluation cache, and returns a run manifest, completed and
#' error records, the per-game analyses and collection summaries.
#'
#' @param db The collection: a `g3_db` handle from `g3_open_game_db()`, a
#'   path to a game database (opened read-only), a list of `g3_game`
#'   objects (or one game), or a character vector of native `.g3.json` game
#'   files.
#' @param filters Query filters passed unchanged to `g3_query_games()` for
#'   databases. For games and files only `list(game_id = <ids>)` is
#'   supported.
#' @param evaluator,budget As for [g3_analyse_game()]; the node budget is
#'   per position.
#' @param output_dir Optional directory for durable, resumable outputs
#'   (created if needed). `NULL` keeps everything in memory.
#' @param chunk_size Games per chunk; outputs are flushed after each chunk.
#' @param stop_check Optional function returning `TRUE` to cancel; polled
#'   between games and inside searches. The interrupted game is not
#'   recorded as completed.
#' @param progress Optional function `function(done, total, unit)` called
#'   after each game (compatible with background-job progress).
#' @param resume If `TRUE` (default) and `output_dir` holds a run with the
#'   same configuration, completed games are reused.
#' @param max_seconds Optional total wall-time limit for the batch; checked
#'   between games.
#' @return A `g3_database_analysis` list:
#'   * `manifest`: list with `schema`, `run_id`, `run_config_hash`, `status`
#'     (`"completed"`, `"completed_with_errors"`, `"cancelled"`,
#'     `"time_limit"`), `created`, `updated`, `source`, `filters`,
#'     `evaluator`, `evaluator_version`, `budget`, `rules_hash`,
#'     `feature_schema`, `package_version`, `chunk_size`, `n_scheduled`,
#'     `n_completed`, `n_errors`, `n_pending`, `output_dir`.
#'   * `completed`: data frame, one row per completed game (`game_id`,
#'     `game_content_hash`, `n_plies`, `result`, `termination`,
#'     `outcome_class`, `n_platform_actions`, `n_evaluated`,
#'     `n_eval_missing`, `elapsed_ms`, `cache_hits`, `chunk`,
#'     `result_file`, `result_sha256`).
#'   * `errors`: data frame (`game_id`, `error_class`, `message`,
#'     `first_invalid_ply`, `chunk`).
#'   * `pending`: game ids not processed (cancellation or time limit).
#'   * `results`: named list of `g3_analysis` for the completed games.
#'   * `summary`: collection summary tables (see Details).
#'
#' @details The summary is a list of data frames: `counts`, `outcomes`,
#'   `terminations`, `lengths`, `platform_use`, `platform_use_by_phase`,
#'   `transfers`, `repeated_positions`, `repetition`, `opening_groups`,
#'   `motifs`, `evaluation_missingness` and `filters`. Rates carry their
#'   numerator and denominator. Truncated and unfinished games are their
#'   own categories, never draws. Opening groups are named descriptively
#'   from this variant's own first actions; no orthodox opening names are
#'   used.
#' @examples
#' # Two short games analysed as a collection, features only.
#' g1 <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' g2 <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:3:1-W:3:2")
#' run <- gambitR3D::g3_analyse_database(base::list(g1, g2), evaluator = "none")
#' run$manifest$status
#' run$summary$outcomes
#' @export
g3_analyse_database <- function(db, filters = list(), evaluator = "search", budget = list(nodes = 2000L),
                                output_dir = NULL, chunk_size = 10L, stop_check = NULL, progress = NULL,
                                resume = TRUE, max_seconds = Inf) {
  t0 <- proc.time()[["elapsed"]]
  ev <- .g3_an_evaluator(evaluator)
  b <- .g3_budget(budget)
  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L) .g3_abort("g3_invalid_state", "`chunk_size` must be a positive integer.")
  if (!is.list(filters)) .g3_abort("g3_invalid_state", "`filters` must be a list.")
  src <- .g3_dbrun_source(db, filters)
  on.exit(src$close(), add = TRUE)
  ids <- src$ids
  config <- list(
    schema = .G3_DBRUN_SCHEMA, source = src$identity, filters = .g3_dbrun_filters(filters),
    game_ids = as.list(ids), evaluator = ev$id, evaluator_version = ev$version,
    model_revision = ev$model_revision,
    budget = list(nodes = format(b$nodes, scientific = FALSE), time_ms = format(b$time_ms), depth = b$depth),
    rules_hash = .g3_tables()$rules_hash, feature_schema = .G3_FEATURE_SCHEMA
  )
  config_hash <- .g3_hash_object(config)
  manifest <- list(
    schema = .G3_DBRUN_SCHEMA, run_id = substr(config_hash, 1L, 16L), run_config_hash = config_hash,
    status = "running", created = .g3_now(), updated = .g3_now(), source = src$description,
    filters = .g3_dbrun_filters(filters), evaluator = ev$id, evaluator_version = ev$version,
    budget = config$budget, budget_semantics = "per position", rules_hash = config$rules_hash,
    feature_schema = .G3_FEATURE_SCHEMA, package_version = as.character(utils::packageVersion("gambitR3D")),
    chunk_size = chunk_size, n_scheduled = length(ids), n_completed = 0L, n_errors = 0L,
    n_pending = length(ids), output_dir = if (is.null(output_dir)) NA_character_ else normalizePath(output_dir, mustWork = FALSE)
  )
  completed_rows <- list()
  error_rows <- list()
  results <- list()
  done_ids <- character()
  if (!is.null(output_dir)) {
    dir.create(file.path(output_dir, "games"), recursive = TRUE, showWarnings = FALSE)
    prev <- .g3_dbrun_read(output_dir)
    if (!is.null(prev)) {
      if (!identical(prev$manifest$run_config_hash, config_hash)) {
        .g3_abort("g3_io_error", paste0(
          "`output_dir` holds a different analysis run (configuration hash ",
          substr(prev$manifest$run_config_hash, 1L, 12L), "); use a new directory."
        ))
      }
      if (isTRUE(resume)) {
        manifest$created <- prev$manifest$created
        for (i in seq_len(nrow(prev$completed))) {
          r <- prev$completed[i, ]
          path <- file.path(output_dir, r$result_file)
          if (file.exists(path) && identical(digest::digest(file = path, algo = "sha256"), r$result_sha256)) {
            completed_rows[[length(completed_rows) + 1L]] <- as.list(r)
            results[[r$game_id]] <- readRDS(path)
            done_ids <- c(done_ids, r$game_id)
          }
        }
      }
    }
  }
  cache <- new.env(hash = TRUE, parent = emptyenv())
  todo <- setdiff(ids, done_ids)
  status <- "completed"
  chunks <- split(todo, ceiling(seq_along(todo) / chunk_size))
  # Chunk numbers continue across resumed attempts.
  chunk0 <- if (length(completed_rows)) max(vapply(completed_rows, function(r) as.integer(r$chunk), 0L)) else 0L
  n_done <- length(done_ids)
  pending <- character()
  stop_now <- function() !is.null(stop_check) && isTRUE(stop_check())
  for (ci in seq_along(chunks)) {
    for (gid in chunks[[ci]]) {
      if (stop_now()) {
        status <- "cancelled"
        break
      }
      if ((proc.time()[["elapsed"]] - t0) > max_seconds) {
        status <- "time_limit"
        break
      }
      tg <- proc.time()[["elapsed"]]
      res <- tryCatch({
        game <- src$load(gid)
        g3_analyse_game(game, evaluator = ev$object %||% "none", budget = b, cache = cache, stop_check = stop_check)
      }, error = function(e) e)
      if (inherits(res, "g3_budget_exceeded") && identical(res$context$reason, "cancelled")) {
        status <- "cancelled"
        break
      }
      if (inherits(res, "error")) {
        cls <- intersect(class(res), .G3_CONDITION_CLASSES)
        error_rows[[length(error_rows) + 1L]] <- list(
          game_id = gid, error_class = if (length(cls)) cls[1L] else "error",
          message = conditionMessage(res),
          first_invalid_ply = as.integer(res$context$first_invalid_ply %||% NA_integer_), chunk = chunk0 + ci
        )
      } else {
        rf <- NA_character_
        rsha <- NA_character_
        if (!is.null(output_dir)) {
          rf <- file.path("games", paste0(.g3_dbrun_safe_name(gid), ".rds"))
          .g3_an_write_rds(res, file.path(output_dir, rf))
          rsha <- digest::digest(file = file.path(output_dir, rf), algo = "sha256")
        }
        s <- res$summary
        completed_rows[[length(completed_rows) + 1L]] <- list(
          game_id = gid, game_content_hash = s$game_content_hash, n_plies = s$n_plies,
          result = s$result, termination = s$termination, outcome_class = s$outcome_class,
          n_platform_actions = s$n_platform_actions, n_evaluated = s$n_evaluated,
          n_eval_missing = s$n_eval_missing, elapsed_ms = round((proc.time()[["elapsed"]] - tg) * 1000, 3),
          cache_hits = res$provenance$cache$hits, chunk = chunk0 + ci, result_file = rf, result_sha256 = rsha
        )
        results[[gid]] <- res
      }
      n_done <- n_done + 1L
      if (is.function(progress)) progress(n_done, length(ids), "games")
    }
    if (!is.null(output_dir)) {
      .g3_dbrun_write(output_dir, manifest, completed_rows, error_rows, ids, status_now = "running")
    }
    if (status != "completed") break
  }
  completed <- .g3_an_rows_df(completed_rows, .g3_dbrun_schema("completed"))
  errors <- .g3_an_rows_df(error_rows, .g3_dbrun_schema("errors"))
  pending <- setdiff(ids, c(completed$game_id, errors$game_id))
  if (status == "completed" && nrow(errors)) status <- "completed_with_errors"
  manifest$status <- status
  manifest$updated <- .g3_now()
  manifest$n_completed <- nrow(completed)
  manifest$n_errors <- nrow(errors)
  manifest$n_pending <- length(pending)
  manifest$elapsed_ms <- (proc.time()[["elapsed"]] - t0) * 1000
  results <- results[intersect(ids, names(results))]
  if (!is.null(output_dir)) {
    .g3_dbrun_write(output_dir, manifest, completed_rows, error_rows, ids, status_now = status)
  }
  structure(
    list(
      manifest = manifest, completed = completed, errors = errors, pending = pending, results = results,
      summary = .g3_dbrun_summary(results, filters, length(ids), nrow(errors), length(pending))
    ),
    class = "g3_database_analysis"
  )
}

#' @export
print.g3_database_analysis <- function(x, ...) {
  m <- x$manifest
  cat("<g3_database_analysis> ", m$status, ": ", m$n_completed, " of ", m$n_scheduled, " games completed, ",
      m$n_errors, " errors, ", m$n_pending, " pending\n", sep = "")
  cat("Evaluator ", m$evaluator, ", budget ", .g3_an_budget_text(.g3_budget(lapply(m$budget, function(v) as.numeric(v)))),
      " per position; filters ", m$filters, "\n", sep = "")
  if (nrow(x$summary$outcomes)) print(x$summary$outcomes, row.names = FALSE)
  invisible(x)
}

# ---- sources ----------------------------------------------------------------

# A game source: ids, a loader, a description and a closer.
#' @noRd
.g3_dbrun_source <- function(db, filters) {
  if (inherits(db, "g3_game")) db <- list(db)
  if (is.list(db) && !inherits(db, "g3_db")) {
    ok <- vapply(db, function(g) inherits(g, "g3_game"), TRUE)
    if (!all(ok)) .g3_abort("g3_invalid_state", "A list source must contain only `g3_game` objects.")
    ids <- vapply(db, function(g) as.character(g$metadata$game_id %||% NA_character_), "")
    if (anyNA(ids) || anyDuplicated(ids)) .g3_abort("g3_invalid_state", "Games in a list need unique, non-missing game ids.")
    keep <- .g3_dbrun_list_filter(ids, filters)
    games <- stats::setNames(db, ids)
    hashes <- vapply(games[keep], g3_game_hash, "")
    return(list(
      ids = ids[keep], load = function(id) games[[id]], close = function() invisible(NULL),
      description = paste0("in-memory list of ", length(ids), " games"),
      identity = list(kind = "games", content_hashes = as.list(unname(hashes)))
    ))
  }
  if (is.character(db) && length(db) && all(grepl("\\.json$", db, ignore.case = TRUE))) {
    if (!all(file.exists(db))) .g3_abort("g3_io_error", paste0("Game file not found: ", db[!file.exists(db)][1L]))
    games <- lapply(db, g3_read_game)
    src <- .g3_dbrun_source(games, filters)
    src$description <- paste0(length(db), " native game files")
    src$identity <- list(kind = "files", sha256 = as.list(unname(vapply(db, function(p) digest::digest(file = p, algo = "sha256"), ""))),
                         content_hashes = src$identity$content_hashes)
    return(src)
  }
  opened <- FALSE
  if (is.character(db) && length(db) == 1L) {
    open_fn <- .g3_dbrun_api("g3_open_game_db")
    if (!file.exists(db)) .g3_abort("g3_io_error", paste0("Database not found: ", db))
    db <- open_fn(db, read_only = TRUE)
    opened <- TRUE
  }
  if (!inherits(db, "g3_db")) {
    .g3_abort("g3_invalid_state", "`db` must be a g3_db handle, a database path, a list of g3_game objects or game files.")
  }
  query <- .g3_dbrun_api("g3_query_games")
  load <- .g3_dbrun_api("g3_load_game")
  ids <- character()
  page <- 1000L
  off <- 0L
  repeat {
    q <- query(db, filters = filters, limit = page, offset = off)
    if (!nrow(q)) break
    ids <- c(ids, as.character(q$game_id))
    if (nrow(q) < page) break
    off <- off + page
  }
  path <- db$path %||% NA_character_
  list(
    ids = ids, load = function(id) load(db, id),
    close = function() if (opened) .g3_dbrun_api("g3_close_game_db")(db),
    description = paste0("game database ", if (is.na(path)) "(handle)" else basename(path)),
    identity = list(kind = "database", path = if (is.na(path)) NA_character_ else normalizePath(path, mustWork = FALSE))
  )
}

# Database API functions are looked up at call time: they live in the
# database module of this package.
#' @noRd
.g3_dbrun_api <- function(name) {
  f <- get0(name, envir = asNamespace("gambitR3D"), mode = "function", inherits = FALSE)
  if (is.null(f)) .g3_abort("g3_io_error", paste0("The database API (", name, ") is not available in this build."))
  f
}

#' @noRd
.g3_dbrun_list_filter <- function(ids, filters) {
  if (!length(filters)) return(rep(TRUE, length(ids)))
  bad <- setdiff(names(filters), "game_id")
  if (length(bad) || is.null(names(filters))) {
    .g3_abort("g3_invalid_state", paste0("Only `game_id` filters apply to in-memory games and files; got: ",
                                         paste(names(filters) %||% "(unnamed)", collapse = ", "), "."))
  }
  ids %in% as.character(filters$game_id)
}

#' @noRd
.g3_dbrun_filters <- function(filters) {
  if (!length(filters)) return("none")
  .g3_canonical_json(filters)
}

#' @noRd
.g3_dbrun_safe_name <- function(id) {
  paste0(substr(gsub("[^A-Za-z0-9_.-]", "_", id), 1L, 60L), "-", substr(.g3_sha256(id), 1L, 8L))
}

#' @noRd
.g3_an_write_rds <- function(obj, path) {
  tmp <- tempfile(pattern = ".g3tmp-", tmpdir = dirname(path))
  saveRDS(obj, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    .g3_abort("g3_io_error", paste0("Could not write ", path))
  }
  invisible(path)
}

#' @noRd
.g3_dbrun_schema <- function(which) {
  if (which == "completed") {
    c(game_id = "character", game_content_hash = "character", n_plies = "integer", result = "character",
      termination = "character", outcome_class = "character", n_platform_actions = "integer",
      n_evaluated = "integer", n_eval_missing = "integer", elapsed_ms = "double", cache_hits = "integer",
      chunk = "integer", result_file = "character", result_sha256 = "character")
  } else {
    c(game_id = "character", error_class = "character", message = "character",
      first_invalid_ply = "integer", chunk = "integer")
  }
}

#' @noRd
.g3_dbrun_write <- function(dir, manifest, completed_rows, error_rows, ids, status_now) {
  manifest$status <- status_now
  manifest$updated <- .g3_now()
  completed <- .g3_an_rows_df(completed_rows, .g3_dbrun_schema("completed"))
  errors <- .g3_an_rows_df(error_rows, .g3_dbrun_schema("errors"))
  manifest$n_completed <- nrow(completed)
  manifest$n_errors <- nrow(errors)
  manifest$n_pending <- length(setdiff(ids, c(completed$game_id, errors$game_id)))
  .g3_an_write_csv(completed, file.path(dir, "completed.csv"))
  .g3_an_write_csv(errors, file.path(dir, "errors.csv"))
  .g3_atomic_write(paste0(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA), "\n"),
                   file.path(dir, "manifest.json"))
}

#' @noRd
.g3_an_write_csv <- function(df, path) {
  txt <- utils::capture.output(utils::write.csv(df, row.names = FALSE, na = ""))
  .g3_atomic_write(paste0(paste(txt, collapse = "\n"), "\n"), path)
}

#' @noRd
.g3_dbrun_read <- function(dir) {
  mp <- file.path(dir, "manifest.json")
  if (!file.exists(mp)) return(NULL)
  m <- jsonlite::fromJSON(mp, simplifyVector = TRUE)
  read <- function(f, schema) {
    p <- file.path(dir, f)
    if (!file.exists(p) || file.size(p) == 0L) return(.g3_an_rows_df(list(), schema))
    x <- utils::read.csv(p, stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
    if (!nrow(x)) return(.g3_an_rows_df(list(), schema))
    cols <- lapply(names(schema), function(nm) .g3_an_typed(x[[nm]], schema[[nm]]))
    .g3_an_new_df(cols, names(schema))
  }
  list(manifest = m, completed = read("completed.csv", .g3_dbrun_schema("completed")),
       errors = read("errors.csv", .g3_dbrun_schema("errors")))
}

# ---- collection summaries ---------------------------------------------------

#' @noRd
.g3_dbrun_summary <- function(results, filters, n_scheduled, n_errors, n_pending) {
  n <- length(results)
  sm <- if (n) do.call(rbind, lapply(results, `[[`, "summary")) else .g3_an_rows_df(list(), .g3_an_schema("summary"))
  rownames(sm) <- NULL
  moves <- if (n) do.call(rbind, lapply(results, `[[`, "moves")) else .g3_an_rows_df(list(), .g3_an_schema("moves"))
  pos <- if (n) do.call(rbind, lapply(results, function(r) r$positions[, c("game_id", "ply", "position_key", "repetition_count")])) else
    data.frame(game_id = character(), ply = integer(), position_key = character(), repetition_count = integer())
  tac <- if (n) do.call(rbind, lapply(results, `[[`, "tactics")) else .g3_an_rows_df(list(), .g3_an_schema("tactics"))
  evs <- if (n) do.call(rbind, lapply(results, function(r) r$evaluations[, c("game_id", "evaluator", "raw_score", "missing_reason")])) else
    data.frame(game_id = character(), evaluator = character(), raw_score = double(), missing_reason = character())

  share <- function(x, d) if (d > 0) x / d else NA_real_
  counts <- data.frame(n_scheduled = as.integer(n_scheduled), n_completed = n, n_errors = as.integer(n_errors),
                       n_pending = as.integer(n_pending), stringsAsFactors = FALSE)
  res_levels <- c("white", "black", "draw", "truncated", "unfinished")
  outcomes <- data.frame(result = res_levels,
                         n_games = vapply(res_levels, function(r) sum(sm$result == r), 0L, USE.NAMES = FALSE),
                         stringsAsFactors = FALSE)
  outcomes$denominator <- n
  outcomes$share <- share(outcomes$n_games, n)
  term_levels <- c("checkmate", "stalemate", "repetition", "halfmove", "bare_kings", "resign", "draw_agreed",
                   "truncated", "unfinished")
  terminations <- data.frame(termination = term_levels,
                             outcome_class = .g3_an_outcome_class(term_levels),
                             n_games = vapply(term_levels, function(r) sum(sm$termination == r), 0L, USE.NAMES = FALSE),
                             stringsAsFactors = FALSE)
  terminations$denominator <- n
  terminations$share <- share(terminations$n_games, n)
  cls <- unique(c("rule_decisive", "rule_draw", "resignation", "agreed_draw", "truncated", "unfinished"))
  lengths <- do.call(rbind, lapply(c(cls, "all"), function(cl) {
    x <- if (cl == "all") sm$n_plies else sm$n_plies[sm$outcome_class == cl]
    q <- if (length(x)) stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE, type = 7) else rep(NA_real_, 5L)
    data.frame(outcome_class = cl, n_games = length(x), min = q[1L], q25 = q[2L], median = q[3L], q75 = q[4L],
               max = q[5L], mean = if (length(x)) mean(x) else NA_real_, stringsAsFactors = FALSE)
  }))
  total_actions <- nrow(moves)
  plat <- sum(moves$platform_action)
  games_plat <- sum(sm$n_platform_actions > 0L)
  platform_use <- data.frame(
    n_games = n, total_actions = total_actions, platform_actions = plat,
    platform_rotations = sum(moves$action_type == "platform_rotation"),
    share_of_actions = share(plat, total_actions), games_with_platform_action = games_plat,
    share_of_games = share(games_plat, n), stringsAsFactors = FALSE
  )
  phase_breaks <- c(0, 20, 40, 80, Inf)
  phase_lab <- c("plies 1-20", "plies 21-40", "plies 41-80", "plies 81+")
  ph <- cut(moves$ply, phase_breaks, labels = phase_lab, right = TRUE)
  platform_use_by_phase <- data.frame(
    phase = phase_lab,
    actions = vapply(phase_lab, function(p) sum(ph == p, na.rm = TRUE), 0L, USE.NAMES = FALSE),
    platform_actions = vapply(phase_lab, function(p) sum(ph == p & moves$platform_action, na.rm = TRUE), 0L, USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  platform_use_by_phase$share <- ifelse(platform_use_by_phase$actions > 0,
                                        platform_use_by_phase$platform_actions / platform_use_by_phase$actions, NA_real_)
  # Level transitions by mode (movement by a piece versus transport).
  tr <- moves[moves$level_transfer | moves$board_transfer, , drop = FALSE]
  transfers <- if (nrow(tr)) {
    key <- paste(tr$from_level, tr$to_level, ifelse(is.na(tr$transfer_mode), "none", tr$transfer_mode), sep = "|")
    u <- sort(unique(key))
    parts <- strsplit(u, "|", fixed = TRUE)
    data.frame(from_level = vapply(parts, `[`, "", 1L), to_level = vapply(parts, `[`, "", 2L),
               transfer_mode = vapply(parts, `[`, "", 3L),
               n_actions = vapply(u, function(k) sum(key == k), 0L, USE.NAMES = FALSE),
               denominator = total_actions, stringsAsFactors = FALSE)
  } else {
    data.frame(from_level = character(), to_level = character(), transfer_mode = character(),
               n_actions = integer(), denominator = integer(), stringsAsFactors = FALSE)
  }
  # Positions recurring across games (start positions reported separately).
  later <- pos[!(pos$game_id %in% sm$game_id & pos$ply == stats::ave(pos$ply, pos$game_id, FUN = min)), , drop = FALSE]
  repeated_positions <- if (nrow(later)) {
    g_by_key <- tapply(later$game_id, later$position_key, function(x) length(unique(x)))
    occ_by_key <- tapply(later$game_id, later$position_key, length)
    keep <- names(g_by_key)[g_by_key >= 2L]
    if (length(keep)) {
      first <- later[match(keep, later$position_key), ]
      out <- data.frame(position_key = keep, n_games = as.integer(g_by_key[keep]),
                        n_occurrences = as.integer(occ_by_key[keep]), example_game_id = first$game_id,
                        example_ply = first$ply, denominator_games = n, stringsAsFactors = FALSE)
      out[order(-out$n_games, -out$n_occurrences, out$position_key), , drop = FALSE]
    } else NULL
  } else NULL
  if (is.null(repeated_positions)) {
    repeated_positions <- data.frame(position_key = character(), n_games = integer(), n_occurrences = integer(),
                                     example_game_id = character(), example_ply = integer(),
                                     denominator_games = integer(), stringsAsFactors = FALSE)
  }
  rownames(repeated_positions) <- NULL
  repetition <- data.frame(
    n_games = n,
    games_with_repeated_position = sum(sm$max_repetition_count >= 2L),
    games_ending_by_repetition = sum(sm$termination == "repetition"),
    distinct_start_positions = length(unique(sm$start_position_key)),
    games_from_standard_start = sum(sm$standard_start), stringsAsFactors = FALSE
  )
  opening_groups <- .g3_dbrun_openings(results, sm, n)
  lab <- .g3_feat_tactic_defs()
  motifs <- data.frame(
    label = lab$label, claim_strength = lab$claim_strength,
    n_labels = vapply(lab$label, function(l) sum(tac$label == l), 0L, USE.NAMES = FALSE),
    games_with_label = vapply(lab$label, function(l) length(unique(tac$game_id[tac$label == l])), 0L, USE.NAMES = FALSE),
    denominator_games = n, stringsAsFactors = FALSE
  )
  motifs$share_of_games <- share(motifs$games_with_label, n)
  miss <- evs$missing_reason
  reasons <- sort(unique(miss[!is.na(miss)]))
  evaluation_missingness <- data.frame(
    status = c("scored", reasons),
    n_positions = c(sum(!is.na(evs$raw_score)), vapply(reasons, function(r) sum(miss == r, na.rm = TRUE), 0L, USE.NAMES = FALSE)),
    denominator = nrow(evs), stringsAsFactors = FALSE
  )
  list(
    counts = counts, outcomes = outcomes, terminations = terminations, lengths = lengths,
    platform_use = platform_use, platform_use_by_phase = platform_use_by_phase, transfers = transfers,
    repeated_positions = repeated_positions, repetition = repetition, opening_groups = opening_groups,
    motifs = motifs, evaluation_missingness = evaluation_missingness,
    filters = data.frame(filters = .g3_dbrun_filters(filters), stringsAsFactors = FALSE),
    games = sm
  )
}

# Descriptive opening groups: start position plus the first two actions,
# named from this variant's own action labels.
#' @noRd
.g3_dbrun_openings <- function(results, sm, n) {
  empty <- data.frame(group = character(), name = character(), n_games = integer(), denominator = integer(),
                      white = integer(), black = integer(), draw = integer(), truncated = integer(),
                      unfinished = integer(), stringsAsFactors = FALSE)
  if (!n) return(empty)
  std <- .g3_initial_core()$rep
  info <- lapply(results, function(r) {
    lab <- utils::head(r$moves$label, 2L)
    ids <- utils::head(r$moves$action_id, 2L)
    start <- r$positions$position_key[1L]
    list(
      key = paste(c(start, ids), collapse = "|"),
      name = paste0(if (identical(start, std)) "Standard start" else paste0("Start ", substr(start, 1L, 8L)),
                    if (length(lab)) paste0(": ", paste(lab, collapse = " / ")) else ": no actions"),
      result = r$summary$result
    )
  })
  keys <- vapply(info, `[[`, "", "key")
  u <- unique(keys)
  out <- do.call(rbind, lapply(u, function(k) {
    sel <- info[keys == k]
    res <- vapply(sel, `[[`, "", "result")
    data.frame(group = substr(.g3_sha256(k), 1L, 10L), name = sel[[1L]]$name, n_games = length(sel), denominator = n,
               white = sum(res == "white"), black = sum(res == "black"), draw = sum(res == "draw"),
               truncated = sum(res == "truncated"), unfinished = sum(res == "unfinished"), stringsAsFactors = FALSE)
  }))
  out <- out[order(-out$n_games, out$name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Background-job type "analysis": a resumable batch analysis.
# `args`: db (path, game list or files), filters, evaluator, budget,
# chunk_size, output_dir (default: <job dir>/analysis).
#' @noRd
.g3_job_analysis <- function(args, ctx) {
  out_dir <- args$output_dir %||% file.path(ctx$dir, "analysis")
  run <- g3_analyse_database(
    args$db, filters = args$filters %||% list(), evaluator = args$evaluator %||% "search",
    budget = args$budget %||% list(nodes = 2000L), output_dir = out_dir,
    chunk_size = args$chunk_size %||% 10L, stop_check = ctx$should_stop,
    progress = function(done, total, unit) ctx$progress(done, total, unit), resume = TRUE
  )
  out <- list(manifest = run$manifest, completed = run$completed, errors = run$errors, pending = run$pending,
              summary = run$summary)
  if (run$manifest$status == "cancelled") {
    attr(out, "stop_reason") <- if (ctx$timed_out()) "timed_out" else "cancelled"
  }
  out
}
