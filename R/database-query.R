# Game database: catalogue queries.
#
# Filters are plain data: a named list checked against an allow-list. Every
# value is bound as an SQL parameter and sort columns come from a fixed
# map, so neither a filter value nor a stored text (player names, tags) is
# ever interpreted as SQL. Ordering always ends with `game_id`, which makes
# paging stable. Random sampling is explicit: a sample size and a seed, with
# the game as the sampling unit.

.G3_QUERY_FILTERS <- c(
  "game_id", "player", "player_contains", "white", "black", "agent", "model_revision",
  "date_from", "date_to", "result", "status", "termination", "rules_version", "rules_hash",
  "min_plies", "max_plies", "source", "tag", "content_hash", "origin", "deleted",
  "order_by", "descending", "sample_n", "sample_seed"
)
.G3_QUERY_ORDER <- c(
  updated = "g.updated_at", created = "g.created_at", date = "g.game_date", plies = "g.plies",
  events = "g.n_events", result = "g.result", white = "pw.name COLLATE NOCASE",
  black = "pb.name COLLATE NOCASE", game_id = "g.game_id"
)

#' Query the game catalogue
#'
#' Filters, sorts and pages game metadata. Filters are plain data (a list
#' that can be saved and reused), checked against an allow-list and bound as
#' SQL parameters; no filter value is ever evaluated as SQL or R code.
#'
#' @param db A `g3_db`.
#' @param filters A named list; `NULL` entries are ignored. Supported names:
#'   `game_id`, `player` (display name or agent, either colour,
#'   case-insensitive, any of several), `player_contains` (substring),
#'   `white`, `black`, `agent`, `model_revision`, `date_from`, `date_to`
#'   (`Date` or `"YYYY-MM-DD"`, inclusive, on the game's creation date),
#'   `result` (`"white"`, `"black"`, `"draw"`, `"truncated"`), `status`
#'   (`"live"`, `"finished"`), `termination`, `rules_version`,
#'   `rules_hash`, `min_plies`, `max_plies`, `source` (source label or
#'   SHA-256), `tag`, `content_hash`, `origin` (`"app"`, `"import"`),
#'   `deleted` (`"exclude"` default, `"include"`, `"only"`), `order_by`
#'   (`"updated"` default, `"created"`, `"date"`, `"plies"`, `"events"`,
#'   `"result"`, `"white"`, `"black"`, `"game_id"`), `descending` (default
#'   `TRUE`), and `sample_n` with `sample_seed` for a reproducible random
#'   sample of matching games (sampling unit: game; the sample is drawn
#'   before sorting and paging).
#' @param limit Page size, 1 to 10000.
#' @param offset Number of rows to skip.
#' @return A data frame with one row per game, sorted by `order_by` and then
#'   `game_id` (stable): `game_id`, `status`, `result`, `termination`,
#'   `white`, `black`, `white_agent`, `black_agent`, `white_model`,
#'   `black_model`, `plies`, `n_events`, `final_ply`, `rules_version`,
#'   `rules_hash`, `game_date`, `created_at`, `updated_at`, `revision`,
#'   `origin`, `n_sources`, `tags` (comma-separated), `deleted` and
#'   `game_content_hash`. Zero rows keep these typed columns. Attribute
#'   `total` is the number of matching games before paging.
#' @examples
#' db <- gambitR3D::g3_open_game_db(base::tempfile(fileext = ".sqlite"))
#' game <- gambitR3D::g3_new_game(players = base::list(white = "Ada", black = "Ben"))
#' gambitR3D::g3_save_game(db, gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2"))
#' gambitR3D::g3_query_games(db, filters = base::list(player = "ada"))[, c("white", "plies")]
#' gambitR3D::g3_query_games(db, filters = base::list(status = "finished"))
#' gambitR3D::g3_close_game_db(db)
#' @export
g3_query_games <- function(db, filters = list(), limit = 100L, offset = 0L) {
  lim <- .g3_db_limits()$page
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) || limit < 1 || limit > lim || limit != round(limit)) {
    .g3_abort("g3_io_error", paste0("`limit` must be a whole number from 1 to ", lim, "."))
  }
  if (!is.numeric(offset) || length(offset) != 1L || is.na(offset) || offset < 0 || offset != round(offset)) {
    .g3_abort("g3_io_error", "`offset` must be a non-negative whole number.")
  }
  q <- .g3_db_query_parts(filters)
  from <- paste(
    "FROM games g JOIN rules_profiles r ON r.rules_hash = g.rules_hash",
    "LEFT JOIN game_players pw ON pw.game_id = g.game_id AND pw.color = 'white'",
    "LEFT JOIN game_players pb ON pb.game_id = g.game_id AND pb.color = 'black'"
  )
  where <- if (length(q$where)) paste("WHERE", paste(q$where, collapse = " AND ")) else ""
  params <- q$params
  if (!is.null(q$sample)) {
    ids <- .g3_db_get(db, paste("SELECT g.game_id", from, where, "ORDER BY g.game_id"), params)$game_id
    pick <- if (length(ids)) .g3_with_seed(q$sample$seed, sort(ids[sample.int(length(ids), min(q$sample$n, length(ids)))])) else character()
    if (!length(pick)) pick <- NA_character_
    where <- paste(if (nzchar(where)) where else "WHERE 1", "AND g.game_id IN (", paste(rep("?", length(pick)), collapse = ", "), ")")
    params <- c(params, as.list(pick))
  }
  total <- .g3_db_get(db, paste("SELECT count(*) AS n", from, where), params)$n
  sql <- paste(
    "SELECT g.game_id, g.status, g.result, g.termination, pw.name AS white, pb.name AS black,",
    "pw.agent AS white_agent, pb.agent AS black_agent, pw.model_revision AS white_model,",
    "pb.model_revision AS black_model, g.plies, g.n_events, g.final_ply, r.rules_version, g.rules_hash,",
    "g.game_date, g.created_at, g.updated_at, g.revision, g.origin,",
    "(SELECT count(*) FROM game_sources s WHERE s.game_id = g.game_id) AS n_sources,",
    "g.deleted_at IS NOT NULL AS deleted, g.game_content_hash",
    from, where, "ORDER BY", q$order, if (q$descending) "DESC" else "ASC", ", g.game_id ASC",
    "LIMIT ? OFFSET ?"
  )
  rows <- .g3_db_get(db, sql, c(params, list(as.integer(limit), as.integer(offset))))
  out <- .g3_db_typed(rows, .G3_QUERY_COLUMNS)
  out$tags <- .g3_db_tags(db, out$game_id)
  out <- out[, c(setdiff(names(.G3_QUERY_COLUMNS), c("deleted", "game_content_hash")), "tags", "deleted", "game_content_hash")]
  attr(out, "total") <- as.integer(total)
  out
}

.G3_QUERY_COLUMNS <- c(
  game_id = "character", status = "character", result = "character", termination = "character",
  white = "character", black = "character", white_agent = "character", black_agent = "character",
  white_model = "character", black_model = "character", plies = "integer", n_events = "integer",
  final_ply = "integer", rules_version = "character", rules_hash = "character", game_date = "character",
  created_at = "character", updated_at = "character", revision = "integer", origin = "character",
  n_sources = "integer", deleted = "logical", game_content_hash = "character"
)

# Coerce a query result to declared column types (also for zero rows).
#' @noRd
.g3_db_typed <- function(df, spec) {
  out <- lapply(names(spec), function(nm) {
    v <- if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))
    switch(spec[[nm]], character = as.character(v), integer = as.integer(v), logical = as.logical(v),
           numeric = as.numeric(v))
  })
  out <- as.data.frame(stats::setNames(out, names(spec)), stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' @noRd
.g3_db_tags <- function(db, ids) {
  if (!length(ids)) return(character())
  t <- .g3_db_get(db, paste0(
    "SELECT game_id, body FROM annotations WHERE kind = 'tag' AND deleted_at IS NULL AND game_id IN (",
    paste(rep("?", length(ids)), collapse = ", "), ")"
  ), as.list(ids))
  vapply(ids, function(id) paste(sort(unique(t$body[t$game_id == id]), method = "radix"), collapse = ", "), "",
         USE.NAMES = FALSE)
}

# Translate a filter list into WHERE fragments and bound parameters.
#' @noRd
.g3_db_query_parts <- function(filters) {
  if (is.null(filters)) filters <- list()
  if (!is.list(filters) || (length(filters) && (is.null(names(filters)) || any(!nzchar(names(filters)))))) {
    .g3_abort("g3_io_error", "`filters` must be a named list.")
  }
  filters <- filters[!vapply(filters, is.null, TRUE)]
  unknown <- setdiff(names(filters), .G3_QUERY_FILTERS)
  if (length(unknown)) {
    .g3_abort("g3_io_error", paste0("Unknown filter(s): ", paste(unknown, collapse = ", "),
                                    ". Allowed: ", paste(.G3_QUERY_FILTERS, collapse = ", "), "."))
  }
  where <- character()
  params <- list()
  add <- function(clause, values = list()) {
    where <<- c(where, clause)
    params <<- c(params, values)
  }
  chr <- function(name, allowed = NULL, max_n = 1000L) {
    v <- filters[[name]]
    if (!is.character(v) || !length(v) || length(v) > max_n || anyNA(v) || any(nchar(v) > .g3_db_limits()$text_chars)) {
      .g3_abort("g3_io_error", paste0("Filter `", name, "` must be a character vector without missing values."))
    }
    if (!is.null(allowed) && !all(v %in% allowed)) {
      .g3_abort("g3_io_error", paste0("Filter `", name, "` must be one or more of: ", paste(allowed, collapse = ", "), "."))
    }
    v
  }
  qs <- function(n) paste(rep("?", n), collapse = ", ")
  int1 <- function(name) {
    v <- filters[[name]]
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 0 || v != round(v)) {
      .g3_abort("g3_io_error", paste0("Filter `", name, "` must be one non-negative whole number."))
    }
    as.integer(v)
  }
  if (!is.null(filters[["game_id"]])) {
    v <- chr("game_id")
    add(paste0("g.game_id IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["player"]])) {
    v <- chr("player")
    add(paste0("EXISTS (SELECT 1 FROM game_players p WHERE p.game_id = g.game_id AND (p.name COLLATE NOCASE IN (",
               qs(length(v)), ") OR p.agent COLLATE NOCASE IN (", qs(length(v)), ")))"), as.list(c(v, v)))
  }
  if (!is.null(filters[["player_contains"]])) {
    v <- chr("player_contains", max_n = 1L)
    add(paste("EXISTS (SELECT 1 FROM game_players p WHERE p.game_id = g.game_id AND",
              "(instr(lower(p.name), lower(?)) > 0 OR instr(lower(coalesce(p.agent, '')), lower(?)) > 0))"),
        list(v, v))
  }
  for (col in c("white", "black")) {
    if (!is.null(filters[[col]])) {
      v <- chr(col)
      alias <- if (col == "white") "pw" else "pb"
      add(paste0(alias, ".name COLLATE NOCASE IN (", qs(length(v)), ")"), as.list(v))
    }
  }
  if (!is.null(filters[["agent"]])) {
    v <- chr("agent")
    add(paste0("EXISTS (SELECT 1 FROM game_players p WHERE p.game_id = g.game_id AND p.agent COLLATE NOCASE IN (",
               qs(length(v)), "))"), as.list(v))
  }
  if (!is.null(filters[["model_revision"]])) {
    v <- chr("model_revision")
    add(paste0("EXISTS (SELECT 1 FROM game_players p WHERE p.game_id = g.game_id AND p.model_revision IN (",
               qs(length(v)), "))"), as.list(v))
  }
  if (!is.null(filters[["date_from"]])) add("g.game_date >= ?", list(.g3_db_date_bound(filters[["date_from"]], "date_from")))
  if (!is.null(filters[["date_to"]])) {
    b <- .g3_db_date_bound(filters[["date_to"]], "date_to")
    if (nchar(b) == 10L) {
      add("g.game_date < ?", list(format(as.Date(b) + 1L, "%Y-%m-%d")))
    } else {
      add("g.game_date <= ?", list(b))
    }
  }
  if (!is.null(filters[["result"]])) {
    v <- chr("result", c("white", "black", "draw", "truncated"))
    add(paste0("g.result IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["status"]])) {
    v <- chr("status", .G3_DB_STATUS)
    add(paste0("g.status IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["termination"]])) {
    v <- chr("termination")
    add(paste0("g.termination IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["rules_version"]])) {
    v <- chr("rules_version")
    add(paste0("r.rules_version IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["rules_hash"]])) {
    v <- chr("rules_hash")
    add(paste0("g.rules_hash IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["min_plies"]])) add("g.plies >= ?", list(int1("min_plies")))
  if (!is.null(filters[["max_plies"]])) add("g.plies <= ?", list(int1("max_plies")))
  if (!is.null(filters[["source"]])) {
    v <- chr("source")
    add(paste0("EXISTS (SELECT 1 FROM game_sources gs JOIN sources s ON s.source_id = gs.source_id ",
               "WHERE gs.game_id = g.game_id AND (s.label IN (", qs(length(v)), ") OR s.source_sha256 IN (",
               qs(length(v)), ")))"), as.list(c(v, v)))
  }
  if (!is.null(filters[["tag"]])) {
    v <- chr("tag")
    add(paste0("EXISTS (SELECT 1 FROM annotations a WHERE a.game_id = g.game_id AND a.kind = 'tag' ",
               "AND a.deleted_at IS NULL AND a.body IN (", qs(length(v)), "))"), as.list(v))
  }
  if (!is.null(filters[["content_hash"]])) {
    v <- chr("content_hash")
    add(paste0("g.game_content_hash IN (", qs(length(v)), ")"), as.list(v))
  }
  if (!is.null(filters[["origin"]])) {
    v <- chr("origin", c("app", "import"))
    add(paste0("g.origin IN (", qs(length(v)), ")"), as.list(v))
  }
  deleted <- if (is.null(filters[["deleted"]])) "exclude" else chr("deleted", c("exclude", "include", "only"), 1L)
  if (deleted == "exclude") add("g.deleted_at IS NULL")
  if (deleted == "only") add("g.deleted_at IS NOT NULL")
  order <- if (is.null(filters[["order_by"]])) "updated" else chr("order_by", names(.G3_QUERY_ORDER), 1L)
  desc <- filters[["descending"]] %||% TRUE
  if (!is.logical(desc) || length(desc) != 1L || is.na(desc)) .g3_abort("g3_io_error", "Filter `descending` must be TRUE or FALSE.")
  sample <- NULL
  if (!is.null(filters[["sample_n"]]) || !is.null(filters[["sample_seed"]])) {
    if (is.null(filters[["sample_n"]]) || is.null(filters[["sample_seed"]])) {
      .g3_abort("g3_io_error", "Random sampling needs both `sample_n` and `sample_seed`.")
    }
    sample <- list(n = int1("sample_n"), seed = int1("sample_seed"))
    if (sample$n < 1L || sample$n > .g3_db_limits()$page) {
      .g3_abort("g3_io_error", paste0("`sample_n` must be from 1 to ", .g3_db_limits()$page, "."))
    }
  }
  list(where = where, params = params, order = .G3_QUERY_ORDER[[order]], descending = desc, sample = sample)
}

#' @noRd
.g3_db_date_bound <- function(x, name) {
  if (inherits(x, "Date") && length(x) == 1L && !is.na(x)) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt") && length(x) == 1L && !is.na(x)) return(format(x, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"))
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) && !is.na(as.Date(x, optional = TRUE))) {
    return(x)
  }
  .g3_abort("g3_io_error", paste0("Filter `", name, "` must be one Date, POSIXct or \"YYYY-MM-DD\" value."))
}
