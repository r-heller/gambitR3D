# Self-contained offline analysis reports.
#
# `g3_export_analysis_report()` writes into one output directory:
#
#   report.html      a single self-contained page: inline CSS, figures as
#                    base64-embedded PNG data URIs, no scripts, no remote
#                    fonts, styles, images or links;
#   tables/*.csv     the machine-readable tables behind the report;
#   manifest.json    file list with byte sizes and SHA-256 checksums.
#
# All text taken from games (player names, annotations, event reasons,
# filters) is HTML-escaped and shown as text; nothing from a game is ever
# evaluated, rendered as markup or fetched. An optional PDF is printed from
# the HTML by a locally installed headless Chromium/Chrome; when none is
# available the HTML is still written and the manifest says why the PDF is
# missing.

.G3_REPORT_SCHEMA <- "g3-report/1"

#' Export an analysis report
#'
#' Writes a self-contained HTML report (and machine-readable CSV tables)
#' for a game analysis, a collection analysis or an agent comparison. The
#' report includes filters, counts and missingness, the rules hash, engine
#' and evaluator versions, budgets, score perspective and scale
#' definitions, provenance, generation time and reproducibility details.
#' It renders without network access.
#'
#' @param result A `g3_analysis`, `g3_database_analysis` or
#'   `g3_comparison`.
#' @param path Output directory (created if needed). The report is written
#'   to `report.html`, tables to `tables/` and a manifest to
#'   `manifest.json`.
#' @param format `"html"` (default) or `"pdf"`. PDF output additionally
#'   prints the HTML with a local headless Chromium/Chrome; if none is
#'   found, a `g3_warning` explains it and only the HTML is produced.
#' @param title Optional report title.
#' @param overwrite Replace existing report files in `path`.
#' @param max_rows Maximum rows of each table shown in the HTML (the CSV
#'   files are always complete).
#' @return Invisibly, an output manifest list: `schema`, `created`,
#'   `result_class`, `format`, `title`, `report` (path of the HTML),
#'   `pdf` (path or `NA`), `pdf_status`, `files` (data frame `file`,
#'   `role`, `rows`, `bytes`, `sha256`), `totals` (the counts printed in
#'   the report), `offline` (`TRUE`) and `package_version`. The same
#'   manifest is written to `manifest.json`, with `report` and `pdf` given
#'   relative to the report directory (no local paths are exported).
#' @examples
#' # Analyse a short game and write an offline report into a temporary folder.
#' game <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' result <- gambitR3D::g3_analyse_game(game, budget = base::list(nodes = 1000L))
#' dir <- base::file.path(base::tempdir(), "g3-report")
#' out <- gambitR3D::g3_export_analysis_report(result, dir, overwrite = TRUE)
#' out$files[, c("file", "rows", "sha256")]
#' @export
g3_export_analysis_report <- function(result, path, format = "html", title = NULL, overwrite = FALSE,
                                      max_rows = 200L) {
  format <- match.arg(format, c("html", "pdf"))
  if (!inherits(result, c("g3_analysis", "g3_database_analysis", "g3_comparison"))) {
    .g3_abort("g3_invalid_state", "`result` must be a g3_analysis, g3_database_analysis or g3_comparison.")
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) .g3_abort("g3_io_error", "`path` must be one directory path.")
  html_path <- file.path(path, "report.html")
  if (file.exists(html_path) && !overwrite) {
    .g3_abort("g3_io_error", paste0("A report already exists in ", path, " (use overwrite = TRUE)."))
  }
  dir.create(file.path(path, "tables"), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) .g3_abort("g3_io_error", paste0("Cannot create the output directory ", path))
  plots_ok <- requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("viridisLite", quietly = TRUE)
  content <- if (inherits(result, "g3_analysis")) {
    .g3_report_game(result, plots_ok, max_rows)
  } else if (inherits(result, "g3_database_analysis")) {
    .g3_report_collection(result, plots_ok, max_rows)
  } else {
    .g3_report_comparison(result, plots_ok, max_rows)
  }
  title <- title %||% content$title
  created <- .g3_now()
  # Tables first: every figure has a table behind it.
  files <- list()
  for (nm in names(content$tables)) {
    rel <- file.path("tables", paste0(nm, ".csv"))
    .g3_an_write_csv(content$tables[[nm]], file.path(path, rel))
    files[[length(files) + 1L]] <- list(file = rel, role = "table", rows = nrow(content$tables[[nm]]))
  }
  html <- .g3_report_html(title, created, content)
  .g3_atomic_write(html, html_path)
  files[[length(files) + 1L]] <- list(file = "report.html", role = "report", rows = NA_integer_)
  pdf_path <- NA_character_
  pdf_status <- "not requested"
  if (format == "pdf") {
    pr <- .g3_report_pdf(html_path, file.path(path, "report.pdf"))
    pdf_status <- pr$status
    if (pr$ok) {
      pdf_path <- file.path(path, "report.pdf")
      files[[length(files) + 1L]] <- list(file = "report.pdf", role = "report_pdf", rows = NA_integer_)
    } else {
      .g3_warn(paste0("PDF not produced: ", pr$status, " The HTML report was written to ", html_path, "."))
    }
  }
  fdf <- .g3_an_rows_df(files, c(file = "character", role = "character", rows = "integer"))
  fdf$bytes <- vapply(fdf$file, function(f) as.numeric(file.size(file.path(path, f))), 0, USE.NAMES = FALSE)
  fdf$sha256 <- vapply(fdf$file, function(f) digest::digest(file = file.path(path, f), algo = "sha256"), "", USE.NAMES = FALSE)
  # manifest.json records paths relative to the report directory, so an
  # exported report never reveals the local directory layout. Only the
  # returned list (which stays in this session) has the local paths.
  manifest <- list(
    schema = .G3_REPORT_SCHEMA, created = created, result_class = class(result)[1L], format = format,
    title = title, report = normalizePath(html_path), pdf = pdf_path, pdf_status = pdf_status,
    files = fdf, totals = content$totals, offline = TRUE,
    package_version = as.character(utils::packageVersion("gambitR3D"))
  )
  mj <- manifest
  mj$report <- "report.html"
  mj$pdf <- if (is.na(pdf_path)) NA_character_ else "report.pdf"
  mj$files <- .g3_rows(fdf)
  .g3_atomic_write(paste0(jsonlite::toJSON(mj, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA), "\n"),
                   file.path(path, "manifest.json"))
  invisible(manifest)
}

# ---- HTML helpers -----------------------------------------------------------

#' @noRd
.g3_report_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' @noRd
.g3_report_cell <- function(v) {
  if (is.logical(v)) return(ifelse(is.na(v), "NA", ifelse(v, "yes", "no")))
  if (is.double(v)) {
    return(ifelse(is.na(v), "NA", ifelse(is.infinite(v), as.character(v),
                                         formatC(v, format = "fg", digits = 4, flag = "#"))))
  }
  ifelse(is.na(v), "NA", as.character(v))
}

#' @noRd
.g3_report_table <- function(df, max_rows = 200L, csv = NULL, caption = NULL) {
  n <- nrow(df)
  shown <- utils::head(df, max_rows)
  head <- paste0("<tr>", paste0("<th scope=\"col\">", .g3_report_escape(names(df)), "</th>", collapse = ""), "</tr>")
  body <- if (nrow(shown)) {
    cells <- lapply(shown, .g3_report_cell)
    rows <- vapply(seq_len(nrow(shown)), function(i) {
      paste0("<tr>", paste0("<td>", .g3_report_escape(vapply(cells, `[`, "", i)), "</td>", collapse = ""), "</tr>")
    }, "")
    paste(rows, collapse = "\n")
  } else {
    paste0("<tr><td colspan=\"", max(1L, ncol(df)), "\" class=\"empty\">No rows.</td></tr>")
  }
  note <- paste0(
    "<p class=\"note\">", n, if (n == 1L) " row" else " rows",
    if (n > max_rows) paste0(", first ", max_rows, " shown") else "",
    if (!is.null(csv)) paste0("; complete table: <a href=\"", .g3_report_escape(csv), "\">", .g3_report_escape(csv), "</a>") else "",
    ".</p>"
  )
  paste0(
    "<div class=\"table-wrap\"><table>",
    if (!is.null(caption)) paste0("<caption>", .g3_report_escape(caption), "</caption>") else "",
    "<thead>", head, "</thead><tbody>", body, "</tbody></table></div>", note
  )
}

#' @noRd
.g3_report_kv <- function(keys, values) {
  rows <- paste0("<tr><th scope=\"row\">", .g3_report_escape(keys), "</th><td>", .g3_report_escape(values), "</td></tr>",
                 collapse = "\n")
  paste0("<div class=\"table-wrap\"><table class=\"kv\"><tbody>", rows, "</tbody></table></div>")
}

# A plot as an embedded PNG with alt text and its table as the text
# alternative.
#' @noRd
.g3_report_figure <- function(p, alt, csv = NULL, width = 8, height = 4.2) {
  meta <- attr(p, "g3_plot_meta")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    ggplot2::ggsave(tmp, p, width = width, height = height, units = "in", dpi = 110, bg = "white")
    TRUE
  }, error = function(e) e)
  if (!isTRUE(ok)) {
    return(paste0("<p class=\"note\">Figure not rendered: ", .g3_report_escape(conditionMessage(ok)), "</p>"))
  }
  b64 <- jsonlite::base64_enc(readBin(tmp, "raw", file.size(tmp)))
  b64 <- gsub("[\r\n]", "", b64)
  empty <- isTRUE(meta$empty)
  paste0(
    "<figure><img src=\"data:image/png;base64,", b64, "\" alt=\"", .g3_report_escape(alt), "\">",
    "<figcaption>", .g3_report_escape(alt),
    if (empty) paste0(" (Empty: ", .g3_report_escape(meta$reason), ")") else "",
    if (!is.null(csv)) paste0(" Data: <a href=\"", .g3_report_escape(csv), "\">", .g3_report_escape(csv), "</a>.") else "",
    "</figcaption></figure>"
  )
}

#' @noRd
.g3_report_css <- function() {
  paste(
    ":root{--bg:#ffffff;--fg:#1d2329;--muted:#4c5a66;--line:#d5dde3;--panel:#f3f6f8;--accent:#0072B2;--amber:#E69F00;}",
    "@media (prefers-color-scheme: dark){:root{--bg:#14191d;--fg:#e8edf1;--muted:#a9b6c0;--line:#33404a;--panel:#1d252b;--accent:#56B4E9;--amber:#E69F00;}}",
    "*{box-sizing:border-box}",
    "body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 system-ui,-apple-system,'Segoe UI',Roboto,'Noto Sans',Ubuntu,Cantarell,'Helvetica Neue',Arial,sans-serif;}",
    "main{max-width:1080px;margin:0 auto;padding:24px 16px 48px}",
    "h1{font-size:1.6rem;margin:0 0 4px}h2{font-size:1.2rem;margin:32px 0 8px;padding-top:8px;border-top:1px solid var(--line)}",
    "h3{font-size:1rem;margin:20px 0 6px}",
    ".sub{color:var(--muted);margin:0 0 16px}.note{color:var(--muted);font-size:13px;margin:4px 0 12px}",
    ".badge{display:inline-block;border:1px solid var(--accent);color:var(--accent);border-radius:8px;padding:0 8px;font-size:13px;margin-right:6px}",
    ".warn{border-left:4px solid var(--amber);background:var(--panel);padding:8px 12px;border-radius:8px}",
    ".table-wrap{overflow-x:auto;border:1px solid var(--line);border-radius:8px;margin:8px 0}",
    "table{border-collapse:collapse;width:100%;font-size:13px;font-variant-numeric:tabular-nums}",
    "th,td{text-align:left;padding:4px 8px;border-bottom:1px solid var(--line);vertical-align:top}",
    "thead th{background:var(--panel);position:sticky;top:0}table.kv th{width:30%;background:var(--panel)}",
    "td.empty{color:var(--muted);font-style:italic}caption{text-align:left;padding:6px 8px;color:var(--muted)}",
    "figure{margin:12px 0}figure img{max-width:100%;height:auto;border:1px solid var(--line);border-radius:8px;background:#fff}",
    "figcaption{color:var(--muted);font-size:13px}pre{white-space:pre-wrap;word-break:break-word;background:var(--panel);padding:8px 12px;border-radius:8px;font-size:13px}",
    "a{color:var(--accent)}nav ul{padding-left:18px}",
    sep = "\n"
  )
}

#' @noRd
.g3_report_html <- function(title, created, content) {
  toc <- paste0("<li><a href=\"#", content$sections$id, "\">", .g3_report_escape(content$sections$title), "</a></li>", collapse = "")
  secs <- paste0("<section id=\"", content$sections$id, "\"><h2>", .g3_report_escape(content$sections$title), "</h2>\n",
                 content$sections$html, "</section>", collapse = "\n")
  paste0(
    "<!DOCTYPE html>\n<html lang=\"en-GB\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<meta name=\"generator\" content=\"gambitR3D ", .g3_report_escape(as.character(utils::packageVersion("gambitR3D"))), "\">\n",
    "<title>", .g3_report_escape(title), "</title>\n<style>\n", .g3_report_css(), "\n</style>\n</head>\n<body>\n<main>\n",
    "<h1>", .g3_report_escape(title), "</h1>\n<p class=\"sub\">", .g3_report_escape(content$subtitle),
    " Generated ", .g3_report_escape(created), " by gambitR3D ", .g3_report_escape(as.character(utils::packageVersion("gambitR3D"))),
    ". Experimental rules profile: a project convention, not an official rulebook.</p>\n",
    "<nav aria-label=\"Contents\"><ul>", toc, "</ul></nav>\n", secs, "\n</main>\n</body>\n</html>\n"
  )
}

#' @noRd
.g3_report_sections <- function(...) {
  x <- list(...)
  data.frame(id = vapply(x, `[[`, "", "id"), title = vapply(x, `[[`, "", "title"),
             html = vapply(x, `[[`, "", "html"), stringsAsFactors = FALSE)
}

#' @noRd
.g3_report_sec <- function(id, title, ...) list(id = id, title = title, html = paste0(..., collapse = "\n"))

#' @noRd
.g3_report_budget_text <- function(b) {
  if (is.null(b)) return("NA")
  sb <- .g3_budget(lapply(b[intersect(names(b), c("nodes", "time_ms", "depth"))], as.numeric))
  # Model budget fields (mode, simulations) are reported when present.
  sb$mode <- b$mode
  sb$simulations <- b$simulations
  .g3_an_budget_text(sb)
}

# ---- report bodies ------------------------------------------------------------

#' @noRd
.g3_report_game <- function(r, plots_ok, max_rows) {
  pv <- r$provenance
  s <- r$summary
  ev <- r$evaluations
  players <- r$game$players
  miss <- table(ev$missing_reason, useNA = "no")
  tac_tab <- if (nrow(r$tactics)) as.data.frame(table(label = r$tactics$label, claim_strength = r$tactics$claim_strength),
                                                stringsAsFactors = FALSE) else
    data.frame(label = character(), claim_strength = character(), Freq = integer())
  tac_tab <- tac_tab[tac_tab$Freq > 0, , drop = FALSE]
  names(tac_tab)[names(tac_tab) == "Freq"] <- "n_labels"
  tables <- list(
    summary = s, moves = r$moves, positions = r$positions, evaluations = r$evaluations, tactics = r$tactics,
    events = r$events, mobility = r$mobility, platforms = r$platforms, pieces = r$pieces, squares = r$squares,
    feature_dictionary = g3_feature_dictionary()
  )
  figs <- ""
  if (plots_ok) {
    figs <- paste(
      .g3_report_figure(g3_plot_game(r, "expected_score", "white"),
                      "Evaluation trajectory: expected score from White's perspective (heuristic logistic map, uncalibrated) by ply, with captures, checks and platform actions marked.",
                      "tables/evaluations.csv"),
      .g3_report_figure(g3_plot_game(r, "move_loss"), "Move loss per action: best alternative minus played action in the same root search, heuristic units.",
                      "tables/evaluations.csv"),
      .g3_report_figure(g3_plot_game(r, "material"), "Material balance, White minus Black, provisional piece values.", "tables/positions.csv"),
      .g3_report_figure(g3_plot_board_heatmap(r, "occupancy", "board_local"),
                      "Occupancy share of every physical square per board in local coordinates (boards never merged).",
                      "tables/squares.csv", width = 8, height = 5.5),
      .g3_report_figure(g3_plot_board_heatmap(r, "occupancy", "projected"),
                      "Projected-coordinate occupancy: occupied squares of all boards sharing a coordinate summed, per selected position.",
                      "tables/squares.csv", width = 7, height = 6),
      sep = "\n"
    )
  } else {
    figs <- "<p class=\"warn\">Figures need the ggplot2 and viridisLite packages; the tables below carry the same data.</p>"
  }
  ann <- r$game$annotations
  ann_html <- if (length(ann)) {
    paste0("<p class=\"note\">Annotations from the game file, shown as escaped text and never executed.</p><pre>",
           .g3_report_escape(.g3_canonical_json(ann)), "</pre>")
  } else "<p class=\"note\">The game carries no annotations.</p>"
  mv <- r$moves[, c("ply", "actor", "notation", "action_type", "capture", "check", "platform_action", "transfer_mode"), drop = FALSE]
  evs <- ev[, c("ply", "side", "score_kind", "score_white", "expected_score_white", "best_action_id",
                "played_action_id", "move_loss_raw", "move_loss_sequential", "depth", "missing_reason"), drop = FALSE]
  totals <- list(actions = nrow(r$moves), positions = nrow(r$positions), evaluated = s$n_evaluated,
                 missing = s$n_eval_missing, tactic_labels = nrow(r$tactics), session_events = nrow(r$events))
  secs <- .g3_report_sections(
    .g3_report_sec("overview", "Overview",
            .g3_report_kv(
              c("Game id", "White", "Black", "Result", "Termination", "Actions", "Positions", "Start position",
                "Game content hash"),
              c(r$game$game_id, players$white %||% "White", players$black %||% "Black", s$result, s$termination,
                totals$actions, totals$positions, if (isTRUE(r$game$standard_start)) "standard start" else r$game$start_position_key,
                r$game$game_content_hash)
            ),
            "<p class=\"note\">A truncated game is a computational cutoff, not a draw; an unfinished game has no result.</p>"),
    .g3_report_sec("evaluation", "Evaluation and figures", figs),
    .g3_report_sec("counts", "Counts and missing data",
            .g3_report_kv(
              c("Actions (rows in moves.csv)", "Positions (rows in positions.csv)", "Positions with a score",
                "Positions without a score", "Tactical labels", "Session events", "Captures", "Checks",
                "Platform actions", "Board transfers (piece moves)", "Level transfers"),
              c(totals$actions, totals$positions, totals$evaluated, totals$missing, totals$tactic_labels,
                totals$session_events, s$n_captures, s$n_checks, s$n_platform_actions, s$n_board_transfers,
                s$n_level_transfers)
            ),
            .g3_report_table(data.frame(missing_reason = names(miss), positions = as.integer(miss), stringsAsFactors = FALSE),
                           max_rows, caption = "Why scores are missing")),
    .g3_report_sec("moves", "Actions", .g3_report_table(mv, max_rows, "tables/moves.csv")),
    .g3_report_sec("scores", "Scores by position", .g3_report_table(evs, max_rows, "tables/evaluations.csv")),
    .g3_report_sec("tactics", "Tactical labels",
            .g3_report_table(tac_tab, max_rows, caption = "Labels by claim strength"),
            .g3_report_table(r$tactics[, c("ply", "actor", "label", "claim_strength", "evidence")], max_rows, "tables/tactics.csv"),
            .g3_report_table(.g3_feat_tactic_defs(), 50L, caption = "Definitions: exact, bounded-search and heuristic claims")),
    .g3_report_sec("session", "Session events and annotations",
            .g3_report_table(r$events, max_rows, "tables/events.csv"), ann_html),
    .g3_report_sec("provenance", "Provenance and reproducibility",
            .g3_report_kv(
              c("Rules version", "Rules hash", "Action schema", "Feature schema", "Evaluator", "Evaluator version",
                "Model revision", "Budget per position", "Budget semantics", "Determinism", "Raw score unit",
                "Raw perspective", "Views", "Mate encoding", "Expected score", "Cache (hits / misses)", "Cache key",
                "Timings (ms)", "Analysis id", "Package version", "R version", "Analysis created"),
              c(pv$rules_version, pv$rules_hash, pv$action_schema, pv$feature_schema, pv$evaluator,
                pv$evaluator_version %||% "NA", pv$model_revision %||% "NA", .g3_an_budget_text(pv$budget), pv$budget_semantics,
                pv$determinism, pv$score_scale$raw_unit, pv$score_scale$raw_perspective, pv$score_scale$views,
                pv$score_scale$mate_encoding, pv$score_scale$expected_score_map,
                paste(pv$cache$hits, pv$cache$misses, sep = " / "), pv$cache$key,
                paste(names(pv$timings_ms), round(unlist(pv$timings_ms)), sep = " ", collapse = ", "),
                pv$analysis_id, pv$package_version, pv$r_version, pv$created)
            ),
            "<p class=\"note\">Reproduce with <code>gambitR3D::g3_analyse_game(game, evaluator = &quot;",
            .g3_report_escape(pv$evaluator), "&quot;, budget = ", .g3_report_escape(.g3_report_budget_call(pv$budget)),
            ")</code> on the game whose content hash is listed above.</p>",
            "<p class=\"note\">Every column is defined in tables/feature_dictionary.csv (units, denominators, missing-value policy).</p>")
  )
  list(title = paste0("Game analysis: ", r$game$game_id),
       subtitle = paste0(s$n_plies, " plies, ", s$result, " (", s$termination, "); evaluator ", pv$evaluator, ", ",
                         .g3_an_budget_text(pv$budget), " per position."),
       sections = secs, tables = tables, totals = totals)
}

#' @noRd
.g3_report_budget_call <- function(b) {
  parts <- c(if (is.finite(b$nodes)) paste0("nodes = ", format(b$nodes, scientific = FALSE), "L"),
             if (is.finite(b$time_ms)) paste0("time_ms = ", b$time_ms),
             paste0("depth = ", b$depth, "L"))
  paste0("base::list(", paste(parts, collapse = ", "), ")")
}

#' @noRd
.g3_report_collection <- function(x, plots_ok, max_rows) {
  m <- x$manifest
  sm <- x$summary
  tables <- c(
    list(completed = x$completed, errors = x$errors,
         pending = data.frame(game_id = as.character(x$pending), stringsAsFactors = FALSE)),
    sm[setdiff(names(sm), "filters")],
    list(filters = sm$filters, feature_dictionary = g3_feature_dictionary())
  )
  figs <- if (plots_ok) {
    paste(
      .g3_report_figure(g3_plot_collection(x, "outcome"), "Outcomes as shares of the analysed games with 95% Wilson intervals; labels give counts over the analysed total; truncated and unfinished are separate from draws.", "tables/outcomes.csv"),
      .g3_report_figure(g3_plot_collection(x, "termination"), "Termination causes as shares of the analysed games with 95% Wilson intervals and counts.", "tables/terminations.csv"),
      .g3_report_figure(g3_plot_collection(x, "length"), "Game length in plies by outcome class, one point per game.", "tables/games.csv"),
      .g3_report_figure(g3_plot_collection(x, "platform_use"), "Share of platform actions by game phase with game-clustered 95% intervals and counts.", "tables/platform_use_by_phase.csv"),
      if (length(x$results)) .g3_report_figure(g3_plot_board_heatmap(x, "occupancy", "board_local"),
                                             "Pooled occupancy share of every physical square, per board in local coordinates.", NULL, width = 8, height = 5.5) else "",
      sep = "\n"
    )
  } else "<p class=\"warn\">Figures need the ggplot2 and viridisLite packages.</p>"
  totals <- list(scheduled = m$n_scheduled, completed = m$n_completed, errors = m$n_errors, pending = m$n_pending,
                 actions = sm$platform_use$total_actions)
  secs <- .g3_report_sections(
    .g3_report_sec("run", "Run and query",
            .g3_report_kv(
              c("Status", "Source", "Filters (query specification)", "Scheduled games", "Completed", "Errors", "Pending",
                "Evaluator", "Evaluator version", "Budget per position", "Rules hash", "Feature schema",
                "Configuration hash", "Package version", "Run created", "Run updated"),
              c(m$status, m$source, m$filters, m$n_scheduled, m$n_completed, m$n_errors, m$n_pending, m$evaluator,
                m$evaluator_version %||% "NA", .g3_report_budget_text(m$budget), m$rules_hash, m$feature_schema,
                m$run_config_hash, m$package_version, m$created, m$updated)
            ),
            "<p class=\"note\">Descriptive summaries of this selection; plies within a game are not independent observations.</p>"),
    .g3_report_sec("figures", "Figures", figs),
    .g3_report_sec("outcomes", "Outcomes and termination",
            paste0("<p class=\"note\">Shares carry 95% Wilson score intervals (ci_low, ci_high). Shares of actions use ",
                   "the game-clustered effective sample size, because actions within a game are not independent. ",
                   "small_sample marks rates based on fewer than ", .G3_SMALL_SAMPLE_GAMES, " games.",
                   if (isTRUE(sm$outcomes$small_sample[1L])) paste0(" This selection has ", m$n_completed,
                                                                    " analysed games: treat its shares as unstable.") else "",
                   "</p>"),
            .g3_report_table(sm$rate_intervals, max_rows, "tables/rate_intervals.csv"),
            .g3_report_table(sm$outcomes, max_rows, "tables/outcomes.csv"),
            .g3_report_table(sm$terminations, max_rows, "tables/terminations.csv"),
            .g3_report_table(sm$lengths, max_rows, "tables/lengths.csv")),
    .g3_report_sec("platforms", "Platform use and transfers",
            .g3_report_table(sm$platform_use, max_rows, "tables/platform_use.csv"),
            .g3_report_table(sm$platform_use_by_phase, max_rows, "tables/platform_use_by_phase.csv"),
            .g3_report_table(sm$transfers, max_rows, "tables/transfers.csv")),
    .g3_report_sec("groups", "Opening groups, repetition and motifs",
            .g3_report_table(sm$opening_groups, max_rows, "tables/opening_groups.csv"),
            .g3_report_table(sm$repetition, max_rows, "tables/repetition.csv"),
            .g3_report_table(sm$repeated_positions, max_rows, "tables/repeated_positions.csv"),
            .g3_report_table(sm$motifs, max_rows, "tables/motifs.csv")),
    .g3_report_sec("missing", "Missing data and failures",
            .g3_report_table(sm$evaluation_missingness, max_rows, "tables/evaluation_missingness.csv"),
            .g3_report_table(x$errors, max_rows, "tables/errors.csv"),
            .g3_report_table(data.frame(game_id = as.character(x$pending), stringsAsFactors = FALSE), max_rows, "tables/pending.csv")),
    .g3_report_sec("games", "Completed games", .g3_report_table(x$completed, max_rows, "tables/completed.csv"))
  )
  list(title = "Game collection analysis",
       subtitle = paste0(m$n_completed, " of ", m$n_scheduled, " games analysed (", m$status, "); filters ", m$filters, "."),
       sections = secs, tables = tables, totals = totals)
}

#' @noRd
.g3_report_comparison <- function(x, plots_ok, max_rows) {
  s <- x$summary
  pv <- x$provenance
  u <- x$uncertainty
  tables <- list(summary = s, by_colour = x$by_colour, costs = x$costs, pair_results = x$pair_results,
                 games = x$games, decisions = x$decisions, intervals = u$intervals, positions = pv$positions)
  figs <- if (plots_ok) {
    paste(
      .g3_report_figure(g3_plot_comparison(x, "score"), "Completed-game expected score of the first agent with its paired-block bootstrap interval and scheduled-game bounds.", "tables/summary.csv"),
      .g3_report_figure(g3_plot_comparison(x, "outcomes"), "Every scheduled game by outcome category and colour.", "tables/games.csv"),
      .g3_report_figure(g3_plot_comparison(x, "latency"), "Decision latency per agent with median and 95th percentile.", "tables/decisions.csv"),
      sep = "\n"
    )
  } else "<p class=\"warn\">Figures need the ggplot2 and viridisLite packages.</p>"
  totals <- list(scheduled = s$scheduled, completed = s$completed, truncated = s$truncated, failed = s$failed,
                 cancelled = s$cancelled, decisions = nrow(x$decisions), illegal_actions = s$illegal_actions)
  secs <- .g3_report_sections(
    .g3_report_sec("design", "Design",
            .g3_report_kv(
              c("Agents", "Budget per decision", "Budget semantics", "Seeds", "Pairs per position and seed",
                "Starting positions", "Max plies", "Colour policy", "Truncation policy", "Illegal-output policy",
                "Score definition", "Rating", "Rules hash", "Search / evaluation versions", "Package version",
                "R version", "Hardware", "Created"),
              c(paste(s$agent1, s$agent2, sep = " vs "), .g3_an_budget_text(pv$budget), pv$budget_semantics,
                paste(pv$seeds, collapse = ", "), pv$pairs, nrow(pv$positions), pv$max_plies, pv$colour_policy,
                pv$truncation_policy, pv$illegal_policy, pv$score_definition, pv$rating, pv$rules_hash,
                paste(pv$search_version, pv$eval_version, sep = " / "), pv$package_version, pv$r_version,
                paste(unlist(pv$hardware), collapse = " "), pv$created)
            )),
    .g3_report_sec("results", "Results",
            .g3_report_kv(c("Scheduled games", "Completed", "Truncated", "Failed", "Cancelled", "Wins / draws / losses (first agent)",
                           "Completed-game score", "Scheduled bounds", "Illegal actions"),
                         c(s$scheduled, s$completed, s$truncated, s$failed, s$cancelled,
                           paste(s$wins, s$draws, s$losses, sep = " / "), format(s$completed_score, digits = 4),
                           paste0("[", format(s$scheduled_lower_bound, digits = 4), ", ", format(s$scheduled_upper_bound, digits = 4), "]"),
                           s$illegal_actions)),
            figs,
            .g3_report_table(x$by_colour, max_rows, "tables/by_colour.csv"),
            "<h3>Uncertainty</h3>",
            .g3_report_kv(c("Method", "Unit", "Blocks", "Replicates", "Seed", "Level", "Note"),
                         c(u$method, u$unit, u$n_blocks, u$replicates, u$seed, u$level, if (nzchar(u$note)) u$note else "-")),
            .g3_report_table(u$intervals, max_rows, "tables/intervals.csv")),
    .g3_report_sec("costs", "Computation costs", .g3_report_table(x$costs, max_rows, "tables/costs.csv")),
    .g3_report_sec("games", "Scheduled games",
            .g3_report_table(x$pair_results, max_rows, "tables/pair_results.csv"),
            .g3_report_table(x$games, max_rows, "tables/games.csv"))
  )
  list(title = paste0("Agent comparison: ", s$agent1, " vs ", s$agent2),
       subtitle = paste0(s$scheduled, " scheduled games, ", s$completed, " completed."),
       sections = secs, tables = tables, totals = totals)
}

# Print the HTML to PDF with a local headless Chromium/Chrome.
#' @noRd
.g3_report_pdf <- function(html, pdf) {
  cand <- Sys.which(c("chromium", "chromium-browser", "google-chrome", "google-chrome-stable"))
  cand <- cand[nzchar(cand)]
  if (!length(cand)) {
    return(list(ok = FALSE, status = "no headless Chromium or Chrome was found on PATH (needed for PDF output)."))
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    return(list(ok = FALSE, status = "PDF output needs the 'processx' package to run the browser safely."))
  }
  prof <- tempfile("g3-chrome-")
  on.exit(unlink(prof, recursive = TRUE), add = TRUE)
  args <- c("--headless", "--disable-gpu", "--no-sandbox", "--disable-background-networking",
            "--disable-extensions", "--no-first-run", paste0("--user-data-dir=", prof),
            "--no-pdf-header-footer", paste0("--print-to-pdf=", normalizePath(pdf, mustWork = FALSE)),
            paste0("file://", normalizePath(html)))
  res <- tryCatch(processx::run(cand[[1L]], args, error_on_status = FALSE, timeout = 120),
                  error = function(e) list(status = -1L, stderr = conditionMessage(e)))
  if (file.exists(pdf) && file.size(pdf) > 0) return(list(ok = TRUE, status = paste0("printed with ", basename(cand[[1L]]))))
  list(ok = FALSE, status = paste0("the browser did not produce a PDF (exit status ", res$status, ")."))
}
