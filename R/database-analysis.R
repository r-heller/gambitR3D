# Game database: analysis runs and position evaluations (internal).
#
# Analysis code registers a run (evaluator, model revision, settings hash,
# rules hash, status) and its per-position results in one transaction, so a
# run is either fully recorded or absent. Scores keep their perspective and
# kind; a missing score is NULL with a reason, never a zero. Win/draw/loss
# triples must sum to one (checked by the schema). Results are keyed by
# `analysis_context_hash`, which must already include the rules hash,
# evaluator/model revision, search settings and draw/repetition context
# (see docs: a repetition key alone is not a valid cache key).

.G3_EVAL_COLUMNS <- c(
  game_id = "character", event_seq = "integer", ply = "integer", state_hash = "character",
  analysis_context_hash = "character", perspective = "character", score = "numeric",
  score_kind = "character", mate_in = "integer", p_win = "numeric", p_draw = "numeric", p_loss = "numeric",
  best_action_id = "character", played_action_id = "character", depth = "integer", nodes = "integer",
  completeness = "character", na_reason = "character"
)

# Record one analysis run and its evaluations atomically. `run` is a list
# with run_id, kind, evaluator, settings (list), status and optional
# model_revision, started_at, finished_at, provenance. Returns the run id.
#' @noRd
.g3_db_add_analysis <- function(db, run, evaluations) {
  .g3_db_con(db, write = TRUE)
  run_id <- .g3_db_valid_id(run$run_id)
  if (is.na(run_id)) .g3_abort("g3_io_error", "`run$run_id` must be a valid identifier.")
  ev <- .g3_db_typed(as.data.frame(evaluations, stringsAsFactors = FALSE), .G3_EVAL_COLUMNS)
  if (any(!is.finite(ev$score) & !is.na(ev$score))) {
    .g3_abort("g3_io_error", "Evaluation scores must be finite or NA (use mate_in for mates).")
  }
  if (any(is.na(ev$score) & is.na(ev$na_reason) & is.na(ev$p_win))) {
    .g3_abort("g3_io_error", "A missing score needs an `na_reason`.")
  }
  settings <- run$settings %||% list()
  tabs <- .g3_tables()
  now <- .g3_now()
  .g3_db_write(db, function(con) {
    .g3_db_register_rules(con, tabs$rules_hash, tabs$rules_version)
    .g3_db_exec(con, paste(
      "INSERT INTO analysis_runs (run_id, kind, evaluator, model_revision, rules_hash, settings_json, settings_hash,",
      "status, started_at, finished_at, package_version, provenance_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ), list(run_id, .g3_chr1(run$kind), .g3_chr1(run$evaluator), .g3_chr1(run$model_revision), tabs$rules_hash,
            .g3_canonical_json(settings), .g3_hash_object(settings), .g3_chr1(run$status) %|na|% "completed",
            .g3_chr1(run$started_at) %|na|% now, .g3_chr1(run$finished_at), .g3_pkg_version(),
            if (is.null(run$provenance)) NA_character_ else .g3_canonical_json(run$provenance)))
    if (nrow(ev)) {
      .g3_db_exec(con, paste(
        "INSERT INTO evaluations (run_id, game_id, event_seq, ply, state_hash, analysis_context_hash, perspective,",
        "score, score_kind, mate_in, p_win, p_draw, p_loss, best_action_id, played_action_id, depth, nodes,",
        "completeness, na_reason) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ), c(list(rep(run_id, nrow(ev))), unname(as.list(ev))))
    }
  }, what = "analysis")
  invisible(run_id)
}

# Stored evaluations, optionally for one game or run.
#' @noRd
.g3_db_evaluations <- function(db, game_id = NULL, run_id = NULL) {
  where <- character()
  params <- list()
  if (!is.null(game_id)) {
    where <- c(where, "e.game_id = ?")
    params <- c(params, list(game_id))
  }
  if (!is.null(run_id)) {
    where <- c(where, "e.run_id = ?")
    params <- c(params, list(run_id))
  }
  rows <- .g3_db_get(db, paste(
    "SELECT e.run_id, r.evaluator, r.model_revision, r.settings_hash, e.* FROM evaluations e",
    "JOIN analysis_runs r ON r.run_id = e.run_id",
    if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else "",
    "ORDER BY e.run_id, e.game_id, e.event_seq, e.evaluation_id"
  ), params)
  .g3_db_typed(rows, c(run_id = "character", evaluator = "character", model_revision = "character",
                       settings_hash = "character", .G3_EVAL_COLUMNS))
}
