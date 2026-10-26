# Model comparisons and evaluation jobs.
#
# g3_compare_models() is a thin layer over g3_compare_agents() (the paired,
# colour-swapped match protocol with truncation bounds and grouped
# bootstrap intervals): it resolves and validates each model, attaches its
# play settings (mode, simulations), and records the model identities in
# the comparison's provenance. Search and baseline agents can be mixed in
# by name. The same per-decision budget (`nodes`, `time_ms`, `depth`) is
# given to both sides; MCTS models also stop at `time_ms`, so wall-time
# matched comparisons are possible. No cross-variant rating is computed.

#' Compare models in paired games
#'
#' Validates the models for play and runs the paired match protocol of
#' [g3_compare_agents()] between them: colour-swapped pairs from each
#' starting position and seed, equal per-decision budgets, a ply cap that
#' records truncation (never a draw), completed-game scores with
#' scheduled-game bounds and grouped bootstrap intervals.
#'
#' @param models Two models: ids, `"model:<id>"` strings, `g3_model`
#'   objects, or built-in agent names (`"search"`, `"greedy"`, `"random"`).
#'   Names, if given, label the agents.
#' @param positions Starting positions (see [g3_compare_agents()]).
#' @param budget Per-decision budget for both sides (`nodes`, `time_ms`,
#'   `depth`).
#' @param seeds Integer seeds; each replicates every position.
#' @param play Play settings for model agents: `mode` (`"mcts"`,
#'   `"policy"`, `"ordering"`), `simulations`, `c_puct`, `temperature`.
#' @param ... Passed to [g3_compare_agents()] (`pairs`, `max_plies`,
#'   `bootstrap`, `stop_check`, `progress`, `keep_games`).
#' @return The `g3_comparison` from [g3_compare_agents()], with
#'   `provenance$models` listing each model's id, kind, weights digest and
#'   play settings.
#' @examples
#' \dontrun{
#' cmp <- gambitR3D::g3_compare_models(
#'   c("model:my-checkpoint", "search"), budget = base::list(nodes = 200L),
#'   seeds = 1L, play = base::list(simulations = 16L), max_plies = 40L
#' )
#' cmp$summary
#' }
#' @export
g3_compare_models <- function(models, positions = NULL, budget = list(nodes = 2000L), seeds = 1L, play = list(), ...) {
  compare <- get0("g3_compare_agents", envir = asNamespace("gambitR3D"), inherits = FALSE)
  if (is.null(compare)) .g3_abort("g3_incompatible_model", "g3_compare_agents() is not available in this build.")
  if (length(models) != 2L) .g3_abort("g3_invalid_state", "`models` must name exactly two models or agents.")
  labels <- names(models)
  models <- if (is.list(models) && !inherits(models, "g3_model")) models else as.list(models)
  agents <- list()
  info <- list()
  for (i in seq_along(models)) {
    a <- models[[i]]
    if (is.character(a) && a %in% .G3_BUILTIN_AGENTS) {
      agents[[i]] <- a
      info[[i]] <- list(agent = a, kind = "builtin_agent")
      next
    }
    m <- .g3_model_resolve(a)
    if (!identical(m$manifest$kind, "search")) {
      s <- .g3_model_play_settings(m, play)
      g3_validate_model(m, list(task = c("play", .g3_mode_task(s$mode))), error = TRUE)
      m$play <- play
    }
    agents[[i]] <- m
    info[[i]] <- list(agent = paste0("model:", m$model_id), kind = m$manifest$kind,
                      weights_digest = m$manifest$weights_digest %||% NA_character_, play = play)
  }
  if (!is.null(labels) && all(nzchar(labels))) names(agents) <- labels
  out <- compare(agents, positions = positions, budget = budget, seeds = seeds, ...)
  out$provenance$models <- info
  out
}

# Held-out evaluation of a trained model on a dataset split (no gradient
# steps): policy cross-entropy, top-1 teacher agreement, WDL loss where
# verified outcomes exist.
#' @noRd
.g3_ml_evaluate_split <- function(model, dataset, split = "validation", ml_env = NULL, loss_weights = list(policy = 1, wdl = 1, aux = 0)) {
  model <- .g3_model_resolve(model)
  g3_validate_model(model, list(task = "policy"), error = TRUE)
  ds <- if (inherits(dataset, "g3_dataset")) dataset else .g3_ml_read_dataset(dataset)
  w <- .g3_model_worker(model, env_path = ml_env, extra_roots = dirname(ds$manifest_path))
  on.exit(.g3_worker_stop(w), add = TRUE)
  res <- .g3_worker_call(w, "train_chunk", list(
    dataset = list(manifest_path = ds$manifest_path, manifest_sha256 = ds$manifest_sha256, split = "train"),
    steps = 0L, batch_size = 1L, loss_weights = loss_weights, evaluate = list(split)
  ), fields = list(model_hash = w$model_hash), timeout_s = 300)
  if (!identical(res$weights_digest_after, w$model_hash)) {
    .g3_abort("g3_io_error", "Evaluation changed the weights; this must not happen.")
  }
  list(model_id = model$model_id, dataset_id = ds$dataset_id, split = split, metrics = res$evaluation[[split]])
}

# Background-job type "evaluation": a paired model comparison
# (`args$kind = "compare"`, default) or a held-out split evaluation
# (`args$kind = "held_out"`).
#' @noRd
.g3_job_evaluation <- function(args, ctx) {
  kind <- args$kind %||% "compare"
  if (identical(kind, "held_out")) {
    return(.g3_ml_evaluate_split(args$model, args$dataset, args$split %||% "validation", args$ml_env))
  }
  if (!is.null(args$ml_env)) options(gambitR3D.ml_env = args$ml_env)
  if (!is.null(args$registry_dir)) options(gambitR3D.registry_dir = args$registry_dir)
  out <- g3_compare_models(
    args$models, positions = args$positions, budget = args$budget %||% list(nodes = 2000L),
    seeds = args$seeds %||% 1L, play = args$play %||% list(), pairs = args$pairs %||% 1L,
    max_plies = args$max_plies %||% 80L, stop_check = ctx$should_stop,
    progress = function(done, total, unit) ctx$progress(done, total, unit)
  )
  if (any(out$games$status == "cancelled")) attr(out, "stop_reason") <- if (ctx$timed_out()) "timed_out" else "cancelled"
  .g3_ml_shutdown()
  out
}
