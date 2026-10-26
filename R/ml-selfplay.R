# Controlled self-play with R-owned MCTS.
#
# Every action comes from the R engine's legal set and is applied with
# g3_game_apply(), which re-validates it. The network only supplies priors
# and values through the worker. Self-play uses training mode (seeded root
# Dirichlet noise; visit-count sampling for the first `temperature_plies`
# plies, then the most visited action). Each position's policy target is
# its normalised root visit counts, or "unavailable" when no root visit
# completed. A game that reaches the ply cap is recorded with a
# `truncated` session event: it is not a draw and its positions get no
# outcome target. Games from tactical fixtures are kept in a separate
# `fixture` split and played until a true terminal state when possible.

#' @noRd
.g3_self_play_config <- function(config) {
  d <- list(model = NULL, games = 2L, max_plies = 80L, simulations = 16L, c_puct = 1.5, dirichlet_alpha = 0.3,
            dirichlet_epsilon = 0.25, temperature = 1, temperature_plies = 16L, seed = 20261026L, start = NULL,
            fixtures = c("mate_in_one", "forced_loss"), fixture_simulations = 128L, fixture_max_plies = 20L,
            threads = 1L, ml_env = NULL, limits = NULL, run_id = NULL)
  unknown <- setdiff(names(config), names(d))
  if (length(unknown)) .g3_abort("g3_budget_exceeded", paste0("Unknown self-play setting: ", unknown[1L]))
  d[names(config)] <- config
  for (k in c("games", "max_plies", "simulations", "temperature_plies", "fixture_simulations", "fixture_max_plies", "threads", "seed")) {
    d[[k]] <- as.integer(d[[k]])
  }
  d
}

#' Generate self-play games and training data
#'
#' Plays games between a trained checkpoint and itself with R-owned Monte
#' Carlo tree search (the network supplies priors and values; the R engine
#' supplies every legal action, transition and terminal result). Positions
#' become training records with normalised root-visit policy targets and,
#' for games that reach a true terminal state, outcome targets from each
#' position's side to act. Games stopped by the ply cap are recorded as
#' truncated, never as draws, and carry no outcome target. Optional
#' tactical fixture games are played separately and stored in their own
#' `fixture` split. Within the CPU smoke envelope at most 2 self-play
#' games of at most 80 plies are played unless larger `limits` are
#' authorised.
#'
#' @param config A list: `model` (required; a trained `g3_model` or id),
#'   `games` (default 2), `max_plies` (80), `simulations` per move (16),
#'   `c_puct` (1.5), `dirichlet_alpha` (0.3), `dirichlet_epsilon` (0.25),
#'   `temperature` (1) for the first `temperature_plies` (16) plies,
#'   `seed`, `start` (`NULL` for the initial position, or a `g3_state`),
#'   `fixtures` (fixture names, default `c("mate_in_one", "forced_loss")`;
#'   `character()` for none), `fixture_simulations` (128),
#'   `fixture_max_plies` (20), `threads`, `ml_env`, `limits`, `run_id`.
#' @param output_dir Directory for `games/` (native game files), `data/`
#'   (a dataset with `train` and `fixture` splits) and `run.json`.
#' @return A `g3_run_manifest` list with `run_id`, `kind = "self_play"`,
#'   `status`, `stop_reason`, `config`, `model` (id, weights digest),
#'   `games` (data frame: `game_id`, `kind`, `start`, `plies`, `status`
#'   (`"terminal"` or `"truncated"`), `result`, `termination`,
#'   `simulations`, `evaluations`, `seconds`, `ms_per_move`, `file`),
#'   `samples` (counts by split and outcome status), `dataset` (manifest
#'   path and hash), `inference` (evaluations and mean milliseconds),
#'   `truncated_games_are_draws` (`FALSE`), `environment`, `timings` and
#'   `limitations`.
#' @examples
#' \dontrun{
#' sp <- gambitR3D::g3_self_play(
#'   base::list(model = "model:my-checkpoint", games = 1L, max_plies = 20L, simulations = 8L),
#'   output_dir = base::file.path(base::tempdir(), "g3-selfplay")
#' )
#' sp$games
#' }
#' @export
g3_self_play <- function(config, output_dir) {
  .g3_self_play_impl(config, output_dir, ctx = NULL)
}

# A dedicated worker with a model checkpoint loaded (used by batch work).
#' @noRd
.g3_model_worker <- function(model, threads = 1L, env_path = NULL, extra_roots = character(), log_path = NULL,
                             job_id = NULL, attempt_id = NULL) {
  env_path <- .g3_ml_env_path(env_path)
  w_art <- .g3_artifact(model, "weights")
  c_art <- .g3_artifact(model, "checkpoint_meta")
  ck <- dirname(.g3_artifact_path(model, w_art))
  w <- .g3_worker_start(env_path, threads = threads, allowed_roots = c(ck, extra_roots), log_path = log_path,
                        job_id = job_id, attempt_id = attempt_id)
  res <- tryCatch(
    .g3_worker_call(w, "load_checkpoint", list(path = ck, with_optimizer = FALSE,
                                               expected = list(weights.safetensors = w_art$sha256, checkpoint.json = c_art$sha256)),
                    timeout_s = 180),
    error = function(e) {
      .g3_worker_kill(w)
      stop(e)
    }
  )
  if (!identical(res$weights_digest, model$manifest$weights_digest)) {
    .g3_worker_abort(w, "g3_incompatible_model", "Loaded weights digest differs from the model manifest.")
  }
  w$model_hash <- res$weights_digest
  w$checkpoint <- paste(ck, w_art$sha256, sep = "|")
  w$model_id <- model$model_id
  w
}

#' @noRd
.g3_self_play_impl <- function(config, output_dir, ctx = NULL) {
  t_start <- proc.time()[["elapsed"]]
  cfg <- .g3_self_play_config(config)
  lim <- .g3_ml_limits(cfg$limits)
  max_games <- if (isTRUE(lim$authorized)) Inf else 2L
  max_plies <- if (isTRUE(lim$authorized)) Inf else 80L
  if (is.null(cfg$model)) .g3_abort("g3_incompatible_model", "Self-play needs a trained model (config$model).")
  if (cfg$games < 0L || cfg$games > max_games) .g3_abort("g3_budget_exceeded", "At most 2 self-play games in the CPU smoke envelope.")
  if (cfg$max_plies < 1L || cfg$max_plies > max_plies) .g3_abort("g3_budget_exceeded", "max_plies must be 1..80 in the CPU smoke envelope.")
  if (cfg$threads < 1L || cfg$threads > lim$max_threads) .g3_abort("g3_budget_exceeded", "Too many threads.")
  model <- .g3_model_resolve(cfg$model)
  g3_validate_model(model, list(task = c("self_play_training", "mcts")), error = TRUE)
  if (missing(output_dir) || !is.character(output_dir) || !nzchar(output_dir)) .g3_abort("g3_io_error", "`output_dir` is required.")
  dir.create(file.path(output_dir, "games"), recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir)
  run_id <- cfg$run_id %||% .g3_new_id("selfplay")
  should_stop <- function() {
    (!is.null(ctx) && isTRUE(ctx$should_stop())) || (proc.time()[["elapsed"]] - t_start > lim$max_wall_seconds)
  }
  w <- .g3_model_worker(model, threads = cfg$threads, env_path = cfg$ml_env,
                        log_path = file.path(output_dir, "worker.log"), job_id = run_id,
                        attempt_id = if (is.null(ctx)) paste0("attempt-", Sys.getpid()) else paste0("attempt-", ctx$attempt))
  on.exit(.g3_worker_stop(w), add = TRUE)
  tabs <- .g3_tables()
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  inf_ms <- 0
  evaluator0 <- .g3_mcts_model_evaluator(w, w$model_hash, tabs, counter)
  evaluator <- function(nodes) {
    t <- proc.time()[["elapsed"]]
    out <- evaluator0(nodes)
    inf_ms <<- inf_ms + (proc.time()[["elapsed"]] - t) * 1000
    out
  }
  fixtures <- .g3_ml_fixtures()
  bad_fx <- setdiff(cfg$fixtures, names(fixtures))
  if (length(bad_fx)) .g3_abort("g3_invalid_state", paste0("Unknown fixture: ", bad_fx[1L]))
  plan <- c(
    lapply(seq_len(cfg$games), function(g) list(kind = "self_play", start = "initial",
                                                state = cfg$start %||% g3_new_game()$state, sims = cfg$simulations,
                                                max_plies = cfg$max_plies, split = "train",
                                                game_id = sprintf("%s-sp%02d", run_id, g))),
    lapply(cfg$fixtures, function(nm) list(kind = "fixture", start = nm, state = fixtures[[nm]]$state,
                                           sims = cfg$fixture_simulations, max_plies = cfg$fixture_max_plies,
                                           split = "fixture", game_id = sprintf("%s-fx-%s", run_id, nm)))
  )
  recs <- list()
  rows <- list()
  games_meta <- list()
  stop_reason <- "completed"
  for (gi in seq_along(plan)) {
    p <- plan[[gi]]
    if (should_stop()) {
      stop_reason <- if (!is.null(ctx) && isTRUE(ctx$cancelled())) "cancelled" else "timed_out"
      break
    }
    tg <- proc.time()[["elapsed"]]
    game <- .g3_game_new(p$state, players = list(white = model$model_id, black = model$model_id), game_id = p$game_id)
    grec <- list()
    sims <- 0L
    ev0 <- counter$calls
    k <- 0L
    cut <- NA_character_
    repeat {
      st <- game$state
      if (identical(st$outcome$status, "terminal")) break
      if (k >= p$max_plies) {
        cut <- "max_plies"
        break
      }
      if (should_stop()) {
        cut <- "stopped"
        break
      }
      core <- .g3_core(st, tabs)
      temp <- if (p$kind == "self_play" && k < cfg$temperature_plies) cfg$temperature else 0
      m <- .g3_mcts(core, evaluator, list(simulations = p$sims, c_puct = cfg$c_puct, training = TRUE,
                                          dirichlet_alpha = cfg$dirichlet_alpha, dirichlet_epsilon = cfg$dirichlet_epsilon,
                                          temperature = temp, seed = cfg$seed + gi * 100003L + k),
                    tabs, should_stop = should_stop)
      sims <- sims + m$simulations
      vis <- m$root$visits > 0L
      grec[[length(grec) + 1L]] <- .g3_ml_record(
        core, tabs, p$game_id, k, if (p$kind == "fixture") "fixture_game" else "self_play", p$split,
        if (m$policy_target_available) m$root$action_id[vis] else character(),
        if (m$policy_target_available) m$root$policy_target[vis] else numeric(),
        if (m$policy_target_available) "mcts_visits" else "unavailable", edited = p$kind == "fixture"
      )
      game <- g3_game_apply(game, m$action_id)
      k <- k + 1L
    }
    truncated <- !is.na(cut)
    if (truncated) game <- g3_game_event(game, "truncated", reason = cut)
    fcore <- .g3_core(game$state, tabs)
    grec <- .g3_ml_label_outcome(grec, fcore, game$state$outcome, truncated)
    recs <- c(recs, grec)
    gfile <- file.path(output_dir, "games", paste0(p$game_id, ".json"))
    g3_write_game(game, gfile, overwrite = TRUE)
    secs <- proc.time()[["elapsed"]] - tg
    rows[[gi]] <- data.frame(
      game_id = p$game_id, kind = p$kind, start = p$start, plies = k,
      status = if (truncated) "truncated" else "terminal",
      result = if (truncated) NA_character_ else game$state$outcome$result,
      termination = if (truncated) cut else game$state$outcome$reason,
      simulations = sims, evaluations = counter$calls - ev0, seconds = secs,
      ms_per_move = if (k) secs * 1000 / k else NA_real_, file = gfile, stringsAsFactors = FALSE
    )
    games_meta[[p$game_id]] <- list(game_id = p$game_id, source = if (p$kind == "fixture") "fixture_game" else "self_play",
                                    split = p$split, plies = k, status = if (truncated) "truncated" else "terminal",
                                    result = if (truncated) NA_character_ else game$state$outcome$result,
                                    reason = if (truncated) cut else game$state$outcome$reason, note = p$start)
    if (!is.null(ctx)) ctx$progress(gi, length(plan), "games")
    if (identical(cut, "stopped")) {
      stop_reason <- if (!is.null(ctx) && isTRUE(ctx$cancelled())) "cancelled" else "timed_out"
      break
    }
  }
  games <- if (length(rows)) do.call(rbind, rows) else data.frame()
  ds <- NULL
  if (length(recs)) {
    ds <- .g3_ml_write_dataset(recs, games_meta, file.path(output_dir, "data"),
                               spec = cfg[setdiff(names(cfg), c("model", "start", "ml_env"))], generator = "self_play",
                               extra = list(model = list(model_id = model$model_id, weights_digest = model$manifest$weights_digest),
                                            mcts = list(version = .G3_MCTS_VERSION, c_puct = cfg$c_puct,
                                                        dirichlet_alpha = cfg$dirichlet_alpha,
                                                        dirichlet_epsilon = cfg$dirichlet_epsilon)))
  }
  ost <- vapply(recs, function(r) r$meta$outcome_status, "")
  spl <- vapply(recs, function(r) r$meta$split, "")
  run <- structure(list(
    run_id = run_id, kind = "self_play", status = if (stop_reason == "completed") "completed" else stop_reason,
    stop_reason = stop_reason, config = cfg[setdiff(names(cfg), c("model", "start", "ml_env"))],
    model = list(model_id = model$model_id, weights_digest = model$manifest$weights_digest),
    games = games,
    samples = list(total = length(recs), by_split = as.list(table(spl)), by_outcome_status = as.list(table(ost)),
                   policy_targets = sum(vapply(recs, function(r) isTRUE(r$meta$policy_available), TRUE))),
    dataset = if (is.null(ds)) NULL else list(dataset_id = ds$dataset_id, manifest = ds$manifest_path, manifest_sha256 = ds$manifest_sha256),
    inference = list(evaluations = counter$calls, mean_ms = if (counter$calls) inf_ms / counter$calls else NA_real_),
    truncated_games_are_draws = FALSE,
    environment = list(python = w$hello$python, torch = w$hello$torch, device = w$hello$device, threads = w$hello$threads),
    timings = list(total_seconds = proc.time()[["elapsed"]] - t_start),
    limitations = c("Self-play with a briefly trained network and few simulations produces low-quality games.",
                    "Truncated games carry no outcome target; only fixture games reach true terminal states reliably.")
  ), class = "g3_run_manifest")
  r <- unclass(run)
  r$games <- .g3_rows(games)
  .g3_atomic_write(jsonlite::toJSON(r, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null"),
                   file.path(output_dir, "run.json"))
  if (stop_reason != "completed") attr(run, "stop_reason") <- stop_reason
  run
}

# Background-job type "self_play".
#' @noRd
.g3_job_self_play <- function(args, ctx) {
  .g3_self_play_impl(args$config, args$output_dir, ctx = ctx)
}
