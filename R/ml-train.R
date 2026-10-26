# Bounded training of the square transformer through the tensor worker.
#
# R owns the run: it prepares or reads the dataset, starts a worker with
# explicit allowed directories, initialises (or resumes) the model, sends
# train_chunk requests, records every step's metrics, and finally asks the
# worker to save an atomic checkpoint (temporary directory, flushed files,
# hashes, rename). R then re-hashes the files and recomputes the weights
# digest itself before writing the model manifest `model.json` next to the
# artifacts; a crash before that leaves an unregistered orphan, never a
# ready model. Only then is the checkpoint registered.
#
# Resource envelope: the CPU smoke limits of the project (at most 20
# optimizer steps per run, 512 positions, 600 s, 2 CPU threads, 100 MB of
# output). Larger runs need an explicit `limits` list with
# `authorized = TRUE`; large training is never started implicitly.

.G3_ML_SMOKE_LIMITS <- list(max_steps = 20L, max_positions = 512L, max_wall_seconds = 600, max_threads = 2L,
                            max_output_mb = 100, authorized = FALSE)
.G3_TRAIN_FORMAT <- "g3-training-run/1"

#' @noRd
.g3_ml_limits <- function(limits = NULL) {
  l <- .G3_ML_SMOKE_LIMITS
  if (!is.null(limits)) {
    unknown <- setdiff(names(limits), names(l))
    if (length(unknown)) .g3_abort("g3_budget_exceeded", paste0("Unknown limit: ", unknown[1L]))
    raised <- vapply(setdiff(names(limits), "authorized"), function(k) isTRUE(limits[[k]] > l[[k]]), TRUE)
    if (any(raised) && !isTRUE(limits$authorized)) {
      .g3_abort("g3_budget_exceeded", paste0("Raising ", paste(names(raised)[raised], collapse = ", "),
                                             " beyond the CPU smoke envelope needs limits$authorized = TRUE."))
    }
    l[names(limits)] <- limits
  }
  l
}

#' @noRd
.g3_train_config <- function(config) {
  d <- list(
    profile = "tiny", model_config = NULL, seed = 20261026L, steps = 20L, batch_size = 32L,
    optimizer = list(lr = 2e-3, weight_decay = 0.01, betas = c(0.9, 0.999), eps = 1e-8),
    schedule = list(kind = "cosine", warmup_steps = 2L, min_lr_ratio = 0.1),
    loss_weights = list(policy = 1, wdl = 1, aux = 0.25), clip_norm = 1, threads = 1L, chunk_steps = 5L,
    evaluate = "validation", resume_from = NULL, ml_env = NULL, model_id = NULL, display_name = NULL,
    register = TRUE, registry_dir = NULL, limits = NULL, run_id = NULL, max_steps_this_run = NULL
  )
  unknown <- setdiff(names(config), names(d))
  if (length(unknown)) .g3_abort("g3_budget_exceeded", paste0("Unknown training setting: ", unknown[1L]))
  d[names(config)] <- config
  d$steps <- as.integer(d$steps)
  d$batch_size <- as.integer(d$batch_size)
  d$threads <- as.integer(d$threads)
  d$chunk_steps <- max(1L, as.integer(d$chunk_steps))
  d$seed <- as.integer(d$seed)
  if (!d$profile %in% c("tiny", "research", "custom")) .g3_abort("g3_budget_exceeded", "profile must be tiny, research or custom.")
  d$schedule$total_steps <- d$steps
  d
}

#' Train a square-transformer checkpoint
#'
#' Runs a bounded, reproducible training run of the built-in square-token
#' transformer in the configured ML worker (see [g3_setup_ml()]). Policy
#' cross-entropy covers the complete legal-action set; the WDL loss uses
#' only verified terminal outcomes (truncated games are masked); an
#' optional, separately named auxiliary head learns normalised search
#' scores. Every step's loss, gradient norm and learning rate are
#' recorded; the run fails on a non-finite loss or gradient. The final
#' checkpoint (safetensors weights and optimizer state, JSON metadata with
#' RNG state and data cursor) is written atomically, verified in R and
#' registered with a manifest whose status is `"pipeline-smoke, no strength
#' claim"`. Runs are capped by the CPU smoke envelope (20 optimizer steps,
#' 512 positions, 600 s, 2 threads, 100 MB) unless larger `limits` are
#' explicitly authorised.
#'
#' A cancelled job or an exceeded time limit is noticed between optimizer
#' chunks and, while training data are generated, before every position's
#' teacher search. A stop during data generation ends the run cleanly with
#' status `"cancelled"` or `"timed_out"`: no worker is started, nothing of
#' the partial dataset is kept and no checkpoint is written (`model`,
#' `checkpoint` are `NULL`); resuming regenerates the same seeded data.
#'
#' @param config A list; all fields optional: `profile` (`"tiny"`,
#'   `"research"`), `model_config` (width, layers, heads, mlp_ratio,
#'   dropout overrides), `seed`, `steps` (total optimizer steps of the run,
#'   counting resumed ones), `batch_size`, `optimizer` (`lr`,
#'   `weight_decay`, `betas`, `eps`; AdamW), `schedule` (`kind` `"cosine"`
#'   or `"constant"`, `warmup_steps`, `min_lr_ratio`), `loss_weights`
#'   (`policy`, `wdl`, `aux`), `clip_norm`, `threads`, `chunk_steps`,
#'   `evaluate` (splits to evaluate at the end), `resume_from` (a checkpoint
#'   directory), `ml_env`, `model_id`, `display_name`, `register`,
#'   `registry_dir`, `limits`, `run_id`, `max_steps_this_run` (stop this
#'   invocation cleanly after that many steps, leaving a resumable
#'   checkpoint; not part of the configuration hash).
#' @param dataset A dataset directory or `dataset.json` path (as written by
#'   [g3_self_play()] or a previous run), or a list of generation settings
#'   (`games`, `positions`, `max_walk_plies`, `teacher`, `fixtures`,
#'   `platform_positions`, `seed`, ...) to generate teacher, fixture and
#'   platform-curriculum data under `output_dir/data`.
#' @param output_dir Run directory (created). Checkpoints go to
#'   `output_dir/checkpoints/ckpt-<step>`.
#' @return A `g3_run_manifest` list: `run_id`, `kind` (`"training"`),
#'   `status` (`"completed"`, `"stopped"` at `max_steps_this_run`,
#'   `"cancelled"`, `"timed_out"`), `stop_reason`,
#'   `config`, `config_hash`, `dataset` (id, manifest path and hash, split
#'   hash, counts), `model` (profile, configuration, parameter count,
#'   initial and final weights digests, `weights_changed`), `steps`
#'   (`start`, `end`, `completed`), `metrics` (data frame, one row per
#'   step: `step`, `loss`, `policy_loss`, `wdl_loss`, `aux_loss`,
#'   `grad_norm`, `lr`, `n_policy`, `n_wdl`, `n_aux`, `policy_entropy`,
#'   `top1_agreement`, `params_with_gradient`, `seconds`), `checks`
#'   (finite losses, finite nonzero gradients, weights changed),
#'   `evaluation`, `checkpoint` (path, file hashes, weights digest,
#'   manifest path), `model_id` (registered id or `NA`),
#'   `registered_model` (the `g3_model` descriptor or `NULL`), `environment`,
#'   `timings`, `output_mb`, `limits` and `limitations`.
#' @examples
#' \dontrun{
#' # Needs g3_setup_ml(); a few seconds on one CPU thread.
#' run <- gambitR3D::g3_train_model(
#'   base::list(profile = "tiny", steps = 10L),
#'   dataset = base::list(games = 4L, positions = 40L),
#'   output_dir = base::file.path(base::tempdir(), "g3-run")
#' )
#' run$metrics[, c("step", "loss", "grad_norm")]
#' }
#' @export
g3_train_model <- function(config = list(), dataset = list(), output_dir) {
  .g3_train_impl(config, dataset, output_dir, ctx = NULL)
}

#' @noRd
.g3_train_impl <- function(config, dataset, output_dir, ctx = NULL) {
  t_start <- proc.time()[["elapsed"]]
  cfg <- .g3_train_config(config)
  lim <- .g3_ml_limits(cfg$limits)
  if (cfg$steps < 0L || cfg$steps > lim$max_steps) {
    .g3_abort("g3_budget_exceeded", paste0("steps must be between 0 and ", lim$max_steps, " (smoke envelope)."))
  }
  if (cfg$threads < 1L || cfg$threads > lim$max_threads) .g3_abort("g3_budget_exceeded", paste0("threads must be 1..", lim$max_threads, "."))
  env_path <- .g3_ml_env_path(cfg$ml_env)
  if (missing(output_dir) || !is.character(output_dir) || !nzchar(output_dir)) .g3_abort("g3_io_error", "`output_dir` is required.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir)
  should_stop <- function() {
    (!is.null(ctx) && isTRUE(ctx$should_stop())) || (proc.time()[["elapsed"]] - t_start > lim$max_wall_seconds)
  }
  stop_reason_now <- function() if (!is.null(ctx) && isTRUE(ctx$cancelled())) "cancelled" else "timed_out"
  # Resume point: explicit, or the latest checkpoint of this run directory.
  resume <- cfg$resume_from
  if (is.null(resume) && !is.null(ctx) && isTRUE(ctx$resume)) resume <- .g3_latest_checkpoint(output_dir)
  run_id <- cfg$run_id %||% (if (file.exists(file.path(output_dir, "run.json"))) {
    tryCatch(jsonlite::fromJSON(file.path(output_dir, "run.json"))$run_id, error = function(e) NULL)
  }) %||% .g3_new_id("run")
  # Identity and invocation settings do not enter the configuration hash,
  # so an interrupted run can be resumed under the same hash.
  cfg_public <- cfg[setdiff(names(cfg), c("ml_env", "registry_dir", "resume_from", "register", "run_id",
                                          "max_steps_this_run", "model_id", "display_name"))]
  config_hash <- .g3_hash_object(cfg_public)
  # Check the resume checkpoint (record, configuration, weights) before any
  # worker starts.
  rc <- if (!is.null(resume)) .g3_resume_checkpoint(resume, config_hash) else NULL
  # Dataset.
  t_data0 <- proc.time()[["elapsed"]]
  ds <- if (inherits(dataset, "g3_dataset")) {
    .g3_ml_read_dataset(dataset$manifest_path)
  } else if (is.character(dataset)) {
    .g3_ml_read_dataset(dataset)
  } else if (is.list(dataset)) {
    ddir <- file.path(output_dir, "data")
    if (file.exists(file.path(ddir, "dataset.json"))) .g3_ml_read_dataset(ddir) else
      .g3_ml_build_dataset(utils::modifyList(list(max_positions = lim$max_positions), dataset), ddir, should_stop = should_stop)
  } else {
    .g3_abort("g3_io_error", "`dataset` must be a dataset path or a list of generation settings.")
  }
  t_data <- proc.time()[["elapsed"]] - t_data0
  if (inherits(ds, "g3_dataset_stopped")) {
    # Stopped while generating data (polled per position): end cleanly
    # without a worker or checkpoint; a resumed run regenerates the data.
    return(.g3_train_stopped_run(stop_reason_now(), run_id, cfg_public, config_hash, lim, resume, rc, ds,
                                 output_dir, t_start, t_data))
  }
  if (ds$counts$records > lim$max_positions) {
    .g3_abort("g3_budget_exceeded", paste0("The dataset has ", ds$counts$records, " positions; the limit is ", lim$max_positions, "."))
  }
  if (!any(vapply(ds$shards, function(s) identical(s$split, "train"), TRUE))) {
    .g3_abort("g3_io_error", "The dataset has no train split.")
  }
  w <- .g3_worker_start(env_path, threads = cfg$threads,
                        allowed_roots = c(output_dir, dirname(ds$manifest_path), if (!is.null(resume)) dirname(resume)),
                        log_path = file.path(output_dir, sprintf("worker-%s.log", format(Sys.time(), "%Y%m%d-%H%M%OS3"))),
                        job_id = run_id, attempt_id = if (is.null(ctx)) paste0("attempt-", Sys.getpid()) else paste0("attempt-", ctx$attempt))
  on.exit(.g3_worker_stop(w), add = TRUE)
  t_worker <- proc.time()[["elapsed"]] - t_start - t_data
  if (!is.null(resume)) {
    init <- .g3_worker_call(w, "load_checkpoint", list(path = resume, expected = rc$expected, with_optimizer = TRUE),
                            timeout_s = 300)
  } else {
    mc <- c(list(profile = cfg$profile), cfg$model_config %||% list())
    init <- .g3_worker_call(w, "initialize_model", list(model_config = mc, seed = cfg$seed), timeout_s = 300)
  }
  w$model_hash <- init$weights_digest
  digest0 <- init$weights_digest
  step0 <- as.integer(init$step)
  metrics <- list()
  mlog <- file.path(output_dir, "metrics.jsonl")
  stop_reason <- "completed"
  evaluation <- NULL
  remaining <- cfg$steps - step0
  if (remaining < 0L) .g3_abort("g3_budget_exceeded", "The checkpoint is already past the requested number of steps.")
  t_train0 <- proc.time()[["elapsed"]]
  positions_seen <- 0
  # An optional per-invocation step budget stops cleanly at a checkpoint.
  this_run <- if (is.null(cfg$max_steps_this_run)) remaining else min(remaining, as.integer(cfg$max_steps_this_run))
  budget_stop <- this_run < remaining
  remaining <- this_run
  repeat {
    last <- remaining <= cfg$chunk_steps
    n <- min(cfg$chunk_steps, remaining)
    if (n == 0L && !last) break
    if (should_stop()) {
      stop_reason <- stop_reason_now()
      break
    }
    payload <- list(
      dataset = list(manifest_path = ds$manifest_path, manifest_sha256 = ds$manifest_sha256, split = "train"),
      steps = n, batch_size = cfg$batch_size, optimizer = cfg$optimizer, schedule = cfg$schedule,
      loss_weights = cfg$loss_weights, clip_norm = cfg$clip_norm, seed = cfg$seed,
      evaluate = if (last) as.list(intersect(cfg$evaluate, vapply(ds$shards, function(s) s$split, ""))) else list()
    )
    deadline <- as.numeric(Sys.time()) + max(1, lim$max_wall_seconds - (proc.time()[["elapsed"]] - t_start))
    res <- .g3_worker_call(
      w, "train_chunk", payload, fields = list(model_hash = w$model_hash, deadline = deadline,
                                               budget = list(steps = n, max_wall_seconds = lim$max_wall_seconds)),
      timeout_s = lim$max_wall_seconds + 30, should_stop = should_stop,
      on_progress = function(p) {
        m <- p$metrics
        cat(as.character(jsonlite::toJSON(m, auto_unbox = TRUE, digits = NA, null = "null")), "\n", sep = "", file = mlog, append = TRUE)
        if (!is.null(ctx)) ctx$progress(m$step, cfg$steps, "optimizer_steps")
      }
    )
    if (isTRUE(attr(res, "killed"))) {
      .g3_abort("g3_io_error", "The worker had to be terminated during training; the last complete checkpoint is kept.")
    }
    metrics <- c(metrics, res$metrics)
    for (m in res$metrics) positions_seen <- positions_seen + (m$n_positions %||% 0)
    w$model_hash <- res$weights_digest_after
    remaining <- remaining - length(res$metrics)
    if (isTRUE(attr(res, "cancelled")) || identical(res$stop_reason, "cancelled")) {
      stop_reason <- stop_reason_now()
      break
    }
    if (identical(res$stop_reason, "deadline")) {
      stop_reason <- "timed_out"
      break
    }
    if (last) {
      evaluation <- res$evaluation
      if (budget_stop) stop_reason <- "step_budget"
      break
    }
  }
  t_train <- proc.time()[["elapsed"]] - t_train0
  # Atomic checkpoint, then independent verification in R, then manifest.
  ck_parent <- file.path(output_dir, "checkpoints")
  dir.create(ck_parent, showWarnings = FALSE)
  final_step <- if (length(metrics)) as.integer(metrics[[length(metrics)]]$step) else step0
  name <- sprintf("ckpt-%06d", final_step)
  if (dir.exists(file.path(ck_parent, name))) name <- sprintf("ckpt-%06d-%s", final_step, substr(.g3_new_id("x"), 3L, 8L))
  saved <- .g3_worker_call(w, "save_checkpoint", list(parent = ck_parent, name = name,
                                                      extra = list(run_id = run_id, config_hash = config_hash)),
                           fields = list(model_hash = w$model_hash), timeout_s = 300)
  ck <- saved$path
  for (f in names(saved$files)) {
    if (!identical(.g3_file_sha256(file.path(ck, f)), saved$files[[f]])) {
      .g3_abort("g3_io_error", paste0("Checkpoint file ", f, " does not match the hash reported by the worker."))
    }
  }
  if (!identical(.g3_weights_digest(file.path(ck, "weights.safetensors")), saved$weights_digest)) {
    .g3_abort("g3_io_error", "The weights digest recomputed in R differs from the worker's.")
  }
  mdf <- .g3_metrics_df(metrics)
  checks <- list(
    losses_finite = nrow(mdf) > 0L && all(is.finite(mdf$loss)),
    gradients_finite_nonzero = nrow(mdf) > 0L && all(is.finite(mdf$grad_norm)) && all(mdf$grad_norm > 0),
    weights_changed = !identical(digest0, saved$weights_digest),
    steps_completed = nrow(mdf)
  )
  desc_cfg <- init$model_config
  manifest <- .g3_checkpoint_manifest(
    model_id = cfg$model_id %||% paste0("square-transformer-", cfg$profile, "-", substr(saved$weights_digest, 1L, 12L)),
    display_name = cfg$display_name, ck = ck, saved = saved, model_config = desc_cfg,
    parameter_count = init$parameter_count, run_id = run_id, seed = cfg$seed, ds = ds, cfg = cfg_public,
    config_hash = config_hash, mdf = mdf, evaluation = evaluation, parent = if (!is.null(resume)) resume else NULL
  )
  mpath <- file.path(ck, "model.json")
  .g3_atomic_write(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null"), mpath)
  model_id <- NA_character_
  reg <- NULL
  if (isTRUE(cfg$register) && stop_reason == "completed") {
    reg <- g3_register_model(mpath, cfg$registry_dir)
    model_id <- reg$model_id
  }
  out_mb <- sum(file.size(list.files(output_dir, recursive = TRUE, full.names = TRUE)), na.rm = TRUE) / 1024^2
  run <- structure(list(
    format = .G3_TRAIN_FORMAT, run_id = run_id, kind = "training",
    status = if (stop_reason == "completed") "completed" else if (stop_reason == "step_budget") "stopped" else stop_reason,
    stop_reason = stop_reason,
    config = cfg_public, config_hash = config_hash,
    dataset = list(dataset_id = ds$dataset_id, manifest = ds$manifest_path, manifest_sha256 = ds$manifest_sha256,
                   split_hash = ds$split_hash, counts = ds$counts),
    model = list(profile = cfg$profile, config = desc_cfg, parameter_count = init$parameter_count,
                 parameter_groups = init$parameter_groups, weights_digest_initial = digest0,
                 weights_digest_final = saved$weights_digest, weights_changed = checks$weights_changed),
    steps = list(start = step0, end = final_step, completed = nrow(mdf)),
    resumed_from = resume,
    metrics = mdf, checks = checks, evaluation = evaluation,
    checkpoint = list(path = ck, files = saved$files, weights_digest = saved$weights_digest, manifest = mpath),
    model_id = model_id, registered_model = reg,
    environment = list(python = w$hello$python, torch = w$hello$torch, numpy = w$hello$numpy,
                       safetensors = w$hello$safetensors, device = w$hello$device, threads = w$hello$threads,
                       deterministic = w$hello$deterministic_algorithms, worker_log = w$log),
    timings = list(total_seconds = proc.time()[["elapsed"]] - t_start, dataset_seconds = t_data,
                   worker_start_seconds = t_worker,
                   train_seconds = t_train, positions_per_second = if (t_train > 0) positions_seen / t_train else NA_real_),
    output_mb = out_mb, limits = lim,
    limitations = c("A bounded CPU run validates the training pipeline; it is not evidence of playing strength.",
                    "WDL targets exist only for the few verified terminal fixture positions.")
  ), class = "g3_run_manifest")
  if (out_mb > lim$max_output_mb) .g3_warn(paste0("Run output is ", round(out_mb, 1), " MB, above the ", lim$max_output_mb, " MB envelope."))
  .g3_write_run(run, output_dir)
  if (stop_reason != "completed") attr(run, "stop_reason") <- stop_reason
  run
}

# Run record of a training run stopped while its dataset was being
# generated: no worker was started and no checkpoint written. Written to
# run.json (so a resumed attempt keeps the run id) and returned with the
# stop reason as attribute, as for runs stopped during training.
#' @noRd
.g3_train_stopped_run <- function(stop_reason, run_id, cfg_public, config_hash, lim, resume, rc, ds, output_dir,
                                  t_start, t_data) {
  step <- as.integer(rc$record$step %||% 0L)
  out_mb <- sum(file.size(list.files(output_dir, recursive = TRUE, full.names = TRUE)), na.rm = TRUE) / 1024^2
  run <- structure(list(
    format = .G3_TRAIN_FORMAT, run_id = run_id, kind = "training", status = stop_reason, stop_reason = stop_reason,
    config = cfg_public, config_hash = config_hash,
    dataset = list(status = "incomplete", records_generated = ds$records_generated,
                   note = "Data generation was stopped; nothing was written and a resumed run regenerates the same data."),
    model = NULL, steps = list(start = step, end = step, completed = 0L), resumed_from = resume,
    metrics = .g3_metrics_df(list()), checks = NULL, evaluation = NULL, checkpoint = NULL,
    model_id = NA_character_, registered_model = NULL, environment = NULL,
    timings = list(total_seconds = proc.time()[["elapsed"]] - t_start, dataset_seconds = t_data,
                   worker_start_seconds = 0, train_seconds = 0, positions_per_second = NA_real_),
    output_mb = out_mb, limits = lim,
    limitations = "Stopped while generating training data: no worker was started and no checkpoint was written."
  ), class = "g3_run_manifest")
  .g3_write_run(run, output_dir)
  attr(run, "stop_reason") <- stop_reason
  run
}

# Read and check a checkpoint before resuming from it: its record must be
# readable and carry the configuration hash it was trained under, that hash
# must equal this run's, and the weights must match the recorded hash. A
# checkpoint without a configuration hash is refused, never assumed to
# match. Returns the record and the file hashes the worker must verify.
#' @noRd
.g3_resume_checkpoint <- function(resume, config_hash) {
  f <- file.path(resume, "checkpoint.json")
  cm <- if (file.exists(f)) tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(cm)) .g3_abort("g3_io_error", paste0("No readable checkpoint record (checkpoint.json) in ", resume, "."))
  ch <- cm$extra$config_hash
  if (!is.character(ch) || length(ch) != 1L || is.na(ch) || !nzchar(ch)) {
    .g3_abort("g3_io_error", paste0("The checkpoint records no training-configuration hash, so its configuration ",
                                    "cannot be verified; refusing to resume."), reason = "config_hash_missing")
  }
  if (!identical(ch, config_hash)) {
    .g3_abort("g3_io_error", "The checkpoint was produced with a different training configuration; refusing to resume.",
              reason = "config_hash_mismatch")
  }
  exp <- list(checkpoint.json = .g3_file_sha256(f),
              weights.safetensors = cm$files$weights.safetensors, optimizer.safetensors = cm$files$optimizer.safetensors)
  wf <- file.path(resume, "weights.safetensors")
  if (!file.exists(wf) || !identical(.g3_file_sha256(wf), exp$weights.safetensors)) {
    .g3_abort("g3_io_error", "The resume checkpoint's weights do not match its record (truncated or altered).")
  }
  list(record = cm, expected = exp)
}

#' @noRd
.g3_metrics_df <- function(metrics) {
  cols <- c("step", "loss", "policy_loss", "wdl_loss", "aux_loss", "grad_norm", "lr", "n_positions", "n_policy",
            "n_wdl", "n_aux", "policy_entropy", "top1_agreement", "params_with_gradient", "seconds")
  if (!length(metrics)) {
    return(as.data.frame(stats::setNames(replicate(length(cols), numeric(), simplify = FALSE), cols)))
  }
  rows <- lapply(metrics, function(m) {
    as.data.frame(lapply(stats::setNames(cols, cols), function(k) {
      v <- m[[k]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }))
  })
  do.call(rbind, rows)
}

#' @noRd
.g3_write_run <- function(run, dir) {
  r <- unclass(run)
  r$metrics <- .g3_rows(r$metrics)
  r$registered_model <- NULL
  .g3_atomic_write(jsonlite::toJSON(r, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null"),
                   file.path(dir, "run.json"))
}

#' @noRd
.g3_latest_checkpoint <- function(output_dir) {
  d <- list.dirs(file.path(output_dir, "checkpoints"), recursive = FALSE)
  d <- d[grepl("^ckpt-", basename(d)) & file.exists(file.path(d, "checkpoint.json"))]
  if (!length(d)) return(NULL)
  steps <- vapply(d, function(x) tryCatch(as.integer(jsonlite::fromJSON(file.path(x, "checkpoint.json"))$step), error = function(e) -1L), 0L)
  d[which.max(steps)]
}

#' @noRd
.g3_checkpoint_manifest <- function(model_id, display_name, ck, saved, model_config, parameter_count, run_id, seed, ds,
                                    cfg, config_hash, mdf, evaluation, parent) {
  tabs <- .g3_tables()
  art <- function(role, f, format) {
    p <- file.path(ck, f)
    list(role = role, file = f, sha256 = .g3_file_sha256(p), bytes = file.size(p), format = format)
  }
  last <- if (nrow(mdf)) mdf[nrow(mdf), ] else NULL
  list(
    schema = .G3_MANIFEST_SCHEMA,
    model_id = model_id,
    display_name = display_name %||% paste0("Square transformer (", model_config$profile, "), run ", run_id),
    kind = "trained_checkpoint",
    backend = "python-pytorch-worker",
    architecture = list(name = "square-transformer", version = model_config$architecture_version,
                        profile = model_config$profile, config = model_config, parameter_count = parameter_count),
    artifacts = list(art("weights", "weights.safetensors", "safetensors"),
                     art("optimizer", "optimizer.safetensors", "safetensors"),
                     art("checkpoint_meta", "checkpoint.json", "json")),
    weights_digest = saved$weights_digest,
    input_schema = list(feature_schema = .g3_feature_schema()$name, feature_schema_hash = .g3_feature_schema()$hash),
    action_schema = .G3_ACTION_SCHEMA,
    rules = list(version = tabs$rules_version, hash = tabs$rules_hash),
    tasks = c("play", "analysis", "policy", "value_wdl", "aux_heuristic", "legal_action_scoring", "mcts",
              "neural_move_ordering", "training", "self_play_training", "export"),
    value_only = FALSE,
    resources = list(device = "cpu", parameter_count = parameter_count,
                     weights_bytes = file.size(file.path(ck, "weights.safetensors"))),
    license = list(declared = "MIT (project licence)", status = "project_generated",
                   evidence = "weights trained locally from project-generated data; no external weights or data"),
    provenance = list(
      status = "accepted", run_id = run_id, seed = seed, created = .g3_now(),
      generator = "gambitR3D::g3_train_model", package_version = as.character(utils::packageVersion("gambitR3D")),
      engine = list(search = .G3_SEARCH_VERSION, evaluator = .G3_EVAL_VERSION),
      training_data = list(list(dataset_id = ds$dataset_id, manifest_sha256 = ds$manifest_sha256, split_hash = ds$split_hash,
                                provenance = ds$provenance$source)),
      external_weights = "none", parent_checkpoint = parent, config_hash = config_hash
    ),
    training = list(steps = if (nrow(mdf)) max(mdf$step) else 0L, optimizer = cfg$optimizer, schedule = cfg$schedule,
                    loss_weights = cfg$loss_weights, batch_size = cfg$batch_size, clip_norm = cfg$clip_norm,
                    final = if (is.null(last)) NULL else as.list(last)),
    evaluation = list(evidence = list(held_out = evaluation),
                      summary = "Pipeline smoke only: no match evaluation, no strength claim."),
    status = "pipeline-smoke, no strength claim",
    limitations = c("Trained for a handful of optimizer steps on a few hundred generated positions.",
                    "Policy imitates a shallow alpha-beta teacher; teacher agreement is not playing strength.",
                    "WDL head saw only a few verified terminal outcomes; values are not calibrated.")
  )
}

#' @export
print.g3_run_manifest <- function(x, ...) {
  cat("<g3_run_manifest> ", x$kind, " ", x$run_id, ": ", x$status, "\n", sep = "")
  if (identical(x$kind, "training")) {
    cat("  steps ", x$steps$start, " -> ", x$steps$end, ", parameters ", format(x$model$parameter_count, big.mark = ","),
        ", weights changed: ", x$model$weights_changed, "\n", sep = "")
    if (nrow(x$metrics)) {
      cat("  loss ", format(signif(x$metrics$loss[1L], 4)), " -> ", format(signif(x$metrics$loss[nrow(x$metrics)], 4)),
          ", grad norm range ", paste(format(signif(range(x$metrics$grad_norm), 3)), collapse = "-"), "\n", sep = "")
    }
  }
  invisible(x)
}

# Background-job type "training".
#' @noRd
.g3_job_training <- function(args, ctx) {
  run <- .g3_train_impl(args$config %||% list(), args$dataset %||% list(), args$output_dir, ctx = ctx)
  if (!identical(run$stop_reason, "completed")) attr(run, "stop_reason") <- run$stop_reason
  run
}
