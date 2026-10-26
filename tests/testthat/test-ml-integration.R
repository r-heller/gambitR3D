# Phases 11-12: optional integration with the real Python/PyTorch worker.
#
# Skipped unless G3_ML_ENV points to an environment created with
# g3_setup_ml(). Run with, for example:
#   G3_ML_ENV=~/.local/share/R/gambitR3D/ml-env \
#     Rscript -e 'devtools::test(filter = "ml-integration")'
# Everything stays on CPU, inside the smoke envelope, in temp directories.

skip_if_no_ml <- function() {
  skip_on_cran()
  env <- Sys.getenv("G3_ML_ENV")
  skip_if(!nzchar(env), "G3_ML_ENV is not set (optional ML integration)")
  skip_if_not_installed("processx")
  env <- normalizePath(path.expand(env), mustWork = FALSE)
  skip_if(is.null(.g3_ml_read_marker(env)), "G3_ML_ENV is not a provisioned gambitR3D ML environment")
  env
}

local_ml <- function(env, frame = parent.frame()) {
  reg <- withr::local_tempdir(.local_envir = frame)
  withr::local_options(gambitR3D.ml_env = env, gambitR3D.registry_dir = reg, .local_envir = frame)
  withr::defer(.g3_ml_shutdown(), envir = frame)
  reg
}

ml_dataset <- function(dir) {
  .g3_ml_build_dataset(list(games = 4L, positions = 48L, max_walk_plies = 10L, platform_positions = 6L,
                            teacher = list(nodes = 200L, depth = 2L), fixture_nodes = 5000L, fixture_max_plies = 4L), dir)
}

worker_pids <- function() {
  if (!dir.exists("/proc")) return(integer())
  p <- suppressWarnings(as.integer(list.files("/proc")))
  p <- p[!is.na(p)]
  keep <- vapply(p, function(x) {
    cl <- tryCatch(suppressWarnings(readBin(file.path("/proc", x, "cmdline"), "raw", 4096L)), error = function(e) raw())
    length(cl) > 0L && grepl("gambitr3d_worker", rawToChar(cl[cl != as.raw(0L)]), fixed = TRUE)
  }, TRUE)
  p[keep]
}

test_that("the worker handshake reports versions, device and the same schema", {
  env <- skip_if_no_ml()
  w <- .g3_worker_start(env)
  on.exit(.g3_worker_stop(w))
  h <- w$hello
  expect_identical(h$protocol_version, "1.0")
  expect_identical(h$feature_schema_hash, .g3_feature_schema()$hash)
  expect_identical(h$device, "cpu")
  expect_true(h$deterministic_algorithms)
  expect_match(h$torch, "\\+cpu$")
  expect_match(h$python, "^3\\.12\\.")
  expect_false(h$fault_injection)
  # Fault injection is refused unless enabled at start.
  expect_error(.g3_worker_call(w, "debug_fault", list(kind = "crash")), class = "g3_io_error")
  expect_true(w$proc$is_alive())
  # A wrong candidate digest is rejected by the worker.
  .g3_worker_call(w, "initialize_model", list(model_config = list(profile = "tiny"), seed = 1L))
  enc <- .g3_encode_state(g3_new_game()$state)
  p <- .g3_enc_payload(enc)
  p$candidate_digest <- strrep("0", 64)
  expect_error(.g3_worker_call(w, "infer_batch", list(positions = list(p))), "digest", class = "g3_incompatible_model")
})

test_that("tiny and research profiles instantiate with measured parameter counts", {
  env <- skip_if_no_ml()
  w <- .g3_worker_start(env)
  on.exit(.g3_worker_stop(w))
  tiny <- .g3_worker_call(w, "initialize_model", list(model_config = list(profile = "tiny"), seed = 20261026L))
  res <- .g3_worker_call(w, "initialize_model", list(model_config = list(profile = "research"), seed = 20261026L), timeout_s = 300)
  expect_identical(tiny$model_config$width, 64L)
  expect_identical(tiny$model_config$layers, 2L)
  expect_identical(tiny$model_config$heads, 4L)
  expect_identical(res$model_config$width, 256L)
  expect_identical(res$model_config$layers, 6L)
  expect_gt(tiny$parameter_count, 1e4)
  expect_lt(tiny$parameter_count, 1e6)
  expect_gt(res$parameter_count, 5e6)
  expect_lt(res$parameter_count, 10e6)
  expect_identical(sum(unlist(res$parameter_groups)), res$parameter_count)
})

test_that("logits are permutation-equivariant and padding does not change them", {
  env <- skip_if_no_ml()
  w <- .g3_worker_start(env)
  on.exit(.g3_worker_stop(w))
  init <- .g3_worker_call(w, "initialize_model", list(model_config = list(profile = "tiny"), seed = 3L))
  st <- .g3_ml_fixtures()$platform_promotion$state
  e <- .g3_encode_state(st)
  base <- .g3_ml_infer(w, list(e), model_hash = init$weights_digest)[[1L]]
  perm <- rev(seq_along(e$action_ids))
  ep <- e
  ep$cand_cat <- e$cand_cat[perm, , drop = FALSE]
  ep$cand_num <- e$cand_num[perm, , drop = FALSE]
  ep$action_ids <- e$action_ids[perm]
  ep$candidate_digest <- .g3_candidate_digest(ep$action_ids)
  pr <- .g3_ml_infer(w, list(ep))[[1L]]
  expect_equal(pr$logits, base$logits[perm], tolerance = 1e-5)
  expect_equal(pr$value, base$value, tolerance = 1e-6)
  # Batched with a position with a different candidate count (padding).
  e2 <- .g3_encode_state(.g3_ml_fixtures()$mate_in_one$state)
  both <- .g3_ml_infer(w, list(e, e2))
  single2 <- .g3_ml_infer(w, list(e2))[[1L]]
  expect_equal(both[[1L]]$logits, base$logits, tolerance = 1e-5)
  expect_equal(both[[2L]]$logits, single2$logits, tolerance = 1e-5)
  expect_equal(sum(both[[1L]]$probability), 1)
  expect_identical(both[[1L]]$action_ids, e$action_ids)
})

test_that("training updates weights; save and reload reproduce inference exactly", {
  env <- skip_if_no_ml()
  dir <- withr::local_tempdir()
  ds <- ml_dataset(file.path(dir, "data"))
  w <- .g3_worker_start(env, allowed_roots = dir)
  on.exit(.g3_worker_stop(w))
  init <- .g3_worker_call(w, "initialize_model", list(model_config = list(profile = "tiny"), seed = 11L))
  res <- .g3_worker_call(w, "train_chunk", list(
    dataset = list(manifest_path = ds$manifest_path, manifest_sha256 = ds$manifest_sha256, split = "train"),
    steps = 3L, batch_size = 16L, optimizer = list(lr = 2e-3), schedule = list(warmup_steps = 1L, total_steps = 3L),
    loss_weights = list(policy = 1, wdl = 1, aux = 0.25), seed = 11L
  ), fields = list(model_hash = init$weights_digest))
  expect_identical(res$steps_completed, 3L)
  m <- .g3_metrics_df(res$metrics)
  expect_true(all(is.finite(m$loss)))
  expect_true(all(is.finite(m$grad_norm) & m$grad_norm > 0))
  expect_true(all(m$params_with_gradient > 0))
  expect_true(res$weights_changed)
  expect_false(identical(res$weights_digest_before, res$weights_digest_after))
  # WDL loss only counts verified terminal outcomes.
  expect_true(all(m$n_wdl <= sum(vapply(ds$shards, function(s) s$n_wdl, 0L))))
  encs <- lapply(.g3_parity_states(), .g3_encode_state)
  before <- .g3_ml_infer(w, encs, model_hash = res$weights_digest_after)
  saved <- .g3_worker_call(w, "save_checkpoint", list(parent = dir, name = "ck"), fields = list(model_hash = res$weights_digest_after))
  expect_identical(.g3_weights_digest(file.path(dir, "ck", "weights.safetensors")), saved$weights_digest)
  expect_false(any(grepl("\\.tmp-", list.files(dir, all.files = TRUE))))
  w2 <- .g3_worker_start(env, allowed_roots = dir)
  on.exit(.g3_worker_stop(w2), add = TRUE)
  loaded <- .g3_worker_call(w2, "load_checkpoint", list(path = file.path(dir, "ck"), expected = saved$files, with_optimizer = TRUE))
  expect_identical(loaded$step, 3L)
  expect_true(loaded$optimizer_restored)
  after <- .g3_ml_infer(w2, encs, model_hash = loaded$weights_digest)
  for (i in seq_along(encs)) {
    expect_identical(after[[i]]$logits, before[[i]]$logits)
    expect_identical(after[[i]]$wdl, before[[i]]$wdl)
  }
  # A truncated (corrupted) checkpoint file is refused.
  wf <- file.path(dir, "ck", "weights.safetensors")
  writeBin(readBin(wf, "raw", file.size(wf) - 10L), wf)
  expect_error(.g3_worker_call(w2, "load_checkpoint", list(path = file.path(dir, "ck"), expected = saved$files)),
               class = "g3_io_error")
})

test_that("resuming from a checkpoint reproduces an uninterrupted CPU run exactly", {
  env <- skip_if_no_ml()
  local_ml(env)
  dir <- withr::local_tempdir()
  ds <- ml_dataset(file.path(dir, "data"))
  base <- list(steps = 6L, batch_size = 16L, chunk_steps = 2L, register = FALSE, seed = 5L)
  a <- g3_train_model(base, ds$manifest_path, file.path(dir, "A"))
  b1 <- g3_train_model(c(base, list(max_steps_this_run = 3L)), ds$manifest_path, file.path(dir, "B"))
  expect_identical(b1$status, "stopped")
  expect_identical(b1$steps$end, 3L)
  b2 <- g3_train_model(c(base, list(resume_from = b1$checkpoint$path)), ds$manifest_path, file.path(dir, "B"))
  expect_identical(c(b2$steps$start, b2$steps$end), c(3L, 6L))
  expect_identical(b2$model$weights_digest_final, a$model$weights_digest_final)
  expect_identical(b2$metrics$loss, a$metrics$loss[4:6])
  # A different configuration cannot resume this checkpoint.
  expect_error(g3_train_model(c(utils::modifyList(base, list(batch_size = 8L)), list(resume_from = b1$checkpoint$path)),
                              ds$manifest_path, file.path(dir, "C")), class = "g3_io_error")
  # A checkpoint whose record lost its configuration hash is refused, not
  # resumed under the requested configuration.
  ckf <- file.path(b1$checkpoint$path, "checkpoint.json")
  rec <- jsonlite::fromJSON(ckf, simplifyVector = FALSE)
  rec$extra$config_hash <- NULL
  writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, digits = NA, null = "null"), ckf)
  expect_error(g3_train_model(c(base, list(resume_from = b1$checkpoint$path)), ds$manifest_path, file.path(dir, "D")),
               "no training-configuration hash", class = "g3_io_error")
})

test_that("a trained checkpoint plays legal actions in every mode", {
  env <- skip_if_no_ml()
  local_ml(env)
  dir <- withr::local_tempdir()
  run <- g3_train_model(list(steps = 4L, batch_size = 16L), ml_dataset(file.path(dir, "data")), file.path(dir, "run"))
  expect_identical(run$status, "completed")
  id <- run$model_id
  expect_true(g3_validate_model(id, list(task = c("play", "mcts")))$valid)
  st <- g3_new_game()$state
  legal <- g3_legal_actions(st)$action_id
  for (mode in c("policy", "mcts", "ordering")) {
    d <- g3_choose_action(st, paste0("model:", id), budget = list(mode = mode, simulations = 8L, nodes = 200))
    expect_true(d$action_id %in% legal)
    expect_identical(d$provenance$mode, mode)
    expect_identical(d$provenance$weights_digest, run$model$weights_digest_final)
  }
  p <- g3_predict_actions(id, st)
  expect_identical(p$actions$action_id, legal)
  expect_equal(sum(p$actions$probability), 1)
  expect_equal(p$value, unname(p$wdl[["win"]] - p$wdl[["loss"]]))
  sub <- g3_predict_actions(id, st, actions = legal[1:3])
  expect_equal(sum(sub$actions$probability_subset), 1)
  expect_error(g3_predict_actions(id, st, actions = "m:W:0:0-B:3:3"), class = "g3_illegal_action")
  # A finished position is valued by the rules, not the network.
  done <- g3_apply_action(.g3_ml_fixtures()$mate_in_one$state, "m:B:0:0-B:3:0")$state
  tp <- g3_predict_actions(id, done)
  expect_identical(tp$status, "terminal")
  expect_identical(tp$value_source, "rules")
  expect_identical(nrow(tp$actions), 0L)
  expect_true(is.na(g3_choose_action(done, paste0("model:", id))$action_id))
  an <- g3_analyse_position(st, paste0("model:", id), budget = list(simulations = 8L))
  expect_s3_class(an, "g3_position_analysis")
  expect_setequal(an$alternatives$action_id, legal)
  # Game analysis and coaching with the model given as an id: model budget
  # fields accepted, revision recorded, WDL views filled, model wording.
  g <- g3_new_game()
  for (a in c("m:W:0:1-W:0:2", "m:B:2:2-B:2:1")) g <- g3_game_apply(g, a)
  ga <- g3_analyse_game(g, paste0("model:", id), budget = list(simulations = 4L))
  e <- ga$evaluations
  expect_identical(ga$provenance$model_revision, run$model$weights_digest_final)
  expect_true(all(e$model_revision == run$model$weights_digest_final))
  expect_identical(unique(e$score_kind), "model_value")
  expect_false(anyNA(e$expected_score_white))
  expect_identical(unique(e$expected_score_kind), "model_wdl")
  expect_equal(e$score_white, ifelse(e$side == "white", e$score_mover, -e$score_mover))
  expect_equal(e$expected_score_mover, (1 + e$raw_score) / 2, tolerance = 1e-6)
  cg <- g3_coach_game(g, paste0("model:", id), budget = list(simulations = 4L), max_exercises = 0L)
  expect_identical(cg$evaluator$model_revision, run$model$weights_digest_final)
  expect_true(any(grepl("4 MCTS simulations", cg$limitations, fixed = TRUE)))
  expect_false(any(grepl("heuristic units", cg$limitations, fixed = TRUE)))
})

test_that("self-play records truncation honestly and exports keep parity", {
  env <- skip_if_no_ml()
  local_ml(env)
  dir <- withr::local_tempdir()
  run <- g3_train_model(list(steps = 3L, batch_size = 16L), ml_dataset(file.path(dir, "data")), file.path(dir, "run"))
  sp <- g3_self_play(list(model = run$model_id, games = 1L, max_plies = 6L, simulations = 4L,
                          fixtures = "mate_in_one", fixture_simulations = 128L), file.path(dir, "sp"))
  g <- sp$games
  expect_identical(g$kind, c("self_play", "fixture"))
  expect_false(sp$truncated_games_are_draws)
  for (i in seq_len(nrow(g))) {
    game <- g3_read_game(g$file[i])
    if (g$status[i] == "truncated") {
      expect_identical(game$metadata$result, "truncated")
      expect_identical(utils::tail(game$events$type, 1L), "truncated")
    } else {
      expect_true(g3_is_terminal(game$state))
    }
  }
  expect_identical(g$status[1L], "truncated")
  v <- .g3_ml_verify_dataset(sp$dataset$manifest)
  expect_true(v$valid, info = paste(v$problems, collapse = "; "))
  ex <- g3_export_model(run$model_id, file.path(dir, "export"))
  expect_true(ex$parity$passed)
  expect_identical(ex$parity$max_abs_logit_diff, 0)
  expect_error(g3_export_model(run$model_id, file.path(dir, "onnx"), format = "onnx"), class = "g3_incompatible_model")
})

test_that("malformed output and crashes stop the worker cleanly; a new one recovers", {
  env <- skip_if_no_ml()
  before <- worker_pids()
  for (kind in c("malformed_line", "crash", "nonfinite", "wrong_echo")) {
    w <- .g3_worker_start(env, fault_injection = TRUE)
    pid <- w$pid
    expect_error(.g3_worker_call(w, "debug_fault", list(kind = kind)), class = "g3_io_error")
    expect_false(w$proc$is_alive())
    expect_false(.g3_pid_alive(pid))
  }
  w <- .g3_worker_start(env, fault_injection = TRUE, stale_s = 3)
  expect_error(.g3_worker_call(w, "debug_fault", list(kind = "hang", seconds = 60)), "stale", class = "g3_io_error")
  expect_false(w$proc$is_alive())
  w <- .g3_worker_start(env, fault_injection = TRUE)
  on.exit(.g3_worker_stop(w))
  .g3_worker_call(w, "debug_fault", list(kind = "stale_response"))
  .g3_worker_call(w, "debug_fault", list(kind = "duplicate_result"))
  init <- .g3_worker_call(w, "initialize_model", list(model_config = list(profile = "tiny"), seed = 1L))
  out <- .g3_ml_infer(w, list(.g3_encode_state(g3_new_game()$state)), model_hash = init$weights_digest)
  expect_equal(sum(out[[1L]]$probability), 1)
  expect_identical(w$stats$stale, 1L)
  expect_identical(w$stats$duplicate, 1L)
  # Worker-side rejection of an oversized line keeps the worker serving.
  left <- w$proc$write_input(paste0(strrep("x", .G3_MAX_LINE_BYTES + 10L), "\n"), sep = "")
  while (length(left)) {
    Sys.sleep(0.01)
    left <- w$proc$write_input(left, sep = "")
  }
  deadline <- Sys.time() + 10
  got <- list()
  while (!length(got) && Sys.time() < deadline) got <- Filter(function(m) identical(m$message_type, "error"), .g3_worker_read(w, 200L))
  expect_identical(got[[1L]]$payload$code, "oversized_line")
  expect_true(w$proc$is_alive())
  .g3_worker_stop(w)
  expect_setequal(worker_pids(), before)
})

test_that("a training job can be cancelled and resumed from its checkpoint", {
  env <- skip_if_no_ml()
  skip_if_not_installed("callr")
  dir <- withr::local_tempdir()
  ds <- ml_dataset(file.path(dir, "data"))
  out <- file.path(dir, "run")
  cfg <- list(steps = 20L, batch_size = 48L, chunk_steps = 20L, ml_env = env, registry_dir = file.path(dir, "reg"))
  job <- g3_start_job(list(type = "training", job_dir = file.path(dir, "jobs"),
                           args = list(config = cfg, dataset = ds$manifest_path, output_dir = out)))
  t0 <- Sys.time()
  repeat {
    s <- g3_job_status(job)
    if (isTRUE(s$progress$done >= 2) || s$status %in% .G3_JOB_TERMINAL || Sys.time() - t0 > 120) break
    Sys.sleep(0.05)
  }
  skip_if(s$status %in% .G3_JOB_TERMINAL, "the job finished before it could be cancelled")
  ack <- g3_cancel_job(job, wait = TRUE, grace_seconds = 15)
  expect_identical(ack$outcome, "stopped")
  job$process$wait(10000)
  expect_false(job$process$is_alive())
  st <- g3_job_status(job)
  expect_identical(st$status, "cancelled")
  r1 <- g3_job_result(job)
  expect_identical(r1$status, "cancelled")
  expect_lt(r1$steps$end, 20L)
  expect_true(file.exists(file.path(r1$checkpoint$path, "checkpoint.json")))
  job2 <- g3_resume_job(job)
  t0 <- Sys.time()
  repeat {
    s <- g3_job_status(job2)
    if (s$status %in% .G3_JOB_TERMINAL || Sys.time() - t0 > 180) break
    Sys.sleep(0.2)
  }
  expect_identical(s$status, "succeeded")
  r2 <- g3_job_result(job2)
  expect_identical(c(r2$steps$start, r2$steps$end), c(r1$steps$end, 20L))
  expect_false(is.na(r2$model_id))
  job2$process$wait(10000)
  expect_length(worker_pids()[worker_pids() %in% c(job$process$get_pid(), job2$process$get_pid())], 0L)
})
