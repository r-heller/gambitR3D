# Training-run checks that need no Python process: the resume gate.

fake_resume_checkpoint <- function(dir, extra) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  .g3_write_safetensors(list(w = list(dtype = "F32", shape = 2L, data = c(0.25, -1))),
                        file.path(dir, "weights.safetensors"))
  .g3_write_safetensors(list(m = list(dtype = "F32", shape = 1L, data = 0)), file.path(dir, "optimizer.safetensors"))
  rec <- list(format = "g3-checkpoint/1", step = 3L,
              files = list(weights.safetensors = .g3_file_sha256(file.path(dir, "weights.safetensors")),
                           optimizer.safetensors = .g3_file_sha256(file.path(dir, "optimizer.safetensors"))),
              extra = extra)
  writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"), file.path(dir, "checkpoint.json"))
  dir
}

test_that("resuming needs the checkpoint's recorded configuration hash", {
  root <- withr::local_tempdir()
  h <- strrep("c", 64)
  ok <- fake_resume_checkpoint(file.path(root, "ok"), list(run_id = "run-1", config_hash = h))
  rc <- .g3_resume_checkpoint(ok, h)
  expect_identical(rc$record$extra$config_hash, h)
  expect_identical(rc$expected$checkpoint.json, .g3_file_sha256(file.path(ok, "checkpoint.json")))
  # A different configuration is refused.
  e <- tryCatch(.g3_resume_checkpoint(ok, strrep("d", 64)), g3_io_error = function(e) e)
  expect_s3_class(e, "g3_io_error")
  expect_identical(e$context$reason, "config_hash_mismatch")
  # A checkpoint without the hash (field missing, null or empty) is refused,
  # never resumed under whatever configuration is asked for.
  for (ex in list(list(run_id = "run-1"), list(run_id = "run-1", config_hash = NULL), list(config_hash = ""))) {
    d <- fake_resume_checkpoint(tempfile("ck-", tmpdir = root), ex)
    e <- tryCatch(.g3_resume_checkpoint(d, h), g3_io_error = function(e) e)
    expect_s3_class(e, "g3_io_error")
    expect_match(conditionMessage(e), "no training-configuration hash", fixed = TRUE)
    expect_identical(e$context$reason, "config_hash_missing")
  }
  # Altered weights and a missing record are refused too.
  bad <- fake_resume_checkpoint(file.path(root, "bad"), list(config_hash = h))
  .g3_write_safetensors(list(w = list(dtype = "F32", shape = 2L, data = c(0.5, -1))), file.path(bad, "weights.safetensors"))
  expect_error(.g3_resume_checkpoint(bad, h), "do not match", class = "g3_io_error")
  expect_error(.g3_resume_checkpoint(file.path(root, "none"), h), "checkpoint.json", class = "g3_io_error")
})

test_that("data generation polls the stop condition per position, inside a game", {
  dir <- withr::local_tempdir()
  polls <- 0L
  stop_after <- function(k) function() {
    polls <<- polls + 1L
    polls > k
  }
  # One walk game of up to 30 positions: the stop arrives inside it.
  spec <- list(games = 1L, positions = 30L, max_walk_plies = 40L, platform_positions = 0L, fixtures = FALSE,
               teacher = list(nodes = 40L, depth = 1L))
  out <- .g3_ml_build_dataset(spec, file.path(dir, "a"), should_stop = stop_after(3L))
  expect_s3_class(out, "g3_dataset_stopped")
  expect_true(out$stopped)
  expect_identical(out$records_generated, 3L)
  expect_identical(polls, 4L)
  # Nothing partial is written; a later run regenerates the data.
  expect_false(file.exists(file.path(dir, "a", "dataset.json")))
  expect_length(list.files(file.path(dir, "a"), pattern = "shard|records"), 0L)
  # Fixture games and edited platform positions are polled too.
  polls <- 0L
  fx <- .g3_ml_build_dataset(list(games = 0L, positions = 0L, platform_positions = 0L, fixtures = TRUE,
                                  fixture_nodes = 200L, fixture_max_plies = 6L),
                             file.path(dir, "b"), should_stop = stop_after(1L))
  expect_s3_class(fx, "g3_dataset_stopped")
  expect_identical(fx$records_generated, 1L)
  polls <- 0L
  pl <- .g3_ml_build_dataset(list(games = 0L, positions = 0L, platform_positions = 3L, fixtures = FALSE,
                                  teacher = list(nodes = 40L, depth = 1L)),
                             file.path(dir, "c"), should_stop = stop_after(0L))
  expect_identical(pl$records_generated, 0L)
  # Without a stop the same spec completes.
  done <- .g3_ml_build_dataset(spec, file.path(dir, "d"), should_stop = function() FALSE)
  expect_s3_class(done, "g3_dataset")
  expect_gt(done$counts$records, 3L)
})

test_that("a training run stopped during data generation ends cleanly as cancelled", {
  skip_on_cran()
  env <- Sys.getenv("G3_ML_ENV")
  skip_if(!nzchar(env) || is.null(.g3_ml_read_marker(normalizePath(path.expand(env), mustWork = FALSE))),
          "G3_ML_ENV is not a provisioned ML environment")
  withr::local_options(gambitR3D.ml_env = env, gambitR3D.registry_dir = withr::local_tempdir())
  dir <- withr::local_tempdir()
  polls <- 0L
  ctx <- new.env(parent = emptyenv())
  ctx$resume <- FALSE
  ctx$attempt <- 1L
  ctx$cancelled <- function() polls > 2L
  ctx$should_stop <- function() {
    polls <<- polls + 1L
    ctx$cancelled()
  }
  ctx$timed_out <- function() FALSE
  ctx$progress <- function(...) invisible(NULL)
  run <- .g3_train_impl(list(steps = 2L, register = TRUE),
                        list(games = 1L, positions = 20L, max_walk_plies = 30L, platform_positions = 0L, fixtures = FALSE,
                             teacher = list(nodes = 40L, depth = 1L)),
                        file.path(dir, "run"), ctx = ctx)
  expect_s3_class(run, "g3_run_manifest")
  expect_identical(run$status, "cancelled")
  expect_identical(attr(run, "stop_reason"), "cancelled")
  expect_identical(run$dataset$status, "incomplete")
  expect_identical(run$dataset$records_generated, 2L)
  expect_null(run$checkpoint)
  expect_true(is.na(run$model_id))
  # No worker was started, no checkpoint or dataset was written; run.json keeps the run id.
  expect_length(list.files(file.path(dir, "run"), pattern = "^worker-"), 0L)
  expect_false(dir.exists(file.path(dir, "run", "checkpoints")))
  expect_false(file.exists(file.path(dir, "run", "data", "dataset.json")))
  expect_identical(jsonlite::fromJSON(file.path(dir, "run", "run.json"))$run_id, run$run_id)
  expect_output(print(run), "cancelled", fixed = TRUE)
})
