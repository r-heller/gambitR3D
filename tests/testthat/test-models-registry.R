# Phase 11: model registry, capabilities and compatibility checks.
# Pure R: no Python process, no download, no network.

# A synthetic trained-checkpoint manifest over tiny files written in R. It
# exercises the validation logic only; it is not a model.
fake_checkpoint <- function(dir, model_id = "fake-tiny") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  .g3_write_safetensors(list(w = list(dtype = "F32", shape = c(2L, 2L), data = c(0.5, -1, 2, 0.25))),
                        file.path(dir, "weights.safetensors"))
  writeLines('{"format": "g3-checkpoint/1", "step": 1}', file.path(dir, "checkpoint.json"))
  art <- function(role, f, fmt) list(role = role, file = f, sha256 = .g3_file_sha256(file.path(dir, f)),
                                     bytes = file.size(file.path(dir, f)), format = fmt)
  m <- list(
    schema = "g3-model-manifest/1", model_id = model_id, display_name = "synthetic test checkpoint",
    kind = "trained_checkpoint", backend = "python-pytorch-worker",
    architecture = list(name = "square-transformer", profile = "tiny", parameter_count = 4L),
    artifacts = list(art("weights", "weights.safetensors", "safetensors"), art("checkpoint_meta", "checkpoint.json", "json")),
    weights_digest = .g3_weights_digest(file.path(dir, "weights.safetensors")),
    input_schema = list(feature_schema = "g3-features/1", feature_schema_hash = .g3_feature_schema()$hash),
    action_schema = "g3-action/1",
    rules = list(version = "experimental-v0.1", hash = .g3_tables()$rules_hash),
    tasks = c("play", "analysis", "policy", "value_wdl", "mcts", "export"),
    value_only = FALSE,
    license = list(declared = "MIT (project licence)", status = "project_generated"),
    provenance = list(status = "accepted", run_id = "run_test", seed = 1L, training_data = "synthetic", external_weights = "none"),
    status = "pipeline-smoke, no strength claim"
  )
  p <- file.path(dir, "model.json")
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE), p)
  p
}

no_runtime <- function(env = parent.frame()) {
  withr::local_options(gambitR3D.ml_env = file.path(tempdir(), "no-such-ml-env"), .local_envir = env)
  withr::local_envvar(G3_ML_ENV = "", .local_envir = env)
}

test_that("the registry lists built-in models without Python", {
  no_runtime()
  reg <- withr::local_tempdir()
  withr::local_options(gambitR3D.registry_dir = reg)
  caps <- g3_model_capabilities()
  ids <- unique(caps$model_id)
  expect_true(all(c("builtin-search", "builtin-square-transformer", "amanm10000/chess-policy-net",
                    "mateuszgrzyb/searchless-chess-vit-medium", "Jotellechea/chess-transformer") %in% ids))
  expect_true(all(caps$status %in% c("available", "requires_runtime", "unavailable", "adaptation_required", "not_applicable")))
  # The model-free search option is playable; the untrained architecture is not.
  s <- g3_model_capabilities("builtin-search")
  expect_identical(s$status[s$task == "play"], "available")
  a <- g3_model_capabilities("model:builtin-square-transformer")
  expect_identical(a$status[a$task == "play"], "unavailable")
  expect_match(a$reason[a$task == "play"], "untrained")
  expect_identical(a$status[a$task == "training"], "requires_runtime")
  expect_null(.g3_env$ml_worker)
})

test_that("Hugging Face candidates are reference-only with recorded evidence", {
  for (id in c("amanm10000/chess-policy-net", "mateuszgrzyb/searchless-chess-vit-medium", "Jotellechea/chess-transformer")) {
    m <- .g3_model_resolve(id)
    expect_identical(m$manifest$kind, "reference_candidate")
    expect_identical(m$manifest$status, "adaptation_required")
    expect_identical(m$manifest$evidence_date, "2026-10-26")
    expect_false(isTRUE(m$manifest$source$downloaded))
    expect_null(m$manifest$source$revision)
    expect_match(m$manifest$source$repository, "^https://huggingface.co/")
    expect_true(nzchar(m$manifest$license$declared))
    expect_identical(m$manifest$adapter$status, "unavailable")
    v <- g3_validate_model(id, "experimental-v0.1", error = FALSE)
    expect_false(v$valid)
    expect_true("adapter" %in% v$checks$check[v$checks$status == "fail"])
    expect_error(g3_choose_action(g3_new_game()$state, paste0("model:", id)), class = "g3_incompatible_model")
  }
})

test_that("a value-only adapter is identified explicitly", {
  caps <- g3_model_capabilities("mateuszgrzyb/searchless-chess-vit-medium")
  expect_true(.g3_model_resolve("mateuszgrzyb/searchless-chess-vit-medium")$manifest$value_only)
  expect_identical(caps$status[caps$task == "policy"], "not_applicable")
  expect_match(caps$reason[caps$task == "policy"], "value-only")
  expect_identical(caps$status[caps$task == "value_wdl"], "adaptation_required")
  v <- g3_validate_model("mateuszgrzyb/searchless-chess-vit-medium", list(task = "policy"), error = FALSE)
  expect_match(v$checks$detail[v$checks$check == "task"], "value-only")
  # The policy reference is not value-only.
  expect_false(.g3_model_resolve("amanm10000/chess-policy-net")$manifest$value_only)
})

test_that("a custom trained checkpoint registers and validates", {
  no_runtime()
  reg <- withr::local_tempdir()
  ck <- withr::local_tempdir()
  p <- fake_checkpoint(ck)
  m <- g3_register_model(p, reg)
  expect_s3_class(m, "g3_model")
  expect_true(file.exists(file.path(reg, "registry.json")))
  expect_true(all(vapply(m$manifest$artifacts, function(a) file.exists(a$path), TRUE)))
  withr::local_options(gambitR3D.registry_dir = reg)
  v <- g3_validate_model("model:fake-tiny", list(task = "play"))
  expect_true(v$valid)
  caps <- g3_model_capabilities("fake-tiny")
  # Valid but no runtime configured: honest status, no Python started.
  expect_identical(caps$status[caps$task == "play"], "requires_runtime")
  expect_error(g3_choose_action(g3_new_game()$state, "model:fake-tiny"), "g3_setup_ml", class = "g3_incompatible_model")
  expect_null(.g3_env$ml_worker)
  # Registering the same id again with identical weights is idempotent.
  expect_s3_class(g3_register_model(p, reg), "g3_model")
  expect_error(g3_register_model(list(kind = "search", model_id = "x"), reg), class = "g3_incompatible_model")
})

test_that("validation rejects rules, artifact, action-schema, task and provenance problems", {
  ck <- withr::local_tempdir()
  p <- fake_checkpoint(ck)
  base <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  as_model <- function(m) .g3_model_obj(m, base_dir = ck, manifest_path = p, source = "file")
  fails <- function(m, task = "play") {
    v <- g3_validate_model(as_model(m), list(task = task), error = FALSE)
    v$checks$check[v$checks$status == "fail"]
  }
  expect_length(fails(base), 0L)
  # Mismatched rules hash.
  m <- base
  m$rules$hash <- strrep("0", 64)
  expect_identical(fails(m), "rules")
  expect_error(g3_validate_model(as_model(m), "experimental-v0.1"), class = "g3_unsupported_rules")
  # Wrong action schema.
  m <- base
  m$action_schema <- "uci-4672"
  expect_identical(fails(m), "action_schema")
  # Unsupported and unknown tasks.
  expect_identical(fails(base, "self_play_training"), "task")
  expect_identical(fails(base, "chess_oracle"), "task")
  # Incomplete or unaccepted provenance.
  m <- base
  m$provenance$run_id <- NULL
  expect_identical(fails(m), "provenance")
  m <- base
  m$provenance$status <- "unresolved"
  expect_identical(fails(m), "provenance")
  m <- base
  m$license <- NULL
  expect_true("provenance" %in% fails(m))
  # Feature schema from another encoder.
  m <- base
  m$input_schema$feature_schema_hash <- strrep("a", 64)
  expect_identical(fails(m), "feature_schema")
  expect_error(g3_validate_model(as_model(m), "experimental-v0.1"), class = "g3_incompatible_model")
  # Changed artifact: same size, different bytes.
  .g3_write_safetensors(list(w = list(dtype = "F32", shape = c(2L, 2L), data = c(0.5, -1, 2, 0.5))),
                        file.path(ck, "weights.safetensors"))
  expect_identical(fails(base), "artifacts")
  reg <- withr::local_tempdir()
  expect_error(g3_register_model(p, reg), "SHA-256", class = "g3_incompatible_model")
})

test_that("the built-in search is a model-free option through the model interface", {
  no_runtime()
  st <- g3_new_game()$state
  d <- g3_choose_action(st, "model:builtin-search", budget = list(nodes = 100L))
  expect_true(d$action_id %in% g3_legal_actions(st)$action_id)
  expect_identical(d$provenance$model_id, "builtin-search")
  expect_null(.g3_env$ml_worker)
})

test_that("compatibility reports are deterministic and display preferences cannot enter", {
  a <- g3_validate_model("builtin-square-transformer", list(task = "training"), error = FALSE)
  b <- g3_validate_model("builtin-square-transformer", list(task = "training"), error = FALSE)
  expect_identical(a, b)
  expect_true(a$valid)
  # The registry's feature-schema hash is the encoder's current hash.
  m <- .g3_model_resolve("builtin-square-transformer")
  expect_identical(m$manifest$input_schema$feature_schema_hash, .g3_feature_schema()$hash)
  expect_identical(m$manifest$rules$hash, .g3_tables()$rules_hash)
})
