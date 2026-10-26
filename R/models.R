# Model registry, compatibility checks and model agents.
#
# Two registries are merged:
#
#   built-in  inst/models/registry.json: the model-free search option, the
#             untrained square-transformer architecture and three Hugging
#             Face research references (adaptation required; never
#             downloaded, never playable).
#   local     `registry_dir` (default tools::R_user_dir("gambitR3D",
#             "data")/models, or options(gambitR3D.registry_dir) or the
#             G3_MODEL_REGISTRY variable): an index of validated, locally
#             trained checkpoints. Each entry is a copy of the checkpoint's
#             manifest (`model.json`, written next to its artifacts) with
#             absolute artifact paths.
#
# Listing, capability reports and validation are pure R: no Python process
# starts and nothing is downloaded. Validation checks the manifest schema,
# complete provenance, the rules hash, the action schema, the feature
# schema hash, artifact SHA-256 hashes and the weights digest (recomputed
# from the safetensors file in R), the requested task and resource limits.
#
# Model agents (`g3_choose_action(state, agent = "model:<id>")` or a
# `g3_model`) dispatch here. Modes, recorded in the decision provenance:
#   "policy"    argmax of the network's legal-action distribution
#   "ordering"  alpha-beta search whose root actions are ordered by the
#               network's priors (neural move ordering)
#   "mcts"      R-owned PUCT search with network priors and values
#               (default), terminal leaves valued by the rules
# Every chosen action is checked against the engine's legal actions.

.G3_MANIFEST_SCHEMA <- "g3-model-manifest/1"
.G3_REGISTRY_SCHEMA <- "g3-model-registry/1"
.G3_MODEL_TASKS <- c("play", "analysis", "policy", "value_wdl", "aux_heuristic", "legal_action_scoring", "mcts",
                     "neural_move_ordering", "training", "self_play_training", "export", "explanation")
.G3_POLICY_TASKS <- c("play", "policy", "legal_action_scoring", "mcts", "neural_move_ordering")
.G3_PROVENANCE_FIELDS <- c("status", "external_weights", "training_data", "run_id", "seed")

# Registry locations ---------------------------------------------------------

#' @noRd
.g3_registry_dir <- function(registry_dir = NULL) {
  d <- c(registry_dir, getOption("gambitR3D.registry_dir"), Sys.getenv("G3_MODEL_REGISTRY"))
  d <- d[!is.na(d) & nzchar(d)]
  if (length(d)) return(normalizePath(path.expand(d[1L]), mustWork = FALSE))
  file.path(tools::R_user_dir("gambitR3D", which = "data"), "models")
}

#' @noRd
.g3_builtin_models <- function() {
  if (!is.null(.g3_env$ml_builtin_models)) return(.g3_env$ml_builtin_models)
  f <- system.file("models", "registry.json", package = "gambitR3D")
  reg <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  out <- lapply(reg$models, function(m) .g3_model_obj(m, base_dir = NULL, manifest_path = f, source = "builtin"))
  names(out) <- vapply(out, function(m) m$model_id, "")
  .g3_env$ml_builtin_models <- out
  out
}

#' @noRd
.g3_model_obj <- function(manifest, base_dir, manifest_path, source) {
  structure(
    list(model_id = manifest$model_id, manifest = manifest, base_dir = base_dir,
         manifest_path = manifest_path, source = source),
    class = "g3_model"
  )
}

#' @noRd
.g3_read_registry_index <- function(dir) {
  f <- file.path(dir, "registry.json")
  if (!file.exists(f)) return(list(schema = .G3_REGISTRY_SCHEMA, models = list()))
  idx <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(idx) || !identical(idx$schema, .G3_REGISTRY_SCHEMA)) {
    .g3_abort("g3_io_error", paste0("Unreadable model registry index: ", f))
  }
  idx
}

#' @noRd
.g3_local_models <- function(registry_dir = NULL) {
  dir <- .g3_registry_dir(registry_dir)
  idx <- .g3_read_registry_index(dir)
  out <- list()
  for (e in idx$models) {
    p <- file.path(dir, e$manifest)
    m <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(err) NULL)
    if (is.null(m)) next
    out[[e$model_id]] <- .g3_model_obj(m, base_dir = dirname(p), manifest_path = p, source = "registry")
  }
  out
}

# Resolve "model:<id>", an id, a manifest path or a g3_model.
#' @noRd
.g3_model_resolve <- function(model, registry_dir = NULL) {
  if (inherits(model, "g3_model")) return(model)
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    .g3_abort("g3_incompatible_model", "A model is a 'model:<id>' string, a model id, a manifest path or a g3_model.")
  }
  id <- sub("^model:", "", model)
  if (file.exists(id) && (dir.exists(id) || grepl("\\.json$", id))) {
    p <- if (dir.exists(id)) file.path(id, "model.json") else id
    m <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(m)) .g3_abort("g3_incompatible_model", paste0("Not a readable model manifest: ", p))
    return(.g3_model_obj(m, base_dir = dirname(normalizePath(p)), manifest_path = normalizePath(p), source = "file"))
  }
  local <- .g3_local_models(registry_dir)
  if (!is.null(local[[id]])) return(local[[id]])
  builtin <- .g3_builtin_models()
  if (!is.null(builtin[[id]])) return(builtin[[id]])
  .g3_abort("g3_incompatible_model", paste0("Unknown model '", id, "'. See g3_model_capabilities() for registered models."),
            model_id = id)
}

#' @export
print.g3_model <- function(x, ...) {
  m <- x$manifest
  cat("<g3_model> ", x$model_id, " [", m$kind, ", ", m$status, "]\n", sep = "")
  if (!is.null(m$display_name)) cat("  ", m$display_name, "\n", sep = "")
  if (!is.null(m$architecture$parameter_count) && is.numeric(m$architecture$parameter_count)) {
    cat("  parameters: ", format(m$architecture$parameter_count, big.mark = ","), "\n", sep = "")
  }
  invisible(x)
}

# Artifacts --------------------------------------------------------------------

#' @noRd
.g3_artifact_path <- function(model, art) {
  if (!is.null(art$path) && nzchar(art$path)) return(art$path)
  if (is.null(model$base_dir)) return(NA_character_)
  file.path(model$base_dir, art$file)
}

#' @noRd
.g3_artifact <- function(model, role) {
  for (a in model$manifest$artifacts %||% list()) if (identical(a$role, role)) return(a)
  NULL
}

# Validation ---------------------------------------------------------------------

#' @noRd
.g3_check_row <- function(check, ok, detail = "") {
  data.frame(check = check, status = if (is.na(ok)) "skip" else if (ok) "pass" else "fail",
             detail = detail, stringsAsFactors = FALSE)
}

#' @noRd
.g3_model_checks <- function(model, tasks = NULL, rules_hash = .g3_tables()$rules_hash, require_runtime = FALSE,
                             max_parameters = Inf, verify_artifacts = TRUE) {
  m <- model$manifest
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- .g3_check_row(...)
  need <- c("schema", "model_id", "kind", "backend", "rules", "action_schema", "tasks", "provenance", "license", "status")
  miss <- need[!need %in% names(m)]
  add("manifest", identical(m$schema, .G3_MANIFEST_SCHEMA) && !length(miss),
      if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else if (!identical(m$schema, .G3_MANIFEST_SCHEMA)) "unknown manifest schema" else "")
  kind <- m$kind %||% NA_character_
  # Provenance: every field present and the status accepted.
  pv <- m$provenance %||% list()
  pmiss <- .G3_PROVENANCE_FIELDS[vapply(.G3_PROVENANCE_FIELDS, function(f) {
    v <- pv[[f]]
    is.null(v) || (is.character(v) && (!length(v) || all(!nzchar(v))))
  }, TRUE)]
  lic_ok <- !is.null(m$license$declared) && !is.null(m$license$status)
  add("provenance", !length(pmiss) && lic_ok && identical(pv$status, "accepted"),
      if (length(pmiss)) paste("incomplete provenance:", paste(pmiss, collapse = ", ")) else
        if (!lic_ok) "licence declaration or status missing" else
          if (!identical(pv$status, "accepted")) paste0("provenance status '", pv$status, "' (only accepted provenance can be used)") else "")
  add("rules", identical(m$rules$hash, rules_hash),
      if (identical(m$rules$hash, rules_hash)) "" else paste0("rules hash ", format(m$rules$hash %||% "none"), " does not match ", rules_hash))
  add("action_schema", identical(m$action_schema, .G3_ACTION_SCHEMA),
      if (identical(m$action_schema, .G3_ACTION_SCHEMA)) "" else paste0("action schema '", format(m$action_schema %||% "none"), "' is not ", .G3_ACTION_SCHEMA))
  if (identical(m$backend, "python-pytorch-worker")) {
    fh <- m$input_schema$feature_schema_hash
    add("feature_schema", identical(fh, .g3_feature_schema()$hash),
        if (identical(fh, .g3_feature_schema()$hash)) "" else "feature schema hash differs from this package's encoder")
  } else {
    add("feature_schema", NA, "not applicable")
  }
  if (identical(kind, "reference_candidate")) {
    add("adapter", FALSE, paste0("adaptation required: ", m$adapter$reason %||% "no adapter"))
  } else {
    add("adapter", NA, "not applicable")
  }
  if (!is.null(tasks)) {
    bad <- setdiff(tasks, .G3_MODEL_TASKS)
    have <- unlist(m$tasks %||% list())
    missing_tasks <- setdiff(tasks, have)
    vo <- isTRUE(m$value_only) && any(tasks %in% .G3_POLICY_TASKS)
    detail <- if (length(bad)) paste0("unknown task: ", paste(bad, collapse = ", ")) else
      if (vo) "value-only model: no legal-action policy" else
        if (length(missing_tasks)) paste0("unsupported task: ", paste(missing_tasks, collapse = ", "),
                                          if (identical(kind, "architecture")) " (untrained architecture: train a checkpoint first)" else "") else ""
    add("task", !length(bad) && !vo && !length(missing_tasks), detail)
  } else {
    add("task", NA, "no task requested")
  }
  if (identical(kind, "trained_checkpoint")) {
    arts <- m$artifacts %||% list()
    roles <- vapply(arts, function(a) a$role %||% "", "")
    if (!all(c("weights", "checkpoint_meta") %in% roles)) {
      add("artifacts", FALSE, "weights and checkpoint metadata artifacts are required")
    } else if (verify_artifacts) {
      probs <- character()
      for (a in arts) {
        p <- .g3_artifact_path(model, a)
        if (is.na(p) || !file.exists(p)) {
          probs <- c(probs, paste0(a$role, " missing"))
        } else if (!identical(.g3_file_sha256(p), a$sha256)) {
          probs <- c(probs, paste0(a$role, " SHA-256 changed"))
        }
      }
      if (!length(probs)) {
        wd <- tryCatch(.g3_weights_digest(.g3_artifact_path(model, .g3_artifact(model, "weights"))), error = function(e) NA)
        if (!identical(wd, m$weights_digest)) probs <- c(probs, "weights digest differs from the manifest")
      }
      add("artifacts", !length(probs), paste(probs, collapse = "; "))
    } else {
      add("artifacts", NA, "not verified")
    }
  } else {
    add("artifacts", NA, if (identical(kind, "reference_candidate")) "not downloaded" else "no weights")
  }
  pc <- m$architecture$parameter_count
  if (is.numeric(pc) && is.finite(max_parameters)) {
    add("resources", pc <= max_parameters, if (pc <= max_parameters) "" else paste0(pc, " parameters exceed the limit ", max_parameters))
  } else {
    add("resources", NA, "no limit requested")
  }
  if (require_runtime && identical(m$backend, "python-pytorch-worker")) {
    rt <- .g3_ml_runtime_status()
    add("runtime", rt$available, if (rt$available) rt$path else rt$reason)
  } else {
    add("runtime", NA, "not required")
  }
  do.call(rbind, rows)
}

#' Validate a model for a rules profile and task
#'
#' Checks, without starting Python, that a model can be used: a known
#' manifest schema, complete and accepted provenance (licence declaration,
#' source, training data, run id, seed, external weights), the rules hash
#' of the profile, the action schema `g3-action/1`, the feature-schema hash
#' of this package's encoder, unchanged artifact SHA-256 hashes and weights
#' digest (recomputed in R from the safetensors file), the requested task
#' and optional resource and runtime requirements. Reference candidates
#' fail with "adaptation required"; an untrained architecture fails for
#' play.
#'
#' @param model A `g3_model`, a model id or `"model:<id>"`, or the path of
#'   a `model.json` manifest.
#' @param profile Either a rules profile id (default task `"play"`) or a
#'   list with any of `rules_profile` (default `"experimental-v0.1"`),
#'   `task` (one or more of `"play"`, `"analysis"`, `"policy"`,
#'   `"value_wdl"`, `"aux_heuristic"`, `"legal_action_scoring"`, `"mcts"`,
#'   `"neural_move_ordering"`, `"training"`, `"self_play_training"`,
#'   `"export"`, `"explanation"`; `NULL` skips the task check),
#'   `require_runtime` (also require a configured ML environment) and
#'   `max_parameters`.
#' @param error If `TRUE`, an invalid model raises `g3_unsupported_rules`
#'   (rules hash mismatch) or `g3_incompatible_model` (anything else).
#' @return A `g3_model_validation` list: `valid` (logical), `model_id`,
#'   `rules_profile`, `task`, `checks` (data frame `check`, `status`
#'   `"pass"`/`"fail"`/`"skip"`, `detail`) and `problems` (character).
#'   Returned invisibly when `error = TRUE` and the model is valid.
#' @examples
#' # Reference candidates are listed but cannot play this game.
#' v <- gambitR3D::g3_validate_model("amanm10000/chess-policy-net", "experimental-v0.1", error = FALSE)
#' v$valid
#' v$checks[v$checks$status == "fail", ]
#' @export
g3_validate_model <- function(model, profile = "experimental-v0.1", error = TRUE) {
  model <- .g3_model_resolve(model)
  p <- if (is.list(profile)) profile else list(rules_profile = profile)
  if (is.list(profile) && !"task" %in% names(profile)) p$task <- "play"
  if (!is.list(profile)) p$task <- "play"
  rp <- g3_rules_profile(p$rules_profile %||% "experimental-v0.1")
  checks <- .g3_model_checks(model, tasks = p$task, rules_hash = rp$rules_hash,
                             require_runtime = isTRUE(p$require_runtime),
                             max_parameters = p$max_parameters %||% Inf)
  bad <- checks[checks$status == "fail", , drop = FALSE]
  res <- structure(
    list(valid = !nrow(bad), model_id = model$model_id, rules_profile = rp$rules_version,
         task = p$task, checks = checks, problems = paste0(bad$check, ": ", bad$detail)[seq_len(nrow(bad))]),
    class = "g3_model_validation"
  )
  if (!error) return(res)
  if (nrow(bad)) {
    cls <- if (identical(bad$check, "rules")) "g3_unsupported_rules" else "g3_incompatible_model"
    .g3_abort(cls, paste0("Model '", model$model_id, "' is not usable: ", paste(res$problems, collapse = "; ")),
              model_id = model$model_id, failed = bad$check)
  }
  invisible(res)
}

#' @export
print.g3_model_validation <- function(x, ...) {
  cat("<g3_model_validation> ", x$model_id, ": ", if (x$valid) "valid" else "NOT valid",
      " for ", paste(x$task, collapse = ", "), "\n", sep = "")
  print(x$checks, row.names = FALSE)
  invisible(x)
}

# Capabilities ------------------------------------------------------------------

#' @noRd
.g3_capability_rows <- function(model, runtime) {
  m <- model$manifest
  kind <- m$kind
  have <- unlist(m$tasks %||% list())
  rows <- lapply(.G3_MODEL_TASKS, function(task) {
    st <- "available"
    why <- NA_character_
    if (identical(kind, "reference_candidate")) {
      if (isTRUE(m$value_only) && task %in% .G3_POLICY_TASKS) {
        st <- "not_applicable"
        why <- "value-only model: no legal-action policy"
      } else {
        st <- "adaptation_required"
        why <- m$adapter$reason %||% "no adapter"
      }
    } else if (!task %in% have) {
      if (identical(kind, "architecture")) {
        st <- "unavailable"
        why <- "untrained architecture: train a checkpoint with g3_train_model()"
      } else {
        st <- "not_applicable"
        why <- if (identical(kind, "search")) "the search opponent has no learned network" else "not provided by this model"
      }
    } else if (identical(m$backend, "python-pytorch-worker") && !runtime$available) {
      st <- "requires_runtime"
      why <- runtime$reason
    }
    if (identical(kind, "trained_checkpoint") && st == "available") {
      w <- .g3_artifact(model, "weights")
      p <- if (is.null(w)) NA_character_ else .g3_artifact_path(model, w)
      if (is.na(p) || !file.exists(p)) {
        st <- "unavailable"
        why <- "weights artifact missing"
      }
    }
    data.frame(model_id = model$model_id, task = task, status = st, reason = why, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Model capabilities
#'
#' Reports, per task, whether a model can perform it now. Statuses are
#' `"available"`, `"requires_runtime"` (a configured ML environment is
#' needed; see [g3_setup_ml()]), `"unavailable"` (for example an untrained
#' architecture or a missing artifact), `"adaptation_required"` (Hugging
#' Face references for conventional chess) and `"not_applicable"`. No
#' Python process is started and nothing is downloaded; artifact hashes are
#' verified by [g3_validate_model()].
#'
#' @param model A `g3_model`, a model id or `"model:<id>"`; `NULL` (the
#'   default) lists every built-in and locally registered model.
#' @return A data frame with one row per model and task: `model_id`,
#'   `task`, `status`, `reason`. With `model = NULL` it has the additional
#'   columns `display_name`, `kind`, `model_status` and `source`.
#' @examples
#' caps <- gambitR3D::g3_model_capabilities()
#' unique(caps[, c("model_id", "kind", "model_status")])
#' gambitR3D::g3_model_capabilities("builtin-search")
#' @export
g3_model_capabilities <- function(model = NULL) {
  rt <- .g3_ml_runtime_status()
  if (!is.null(model)) return(.g3_capability_rows(.g3_model_resolve(model), rt))
  models <- c(.g3_builtin_models(), .g3_local_models())
  out <- do.call(rbind, lapply(models, function(md) {
    r <- .g3_capability_rows(md, rt)
    r$display_name <- md$manifest$display_name %||% md$model_id
    r$kind <- md$manifest$kind
    r$model_status <- md$manifest$status
    r$source <- md$source
    r
  }))
  rownames(out) <- NULL
  out
}

# Registration ---------------------------------------------------------------------

#' Register a trained model
#'
#' Validates a model manifest (normally the `model.json` written next to a
#' checkpoint by [g3_train_model()]) and adds it to a local registry. The
#' manifest must describe a `trained_checkpoint` with complete accepted
#' provenance, the current rules hash, action schema and feature schema,
#' and artifacts whose SHA-256 hashes and weights digest match the files on
#' disk. Built-in ids cannot be reused, and an id already registered with
#' different weights is refused.
#'
#' @param manifest Path of a `model.json` (or of the directory holding it),
#'   or a manifest list (artifact `path`s must then be absolute, or the list
#'   must carry `base_dir`).
#' @param registry_dir Registry directory; `NULL` uses
#'   `options(gambitR3D.registry_dir)`, the `G3_MODEL_REGISTRY` variable or
#'   `tools::R_user_dir("gambitR3D", "data")/models`.
#' @return A `g3_model` descriptor: `model_id`, `manifest` (with absolute
#'   artifact paths), `base_dir`, `manifest_path` (the registry copy) and
#'   `source = "registry"`.
#' @examples
#' \dontrun{
#' run <- gambitR3D::g3_train_model(base::list(steps = 5L), dataset, tempfile())
#' m <- gambitR3D::g3_register_model(run$checkpoint$manifest)
#' gambitR3D::g3_model_capabilities(m)
#' }
#' @export
g3_register_model <- function(manifest, registry_dir = NULL) {
  if (is.character(manifest)) {
    p <- if (dir.exists(manifest)) file.path(manifest, "model.json") else manifest
    if (!file.exists(p)) .g3_abort("g3_io_error", paste0("No model manifest at ", p))
    m <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(m)) .g3_abort("g3_incompatible_model", paste0("Unreadable model manifest: ", p))
    base <- dirname(normalizePath(p))
  } else if (is.list(manifest)) {
    m <- manifest
    base <- m$base_dir
    m$base_dir <- NULL
  } else {
    .g3_abort("g3_incompatible_model", "`manifest` must be a path or a list.")
  }
  if (!identical(m$kind, "trained_checkpoint")) {
    .g3_abort("g3_incompatible_model", "Only trained checkpoints can be registered; built-in entries are fixed.")
  }
  id <- m$model_id
  if (!is.character(id) || length(id) != 1L || !nzchar(id) || grepl("^model:", id)) {
    .g3_abort("g3_incompatible_model", "The manifest needs a model_id (without the 'model:' prefix).")
  }
  if (id %in% names(.g3_builtin_models())) .g3_abort("g3_incompatible_model", paste0("'", id, "' is a built-in id."))
  obj <- .g3_model_obj(m, base_dir = base, manifest_path = NULL, source = "file")
  checks <- .g3_model_checks(obj, tasks = unlist(m$tasks), verify_artifacts = TRUE)
  bad <- checks[checks$status == "fail", , drop = FALSE]
  if (nrow(bad)) {
    cls <- if (identical(bad$check, "rules")) "g3_unsupported_rules" else "g3_incompatible_model"
    .g3_abort(cls, paste0("Cannot register '", id, "': ", paste(paste0(bad$check, ": ", bad$detail), collapse = "; ")),
              model_id = id, failed = bad$check)
  }
  # Store a copy with absolute artifact paths.
  m$artifacts <- lapply(m$artifacts, function(a) {
    a$path <- normalizePath(.g3_artifact_path(obj, a), mustWork = TRUE)
    a
  })
  dir <- .g3_registry_dir(registry_dir)
  dir.create(file.path(dir, "manifests"), recursive = TRUE, showWarnings = FALSE)
  idx <- .g3_read_registry_index(dir)
  safe <- gsub("[^A-Za-z0-9._-]", "_", id)
  rel <- file.path("manifests", paste0(safe, ".json"))
  for (e in idx$models) {
    if (identical(e$model_id, id)) {
      old <- tryCatch(jsonlite::fromJSON(file.path(dir, e$manifest), simplifyVector = FALSE), error = function(err) NULL)
      if (!is.null(old) && !identical(old$weights_digest, m$weights_digest)) {
        .g3_abort("g3_incompatible_model", paste0("'", id, "' is already registered with different weights; choose a new model_id."))
      }
    }
  }
  txt <- jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
  .g3_atomic_write(txt, file.path(dir, rel))
  idx$models <- Filter(function(e) !identical(e$model_id, id), idx$models)
  idx$models[[length(idx$models) + 1L]] <- list(
    model_id = id, manifest = rel, manifest_sha256 = .g3_file_sha256(file.path(dir, rel)),
    weights_digest = m$weights_digest, registered = .g3_now()
  )
  .g3_atomic_write(jsonlite::toJSON(idx, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(dir, "registry.json"))
  .g3_model_obj(m, base_dir = file.path(dir, "manifests"), manifest_path = file.path(dir, rel), source = "registry")
}

# Runtime ---------------------------------------------------------------------------

# Full validation once per model, task set and artifact file state
# (path, size, modification time) in a session; cheap afterwards. A
# changed artifact file triggers full validation again.
#' @noRd
.g3_model_validate_cached <- function(model, task) {
  arts <- model$manifest$artifacts %||% list()
  stamp <- paste(vapply(arts, function(a) {
    p <- .g3_artifact_path(model, a)
    fi <- file.info(p)
    paste(p, fi$size, as.numeric(fi$mtime))
  }, ""), collapse = ";")
  key <- paste(model$model_id, model$manifest$weights_digest %||% "", stamp, paste(task, collapse = ","), sep = "|")
  if (isTRUE(.g3_env$ml_validated[[key]])) return(invisible(TRUE))
  g3_validate_model(model, list(task = task), error = TRUE)
  if (is.null(.g3_env$ml_validated)) .g3_env$ml_validated <- list()
  .g3_env$ml_validated[[key]] <- TRUE
  invisible(TRUE)
}

# A worker with this model's checkpoint loaded (weights only), reusing the
# session worker. Validates the model first.
#' @noRd
.g3_model_runtime <- function(model, task = "play") {
  .g3_model_validate_cached(model, task)
  w_art <- .g3_artifact(model, "weights")
  c_art <- .g3_artifact(model, "checkpoint_meta")
  ck <- dirname(.g3_artifact_path(model, w_art))
  w <- .g3_ml_session_worker(roots = ck)
  key <- paste(ck, w_art$sha256, sep = "|")
  if (!identical(w$checkpoint, key)) {
    res <- .g3_worker_call(w, "load_checkpoint", list(
      path = ck, with_optimizer = FALSE,
      expected = list(weights.safetensors = w_art$sha256, checkpoint.json = c_art$sha256)
    ), timeout_s = 180)
    if (!identical(res$weights_digest, model$manifest$weights_digest)) {
      .g3_worker_abort(w, "g3_incompatible_model", "Loaded weights digest differs from the model manifest.")
    }
    w$checkpoint <- key
    w$model_hash <- res$weights_digest
    w$model_id <- model$model_id
  }
  w
}

# Model budgets and play settings ----------------------------------------------------

#' @noRd
.g3_model_play_settings <- function(model, budget) {
  s <- list(mode = "mcts", simulations = 32L, c_puct = 1.5, temperature = 0, nodes = 2000, time_ms = Inf, depth = 8L)
  if (!is.null(model$play)) s[names(model$play)] <- model$play
  if (!is.null(budget)) {
    if (!is.list(budget)) .g3_abort("g3_budget_exceeded", "`budget` must be a list.")
    unknown <- setdiff(names(budget), names(s))
    if (length(unknown)) .g3_abort("g3_budget_exceeded", paste0("Unknown model budget field: ", unknown[1L]))
    s[names(budget)] <- budget
  }
  if (!s$mode %in% c("policy", "ordering", "mcts")) {
    .g3_abort("g3_budget_exceeded", "Model mode must be 'policy', 'ordering' or 'mcts'.")
  }
  s$simulations <- as.integer(s$simulations)
  s$nodes <- as.numeric(s$nodes)
  s$time_ms <- as.numeric(s$time_ms)
  s$depth <- as.integer(s$depth)
  s
}

#' @noRd
.g3_mode_task <- function(mode) c(policy = "policy", ordering = "neural_move_ordering", mcts = "mcts")[[mode]]

# Model decision and analysis hooks (called from R/agents.R) -----------------------

#' @noRd
.g3_model_decision <- function(state, agent, budget, seed) {
  model <- .g3_model_resolve(agent)
  if (identical(model$manifest$kind, "search")) {
    sb <- if (is.null(budget)) NULL else budget[intersect(names(budget), c("nodes", "time_ms", "depth"))]
    d <- g3_choose_action(state, "search", sb %||% list(nodes = 2000L), seed)
    d$provenance$model_id <- model$model_id
    return(d)
  }
  s <- .g3_model_play_settings(model, budget)
  task <- c("play", .g3_mode_task(s$mode))
  .g3_model_validate_cached(model, task)
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  prov <- .g3_decision_provenance(state, paste0("model:", model$model_id), s, seed)
  prov <- c(prov, list(model_id = model$model_id, mode = s$mode, weights_digest = model$manifest$weights_digest,
                       feature_schema_hash = .g3_feature_schema()$hash, action_schema = .G3_ACTION_SCHEMA))
  if (identical(state$outcome$status, "terminal")) {
    d <- .g3_decision(NA_character_, NA_character_, "terminal", NA_real_, 0L, 0, 0, character(), NULL, prov)
    d$score_kind <- NA_character_
    return(d)
  }
  w <- .g3_model_runtime(model, task)
  prov$worker <- list(torch = w$hello$torch, python = w$hello$python, device = w$hello$device, threads = w$hello$threads)
  t0 <- proc.time()[["elapsed"]]
  r <- .g3_model_core_decision(core, tabs, w, s, seed)
  elapsed <- (proc.time()[["elapsed"]] - t0) * 1000
  legal_ids <- .g3_action_ids(core, .g3_legal(core, tabs), tabs)
  if (!r$action_id %in% legal_ids) {
    .g3_abort("g3_illegal_action", "The model selected an action that is not legal; it was rejected.", action_id = r$action_id)
  }
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  i <- match(r$action_id, legal_ids)
  lab <- .g3_action_labels(core, legal[i, , drop = FALSE], tabs, ctx)
  prov$mcts <- r$mcts_settings
  d <- .g3_decision(r$action_id, lab, r$status, r$score %||% r$value, r$depth, r$nodes, elapsed, r$pv,
                    .g3_model_alternatives(core, legal, legal_ids, tabs, ctx, r), prov)
  # MCTS scores are backed-up values (mcts_value); policy and ordering
  # report the network's value. Both are p(win) - p(loss) style numbers in
  # [-1, 1] for the side to act, not heuristic units.
  d$score_kind <- if (!is.null(r$score)) "mcts_value" else "model_value"
  d$value <- r$value
  d$wdl <- r$wdl
  d$expected_score <- if (is.null(r$wdl)) NA_real_ else unname(r$wdl[["win"]] + 0.5 * r$wdl[["draw"]])
  d$mode <- s$mode
  d
}

# The decision itself, on a core, given a ready worker.
#' @noRd
.g3_model_core_decision <- function(core, tabs, w, s, seed) {
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  enc <- .g3_encode_core(core, tabs, ctx, legal)
  if (s$mode == "mcts") {
    ms <- list(simulations = s$simulations, c_puct = s$c_puct, time_ms = s$time_ms, training = FALSE)
    m <- .g3_mcts(core, .g3_mcts_model_evaluator(w, w$model_hash, tabs), ms, tabs,
                  should_stop = getOption("gambitR3D.stop_check"))
    rt <- m$root
    pv <- m$action_id
    root_out <- .g3_ml_infer(w, list(enc), model_hash = w$model_hash)[[1L]]
    return(list(action_id = m$action_id, status = m$status, value = m$root_value, wdl = root_out$wdl,
                score = m$search_value, depth = m$stats$max_depth, nodes = m$simulations, pv = pv,
                probability = rt$prior, logit = root_out$logits, visits = rt$visits, q = rt$q,
                mcts_settings = c(ms, list(simulations_completed = m$simulations, stop_reason = m$stop_reason,
                                           evaluations = m$stats$evaluations, version = .G3_MCTS_VERSION))))
  }
  out <- .g3_ml_infer(w, list(enc), model_hash = w$model_hash)[[1L]]
  if (s$mode == "policy") {
    i <- if (s$temperature > 0) {
      .g3_with_seed(seed, sample.int(length(out$probability), 1L, prob = out$probability^(1 / s$temperature)))
    } else {
      order(-out$probability, seq_along(out$probability))[1L]
    }
    return(list(action_id = out$action_ids[i], status = "completed", value = out$value, wdl = out$wdl, depth = 0L,
                nodes = 1, pv = out$action_ids[i], probability = out$probability, logit = out$logits,
                visits = NULL, q = NULL, mcts_settings = NULL))
  }
  # Neural move ordering: alpha-beta with root actions in prior order.
  ord <- out$action_ids[order(-out$probability, seq_along(out$probability))]
  b <- .g3_budget(list(nodes = s$nodes, time_ms = s$time_ms, depth = s$depth))
  r <- .g3_ml_ordered_search(core, tabs, b, ord)
  list(action_id = r$ids[r$best], status = r$status, value = out$value, wdl = out$wdl, depth = r$depth,
       nodes = r$nodes, pv = r$ids[r$best], probability = out$probability, logit = out$logits,
       visits = NULL, q = NULL, search_scores = r$scores, mcts_settings = NULL)
}

#' @noRd
.g3_model_alternatives <- function(core, legal, ids, tabs, ctx, r) {
  n <- length(ids)
  if (!is.null(r$visits)) {
    score <- r$q
    kind <- "mcts_q"
    rk <- order(order(-r$visits, -r$probability, seq_len(n)))
  } else if (!is.null(r$search_scores)) {
    score <- r$search_scores
    kind <- "heuristic"
    rk <- rank(-ifelse(is.na(score), -Inf, score), ties.method = "first")
  } else {
    score <- r$probability
    kind <- "policy_probability"
    rk <- rank(-score, ties.method = "first")
  }
  data.frame(
    action_id = ids, label = .g3_action_labels(core, legal, tabs, ctx),
    type = c("move", "platform", "castle")[legal[, "type"]],
    score = score, score_kind = kind, mate_in = NA_integer_, bound = "model", rank = as.integer(rk),
    probability = r$probability, logit = r$logit %||% NA_real_,
    visits = r$visits %||% NA_integer_, q = r$q %||% NA_real_,
    stringsAsFactors = FALSE
  )
}

#' @noRd
.g3_model_position_analysis <- function(state, evaluator, budget, actions) {
  model <- .g3_model_resolve(evaluator)
  if (identical(model$manifest$kind, "search")) {
    sb <- if (is.null(budget)) NULL else budget[intersect(names(budget), c("nodes", "time_ms", "depth"))]
    return(g3_analyse_position(state, "search", sb %||% list(nodes = 2000L), actions))
  }
  s <- .g3_model_play_settings(model, budget)
  task <- c("analysis", .g3_mode_task(s$mode))
  .g3_model_validate_cached(model, task)
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  ids <- .g3_action_ids(core, legal, tabs)
  if (!is.null(actions)) {
    bad <- setdiff(actions, ids)
    if (length(bad)) .g3_abort("g3_illegal_action", paste0("Not legal here: ", bad[1L]), action_id = bad[1L])
  }
  prov <- .g3_decision_provenance(state, paste0("model:", model$model_id), s, NULL)
  prov <- c(prov, list(model_id = model$model_id, mode = s$mode, weights_digest = model$manifest$weights_digest))
  empty <- data.frame(action_id = character(), label = character(), type = character(), score = numeric(),
                      score_kind = character(), mate_in = integer(), bound = character(), rank = integer(),
                      probability = numeric(), logit = numeric(), visits = integer(), q = numeric(),
                      stringsAsFactors = FALSE)
  if (identical(state$outcome$status, "terminal") || !length(ids)) {
    v <- .g3_terminal_value(state)
    return(structure(list(alternatives = empty, root_score = v, score_kind = "model_value", perspective = "side_to_act",
                          side = state$side, pv = character(), pv_labels = character(), depth = 0L,
                          completeness = "terminal", cost = list(nodes = 0, elapsed_ms = 0), provenance = prov,
                          wdl = .g3_terminal_wdl(state), expected_score = (v + 1) / 2, value_source = "rules"),
                     class = "g3_position_analysis"))
  }
  w <- .g3_model_runtime(model, task)
  t0 <- proc.time()[["elapsed"]]
  r <- .g3_model_core_decision(core, tabs, w, s, NULL)
  alt <- .g3_model_alternatives(core, legal, ids, tabs, ctx, r)
  if (!is.null(actions)) alt <- alt[alt$action_id %in% actions, , drop = FALSE]
  rownames(alt) <- NULL
  structure(
    list(
      alternatives = alt, root_score = r$value, score_kind = "model_value", perspective = "side_to_act",
      side = state$side, pv = r$pv, pv_labels = .g3_pv_labels(core, r$pv, tabs), depth = as.integer(r$depth),
      completeness = if (r$status %in% c("completed")) "complete" else if (r$status == "fallback") "none" else "partial",
      cost = list(nodes = r$nodes, elapsed_ms = (proc.time()[["elapsed"]] - t0) * 1000), provenance = prov,
      wdl = r$wdl, expected_score = unname(r$wdl[["win"]] + 0.5 * r$wdl[["draw"]]), value_source = "network"
    ),
    class = "g3_position_analysis"
  )
}

# Value of a finished position for the side to act, from the rules.
#' @noRd
.g3_terminal_value <- function(state) {
  res <- state$outcome$result
  if (identical(res, "draw")) 0 else if (identical(res, state$side)) 1 else -1
}

#' @noRd
.g3_terminal_wdl <- function(state) {
  v <- .g3_terminal_value(state)
  stats::setNames(c(as.numeric(v == 1), as.numeric(v == 0), as.numeric(v == -1)), c("win", "draw", "loss"))
}

# Predictions -------------------------------------------------------------------------

#' Score the legal actions of a position with a model
#'
#' Encodes the position and its complete canonical legal-action set in R,
#' asks the model's worker for one logit per candidate and the WDL head,
#' validates the reply (candidate digest, counts, finiteness) and forms the
#' probabilities in R over the full legal set. A finished position is
#' valued by the rules (no network call, no empty softmax).
#'
#' @param model A trained `g3_model`, model id or `"model:<id>"`.
#' @param state A `g3_state`.
#' @param actions Optional subset of legal action ids to report; every id
#'   must be legal. Scores are still computed over the full legal set, and
#'   `probability_subset` renormalises within the subset.
#' @return A `g3_prediction` list: `actions` (data frame `action_id`,
#'   `label`, `type`, `logit`, `probability`, `probability_subset`, `rank`),
#'   `value` (p(win) - p(loss) in \[-1, 1\]), `wdl` (named `win`, `draw`,
#'   `loss`), `expected_score` (p(win) + p(draw)/2), `aux_heuristic`,
#'   `perspective` (`"side_to_act"`), `side`, `status` (`"scored"` or
#'   `"terminal"`), `value_source` (`"network"` or `"rules"`) and
#'   `provenance` (model id, weights digest, feature-schema and rules
#'   hashes, state hash, candidate digest, worker versions, elapsed time).
#' @examples
#' \dontrun{
#' # Needs a configured ML environment and a trained checkpoint.
#' p <- gambitR3D::g3_predict_actions("model:my-checkpoint", gambitR3D::g3_new_game()$state)
#' utils::head(p$actions)
#' p$wdl
#' }
#' @export
g3_predict_actions <- function(model, state, actions = NULL) {
  .g3_check_state(state)
  model <- .g3_model_resolve(model)
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  terminal <- identical(state$outcome$status, "terminal")
  legal <- .g3_legal(core, tabs, ctx)
  if (terminal) legal <- legal[0L, , drop = FALSE]
  ids <- .g3_action_ids(core, legal, tabs)
  if (!is.null(actions)) {
    bad <- setdiff(actions, ids)
    if (length(bad)) .g3_abort("g3_illegal_action", paste0("Not a legal action here: ", bad[1L]), action_id = bad[1L])
  }
  .g3_model_validate_cached(model, c("policy", "value_wdl"))
  prov <- list(model_id = model$model_id, weights_digest = model$manifest$weights_digest,
               feature_schema_hash = .g3_feature_schema()$hash, rules_hash = state$rules_hash,
               action_schema = .G3_ACTION_SCHEMA, state_hash = .g3_state_hash(state))
  empty <- data.frame(action_id = character(), label = character(), type = character(), logit = numeric(),
                      probability = numeric(), probability_subset = numeric(), rank = integer(), stringsAsFactors = FALSE)
  if (!length(ids)) {
    v <- .g3_terminal_value(state)
    return(structure(list(actions = empty, value = v, wdl = .g3_terminal_wdl(state), expected_score = (v + 1) / 2,
                          aux_heuristic = NA_real_, perspective = "side_to_act", side = state$side, status = "terminal",
                          value_source = "rules", provenance = prov), class = "g3_prediction"))
  }
  w <- .g3_model_runtime(model, c("policy", "value_wdl"))
  t0 <- proc.time()[["elapsed"]]
  enc <- .g3_encode_core(core, tabs, ctx, legal)
  o <- .g3_ml_infer(w, list(enc), model_hash = w$model_hash, state_hash = prov$state_hash)[[1L]]
  tab <- data.frame(
    action_id = o$action_ids, label = .g3_action_labels(core, legal, tabs, ctx),
    type = c("move", "platform", "castle")[legal[, "type"]], logit = o$logits, probability = o$probability,
    probability_subset = o$probability, rank = as.integer(rank(-o$probability, ties.method = "first")),
    stringsAsFactors = FALSE
  )
  if (!is.null(actions)) {
    tab <- tab[tab$action_id %in% actions, , drop = FALSE]
    tab$probability_subset <- tab$probability / sum(tab$probability)
    rownames(tab) <- NULL
  }
  prov$candidate_digest <- enc$candidate_digest
  prov$n_candidates <- length(ids)
  prov$worker <- list(torch = w$hello$torch, python = w$hello$python, device = w$hello$device)
  prov$elapsed_ms <- (proc.time()[["elapsed"]] - t0) * 1000
  structure(list(actions = tab, value = o$value, wdl = o$wdl, expected_score = o$expected_score,
                 aux_heuristic = o$aux_heuristic, perspective = "side_to_act", side = state$side, status = "scored",
                 value_source = "network", provenance = prov), class = "g3_prediction")
}

#' @export
print.g3_prediction <- function(x, ...) {
  cat("<g3_prediction> ", x$provenance$model_id, ", ", x$side, " to act: value ", format(round(x$value, 3)),
      " (W/D/L ", paste(format(round(x$wdl, 3)), collapse = "/"), ", ", x$value_source, ")\n", sep = "")
  if (nrow(x$actions)) print(utils::head(x$actions[order(x$actions$rank), c("rank", "label", "probability")], 8L), row.names = FALSE)
  invisible(x)
}

# Neural move ordering ---------------------------------------------------------------

# Iterative-deepening alpha-beta whose root actions start in the given
# order (the network's priors) instead of the static ordering; deeper
# plies use the ordinary search. Mirrors the root loop of the R reference
# search and reports the same fields.
#' @noRd
.g3_ml_ordered_search <- function(core, tabs, budget, order_ids) {
  e <- .g3_search_env(budget, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  ids <- .g3_action_ids(core, legal, tabs)
  n <- nrow(legal)
  res <- list(legal = legal, ids = ids, depth = 0L, best = NA_integer_, scores = rep(NA_real_, n),
              status = "terminal", nodes = 0, elapsed = 0)
  if (!n) return(res)
  o <- match(order_ids, ids)
  o <- c(o[!is.na(o)], setdiff(seq_len(n), o))
  ord_ids <- ids[o]
  children <- lapply(o, function(i) .g3_make(core, legal[i, ], tabs))
  best_completed <- NA_integer_
  for (d in seq_len(budget$depth)) {
    alpha <- -Inf
    sc <- rep(NA_real_, n)
    best_i <- NA_integer_
    for (i in seq_len(n)) {
      s <- -.g3_negamax(e, children[[i]], d - 1L, -Inf, -alpha, 1L)
      if (e$stop) break
      sc[i] <- s
      if (is.na(best_i) || s > sc[best_i]) best_i <- i
      if (s > alpha) alpha <- s
    }
    if (e$stop) break
    res$depth <- d
    best_completed <- best_i
    res$scores <- sc[match(ids, ord_ids)]
    res$best <- match(ord_ids[best_i], ids)
    k <- c(best_i, setdiff(order(-sc, seq_len(n)), best_i))
    ord_ids <- ord_ids[k]
    children <- children[k]
    if (abs(sc[best_i]) > .G3_MATE_BOUND) break
  }
  res$nodes <- e$nodes
  res$elapsed <- (proc.time()[["elapsed"]] - e$t0) * 1000
  if (is.na(best_completed)) {
    res$status <- "fallback"
    res$best <- match(ord_ids[1L], ids)
  } else {
    res$status <- if (e$stop) "budget" else "completed"
  }
  res
}
