# Export of trained checkpoints with parity evidence.
#
# The export directory holds the safetensors weights, the checkpoint's JSON
# metadata, a plain `config.json` (architecture, feature and action schema,
# output conventions), a relative-path `model.json` (so the directory can
# itself be registered) and `parity.json`. Parity is measured, not
# assumed: reference outputs come from the original checkpoint in one
# worker process, the exported files are then loaded in a fresh worker,
# and logits, WDL probabilities, values and action rankings are compared
# on fixed positions (including platform and promotion candidates). Only
# safetensors export exists; ONNX is reported as unavailable.

.G3_EXPORT_FORMAT <- "g3-export/1"
.G3_PARITY_TOLERANCE <- 1e-6

#' Export a trained model
#'
#' Writes a trained checkpoint's inference artifacts to a new directory and
#' verifies parity: the exported files are reloaded in a separate worker
#' process and must reproduce the original checkpoint's logits, WDL
#' probabilities, values and action ranking on fixed reference positions
#' within the declared tolerance (`1e-6`). The R engine still validates
#' every action chosen with an exported model.
#'
#' @param model A trained `g3_model`, model id or `"model:<id>"`.
#' @param path Export directory; must not exist or be empty.
#' @param format `"safetensors"` (the only implemented format).
#' @return A `g3_export_manifest` list: `format` (`"g3-export/1"`),
#'   `export_format`, `path`, `model_id`, `weights_digest`, `files` (data
#'   frame `file`, `sha256`, `bytes`), `parity` (list `positions`,
#'   `max_abs_logit_diff`, `max_abs_wdl_diff`, `max_abs_value_diff`,
#'   `ranking_identical`, `tolerance`, `passed`), `created` and
#'   `manifest` (path of `export-manifest.json`).
#' @examples
#' \dontrun{
#' ex <- gambitR3D::g3_export_model("model:my-checkpoint",
#'                                  base::file.path(base::tempdir(), "export"))
#' ex$parity$passed
#' }
#' @export
g3_export_model <- function(model, path, format = "safetensors") {
  if (!identical(format, "safetensors")) {
    .g3_abort("g3_incompatible_model", paste0("Export format '", format, "' is not available: only safetensors export ",
                                              "is implemented; ONNX would need a parity-tested conversion that this build does not have."))
  }
  model <- .g3_model_resolve(model)
  g3_validate_model(model, list(task = "export"), error = TRUE)
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE))) {
    .g3_abort("g3_io_error", paste0("The export directory is not empty: ", path))
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path)
  m <- model$manifest
  w_art <- .g3_artifact(model, "weights")
  c_art <- .g3_artifact(model, "checkpoint_meta")
  file.copy(.g3_artifact_path(model, w_art), file.path(path, "weights.safetensors"))
  file.copy(.g3_artifact_path(model, c_art), file.path(path, "checkpoint.json"))
  if (!identical(.g3_file_sha256(file.path(path, "weights.safetensors")), w_art$sha256)) {
    .g3_abort("g3_io_error", "The copied weights do not match the checkpoint's hash.")
  }
  sch <- .g3_feature_schema()
  cfgj <- list(
    format = .G3_EXPORT_FORMAT, architecture = m$architecture, feature_schema = sch$name,
    feature_schema_hash = sch$hash, action_schema = .G3_ACTION_SCHEMA, rules = m$rules,
    inputs = list(tokens_cat = "[B, 72, 29] int", tokens_num = "[B, 72, 5] int (divided by schema divisors)",
                  cand_cat = "[B, A, 18] int", cand_num = "[B, A, 2] int", cand_mask = "[B, A] bool"),
    outputs = list(logits = "[B, A] one per legal candidate, canonical order", wdl = "[B, 3] win, draw, loss from the side to act",
                   value = "p_win - p_loss", expected_score = "p_win + 0.5 * p_draw"),
    note = "Legal candidates and every state transition come from the gambitR3D R engine."
  )
  .g3_atomic_write(jsonlite::toJSON(cfgj, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), file.path(path, "config.json"))
  em <- m
  em$model_id <- paste0(m$model_id, "-export")
  em$artifacts <- list(
    list(role = "weights", file = "weights.safetensors", sha256 = w_art$sha256, bytes = file.size(file.path(path, "weights.safetensors")), format = "safetensors"),
    list(role = "checkpoint_meta", file = "checkpoint.json", sha256 = .g3_file_sha256(file.path(path, "checkpoint.json")),
         bytes = file.size(file.path(path, "checkpoint.json")), format = "json")
  )
  em$tasks <- setdiff(unlist(m$tasks), c("training", "self_play_training"))
  em$provenance$parent_checkpoint <- m$model_id
  .g3_atomic_write(jsonlite::toJSON(em, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA),
                   file.path(path, "model.json"))
  # Parity: original checkpoint in worker A, exported files in worker B.
  tabs <- .g3_tables()
  states <- .g3_parity_states()
  encs <- lapply(states, .g3_encode_state)
  wa <- .g3_model_worker(model)
  ref <- tryCatch(.g3_ml_infer(wa, encs, model_hash = wa$model_hash), finally = .g3_worker_stop(wa))
  exp_model <- .g3_model_obj(em, base_dir = path, manifest_path = file.path(path, "model.json"), source = "file")
  wb <- .g3_model_worker(exp_model)
  got <- tryCatch(.g3_ml_infer(wb, encs, model_hash = wb$model_hash), finally = .g3_worker_stop(wb))
  dl <- max(vapply(seq_along(encs), function(i) max(abs(ref[[i]]$logits - got[[i]]$logits)), 0))
  dw <- max(vapply(seq_along(encs), function(i) max(abs(ref[[i]]$wdl - got[[i]]$wdl)), 0))
  dv <- max(vapply(seq_along(encs), function(i) abs(ref[[i]]$value - got[[i]]$value), 0))
  rk <- all(vapply(seq_along(encs), function(i) {
    identical(order(-ref[[i]]$probability, seq_along(ref[[i]]$probability)),
              order(-got[[i]]$probability, seq_along(got[[i]]$probability)))
  }, TRUE))
  parity <- list(positions = length(encs), max_abs_logit_diff = dl, max_abs_wdl_diff = dw, max_abs_value_diff = dv,
                 ranking_identical = rk, tolerance = .G3_PARITY_TOLERANCE,
                 passed = rk && dl <= .G3_PARITY_TOLERANCE && dw <= .G3_PARITY_TOLERANCE && dv <= .G3_PARITY_TOLERANCE,
                 reference = "original checkpoint, worker process A", candidate = "exported files, fresh worker process B")
  fixtures <- lapply(seq_along(encs), function(i) list(
    name = names(states)[i], state_hash = .g3_state_hash(states[[i]]), candidate_digest = encs[[i]]$candidate_digest,
    action_ids = as.list(encs[[i]]$action_ids), logits = as.list(ref[[i]]$logits), wdl = as.list(unname(ref[[i]]$wdl)),
    value = ref[[i]]$value
  ))
  .g3_atomic_write(jsonlite::toJSON(list(parity = parity, fixtures = fixtures), auto_unbox = TRUE, pretty = TRUE, digits = NA),
                   file.path(path, "parity.json"))
  fl <- list.files(path)
  files <- data.frame(file = fl, sha256 = vapply(file.path(path, fl), .g3_file_sha256, ""),
                      bytes = file.size(file.path(path, fl)), stringsAsFactors = FALSE, row.names = NULL)
  out <- list(format = .G3_EXPORT_FORMAT, export_format = "safetensors", path = path, model_id = m$model_id,
              weights_digest = m$weights_digest, files = files, parity = parity, created = .g3_now(),
              manifest = file.path(path, "export-manifest.json"))
  w <- out
  w$files <- .g3_rows(files)
  .g3_atomic_write(jsonlite::toJSON(w, auto_unbox = TRUE, pretty = TRUE, digits = NA), out$manifest)
  if (!parity$passed) .g3_warn("Export parity failed; see parity.json.")
  structure(out, class = "g3_export_manifest")
}

#' @export
print.g3_export_manifest <- function(x, ...) {
  cat("<g3_export_manifest> ", x$model_id, " -> ", x$path, " (", x$export_format, "); parity ",
      if (isTRUE(x$parity$passed)) "passed" else "FAILED", " on ", x$parity$positions, " positions\n", sep = "")
  invisible(x)
}

# Reference positions for parity checks: the start, tactical fixtures and
# a position with platform-promotion candidates.
#' @noRd
.g3_parity_states <- function() {
  fx <- .g3_ml_fixtures()
  list(start = g3_new_game()$state, mate_in_one = fx$mate_in_one$state,
       platform_escape = fx$platform_escape$state, promotion_choice = fx$promotion_choice$state,
       platform_promotion = fx$platform_promotion$state)
}
