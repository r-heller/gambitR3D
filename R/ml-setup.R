# Optional ML runtime: an isolated Python/PyTorch environment.
#
# The runtime is a virtual environment at a path the user chooses. It is
# created with `uv` from a CPython 3.12 interpreter and the hashed lock
# `inst/python/requirements-cpu.lock` (CPU-only torch from the official
# PyTorch wheel index, numpy, safetensors), never from whichever `python`
# happens to be on PATH. A marker file `gambitr3d-ml.json` inside the
# environment records the interpreter, the lock hash, resolved package
# versions and the last probe.
#
# Nothing here runs at package load. Other functions find the environment
# through (in order) an explicit argument, `options(gambitR3D.ml_env)`, the
# `G3_ML_ENV` environment variable, or the default location
# `tools::R_user_dir("gambitR3D", "data")/ml-env` if a marker is present.
# They never create or download anything themselves.

.G3_ML_MARKER <- "gambitr3d-ml.json"
.G3_ML_ENV_SCHEMA <- "g3-ml-env/1"
.G3_ML_PYTHON <- "3.12"
.G3_ML_INDEX_URL <- "https://pypi.org/simple"
.G3_ML_TORCH_INDEX <- "https://download.pytorch.org/whl/cpu"

#' Set up or inspect the optional ML environment
#'
#' Creates (or, if it already exists, inspects) an isolated Python virtual
#' environment for the optional transformer worker. Creation uses `uv` with
#' a pinned CPython 3.12 and installs exact, hash-checked versions of
#' CPU-only `torch`, `numpy` and `safetensors` from the lock file shipped in
#' `system.file("python", "requirements-cpu.lock", package = "gambitR3D")`.
#' The environment then runs a version probe and a protocol handshake with
#' the worker. Core gameplay, search and the model registry never need this
#' environment.
#'
#' Downloads happen only here, only when the environment is missing
#' packages, and only from the Python Package Index and the official
#' PyTorch CPU wheel index. With `offline = TRUE` only the local `uv` cache
#' is used; a missing package then yields a `"blocked"` report instead of a
#' download.
#'
#' @param path Directory of the virtual environment. Use a location outside
#'   any source repository, for example
#'   `file.path(tools::R_user_dir("gambitR3D", "data"), "ml-env")`.
#' @param python `NULL` (default) for CPython `"3.12"` as found or managed by
#'   `uv`, a version string, or the path of a CPython 3.12 interpreter.
#' @param offline If `TRUE`, never download; use only cached packages.
#' @return A `g3_ml_report` list: `status` (`"ready"` or `"blocked"`),
#'   `reason` (`NA` when ready), `path`, `python` (list `executable`,
#'   `version`), `packages` (data frame `name`, `version`), `torch` (list
#'   `version`, `device`, `cuda_available`, `threads_default`), `worker`
#'   (the handshake reply: protocol and worker versions, feature-schema
#'   hash, device, dtypes, threads), `schema_match` (logical), `lock` (list
#'   `file`, `sha256`, `target`), `actions` (what this call did),
#'   `timings` (seconds), `log` (setup log path) and `marker` (path).
#' @examples
#' \dontrun{
#' # Needs `uv`. The first run downloads the locked packages; the installed
#' # environment takes about 820 MB on disk (measured on Linux x86-64 with
#' # CPU-only torch).
#' env <- file.path(tools::R_user_dir("gambitR3D", "data"), "ml-env")
#' rep <- gambitR3D::g3_setup_ml(env)
#' rep$status
#' rep$packages
#' }
#' @export
g3_setup_ml <- function(path, python = NULL, offline = FALSE) {
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .g3_abort("g3_io_error", "`path` must name the directory of the ML environment explicitly.")
  }
  .g3_need_processx()
  t_start <- proc.time()[["elapsed"]]
  path <- normalizePath(path.expand(path), mustWork = FALSE)
  .g3_ml_check_location(path)
  lock <- .g3_ml_lock_file()
  lock_sha <- digest::digest(file = lock, algo = "sha256")
  py <- .g3_venv_python(path)
  actions <- character()
  blocked <- function(reason, log = NA_character_) {
    .g3_ml_report("blocked", reason, path, lock, lock_sha, actions, t_start, log = log)
  }
  uv <- .g3_find_uv()
  marker <- .g3_ml_read_marker(path)
  need_venv <- !file.exists(py)
  need_install <- need_venv || is.null(marker) || !identical(marker$lock$sha256, lock_sha)
  if ((need_venv || need_install) && is.null(uv)) {
    return(blocked("`uv` was not found (looked at options(gambitR3D.uv), G3_UV, PATH and ~/.local/bin). Install uv from https://docs.astral.sh/uv/ and retry."))
  }
  tmp_log <- tempfile("g3-ml-setup-", fileext = ".log")
  on.exit(unlink(tmp_log), add = TRUE)
  t_install <- 0
  if (need_venv) {
    if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE))) {
      return(blocked(paste0("`", path, "` exists, is not empty and is not a virtual environment; choose an empty or new directory.")))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    args <- c("venv", path, "--python", python %||% .G3_ML_PYTHON, "--no-config")
    if (offline) args <- c(args, "--offline")
    env <- if (offline) c("current", UV_PYTHON_DOWNLOADS = "never") else NULL
    r <- .g3_run_logged(uv, args, tmp_log, timeout = 600, env = env)
    if (r$status != 0L || !file.exists(py)) {
      return(blocked(paste0("Creating the virtual environment failed: ", .g3_last_line(r$stderr)), .g3_keep_log(tmp_log)))
    }
    actions <- c(actions, "created_venv")
  }
  log <- file.path(path, "setup-log.txt")
  if (file.exists(tmp_log)) file.append(log, tmp_log)
  if (need_install) {
    t0 <- proc.time()[["elapsed"]]
    args <- c("pip", "sync", lock, "--python", py, "--require-hashes",
              "--index-url", .G3_ML_INDEX_URL, "--extra-index-url", .G3_ML_TORCH_INDEX, "--no-config")
    if (offline) args <- c(args, "--offline")
    r <- .g3_run_logged(uv, args, log, timeout = 3600)
    t_install <- proc.time()[["elapsed"]] - t0
    if (r$status != 0L) {
      why <- if (offline) "Offline installation failed (packages missing from the uv cache): " else "Installing the locked packages failed: "
      return(blocked(paste0(why, .g3_last_line(r$stderr)), log))
    }
    actions <- c(actions, "installed_lock")
  }
  # Probe the interpreter and packages.
  t0 <- proc.time()[["elapsed"]]
  probe <- .g3_ml_probe(py)
  t_probe <- proc.time()[["elapsed"]] - t0
  if (!is.null(probe$error)) return(blocked(paste0("The environment probe failed: ", probe$error), log))
  if (!startsWith(probe$python, paste0(.G3_ML_PYTHON, "."))) {
    return(blocked(paste0("The lock targets CPython ", .G3_ML_PYTHON, " but the environment runs ", probe$python, "."), log))
  }
  pk <- .g3_ml_packages(uv, py, log)
  # A real protocol handshake with the worker.
  t0 <- proc.time()[["elapsed"]]
  hello <- tryCatch({
    w <- .g3_worker_start(path, threads = 1L)
    h <- w$hello
    .g3_worker_stop(w)
    h
  }, error = function(e) e)
  t_hello <- proc.time()[["elapsed"]] - t0
  if (inherits(hello, "error")) return(blocked(paste0("Worker handshake failed: ", conditionMessage(hello)), log))
  marker <- list(
    schema = .G3_ML_ENV_SCHEMA,
    created = marker$created %||% .g3_now(),
    updated = .g3_now(),
    path = path,
    python = list(executable = py, version = probe$python),
    uv = if (is.null(uv)) NA_character_ else .g3_uv_version(uv),
    lock = list(file = basename(lock), sha256 = lock_sha, target = "CPython 3.12, Linux x86_64"),
    packages = pk,
    probe = probe,
    r_platform = R.version$platform
  )
  .g3_atomic_write(jsonlite::toJSON(marker, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
                   file.path(path, .G3_ML_MARKER))
  rep <- .g3_ml_report("ready", NA_character_, path, lock, lock_sha, actions, t_start, log = log)
  rep$python <- list(executable = py, version = probe$python)
  rep$packages <- data.frame(name = names(pk), version = unname(unlist(pk)), stringsAsFactors = FALSE)
  rep$torch <- list(version = probe$torch, device = "cpu", cuda_available = isTRUE(probe$cuda_available),
                    threads_default = probe$threads_default)
  rep$worker <- hello
  rep$schema_match <- identical(hello$feature_schema_hash, .g3_feature_schema()$hash)
  rep$timings <- list(total = proc.time()[["elapsed"]] - t_start, install = t_install, probe = t_probe, handshake = t_hello)
  rep
}

#' @export
print.g3_ml_report <- function(x, ...) {
  cat("<g3_ml_report> ", x$status, " at ", x$path, "\n", sep = "")
  if (!is.na(x$reason)) cat("  reason: ", x$reason, "\n", sep = "")
  if (identical(x$status, "ready")) {
    cat("  python ", x$python$version, ", torch ", x$torch$version, " (", x$torch$device, ")",
        ", worker protocol ", x$worker$protocol_version, ", schema match ", x$schema_match, "\n", sep = "")
  }
  invisible(x)
}

#' @noRd
.g3_ml_report <- function(status, reason, path, lock, lock_sha, actions, t_start, log = NA_character_) {
  structure(
    list(
      status = status, reason = reason, path = path,
      python = list(executable = .g3_venv_python(path), version = NA_character_),
      packages = data.frame(name = character(), version = character(), stringsAsFactors = FALSE),
      torch = list(version = NA_character_, device = NA_character_, cuda_available = NA, threads_default = NA_integer_),
      worker = NULL, schema_match = NA,
      lock = list(file = lock, sha256 = lock_sha, target = "CPython 3.12, Linux x86_64"),
      actions = actions,
      timings = list(total = proc.time()[["elapsed"]] - t_start),
      log = log,
      marker = file.path(path, .G3_ML_MARKER)
    ),
    class = "g3_ml_report"
  )
}

#' @noRd
.g3_need_processx <- function() {
  if (!requireNamespace("processx", quietly = TRUE)) {
    .g3_abort("g3_incompatible_model", "The ML worker needs the 'processx' package: install.packages('processx').")
  }
}

#' @noRd
.g3_venv_python <- function(path) {
  if (.Platform$OS.type == "windows") file.path(path, "Scripts", "python.exe") else file.path(path, "bin", "python")
}

#' @noRd
.g3_ml_lock_file <- function() {
  f <- system.file("python", "requirements-cpu.lock", package = "gambitR3D")
  if (!nzchar(f)) .g3_abort("g3_io_error", "The package's Python lock file is missing.")
  f
}

#' @noRd
.g3_worker_main <- function() {
  f <- system.file("python", "gambitr3d_worker", "__main__.py", package = "gambitR3D")
  if (!nzchar(f)) .g3_abort("g3_io_error", "The package's Python worker is missing.")
  f
}

# Refuse to place an environment inside the package (source or installed).
#' @noRd
.g3_ml_check_location <- function(path) {
  pkg <- normalizePath(system.file(package = "gambitR3D"), mustWork = FALSE)
  roots <- unique(c(pkg, dirname(pkg)))
  roots <- roots[file.exists(file.path(roots, "DESCRIPTION"))]
  for (r in roots) {
    if (identical(path, r) || startsWith(path, paste0(r, .Platform$file.sep))) {
      .g3_abort("g3_io_error", "Keep the ML environment outside the package directory.")
    }
  }
  invisible(TRUE)
}

#' @noRd
.g3_find_uv <- function() {
  home <- Sys.getenv("HOME")
  exe <- if (.Platform$OS.type == "windows") "uv.exe" else "uv"
  cands <- c(getOption("gambitR3D.uv"), Sys.getenv("G3_UV"), unname(Sys.which("uv")),
             if (nzchar(home)) file.path(home, ".local", "bin", exe), if (nzchar(home)) file.path(home, ".cargo", "bin", exe))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  for (c in cands) if (file.exists(c)) return(normalizePath(c))
  NULL
}

#' @noRd
.g3_uv_version <- function(uv) {
  r <- tryCatch(processx::run(uv, "--version", error_on_status = FALSE, timeout = 30), error = function(e) NULL)
  if (is.null(r) || r$status != 0L) NA_character_ else trimws(r$stdout)
}

# Run a command (argument vector, never a shell string) and append its
# output to a log file.
#' @noRd
.g3_run_logged <- function(cmd, args, log, timeout = 600, env = NULL) {
  r <- tryCatch(
    processx::run(cmd, args, error_on_status = FALSE, timeout = timeout, env = env, windows_hide_window = TRUE),
    error = function(e) list(status = -1L, stdout = "", stderr = conditionMessage(e))
  )
  cat(paste0("[", .g3_now(), "] ", cmd, " ", paste(args, collapse = " ")), r$stdout, r$stderr,
      file = log, sep = "\n", append = TRUE)
  r
}

#' @noRd
.g3_last_line <- function(txt) {
  l <- strsplit(trimws(txt %||% ""), "\n", fixed = TRUE)[[1L]]
  if (!length(l)) "no output" else l[length(l)]
}

#' @noRd
.g3_keep_log <- function(tmp) {
  keep <- tempfile("g3-ml-setup-failed-", fileext = ".log")
  if (file.exists(tmp)) file.copy(tmp, keep)
  keep
}

#' @noRd
.g3_ml_probe <- function(py) {
  r <- tryCatch(processx::run(py, c("-I", "-B", .g3_worker_main(), "--probe"), error_on_status = FALSE, timeout = 300),
                error = function(e) list(status = -1L, stdout = "", stderr = conditionMessage(e)))
  if (r$status != 0L) return(list(error = .g3_last_line(r$stderr)))
  out <- tryCatch(jsonlite::fromJSON(.g3_last_line(r$stdout)), error = function(e) NULL)
  if (is.null(out)) return(list(error = "unreadable probe output"))
  out
}

#' @noRd
.g3_ml_packages <- function(uv, py, log) {
  if (is.null(uv)) return(list())
  r <- .g3_run_logged(uv, c("pip", "freeze", "--python", py, "--no-config"), log, timeout = 120)
  if (r$status != 0L) return(list())
  l <- strsplit(trimws(r$stdout), "\n", fixed = TRUE)[[1L]]
  l <- l[grepl("==", l, fixed = TRUE)]
  kv <- strsplit(l, "==", fixed = TRUE)
  stats::setNames(lapply(kv, `[`, 2L), vapply(kv, `[`, "", 1L))
}

#' @noRd
.g3_ml_read_marker <- function(path) {
  f <- file.path(path, .G3_ML_MARKER)
  if (!file.exists(f)) return(NULL)
  m <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(m) || !identical(m$schema, .G3_ML_ENV_SCHEMA)) return(NULL)
  m
}

# Locate a configured environment without starting Python. Returns the
# path, or NULL when none is configured (`required = FALSE`).
#' @noRd
.g3_ml_env_path <- function(path = NULL, required = TRUE) {
  cands <- c(path, getOption("gambitR3D.ml_env"), Sys.getenv("G3_ML_ENV"))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  explicit <- length(cands) > 0L
  if (!explicit) cands <- file.path(tools::R_user_dir("gambitR3D", which = "data"), "ml-env")
  p <- normalizePath(path.expand(cands[1L]), mustWork = FALSE)
  ok <- file.exists(.g3_venv_python(p)) && !is.null(.g3_ml_read_marker(p))
  if (ok) return(p)
  if (!required) return(NULL)
  .g3_abort(
    "g3_incompatible_model",
    paste0("No configured ML environment", if (explicit) paste0(" at ", p) else "",
           ". Run g3_setup_ml(path) once and set options(gambitR3D.ml_env = path) or the G3_ML_ENV variable."),
    ml_env = p
  )
}

# Runtime availability for capability reports (file checks only).
#' @noRd
.g3_ml_runtime_status <- function() {
  if (!requireNamespace("processx", quietly = TRUE)) {
    return(list(available = FALSE, reason = "the 'processx' package is not installed"))
  }
  p <- .g3_ml_env_path(required = FALSE)
  if (is.null(p)) return(list(available = FALSE, reason = "no ML environment configured (see g3_setup_ml())"))
  list(available = TRUE, reason = NA_character_, path = p)
}
