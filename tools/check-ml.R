# Optional CPU training qualification for the installed package.
if (!identical(Sys.getenv("NOT_CRAN"), "true")) stop("Set NOT_CRAN=true for explicit ML qualification.")
if (!identical(Sys.info()[["sysname"]], "Linux") || !identical(Sys.info()[["machine"]], "x86_64")) {
  stop("The committed CPU lock is qualified only for Linux x86_64; use that platform.")
}
lib <- Sys.getenv("GAMBITR3D_APP_LIB")
base <- Sys.getenv("GAMBITR3D_ML_DIR")
if (!nzchar(lib) || !nzchar(base)) stop("Set GAMBITR3D_APP_LIB and GAMBITR3D_ML_DIR explicitly.")
lib <- normalizePath(lib, mustWork = TRUE)
.libPaths(c(lib, .libPaths()))
base::library(gambitR3D, lib.loc = lib)
expected <- normalizePath(file.path(lib, "gambitR3D"), mustWork = TRUE)
if (!identical(normalizePath(getNamespaceInfo(asNamespace("gambitR3D"), "path")), expected)) {
  stop("The loaded namespace is not the requested installation.")
}
for (package in c("testthat", "processx", "callr", "withr", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Missing ML test dependency: ", package)
}
if (!nzchar(Sys.which("uv")) || !nzchar(Sys.which("python"))) stop("Pinned uv and Python3.12 must be available.")
# The Linux R launcher can prepend system library directories. Preserve the
# caller's setup-python loader path for child processes so its interpreter and
# extension modules use the same libpython instead of Ubuntu's older one.
loader_path <- Sys.getenv("G3_ML_LD_LIBRARY_PATH", unset = NA_character_)
if (!is.na(loader_path)) Sys.setenv(LD_LIBRARY_PATH = loader_path)
python_probe <- processx::run(Sys.which("python"), c("-I", "-c",
  "import ctypes, json, sys; print(json.dumps({'executable': sys.executable, 'version': sys.version.split()[0]}))"))
python_identity <- jsonlite::fromJSON(python_probe$stdout)
expected_python <- Sys.getenv("G3_ML_PYTHON_VERSION")
if (nzchar(expected_python) && !identical(python_identity$version, expected_python)) {
  stop("The Python child runtime differs from the selected interpreter: ", python_identity$version)
}
dir.create(base, recursive = TRUE, showWarnings = FALSE)
runtime <- tempfile("run-", tmpdir = normalizePath(base))
dir.create(runtime)
for (name in c("data", "config", "cache", "registry")) dir.create(file.path(runtime, name))
Sys.setenv(R_USER_DATA_DIR = file.path(runtime, "data"),
           R_USER_CONFIG_DIR = file.path(runtime, "config"),
           R_USER_CACHE_DIR = file.path(runtime, "cache"),
           G3_MODEL_REGISTRY = file.path(runtime, "registry"),
           G3_ML_TEST_EVIDENCE = file.path(runtime, "test-evidence"),
           G3_ML_ENV = file.path(runtime, "python"))
cat("Installed package:", expected, "\nEvidence:", runtime, "\n")
capture.output(sessionInfo(), file = file.path(runtime, "session-info.txt"))
writeLines(python_probe$stdout, file.path(runtime, "python-identity.json"))
setup <- g3_setup_ml(Sys.getenv("G3_ML_ENV"), python = Sys.which("python"))
saveRDS(setup, file.path(runtime, "setup.rds"))
jsonlite::write_json(unclass(setup), file.path(runtime, "setup.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
if (!identical(setup$status, "ready")) stop("CPU ML environment is not ready: ", setup$reason)
suites <- c("ml-integration", "ml-train")
result <- testthat::test_package("gambitR3D", filter = paste0("^(", paste(suites, collapse = "|"), ")$"),
                                 reporter = "summary", stop_on_failure = FALSE)
saveRDS(result, file.path(runtime, "results.rds"))
summary <- as.data.frame(result)
columns <- c("file", "test", "failed", "skipped", "error", "warning", "passed")
if (!all(columns %in% names(summary))) stop("Incomplete test results; refusing to infer success.")
utils::write.csv(summary[columns], file.path(runtime, "results.csv"), row.names = FALSE)
if (!nrow(summary) || !all(paste0("test-", suites, ".R") %in% summary$file)) stop("A required ML suite did not run.")
if (sum(summary$file == "test-ml-integration.R") < 9L) stop("The nine required native worker scenarios did not all run.")
if (anyNA(summary[columns[-c(1, 2)]]) || any(summary$failed > 0 | summary$error |
    summary$skipped | summary$warning > 0 | summary$passed < 1)) {
  stop("ML qualification requires assertions in every test and no failures, errors, warnings or skips.")
}
cat("PASS:", nrow(summary), "tests;", sum(summary$passed), "assertions; all required ML scenarios executed.\n")
