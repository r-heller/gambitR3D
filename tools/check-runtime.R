# Qualify the installed R worker, app state and real-browser surfaces.
required_flags <- c("NOT_CRAN", "GAMBITR3D_SLOW_TESTS", "GAMBITR3D_BROWSER_TESTS")
if (!all(Sys.getenv(required_flags) == "true")) {
  stop("Set NOT_CRAN, GAMBITR3D_SLOW_TESTS and GAMBITR3D_BROWSER_TESTS to true.")
}
lib <- Sys.getenv("GAMBITR3D_APP_LIB")
if (!nzchar(lib)) stop("GAMBITR3D_APP_LIB must name the explicitly installed library.")
lib <- normalizePath(lib, mustWork = TRUE)
.libPaths(c(lib, .libPaths()))
base::library(gambitR3D, lib.loc = lib)
expected <- normalizePath(file.path(lib, "gambitR3D"), mustWork = TRUE)
if (!identical(normalizePath(getNamespaceInfo(asNamespace("gambitR3D"), "path")), expected)) {
  stop("The loaded gambitR3D namespace is not the requested installation.")
}
for (package in c("testthat", "shinytest2", "chromote", "shiny", "bslib", "callr", "processx", "withr")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Missing runtime dependency: ", package)
}
chrome <- chromote::find_chrome()
if (is.null(chrome) || !length(chrome) || !file.exists(chrome)) stop("Chrome is required; browser tests must not skip.")
base <- Sys.getenv("GAMBITR3D_RUNTIME_DIR", unset = tempdir())
dir.create(base, recursive = TRUE, showWarnings = FALSE)
runtime <- tempfile("run-", tmpdir = normalizePath(base))
dir.create(runtime)
for (name in c("data", "config", "cache", "registry", "evidence")) dir.create(file.path(runtime, name))
Sys.setenv(R_USER_DATA_DIR = file.path(runtime, "data"),
           R_USER_CONFIG_DIR = file.path(runtime, "config"),
           R_USER_CACHE_DIR = file.path(runtime, "cache"),
           G3_MODEL_REGISTRY = file.path(runtime, "registry"),
           G3_ML_ENV = file.path(runtime, "unprovisioned-ml"),
           GAMBITR3D_EVIDENCE_DIR = file.path(runtime, "evidence"))
cat("Installed package:", expected, "\nEvidence:", runtime, "\n")
print(system2(chrome, "--version", stdout = TRUE, stderr = TRUE))
print(sessionInfo())
suites <- c("app-browser", "app-jobs", "app-sections", "jobs", "app-server", "app-dispatch")
result <- testthat::test_package("gambitR3D", filter = paste0("^(", paste(suites, collapse = "|"), ")$"),
                                 reporter = "summary", stop_on_failure = FALSE)
saveRDS(result, file.path(runtime, "results.rds"))
summary <- as.data.frame(result)
columns <- c("file", "test", "failed", "skipped", "error", "warning", "passed")
if (!all(columns %in% names(summary))) stop("Incomplete test results; refusing to infer success.")
utils::write.csv(summary[columns], file.path(runtime, "results.csv"), row.names = FALSE)
if (!all(paste0("test-", suites, ".R") %in% summary$file) || !nrow(summary)) {
  stop("One or more required runtime suites did not run.")
}
if (anyNA(summary[columns[-c(1, 2)]]) || any(summary$failed > 0 | summary$error |
    summary$skipped | summary$warning > 0 | summary$passed < 1)) {
  stop("Runtime qualification requires assertions in every test and no failures, errors, warnings or skips.")
}
cat("PASS:", nrow(summary), "tests;", sum(summary$passed), "assertions; all required runtime suites executed.\n")
