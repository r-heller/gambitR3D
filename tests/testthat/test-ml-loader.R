# The Python worker loads its interpreter's own shared libpython on every
# launch path. On Linux, R's start-up script puts R's library directories
# (often including the system one, which may hold an older libpython of the
# same soname) in front of LD_LIBRARY_PATH in every R process it starts,
# background jobs included. These checks use a stand-in interpreter (a shell
# script), so they need neither Python nor PyTorch.

skip_unless_linux <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"), "LD_LIBRARY_PATH handling is specific to Linux")
  skip_if_not_installed("processx")
}

# A provisioned-looking ML environment whose `bin/python` answers the loader
# query like a CPython built with a shared libpython, while an older system
# copy is the one mapped. Started as the worker, it appends its
# LD_LIBRARY_PATH to `record`, reads the hello request and exits.
fake_shared_python <- function(root, mapped = TRUE, recorded = FALSE) {
  prefix <- file.path(root, "toolcache", "Python", "3.12.16", "x64")
  libdir <- file.path(prefix, "lib")
  dir.create(libdir, recursive = TRUE)
  file.create(file.path(libdir, "libpython3.12.so.1.0"))
  env <- file.path(root, "ml-env")
  dir.create(file.path(env, "bin"), recursive = TRUE)
  record <- file.path(root, "worker-loader-paths.txt")
  script <- r"(#!/bin/sh
for arg in "$@"; do
  if [ "$arg" = "-c" ]; then
    printf 'base_exec_prefix\t%s\n' '@PREFIX@'
    printf 'libdir\t%s\n' '/opt/hostedtoolcache/Python/3.12.16/x64/lib'
    printf 'soname\t%s\n' 'libpython3.12.so.1.0'
    @MAPPED@
    exit 0
  fi
done
printf '%s\n' "${LD_LIBRARY_PATH-}" >> '@RECORD@'
read -r request
exit 1
)"
  mapped_line <- if (mapped) r"(printf 'mapped\t%s\n' '/usr/lib/x86_64-linux-gnu/libpython3.12.so.1.0')" else ":"
  script <- sub("@PREFIX@", prefix, script, fixed = TRUE)
  script <- sub("@MAPPED@", mapped_line, script, fixed = TRUE)
  script <- sub("@RECORD@", record, script, fixed = TRUE)
  py <- file.path(env, "bin", "python")
  writeLines(script, py)
  Sys.chmod(py, "0755")
  python <- list(executable = py, version = "3.12.16")
  if (recorded) python$libdir <- libdir
  writeLines(jsonlite::toJSON(list(schema = .G3_ML_ENV_SCHEMA, python = python), auto_unbox = TRUE),
             file.path(env, .G3_ML_MARKER))
  list(env = env, py = py, libdir = libdir, record = record)
}

# `libdir` followed by the other entries of `path`, as the worker should see it.
libdir_first <- function(libdir, path) {
  rest <- strsplit(path, ":", fixed = TRUE)[[1L]]
  paste(c(libdir, rest[nzchar(rest) & rest != libdir]), collapse = ":")
}

test_that("the loader query selects the interpreter's own library directory", {
  root <- withr::local_tempdir()
  runtime <- file.path(root, "runtime")
  built <- file.path(root, "built")
  dir.create(file.path(runtime, "lib"), recursive = TRUE)
  dir.create(file.path(built, "lib"), recursive = TRUE)
  so <- "libpython3.12.so.1.0"
  answer <- function(prefix, libdir, mapped = "/usr/lib/x86_64-linux-gnu/libpython3.12.so.1.0") {
    paste0(c(paste0("base_exec_prefix\t", prefix), paste0("libdir\t", libdir), paste0("soname\t", so),
             if (length(mapped)) paste0("mapped\t", mapped)), "\n", collapse = "")
  }
  # A relocated installation: the build-time LIBDIR has no library, the
  # run-time prefix has.
  file.create(file.path(runtime, "lib", so))
  expect_identical(.g3_python_libdir_from(answer(runtime, "/opt/hostedtoolcache/Python/3.12.16/x64/lib")),
                   file.path(runtime, "lib"))
  # The run-time prefix wins over another LIBDIR that also has one.
  file.create(file.path(built, "lib", so))
  expect_identical(.g3_python_libdir_from(answer(runtime, file.path(built, "lib"))), file.path(runtime, "lib"))
  # A distribution layout: the library lives in LIBDIR, not under the prefix.
  expect_identical(.g3_python_libdir_from(answer(file.path(root, "none"), file.path(built, "lib"))),
                   file.path(built, "lib"))
  # A statically linked interpreter maps no libpython: nothing to change.
  expect_true(is.na(.g3_python_libdir_from(answer(runtime, file.path(built, "lib"), mapped = NULL))))
  # No library where the interpreter says it is, or no usable answer.
  expect_true(is.na(.g3_python_libdir_from(answer(file.path(root, "none"), file.path(root, "none")))))
  expect_true(is.na(.g3_python_libdir_from("")))
  expect_true(is.na(.g3_python_libdir_from(NULL)))
  expect_true(is.na(.g3_python_libdir_from("Traceback (most recent call last):\n")))
})

test_that("the worker environment puts the library directory first and keeps the rest", {
  lib <- "/opt/hostedtoolcache/Python/3.12.16/x64/lib"
  r_child <- paste("/opt/R/4.6.1/lib/R/lib", "/usr/local/lib", "/usr/lib/x86_64-linux-gnu",
                   "/usr/lib/jvm/temurin-17-jdk-amd64/lib/server", lib, sep = ":")
  env <- .g3_python_env(lib, OMP_NUM_THREADS = 1L, PYTHONHASHSEED = "0", loader_path = r_child)
  expect_identical(env[[1L]], "current")
  expect_identical(names(env)[-1L], c("OMP_NUM_THREADS", "PYTHONHASHSEED", "LD_LIBRARY_PATH"))
  expect_identical(unname(env[["OMP_NUM_THREADS"]]), "1")
  expect_identical(unname(env[["LD_LIBRARY_PATH"]]),
                   paste(lib, "/opt/R/4.6.1/lib/R/lib", "/usr/local/lib", "/usr/lib/x86_64-linux-gnu",
                         "/usr/lib/jvm/temurin-17-jdk-amd64/lib/server", sep = ":"))
  # Empty entries (the working directory) are dropped; an empty path gives the directory alone.
  expect_identical(unname(.g3_python_env(lib, loader_path = "/a::/b:")[["LD_LIBRARY_PATH"]]),
                   paste(lib, "/a", "/b", sep = ":"))
  expect_identical(unname(.g3_python_env(lib, loader_path = "")[["LD_LIBRARY_PATH"]]), lib)
  # Without a directory the loader path is left alone.
  expect_identical(.g3_python_env(NA_character_, A = "1"), c("current", A = "1"))
  expect_false("LD_LIBRARY_PATH" %in% names(.g3_python_env(NULL, A = "1")))
  expect_null(.g3_python_env(NA_character_))
})

test_that("the library directory comes from the marker, or from the interpreter when not recorded", {
  skip_unless_linux()
  # Not recorded (an environment set up by an earlier version): the
  # interpreter is asked.
  fake <- fake_shared_python(withr::local_tempdir())
  expect_identical(.g3_python_libdir(fake$py), fake$libdir)
  expect_identical(.g3_ml_libdir(fake$env), fake$libdir)
  static <- fake_shared_python(withr::local_tempdir(), mapped = FALSE)
  expect_true(is.na(.g3_python_libdir(static$py)))
  expect_true(is.na(.g3_ml_libdir(static$env)))
  expect_true(is.na(.g3_python_libdir(file.path(static$env, "bin", "missing"))))
  set_recorded <- function(env, libdir) {
    marker <- file.path(env, .G3_ML_MARKER)
    m <- jsonlite::fromJSON(marker, simplifyVector = FALSE)
    m$python["libdir"] <- list(libdir)
    writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, null = "null"), marker)
  }
  # A recorded directory is used without asking the interpreter.
  set_recorded(static$env, fake$libdir)
  expect_identical(.g3_ml_libdir(static$env), fake$libdir)
  # A recorded null means that none is needed.
  set_recorded(fake$env, NULL)
  expect_true(is.na(.g3_ml_libdir(fake$env)))
  # A recorded directory that has gone is looked up again.
  set_recorded(fake$env, file.path(fake$env, "gone"))
  expect_identical(.g3_ml_libdir(fake$env), fake$libdir)
})

test_that("a foreground worker starts with its library directory first", {
  skip_unless_linux()
  fake <- fake_shared_python(withr::local_tempdir())
  # As in an R session started by R's launcher script: the system directory
  # holding an older libpython comes before the interpreter's own.
  parent <- paste("/usr/lib/x86_64-linux-gnu", file.path(tempdir(), "g3-sentinel"), fake$libdir, sep = ":")
  withr::local_envvar(LD_LIBRARY_PATH = parent)
  expect_error(.g3_worker_start(fake$env, startup_timeout = 30), "exited unexpectedly", class = "g3_io_error")
  seen <- readLines(fake$record)
  expect_length(seen, 1L)
  expect_identical(seen, libdir_first(fake$libdir, parent))
  expect_identical(strsplit(seen, ":", fixed = TRUE)[[1L]][1L], fake$libdir)
})

test_that("a background job starts its worker with the same library directory first", {
  skip_unless_linux()
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  fake <- fake_shared_python(root, recorded = TRUE)
  # As set up by actions/setup-python: only the interpreter's directory.
  withr::local_envvar(LD_LIBRARY_PATH = fake$libdir)
  # What an R child process (started like a job, through R's launcher)
  # has on its loader path before the package adds anything.
  child_path <- callr::r(function() Sys.getenv("LD_LIBRARY_PATH"))
  ds <- .g3_ml_build_dataset(list(games = 0L, positions = 0L, platform_positions = 0L, fixtures = TRUE,
                                  fixture_nodes = 200L, fixture_max_plies = 4L), file.path(root, "data"))
  cfg <- list(steps = 1L, ml_env = fake$env, registry_dir = file.path(root, "reg"))
  job <- g3_start_job(list(type = "training", job_dir = file.path(root, "jobs"),
                           args = list(config = cfg, dataset = ds$manifest_path, output_dir = file.path(root, "run"))))
  withr::defer(if (job$process$is_alive()) job$process$kill_tree())
  t0 <- Sys.time()
  repeat {
    s <- g3_job_status(job)
    if (s$status %in% .G3_JOB_TERMINAL || Sys.time() - t0 > 120) break
    Sys.sleep(0.1)
  }
  expect_identical(s$status, "failed")
  expect_match(s$error, "exited unexpectedly", fixed = TRUE)
  seen <- readLines(fake$record)
  expect_length(seen, 1L)
  expect_identical(seen, libdir_first(fake$libdir, child_path))
  expect_identical(strsplit(seen, ":", fixed = TRUE)[[1L]][1L], fake$libdir)
})
