# Canonical JSON and hashing.
#
# Hashes are computed from a canonical JSON text: object keys sorted
# recursively, no whitespace, integers written as integers and doubles with
# full precision. Pretty-printing or key order in the input never changes a
# hash. Runtime object serialisation is never hashed.

# Sort the names of every named list, recursively. Unnamed lists keep their
# order, because array order is meaningful.
#' @noRd
.g3_sort_keys <- function(x) {
  if (is.data.frame(x)) {
    x <- as.list(x)
  }
  if (is.list(x)) {
    if (!is.null(names(x)) && length(x)) {
      x <- x[order(names(x), method = "radix")]
    }
    x <- lapply(x, .g3_sort_keys)
  }
  x
}

#' @noRd
.g3_canonical_json <- function(x) {
  txt <- jsonlite::toJSON(
    .g3_sort_keys(x),
    auto_unbox = TRUE, digits = NA, null = "null", na = "null",
    dataframe = "columns"
  )
  as.character(txt)
}

#' @noRd
.g3_sha256 <- function(text) {
  digest::digest(text, algo = "sha256", serialize = FALSE)
}

#' @noRd
.g3_hash_object <- function(x) {
  .g3_sha256(.g3_canonical_json(x))
}

# Small helpers used throughout.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
.g3_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

#' @noRd
.g3_new_id <- function(prefix = "id") {
  # Random hex id; the RNG state of the caller is not disturbed.
  hex <- digest::digest(
    paste(Sys.time(), Sys.getpid(), stats::runif(1), sep = "|"),
    algo = "md5", serialize = FALSE
  )
  paste0(prefix, "_", substr(hex, 1L, 16L))
}

# Write a file atomically: write to a temporary sibling, then rename.
#' @noRd
.g3_atomic_write <- function(text, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    .g3_abort("g3_io_error", paste0("Directory does not exist: ", dir))
  }
  tmp <- tempfile(pattern = ".g3tmp-", tmpdir = dir)
  ok <- FALSE
  on.exit(if (!ok && file.exists(tmp)) unlink(tmp), add = TRUE)
  con <- file(tmp, open = "wb")
  writeBin(charToRaw(enc2utf8(text)), con)
  close(con)
  if (!file.rename(tmp, path)) {
    .g3_abort("g3_io_error", paste0("Could not move the written file into place: ", path))
  }
  ok <- TRUE
  invisible(path)
}
