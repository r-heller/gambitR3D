# Minimal safetensors reader/writer in base R.
#
# A safetensors file is an 8-byte little-endian header length, a JSON
# header mapping tensor names to dtype, shape and byte offsets, and the raw
# little-endian tensor bytes in row-major order. R writes training shards
# in this format and reads checkpoints back to verify them; no executable
# deserialisation is involved on either side.
#
# The weights digest framing matches the worker's `tensor_digest()`: for
# each tensor in sorted-name order, `name NUL dtype NUL shape NUL bytes`.

.G3_ST_SIZES <- c(F32 = 4L, I32 = 4L, U8 = 1L, F64 = 8L, I64 = 8L, BOOL = 1L)

# tensors: named list of list(dtype = "I32" | "F32" | "U8", shape, data),
# with data in row-major order.
#' @noRd
.g3_write_safetensors <- function(tensors, path, metadata = NULL) {
  nms <- names(tensors)
  if (is.null(nms) || anyDuplicated(nms) || any(!nzchar(nms))) {
    .g3_abort("g3_io_error", "Tensors need unique non-empty names.")
  }
  blobs <- vector("list", length(tensors))
  header <- list()
  off <- 0
  for (i in seq_along(tensors)) {
    t <- tensors[[i]]
    n <- prod(t$shape)
    if (length(t$data) != n) .g3_abort("g3_io_error", paste0("Tensor ", nms[i], " has ", length(t$data), " values for shape ", paste(t$shape, collapse = "x")))
    blob <- switch(
      t$dtype,
      I32 = writeBin(as.integer(t$data), raw(), size = 4L, endian = "little"),
      F32 = {
        if (any(!is.finite(t$data))) .g3_abort("g3_io_error", paste0("Tensor ", nms[i], " is not finite."))
        writeBin(as.double(t$data), raw(), size = 4L, endian = "little")
      },
      U8 = as.raw(t$data),
      .g3_abort("g3_io_error", paste0("Unsupported dtype ", t$dtype))
    )
    blobs[[i]] <- blob
    header[[nms[i]]] <- list(dtype = t$dtype, shape = as.list(as.integer(t$shape)),
                             data_offsets = list(as.integer(off), as.integer(off + length(blob))))
    off <- off + length(blob)
  }
  if (!is.null(metadata)) header[["__metadata__"]] <- lapply(metadata, as.character)
  h <- as.character(jsonlite::toJSON(header, auto_unbox = TRUE, digits = NA))
  h <- charToRaw(enc2utf8(h))
  pad <- (8L - length(h) %% 8L) %% 8L
  h <- c(h, rep(charToRaw(" "), pad))
  len <- length(h)
  lenbytes <- writeBin(as.integer(c(len %% 2^32, len %/% 2^32)), raw(), size = 4L, endian = "little")
  bytes <- c(lenbytes, h, unlist(blobs, use.names = FALSE))
  dir <- dirname(path)
  tmp <- tempfile(".g3st-", tmpdir = dir)
  con <- file(tmp, "wb")
  writeBin(bytes, con)
  close(con)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    .g3_abort("g3_io_error", paste0("Could not write ", path))
  }
  invisible(digest::digest(file = path, algo = "sha256"))
}

# Read a safetensors file. With decode = FALSE only raw bytes are returned
# (enough for digests). Returns a named list of list(dtype, shape, raw,
# data).
#' @noRd
.g3_read_safetensors <- function(path, decode = TRUE, max_bytes = 2^31) {
  size <- file.size(path)
  if (is.na(size) || size < 8) .g3_abort("g3_io_error", paste0("Not a safetensors file: ", path))
  if (size > max_bytes) .g3_abort("g3_budget_exceeded", paste0("Tensor file too large: ", path))
  con <- file(path, "rb")
  on.exit(close(con))
  lb <- readBin(con, "integer", n = 2L, size = 4L, endian = "little")
  hlen <- (lb[1L] %% 2^32) + lb[2L] * 2^32
  if (hlen <= 0 || hlen > size - 8) .g3_abort("g3_io_error", paste0("Corrupt safetensors header: ", path))
  header <- jsonlite::fromJSON(rawToChar(readBin(con, "raw", n = hlen)), simplifyVector = FALSE)
  body <- readBin(con, "raw", n = size - 8 - hlen)
  nms <- setdiff(names(header), "__metadata__")
  out <- list()
  for (nm in nms) {
    h <- header[[nm]]
    o <- unlist(h$data_offsets)
    raw <- if (o[2L] > o[1L]) body[(o[1L] + 1):o[2L]] else raw()
    shape <- as.integer(unlist(h$shape))
    if (length(raw) != prod(shape) * .G3_ST_SIZES[[h$dtype]]) .g3_abort("g3_io_error", paste0("Tensor ", nm, " has the wrong byte length."))
    data <- NULL
    if (decode) {
      n <- prod(shape)
      data <- switch(
        h$dtype,
        F32 = readBin(raw, "double", n = n, size = 4L, endian = "little"),
        I32 = readBin(raw, "integer", n = n, size = 4L, endian = "little"),
        U8 = as.integer(raw),
        BOOL = as.logical(as.integer(raw)),
        F64 = readBin(raw, "double", n = n, size = 8L, endian = "little"),
        NULL
      )
    }
    out[[nm]] <- list(dtype = h$dtype, shape = shape, raw = raw, data = data)
  }
  attr(out, "metadata") <- header[["__metadata__"]]
  out
}

# Weights digest of a safetensors file (same framing as the worker).
#' @noRd
.g3_weights_digest <- function(path) {
  t <- .g3_read_safetensors(path, decode = FALSE)
  nms <- sort(names(t), method = "radix")
  parts <- lapply(nms, function(nm) {
    x <- t[[nm]]
    c(charToRaw(enc2utf8(nm)), as.raw(0L), charToRaw(x$dtype), as.raw(0L),
      charToRaw(paste(x$shape, collapse = ",")), as.raw(0L), x$raw)
  })
  digest::digest(unlist(parts, use.names = FALSE), algo = "sha256", serialize = FALSE)
}

#' @noRd
.g3_file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")
