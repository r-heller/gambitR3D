# Typed conditions.
#
# Every error raised by the public API carries one of the classes below, plus
# `g3_error`, so callers can catch a precise failure. Context fields are small
# and safe: ids, hashes and counts, never full private game dumps.

.G3_CONDITION_CLASSES <- c(
  "g3_invalid_state", "g3_illegal_action", "g3_stale_revision",
  "g3_unsupported_rules", "g3_incompatible_model", "g3_budget_exceeded",
  "g3_io_error"
)

#' @noRd
.g3_abort <- function(class, message, ..., call = NULL) {
  stopifnot(class %in% .G3_CONDITION_CLASSES)
  cond <- structure(
    class = c(class, "g3_error", "error", "condition"),
    list(message = message, call = call, context = list(...))
  )
  stop(cond)
}

#' @noRd
.g3_warn <- function(message, ...) {
  cond <- structure(
    class = c("g3_warning", "warning", "condition"),
    list(message = message, call = NULL, context = list(...))
  )
  warning(cond)
}
