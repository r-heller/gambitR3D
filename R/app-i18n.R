# Externalised UI messages.
#
# The app's words live in JSON catalogues under inst/app/i18n/ (`en.json`
# is the default and the reference; `de.json` translates it). Keys are
# dotted names such as "nav.play". Placeholders use braces: "{side} to act".
#
# Static UI text is rendered in R in the session's language and marked with
# `data-i18n` (text) or `data-i18n-attr` ("aria-label:key;title:key")
# attributes, so the browser can switch language without rebuilding the
# page. Dynamic outputs are rendered by the server with `.g3_t()` in the
# current language. A key missing from a translation falls back to English;
# a key missing from English is shown as the key itself so it is noticed.

.G3_LANGUAGES <- c(en = "English", de = "Deutsch")

#' @noRd
.g3_i18n_dir <- function() {
  system.file("app", "i18n", package = "gambitR3D")
}

# Read and cache a catalogue as a named character vector.
#' @noRd
.g3_i18n_catalogue <- function(lang = "en") {
  lang <- if (lang %in% names(.G3_LANGUAGES)) lang else "en"
  key <- paste0("i18n:", lang)
  if (!is.null(.g3_env[[key]])) {
    return(.g3_env[[key]])
  }
  path <- file.path(.g3_i18n_dir(), paste0(lang, ".json"))
  if (!file.exists(path)) {
    .g3_abort("g3_io_error", paste0("Missing UI catalogue: ", path))
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  cat <- unlist(raw$messages)
  if (lang != "en") {
    en <- .g3_i18n_catalogue("en")
    missing <- setdiff(names(en), names(cat))
    cat <- c(cat, en[missing])
  }
  .g3_env[[key]] <- cat
  cat
}

# Translate one key, interpolating `{name}` placeholders from `...`.
#' @noRd
.g3_t <- function(key, lang = "en", ...) {
  cat <- .g3_i18n_catalogue(lang)
  txt <- cat[key]
  txt <- if (is.na(txt)) key else unname(txt)
  vars <- list(...)
  for (nm in names(vars)) {
    # A missing value prints as empty text rather than failing the output.
    val <- vars[[nm]]
    val <- if (is.null(val) || !length(val)) "" else paste(ifelse(is.na(val), "NA", as.character(val)), collapse = ", ")
    txt <- gsub(paste0("{", nm, "}"), val, txt, fixed = TRUE)
  }
  txt
}

# A span whose text the browser can re-translate in place.
#' @noRd
.g3_txt <- function(key, lang = "en", tag = htmltools::span, ...) {
  tag(`data-i18n` = key, .g3_t(key, lang), ...)
}

# Attribute translations: `.g3_i18n_attr(c("aria-label" = "key"))` returns
# the attributes plus a `data-i18n-attr` marker for the browser.
#' @noRd
.g3_i18n_attr <- function(map, lang = "en") {
  attrs <- lapply(map, function(k) .g3_t(k, lang))
  attrs[["data-i18n-attr"]] <- paste(paste0(names(map), ":", unname(map)), collapse = ";")
  attrs
}
