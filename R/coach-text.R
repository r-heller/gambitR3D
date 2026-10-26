# Coaching text catalogues.
#
# Every sentence the coach shows comes from a versioned template catalogue
# under inst/coach/ (`templates-en.json`, `templates-de.json`). A template
# is plain text with `{name}` placeholders; values are always engine facts
# (square names, piece names, numbers) computed elsewhere. No free text is
# generated, so the same facts always give the same sentence, and a German
# sentence carries exactly the same placeholders as its English original.
#
# Catalogue layout (schema `g3-coach-templates/1`):
#
#   lang, language_name
#   terms       short words: piece names, colours, themes, statuses
#   templates   full sentences keyed by dotted ids, e.g. "fact.check_given"
#
# English is the default; any other supported language must define every
# key of the English catalogue with the same placeholder set. This is
# checked by `.g3_coach_catalogue_problems()` and by the tests.

.G3_COACH_TEMPLATE_SCHEMA <- "g3-coach-templates/1"
.G3_COACH_LANGS <- c("en", "de")

# Load (once) and return a template catalogue.
#' @noRd
.g3_coach_catalogue <- function(lang = "en") {
  lang <- .g3_coach_lang(lang)
  key <- paste0("coach_templates:", lang)
  if (!is.null(.g3_env[[key]])) {
    return(.g3_env[[key]])
  }
  path <- system.file("coach", paste0("templates-", lang, ".json"), package = "gambitR3D")
  if (!nzchar(path)) {
    .g3_abort("g3_io_error", paste0("Coaching templates missing for language '", lang, "'."))
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(raw$schema, .G3_COACH_TEMPLATE_SCHEMA) || !identical(raw$lang, lang)) {
    .g3_abort("g3_io_error", paste0("Unexpected coaching template file for '", lang, "'."))
  }
  cat <- list(
    lang = lang,
    terms = vapply(raw$terms, as.character, ""),
    templates = vapply(raw$templates, as.character, "")
  )
  .g3_env[[key]] <- cat
  cat
}

#' @noRd
.g3_coach_lang <- function(lang) {
  if (!is.character(lang) || length(lang) != 1L || is.na(lang) || !lang %in% .G3_COACH_LANGS) {
    .g3_abort("g3_invalid_state", paste0(
      "Unsupported coaching language '", format(lang)[1L], "'. Use one of: ",
      paste(.G3_COACH_LANGS, collapse = ", "), "."
    ))
  }
  lang
}

# Render one template. Named arguments fill `{name}` placeholders; values
# are converted with as.character(). A missing value is an error, never a
# silently empty sentence.
#' @noRd
.g3_tr <- function(.key, .lang = "en", ...) {
  # Dotted formals: placeholder names such as `k` or `to` must never be
  # partially matched to an argument of this function.
  cat <- .g3_coach_catalogue(.lang)
  txt <- cat$templates[.key]
  if (is.na(txt)) {
    .g3_abort("g3_io_error", paste0("Missing coaching template '", .key, "' (", .lang, ")."))
  }
  vals <- list(...)
  for (nm in names(vals)) {
    v <- vals[[nm]]
    v <- if (length(v) == 0L || (length(v) == 1L && is.na(v))) "" else paste(as.character(v), collapse = ", ")
    txt <- gsub(paste0("{", nm, "}"), v, txt, fixed = TRUE)
  }
  left <- regmatches(txt, gregexpr("\\{[a-z_0-9]+\\}", txt))[[1L]]
  if (length(left)) {
    .g3_abort("g3_io_error", paste0("Template '", .key, "' needs ", paste(left, collapse = ", "), "."))
  }
  unname(txt)
}

# A short term, e.g. .g3_term("piece.N", "de") -> "Springer".
#' @noRd
.g3_term <- function(key, lang = "en") {
  cat <- .g3_coach_catalogue(lang)
  out <- cat$terms[key]
  if (any(is.na(out))) {
    .g3_abort("g3_io_error", paste0("Missing coaching term '", key[is.na(out)][1L], "' (", lang, ")."))
  }
  unname(out)
}

#' @noRd
.g3_piece_word <- function(type, lang) .g3_term(paste0("piece.", type), lang)

#' @noRd
.g3_color_word <- function(color, lang) .g3_term(paste0("color.", color), lang)

#' @noRd
.g3_theme_word <- function(theme, lang) .g3_term(paste0("theme.", theme), lang)

# Join a vector of words as "a, b and c" in the catalogue's language.
#' @noRd
.g3_join_words <- function(x, lang) {
  x <- as.character(x)
  if (length(x) <= 1L) {
    return(paste(x, collapse = ""))
  }
  paste0(paste(x[-length(x)], collapse = ", "), " ", .g3_term("word.and", lang), " ", x[length(x)])
}

# Problems of a catalogue relative to English: missing or extra keys and
# placeholder sets that differ. Returns a character vector (empty if fine).
#' @noRd
.g3_coach_catalogue_problems <- function(lang) {
  en <- .g3_coach_catalogue("en")
  other <- .g3_coach_catalogue(lang)
  p <- character()
  for (part in c("terms", "templates")) {
    miss <- setdiff(names(en[[part]]), names(other[[part]]))
    extra <- setdiff(names(other[[part]]), names(en[[part]]))
    if (length(miss)) p <- c(p, paste0(part, " missing in ", lang, ": ", paste(miss, collapse = ", ")))
    if (length(extra)) p <- c(p, paste0(part, " not in English: ", paste(extra, collapse = ", ")))
  }
  ph <- function(t) sort(unique(regmatches(t, gregexpr("\\{[a-z_0-9]+\\}", t))[[1L]]))
  for (k in intersect(names(en$templates), names(other$templates))) {
    if (!identical(ph(en$templates[[k]]), ph(other$templates[[k]]))) {
      p <- c(p, paste0("placeholders differ for ", k))
    }
    if (!nzchar(trimws(other$templates[[k]]))) p <- c(p, paste0("empty template ", k))
  }
  p
}

# Format a search value for display. `kind` is "heuristic", "mate" or a
# model value kind; `mate_in` is in plies, positive when the side to act
# mates. Never "centipawns", never a rating.
#' @noRd
.g3_fmt_score <- function(score, kind, mate_in, lang) {
  if (is.na(score) || is.na(kind)) {
    return(.g3_tr("score.na", lang))
  }
  if (identical(kind, "mate")) {
    if (!is.na(mate_in) && mate_in > 0) {
      return(.g3_tr("score.mate_for", lang, plies = mate_in))
    }
    return(.g3_tr("score.mate_against", lang, plies = abs(mate_in)))
  }
  if (identical(kind, "heuristic")) {
    v <- round(score)
    return(.g3_tr("score.heuristic", lang, value = if (v > 0) paste0("+", v) else as.character(v)))
  }
  .g3_tr("score.model_value", lang, value = format(signif(score, 3)), kind = kind)
}
