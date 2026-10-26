# Shared helpers for section modules: availability detection and the
# honest "not available" panel. A capability that cannot run says so
# precisely, names what it needs, and shows no button that would pretend to
# work. Availability is detected from this build, never assumed.

#' @noRd
.g3_api_available <- function(fun) {
  is.function(get0(fun, envir = asNamespace("gambitR3D"), inherits = FALSE))
}

#' @noRd
.g3_unavailable <- function(title_key, body_key, needs, lang = "en", ...) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  htmltools::div(
    class = "g3-unavailable", role = "note",
    htmltools::p(class = "g3-unavailable-title",
                 htmltools::span(class = "g3-badge g3-badge-off", htmltools::span(`aria-hidden` = "true", "\u25CB"),
                                 t("common.not_connected")),
                 " ", htmltools::strong(t(title_key))),
    htmltools::p(t(body_key, ...)),
    if (length(needs)) htmltools::p(class = "g3-muted g3-small", t("common.needs", what = paste(needs, collapse = ", ")))
  )
}

#' @noRd
.g3_section_ui <- function(id, title_key, lang = "en") {
  ns <- shiny::NS(id)
  htmltools::div(
    class = paste0("g3-section g3-", id),
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", .g3_txt(title_key, lang)),
    shiny::uiOutput(ns("body"))
  )
}
