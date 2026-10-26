# Navigation: a desktop rail with all seven sections and, on narrow
# screens, a bottom bar with Play, Analyse and Training plus a labelled More
# menu for Database, Coach, Models and Settings. Both are native buttons
# with visible names; the icons are decorative. Switching section keeps the
# live game (sections are hidden, not rebuilt) and moves focus to the new
# section's heading.

.G3_SECTIONS <- c("play", "analyse", "database", "coach", "models", "training", "settings")
.G3_SECTIONS_PRIMARY <- c("play", "analyse", "training")

# Original line icons, 24x24, drawn with currentColor.
#' @noRd
.g3_icon <- function(name) {
  paths <- switch(name,
    play = "M7 4.5v15l12-7.5z",
    analyse = "M10.5 4a6.5 6.5 0 1 1 0 13 6.5 6.5 0 0 1 0-13zm4.6 11.6L20 20.5",
    database = "M5 6c0-1.7 3.1-3 7-3s7 1.3 7 3-3.1 3-7 3-7-1.3-7-3zm0 0v12c0 1.7 3.1 3 7 3s7-1.3 7-3V6M5 12c0 1.7 3.1 3 7 3s7-1.3 7-3",
    coach = "M4 5h16v11H9l-4 4v-4H4z",
    models = "M8 8h8v8H8zM10 3v3m4-3v3m-4 12v3m4-3v3M3 10h3m-3 4h3m12-4h3m-3 4h3",
    training = "M12 3a9 9 0 1 1 0 18 9 9 0 0 1 0-18zm0 4a5 5 0 1 1 0 10 5 5 0 0 1 0-10zm0 4a1 1 0 1 1 0 2 1 1 0 0 1 0-2z",
    settings = "M4 7h10m4 0h2M4 17h2m4 0h10M14 4v6M6 14v6",
    more = "M5 12h.01M12 12h.01M19 12h.01",
    ""
  )
  htmltools::HTML(paste0(
    '<svg class="g3-icon" viewBox="0 0 24 24" width="22" height="22" aria-hidden="true" focusable="false">',
    '<path d="', paths, '" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" ',
    'stroke-linejoin="round"/></svg>'
  ))
}

#' @noRd
.g3_nav_ui <- function(lang = "en") {
  item <- function(sec, extra_class = NULL) {
    htmltools::tags$button(
      type = "button", class = paste("g3-nav-btn", extra_class), `data-section` = sec,
      `aria-current` = if (sec == "play") "page" else NULL,
      .g3_icon(sec), .g3_txt(paste0("nav.", sec), lang, class = "g3-nav-label")
    )
  }
  more <- setdiff(.G3_SECTIONS, .G3_SECTIONS_PRIMARY)
  htmltools::tagList(
    htmltools::tags$nav(
      class = "g3-rail", !!!.g3_i18n_attr(c("aria-label" = "nav.label"), lang),
      htmltools::tags$ul(lapply(.G3_SECTIONS, function(s) htmltools::tags$li(item(s))))
    ),
    htmltools::tags$nav(
      class = "g3-bottombar", !!!.g3_i18n_attr(c("aria-label" = "nav.label"), lang),
      htmltools::tags$ul(
        lapply(.G3_SECTIONS_PRIMARY, function(s) htmltools::tags$li(item(s))),
        htmltools::tags$li(
          class = "g3-more",
          htmltools::tags$button(
            type = "button", class = "g3-nav-btn g3-more-btn", id = "g3-more-btn",
            `aria-haspopup` = "true", `aria-expanded` = "false", `aria-controls` = "g3-more-menu",
            .g3_icon("more"), .g3_txt("nav.more", lang, class = "g3-nav-label")
          ),
          htmltools::tags$ul(
            id = "g3-more-menu", class = "g3-more-menu", hidden = NA,
            !!!.g3_i18n_attr(c("aria-label" = "nav.more"), lang),
            lapply(more, function(s) htmltools::tags$li(item(s, "g3-more-item")))
          )
        )
      )
    )
  )
}

#' @noRd
.g3_nav_server <- function(input, session, app) {
  shiny::observeEvent(input$nav, {
    sec <- if (is.list(input$nav)) input$nav$section else input$nav
    if (!is.character(sec) || length(sec) != 1L || !sec %in% .G3_SECTIONS) return()
    app$go(sec)
  })
}
