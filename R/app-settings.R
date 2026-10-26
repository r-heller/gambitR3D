# Settings section: appearance, accessibility, board view, language and
# local storage.
#
# Everything here is a presentation or storage preference. None of it is
# part of a game: the live store's game, its position key and its legal
# actions are untouched by any change below (see test-app-server.R). The
# browser remembers appearance choices in localStorage as a convenience and
# reports them when it connects; they are validated here like any input.

#' @noRd
.g3_settings_ui <- function(id, lang = "en", config = list()) {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  card <- function(title_key, ...) {
    htmltools::tags$section(class = "g3-card", htmltools::tags$h3(class = "g3-card-title", t(title_key)), ...)
  }
  radios <- function(input_id, label_key, values, keys, selected) {
    shiny::radioButtons(ns(input_id), t(label_key), choiceValues = as.list(values),
                        choiceNames = lapply(keys, t), selected = selected)
  }
  htmltools::div(
    class = "g3-section g3-settings",
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", t("nav.settings")),
    htmltools::p(class = "g3-lead", t("settings.lead")),
    htmltools::div(
      class = "g3-two-col",
      card(
        "settings.appearance",
        radios("theme", "settings.theme", c("light", "dark"), c("settings.theme_light", "settings.theme_dark"), "light"),
        shiny::checkboxInput(ns("reduced_motion"), t("settings.reduced_motion"), value = FALSE),
        htmltools::p(class = "g3-muted g3-small", t("settings.reduced_motion_note"))
      ),
      card(
        "settings.board",
        radios("default_view", "settings.default_view", c("2d", "3d"), c("board.view2d", "board.view3d"),
               config$default_view %||% "3d"),
        radios("neutral_height", "settings.neutral_height", c("highest", "lowest"),
               c("board.neutral_highest", "board.neutral_lowest"), config$neutral_height %||% "highest"),
        htmltools::p(class = "g3-muted g3-small", t("settings.board_note"))
      ),
      card(
        "settings.language",
        shiny::selectInput(ns("lang"), t("settings.language_label"),
                           choices = stats::setNames(names(.G3_LANGUAGES), unname(.G3_LANGUAGES)),
                           selected = lang)
      ),
      card(
        "settings.storage",
        shiny::uiOutput(ns("storage_now")),
        shiny::textInput(ns("storage_dir"), t("settings.storage_dir"), value = "", width = "100%"),
        htmltools::div(class = "g3-btn-row",
                       shiny::actionButton(ns("storage_apply"), t("settings.storage_apply"),
                                           class = "g3-btn btn-outline-secondary")),
        shiny::uiOutput(ns("storage_msg"))
      )
    ),
    htmltools::p(class = "g3-muted g3-small", t("settings.clock_note"))
  )
}

#' @noRd
.g3_settings_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    prefs <- app$prefs
    msg <- shiny::reactiveVal(NULL)

    # Inputs -> preferences.
    shiny::observeEvent(input$theme, .g3_apply_pref(prefs, "theme", input$theme), ignoreInit = TRUE)
    shiny::observeEvent(input$reduced_motion, .g3_apply_pref(prefs, "reduced_motion", input$reduced_motion),
                        ignoreInit = TRUE)
    shiny::observeEvent(input$neutral_height, .g3_apply_pref(prefs, "neutral_height", input$neutral_height),
                        ignoreInit = TRUE)
    shiny::observeEvent(input$default_view, {
      if (isTRUE(input$default_view %in% c("2d", "3d"))) {
        prefs$default_view <- input$default_view
        prefs$view <- input$default_view
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$lang, {
      if (isTRUE(input$lang %in% names(.G3_LANGUAGES))) app$lang(input$lang)
    }, ignoreInit = TRUE)

    # Preferences -> inputs (the board toolbar can change them too).
    shiny::observe({
      shiny::updateRadioButtons(session, "neutral_height", selected = prefs$neutral_height)
      shiny::updateRadioButtons(session, "theme", selected = prefs$theme)
      shiny::updateCheckboxInput(session, "reduced_motion", value = isTRUE(prefs$reduced_motion))
      shiny::updateRadioButtons(session, "default_view", selected = prefs$default_view)
    })
    shiny::observe({
      shiny::updateSelectInput(session, "lang", selected = app$lang())
    })

    output$storage_now <- shiny::renderUI({
      app$storage_tick()
      st <- app$storage()
      lang <- app$lang()
      htmltools::tagList(
        htmltools::p(class = "g3-small", .g3_t("settings.storage_now", lang, dir = st$root)),
        htmltools::p(class = "g3-muted g3-small", .g3_t(paste0("settings.backend_", st$backend), lang)),
        if (!is.na(st$note %||% NA)) htmltools::p(class = "g3-muted g3-small", st$note)
      )
    })
    shiny::observeEvent(input$storage_apply, {
      dir <- trimws(input$storage_dir %||% "")
      lang <- app$lang()
      res <- .g3_check_storage_dir(dir)
      if (!isTRUE(res$ok)) {
        msg(list(type = "error", text = .g3_t("settings.storage_bad", lang, message = res$message)))
        return()
      }
      old <- app$storage()
      app$storage(.g3_storage_service(res$path, app$config$db_path))
      try(old$close(), silent = TRUE)
      app$storage_bump()
      msg(list(type = "ok", text = .g3_t("settings.storage_ok", lang, dir = res$path)))
    })
    output$storage_msg <- shiny::renderUI({
      m <- msg()
      if (is.null(m)) return(NULL)
      htmltools::p(class = if (m$type == "error") "g3-error" else "g3-ok",
                   role = if (m$type == "error") "alert" else "status", m$text)
    })
  })
}

# A storage folder must exist (or be creatable) and be writable.
#' @noRd
.g3_check_storage_dir <- function(dir) {
  if (!nzchar(dir)) return(list(ok = FALSE, message = "empty path"))
  path <- normalizePath(path.expand(dir), mustWork = FALSE)
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    return(list(ok = FALSE, message = paste("cannot create", path)))
  }
  probe <- tempfile(".g3probe-", tmpdir = path)
  ok <- tryCatch({
    writeLines("probe", probe)
    unlink(probe)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) return(list(ok = FALSE, message = paste("cannot write to", path)))
  list(ok = TRUE, path = path)
}
