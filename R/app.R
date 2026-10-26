# The Shiny application: entry point, page shell and session composition.
#
# Module boundaries (one file each):
#   app-nav.R       navigation rail / bottom bar and section switching
#   app-store.R     session game store, the single action dispatcher and
#                   the storage service (files or game database)
#   app-board.R     board widget server side (payload, requests, prefs)
#   app-play.R      Play: status, controls, computer opponent wiring
#   app-opponent.R  background computation (opponent jobs, analysis task)
#   app-history.R   move list and engine move facts
#   app-rules.R     rules-profile inspector
#   app-files.R     autosave receipt, download, load and resume
#   app-analyse.R   Analyse: an unsaved branch of the live game
#   app-sections.R  Database, Coach, Models and Training shells
#   app-settings.R  appearance, board, language and storage preferences
#   app-i18n.R      externalised UI messages (inst/app/i18n/*.json)
#
# Browser code and assets live in inst/app/www (g3-app.js, g3-scene.js,
# g3-app.css, vendored three.js); see inst/app/www/vendor/PROVENANCE.md.
# Nothing is loaded from the network.

#' Run the gambitR3D app
#'
#' Starts the local Shiny application: play hotseat or against the built-in
#' search opponent on synchronised 2D and 3D boards, analyse a branch of the
#' game, and inspect the rules profile. Games are autosaved after every
#' committed action, as native game files under
#' `tools::R_user_dir("gambitR3D", "data")/games` (or in the game database
#' when `db_path` is given and the database API is available). All assets
#' are bundled; the app makes no network requests.
#'
#' @param db_path Optional path to a gambitR3D SQLite game database. When it
#'   is given and this build provides the database API, games are saved
#'   there; otherwise games are saved as files.
#' @param host Address to listen on. The default `"127.0.0.1"` accepts
#'   connections from this computer only. Any other address makes the app
#'   reachable from other machines without authentication, and a warning is
#'   given.
#' @param port Port number, or `NULL` for Shiny's default (a random free
#'   port unless the `shiny.port` option is set).
#' @param launch.browser Open a browser window (default: in interactive
#'   sessions).
#' @return Invisibly, the value passed to [shiny::stopApp()] when the app
#'   stops (normally `NULL`). Called for its side effect of running the app.
#' @examples
#' \dontrun{
#' # Opens a browser window; stop with Ctrl+C or Esc in the console.
#' gambitR3D::g3_run_app()
#' # Neutral board lowest and the game database (if available):
#' gambitR3D::g3_run_app(db_path = base::file.path(base::tempdir(), "games.sqlite"))
#' }
#' @export
g3_run_app <- function(db_path = NULL, host = "127.0.0.1", port = NULL, launch.browser = interactive()) {
  for (pkg in c("shiny", "bslib", "htmltools", "callr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      .g3_abort("g3_io_error", paste0("The app needs the '", pkg, "' package: install.packages('", pkg, "')."))
    }
  }
  if (!is.character(host) || length(host) != 1L || is.na(host)) {
    .g3_abort("g3_io_error", "`host` must be one address, for example \"127.0.0.1\".")
  }
  if (!host %in% c("127.0.0.1", "localhost", "::1")) {
    .g3_warn(paste0("Listening on ", host, " makes the app reachable from other machines; it has no authentication."))
  }
  app <- .g3_app(db_path = db_path)
  out <- shiny::runApp(app, host = host, port = port %||% getOption("shiny.port"),
                       launch.browser = launch.browser)
  invisible(out)
}

#' Build the app object
#'
#' @param db_path Optional game database path.
#' @param storage_dir Storage folder (default `tools::R_user_dir()`).
#' @param lang Initial language (`"en"` or `"de"`).
#' @param default_view `"3d"` (default) or `"2d"`.
#' @param neutral_height `"highest"` or `"lowest"`.
#' @param saver Optional function `(game)` replacing the durable save; for
#'   failure-injection tests only.
#' @return A `shiny.appobj`.
#' @noRd
.g3_app <- function(db_path = NULL, storage_dir = NULL, lang = "en", default_view = "3d",
                    neutral_height = "highest", saver = NULL) {
  config <- .g3_app_config(db_path, storage_dir, lang, default_view, neutral_height, saver)
  shiny::addResourcePath("g3www", .g3_www_dir())
  shiny::shinyApp(ui = .g3_app_ui(config), server = .g3_app_server(config))
}

#' @noRd
.g3_app_config <- function(db_path = NULL, storage_dir = NULL, lang = "en", default_view = "3d",
                           neutral_height = "highest", saver = NULL) {
  list(
    db_path = db_path,
    storage_dir = storage_dir,
    lang = if (isTRUE(lang %in% names(.G3_LANGUAGES))) lang else "en",
    default_view = if (identical(default_view, "2d")) "2d" else "3d",
    neutral_height = if (identical(neutral_height, "lowest")) "lowest" else "highest",
    saver = saver
  )
}

#' @noRd
.g3_www_dir <- function() {
  dir <- system.file("app", "www", package = "gambitR3D")
  if (!nzchar(dir)) .g3_abort("g3_io_error", "The app's assets are missing from this installation.")
  dir
}

#' @noRd
.g3_asset <- function(file) {
  paste0("g3www/", file, "?v=", utils::packageVersion("gambitR3D"))
}

#' @noRd
.g3_emblem <- function() {
  htmltools::HTML(paste0(
    '<svg class="g3-emblem" viewBox="0 0 32 32" width="30" height="30" aria-hidden="true" focusable="false">',
    '<path d="M16 2.5 27 7.5 16 12.5 5 7.5Z" fill="var(--g3-sq-dark)"/>',
    '<path d="M16 10.5 27 15.5 16 20.5 5 15.5Z" fill="var(--g3-sq-light)" stroke="var(--g3-ink)" stroke-width="1"/>',
    '<path d="M16 18.5 27 23.5 16 28.5 5 23.5Z" fill="var(--g3-sq-dark)"/>',
    '<circle cx="6" cy="23.5" r="2.2" fill="var(--g3-amber)"/>',
    '<circle cx="26" cy="7.5" r="2.2" fill="var(--g3-blue)"/>',
    '</svg>'
  ))
}

#' @noRd
.g3_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2F4B63",
    "font-size-base" = "0.9375rem",
    # System fonts only: the app works offline and never fetches fonts.
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                       "Helvetica Neue", "Arial", "sans-serif"),
    code_font = bslib::font_collection("ui-monospace", "SFMono-Regular", "Menlo", "Consolas",
                                       "Liberation Mono", "monospace")
  )
}

#' @noRd
.g3_app_ui <- function(config) {
  lang <- config$lang
  # Import-map addresses must be URL-like ("./..."), or they are ignored.
  importmap <- jsonlite::toJSON(list(imports = list(
    three = paste0("./", .g3_asset("vendor/three/three.module.js")),
    `three/addons/` = "./g3www/vendor/three/addons/",
    `g3/scene` = paste0("./", .g3_asset("g3-scene.js"))
  )), auto_unbox = TRUE)
  sections <- list(
    play = .g3_play_ui("play", lang),
    analyse = .g3_analyse_ui("analyse", lang),
    database = .g3_section_ui("database", "nav.database", lang),
    coach = .g3_section_ui("coach", "nav.coach", lang),
    models = .g3_section_ui("models", "nav.models", lang),
    training = .g3_section_ui("training", "nav.training", lang),
    settings = .g3_settings_ui("settings", lang, config)
  )
  ui <- bslib::page(
    title = "gambitR3D",
    theme = .g3_bs_theme(),
    lang = lang,
    htmltools::tags$head(
      htmltools::tags$meta(name = "color-scheme", content = "light dark"),
      htmltools::tags$link(rel = "icon", type = "image/svg+xml", href = "g3www/g3-emblem.svg"),
      # The import map must precede any module loading.
      htmltools::tags$script(type = "importmap", htmltools::HTML(importmap)),
      htmltools::tags$link(rel = "stylesheet", href = .g3_asset("g3-app.css")),
      htmltools::tags$script(src = .g3_asset("g3-app.js"))
    ),
    htmltools::div(
      class = "g3-app",
      htmltools::tags$a(class = "g3-skip", href = "#g3-main", .g3_txt("nav.skip", lang)),
      htmltools::tags$header(
        class = "g3-header",
        htmltools::div(
          class = "g3-brand",
          .g3_emblem(),
          htmltools::span(class = "g3-brand-name", "gambitR3D"),
          htmltools::span(class = "g3-brand-sub", .g3_txt("app.subtitle", lang))
        ),
        shiny::uiOutput("jobs", class = "g3-jobs")
      ),
      .g3_nav_ui(lang),
      htmltools::tags$main(
        id = "g3-main", class = "g3-main", tabindex = "-1",
        do.call(shiny::tabsetPanel, c(
          list(id = "sections", type = "hidden"),
          lapply(names(sections), function(s) shiny::tabPanelBody(s, sections[[s]]))
        ))
      ),
      htmltools::div(id = "g3-live", class = "g3-sr-only", `aria-live` = "polite", `aria-atomic` = "true")
    )
  )
  ui
}

#' @noRd
.g3_app_server <- function(config) {
  force(config)
  function(input, output, session) {
    app <- new.env(parent = emptyenv())
    app$config <- config
    app$session <- session
    app$lang <- shiny::reactiveVal(config$lang)
    app$section <- shiny::reactiveVal("play")
    app$prefs <- shiny::reactiveValues(
      view = config$default_view, default_view = config$default_view,
      neutral_height = config$neutral_height, exploded = FALSE, xray = FALSE,
      level = "all", camera = "white", reduced_motion = FALSE, theme = "light"
    )
    app$storage <- shiny::reactiveVal(.g3_storage_service(config$storage_dir, config$db_path))
    app$storage_tick <- shiny::reactiveVal(0L)
    app$storage_bump <- function() app$storage_tick(shiny::isolate(app$storage_tick()) + 1L)

    # The session's live game. Its saver writes the game durably before the
    # dispatcher commits; display preferences go into the file's separate
    # `ui` envelope, never into the game.
    game0 <- g3_new_game(players = list(white = .g3_t("side.white", "en"), black = .g3_t("side.black", "en")))
    game0$metadata$mode <- "hotseat"
    saver <- config$saver %||% function(game) {
      shiny::isolate(app$storage()$save(
        game, ui = list(view = app$prefs$view, neutral_height = app$prefs$neutral_height)
      ))
    }
    app$live <- .g3_new_store(game0, "live", saver = saver)
    app$live_tick <- shiny::reactiveVal(0L)
    app$live_bump <- function() app$live_tick(shiny::isolate(app$live_tick()) + 1L)
    app$jobs <- shiny::reactiveValues(opponent = FALSE, analysis = FALSE)
    app$set_job <- function(name, active) app$jobs[[name]] <- isTRUE(active)
    app$notify <- function(text, type = "message") {
      shiny::showNotification(text, type = if (type == "error") "error" else "message", session = session)
    }
    app$go <- function(section) {
      app$section(section)
      shiny::updateTabsetPanel(session, "sections", selected = section)
      session$sendCustomMessage("g3-nav", list(section = section, focus = TRUE))
    }
    session$userData$g3 <- app

    .g3_nav_server(input, session, app)
    .g3_play_server("play", app)
    .g3_analyse_server("analyse", app)
    .g3_database_server("database", app)
    .g3_coach_server("coach", app)
    .g3_models_server("models", app)
    .g3_training_server("training", app)
    .g3_settings_server("settings", app)

    # Presentation preferences and UI language go to the browser; neither
    # touches the game.
    app$client_synced <- shiny::reactiveVal(FALSE)
    shiny::observe({
      p <- shiny::reactiveValuesToList(app$prefs)
      p$lang <- app$lang()
      # The browser remembers preferences only after its stored ones have
      # been applied, so server defaults never overwrite them.
      p$synced <- app$client_synced()
      session$sendCustomMessage("g3-prefs", p)
    })
    shiny::observe({
      l <- app$lang()
      session$sendCustomMessage("g3-i18n", list(lang = l, messages = as.list(.g3_i18n_catalogue(l))))
    })
    # Stored browser preferences, reported once on connection.
    shiny::observeEvent(input$client_prefs, {
      cp <- input$client_prefs
      for (nm in c("theme", "reduced_motion", "neutral_height")) {
        if (!is.null(cp[[nm]])) .g3_apply_pref(app$prefs, nm, cp[[nm]])
      }
      if (isTRUE(cp$default_view %in% c("2d", "3d"))) {
        app$prefs$default_view <- cp$default_view
        app$prefs$view <- cp$default_view
      }
      if (isTRUE(cp$lang %in% names(.G3_LANGUAGES))) app$lang(cp$lang)
      app$client_synced(TRUE)
    }, once = TRUE)

    output$jobs <- shiny::renderUI({
      l <- app$lang()
      active <- c(opponent = app$jobs$opponent, analysis = app$jobs$analysis)
      if (!any(active)) return(NULL)
      htmltools::tagList(lapply(names(active)[active], function(j) {
        htmltools::span(class = "g3-chip g3-chip-job", role = "status",
                        htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
                        .g3_t(paste0("jobs.", j), l))
      }))
    })
    session$onSessionEnded(function() {
      st <- shiny::isolate(app$storage())
      try(st$close(), silent = TRUE)
    })
  }
}
