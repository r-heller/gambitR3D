# Save, load and resume.
#
# Autosave happens inside the dispatcher (a move is acknowledged only after
# its game file is written). This module shows the save receipt, offers a
# download of the current game, loads a game or position file with full
# validation, and lists saved games for resuming. Loading never trusts the
# file: g3_read_game() replays every action through the rules engine and
# compares checksums, and an unsupported format or rules version is reported
# as such rather than half-loaded.

#' @noRd
.g3_files_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  htmltools::tagList(
    shiny::uiOutput(ns("status")),
    htmltools::div(
      class = "g3-btn-row",
      shiny::downloadButton(ns("download"), label = t("files.download"), class = "g3-btn btn-outline-secondary",
                            icon = NULL)
    ),
    # The button label and the placeholder are re-translated in the browser.
    htmltools::tagQuery(
      shiny::fileInput(ns("upload"), label = t("files.load"), accept = c(".json", "application/json"),
                       buttonLabel = t("files.browse"), placeholder = .g3_t("files.no_file", lang))
    )$find(".form-control")$addAttrs(`data-i18n-attr` = "placeholder:files.no_file")$allTags(),
    shiny::uiOutput(ns("upload_msg")),
    shiny::selectInput(ns("saved"), label = t("files.saved"), choices = c(), width = "100%"),
    htmltools::div(class = "g3-btn-row",
                   shiny::actionButton(ns("resume"), t("files.resume"), class = "g3-btn btn-outline-secondary")),
    shiny::uiOutput(ns("saved_msg"))
  )
}

#' @noRd
.g3_files_server <- function(id, app, dispatch, before_replace = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    lang <- app$lang
    store <- app$live
    saved_tick <- shiny::reactiveVal(0L)
    msg <- shiny::reactiveVal(NULL)

    output$status <- shiny::renderUI({
      app$live_tick()
      app$storage_tick()
      .g3_save_status(store, app$storage(), lang())
    })

    output$download <- shiny::downloadHandler(
      filename = function() paste0(store$game$metadata$game_id, ".g3.json"),
      content = function(file) g3_write_game(store$game, file, overwrite = TRUE),
      contentType = "application/json"
    )

    load_game <- function(game, origin) {
      before_replace()
      ack <- dispatch(.g3_store_request(store, "replace", game = game))
      if (identical(ack$status, "committed")) {
        msg(list(type = "ok", text = .g3_t("files.loaded", lang(), origin = origin, ply = game$state$ply)))
      } else {
        msg(list(type = "error", text = .g3_ack_text(ack, lang())))
      }
      saved_tick(saved_tick() + 1L)
      ack
    }

    read_or_report <- function(path) {
      tryCatch(app$storage()$read(path), g3_error = function(e) e, error = function(e) e)
    }

    shiny::observeEvent(input$upload, {
      f <- input$upload
      game <- read_or_report(f$datapath)
      if (inherits(game, "error")) {
        msg(list(type = "error", text = .g3_load_error_text(game, lang())))
        return()
      }
      game$metadata$source_file <- f$name
      load_game(game, f$name)
    })
    output$upload_msg <- shiny::renderUI({
      m <- msg()
      if (is.null(m)) return(NULL)
      htmltools::p(class = if (m$type == "error") "g3-error" else "g3-ok",
                   role = if (m$type == "error") "alert" else "status", m$text)
    })

    saved <- shiny::reactive({
      saved_tick()
      app$live_tick()
      app$storage_tick()
      tryCatch(app$storage()$list_games(), error = function(e) NULL)
    })
    shiny::observe({
      s <- saved()
      cur <- shiny::isolate(store$game$metadata$game_id)
      if (is.null(s) || !nrow(s)) {
        shiny::updateSelectInput(session, "saved", choices = stats::setNames("", .g3_t("files.none_saved", lang())))
        return()
      }
      lab <- vapply(seq_len(nrow(s)), function(i) {
        .g3_t("files.saved_item", lang(),
              when = format(s$modified[i], "%Y-%m-%d %H:%M"), ply = s$ply[i],
              white = s$white[i], black = s$black[i],
              status = .g3_result_short(s$result[i], lang()),
              current = if (identical(s$game_id[i], cur)) .g3_t("files.current", lang()) else "")
      }, "")
      shiny::updateSelectInput(session, "saved", choices = stats::setNames(s$path, lab))
    })
    shiny::observeEvent(input$resume, {
      path <- input$saved
      if (is.null(path) || !nzchar(path)) return()
      game <- read_or_report(path)
      if (inherits(game, "error")) {
        msg(list(type = "error", text = .g3_load_error_text(game, lang())))
        return()
      }
      load_game(game, basename(path))
    })

    latest <- shiny::reactive({
      s <- saved()
      if (is.null(s) || !nrow(s)) return(NULL)
      s <- s[is.na(s$result), , drop = FALSE]
      if (!nrow(s)) return(NULL)
      s[1L, ]
    })
    list(
      latest = latest,
      resume_path = function(path) {
        game <- read_or_report(path)
        if (inherits(game, "error")) {
          msg(list(type = "error", text = .g3_load_error_text(game, shiny::isolate(lang()))))
          return(invisible(NULL))
        }
        load_game(game, basename(path))
      }
    )
  })
}

#' @noRd
.g3_load_error_text <- function(e, lang = "en") {
  key <- if (inherits(e, "g3_unsupported_rules")) {
    "files.error_unsupported"
  } else if (inherits(e, "g3_io_error")) {
    "files.error_io"
  } else {
    "files.error_invalid"
  }
  .g3_t(key, lang, message = conditionMessage(e))
}

#' @noRd
.g3_save_status <- function(store, storage, lang = "en") {
  t <- function(key, ...) .g3_t(key, lang, ...)
  r <- store$receipt
  note <- if (!is.na(storage$note %||% NA)) htmltools::p(class = "g3-muted g3-small", storage$note)
  if (is.null(r)) {
    return(htmltools::tagList(
      htmltools::p(class = "g3-muted g3-small", t("files.not_saved_yet", dir = storage$games_dir)),
      note
    ))
  }
  where <- if (identical(r$backend, "db")) t("files.saved_db", path = r$path) else t("files.saved_file", path = r$path)
  htmltools::tagList(
    htmltools::p(class = "g3-ok g3-small", role = "status",
                 htmltools::span(class = "g3-badge g3-badge-ok", htmltools::span(`aria-hidden` = "true", "✓"),
                                 t("files.saved_badge")),
                 " ", t("files.saved_at", time = r$time), " ", where),
    note
  )
}

#' @noRd
.g3_result_short <- function(result, lang = "en") {
  if (is.na(result)) return(.g3_t("files.unfinished", lang))
  key <- switch(result, white = "result.white_wins", black = "result.black_wins", draw = "result.draw",
                "result.truncated")
  .g3_t(key, lang)
}
