# Database section: the local game catalogue.
#
# Uses the session's open catalogue (the storage service's `g3_db`, the same
# handle Play autosaves into). Everything the catalogue API offers is here:
# filtered, paged queries (`g3_query_games()`), a game's detail with its
# move list (`g3_load_game()`), resume a live game in Play or open any game
# in Analyse, import with the per-record report including quarantine and the
# first invalid event, export of a selection (`g3_export_games()`), soft
# delete with explicit confirmation and restore, and a consistent backup.
#
# Import and export replay every game through the rules engine, so they run
# in a background R process with its own catalogue handle (SQLite serialises
# the writers); the session stays usable meanwhile. Stored text (names, tags,
# file names) is rendered through htmltools, which escapes it.

.G3_DB_PAGE <- 20L

#' @noRd
.g3_database_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  card <- function(title_key, ..., class = NULL) {
    htmltools::tags$section(class = paste("g3-card", class),
                            htmltools::tags$h3(class = "g3-card-title", t(title_key)), ...)
  }
  sel <- function(input_id, label_key, values, keys, selected = values[[1]]) {
    shiny::selectInput(ns(input_id), t(label_key),
                       choices = stats::setNames(values, vapply(keys, .g3_t, "", lang = lang)), selected = selected)
  }
  htmltools::div(
    class = "g3-section g3-database",
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", t("nav.database")),
    shiny::uiOutput(ns("status")),
    htmltools::div(
      class = "g3-db-grid",
      htmltools::div(
        class = "g3-db-main",
        card(
          "db.filters",
          htmltools::div(
            class = "g3-filter-grid",
            shiny::textInput(ns("player"), t("db.player"), value = ""),
            sel("result", "db.result", c("", "white", "black", "draw", "truncated"),
                c("db.any", "result.white_wins", "result.black_wins", "result.draw", "result.truncated")),
            sel("status_filter", "db.status", c("", "live", "finished"), c("db.any", "db.status_live", "db.status_finished")),
            sel("deleted", "db.deleted", c("exclude", "include", "only"),
                c("db.deleted_exclude", "db.deleted_include", "db.deleted_only")),
            shiny::numericInput(ns("min_plies"), t("db.min_plies"), value = NA, min = 0, step = 1),
            sel("order_by", "db.order_by", c("updated", "created", "plies", "result", "white", "black"),
                c("db.order_updated", "db.order_created", "db.order_plies", "db.order_result",
                  "db.order_white", "db.order_black"))
          ),
          htmltools::div(class = "g3-btn-row",
                         shiny::actionButton(ns("apply"), t("db.apply"), class = "g3-btn btn-primary"),
                         shiny::actionButton(ns("clear"), t("db.clear"), class = "g3-btn btn-outline-secondary"))
        ),
        card(
          "db.games",
          shiny::uiOutput(ns("table")),
          htmltools::div(
            class = "g3-btn-row",
            shiny::actionButton(ns("prev_page"), t("db.prev_page"), class = "g3-btn btn-outline-secondary"),
            shiny::actionButton(ns("next_page"), t("db.next_page"), class = "g3-btn btn-outline-secondary")
          ),
          htmltools::div(
            class = "g3-btn-row",
            shiny::actionButton(ns("export_sel"), t("db.export_selected"), class = "g3-btn btn-outline-secondary"),
            shiny::actionButton(ns("delete_sel"), t("db.delete_selected"), class = "g3-btn btn-outline-secondary"),
            shiny::actionButton(ns("restore_sel"), t("db.restore_selected"), class = "g3-btn btn-outline-secondary")
          ),
          shiny::uiOutput(ns("sel_msg"))
        )
      ),
      htmltools::div(
        class = "g3-db-side",
        card("db.detail", shiny::uiOutput(ns("detail"))),
        card(
          "db.import",
          htmltools::p(class = "g3-muted g3-small", t("db.import_note")),
          shiny::fileInput(ns("import_files"), t("db.import_files"), multiple = TRUE,
                           accept = c(".json", ".jsonl", ".gz", "application/json"),
                           buttonLabel = t("files.browse"), placeholder = .g3_t("files.no_file", lang)),
          shiny::uiOutput(ns("import_report")),
          htmltools::div(class = "g3-btn-row",
                         shiny::actionButton(ns("show_quarantine"), t("db.show_quarantine"),
                                             class = "g3-btn btn-outline-secondary")),
          shiny::uiOutput(ns("quarantine"))
        ),
        card("db.export", shiny::uiOutput(ns("export_report"))),
        card(
          "db.backup",
          htmltools::p(class = "g3-muted g3-small", t("db.backup_note")),
          htmltools::div(class = "g3-btn-row",
                         shiny::actionButton(ns("backup"), t("db.backup_now"), class = "g3-btn btn-outline-secondary")),
          shiny::uiOutput(ns("backup_report"))
        )
      )
    )
  )
}

#' @noRd
.g3_database_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- app$lang
    refresh <- shiny::reactiveVal(0L)
    bump_refresh <- function() refresh(shiny::isolate(refresh()) + 1L)
    app$db_refresh <- bump_refresh
    filters <- shiny::reactiveVal(list())
    offset <- shiny::reactiveVal(0L)
    detail_id <- shiny::reactiveVal(NULL)
    msg <- shiny::reactiveVal(NULL)
    import_bg <- .g3_bg_controller(app, session, "import")
    export_bg <- .g3_bg_controller(app, session, "export")
    backup_res <- shiny::reactiveVal(NULL)
    quarantine_open <- shiny::reactiveVal(FALSE)

    db <- shiny::reactive({
      app$storage_tick()
      app$storage()$db
    })

    output$status <- shiny::renderUI({
      st <- app$storage()
      app$storage_tick()
      l <- lang()
      t <- function(key, ...) .g3_t(key, l, ...)
      if (is.null(st$db)) {
        return(.g3_unavailable("db.catalogue", "db.unavailable_body", c("DBI", "RSQLite"), l,
                               reason = st$note %||% ""))
      }
      d <- st$db
      htmltools::p(class = "g3-muted g3-small",
                   t("db.open_status", path = d$path, schema = d$schema_version, journal = d$journal_mode))
    })

    shiny::observeEvent(input$apply, {
      f <- list()
      if (nzchar(trimws(input$player %||% ""))) f$player_contains <- trimws(input$player)
      if (nzchar(input$result %||% "")) f$result <- input$result
      if (nzchar(input$status_filter %||% "")) f$status <- input$status_filter
      f$deleted <- input$deleted %||% "exclude"
      if (isTRUE(is.finite(input$min_plies))) f$min_plies <- as.integer(input$min_plies)
      f$order_by <- input$order_by %||% "updated"
      filters(f)
      offset(0L)
    })
    shiny::observeEvent(input$clear, {
      shiny::updateTextInput(session, "player", value = "")
      for (nm in c("result", "status_filter")) shiny::updateSelectInput(session, nm, selected = "")
      shiny::updateSelectInput(session, "deleted", selected = "exclude")
      shiny::updateNumericInput(session, "min_plies", value = NA)
      filters(list())
      offset(0L)
    })

    page <- shiny::reactive({
      refresh()
      app$live_tick()
      d <- db()
      if (is.null(d)) return(NULL)
      tryCatch(g3_query_games(d, filters(), limit = .G3_DB_PAGE, offset = offset()), error = function(e) e)
    })
    shiny::observeEvent(input$next_page, {
      p <- page()
      if (!inherits(p, "error") && !is.null(p) && offset() + .G3_DB_PAGE < attr(p, "total")) {
        offset(offset() + .G3_DB_PAGE)
      }
    })
    shiny::observeEvent(input$prev_page, offset(max(0L, offset() - .G3_DB_PAGE)))
    shiny::observe({
      p <- page()
      total <- if (is.null(p) || inherits(p, "error")) 0L else attr(p, "total")
      shiny::updateActionButton(session, "prev_page", disabled = offset() == 0L)
      shiny::updateActionButton(session, "next_page", disabled = offset() + .G3_DB_PAGE >= total)
    })

    output$table <- shiny::renderUI({
      p <- page()
      l <- lang()
      if (is.null(p)) return(htmltools::p(class = "g3-empty", .g3_t("db.no_catalogue", l)))
      if (inherits(p, "error")) return(htmltools::p(class = "g3-error", role = "alert", conditionMessage(p)))
      .g3_db_table(p, offset(), ns, l, detail_id(), checked = shiny::isolate(selected()))
    })

    selected <- shiny::reactive({
      s <- input$selected
      if (is.null(s)) character() else as.character(unlist(s))
    })

    # ---- detail
    shiny::observeEvent(input$detail, {
      id <- input$detail$value %||% input$detail
      if (is.character(id) && length(id) == 1L) detail_id(id)
    })
    detail_game <- shiny::reactive({
      id <- detail_id()
      refresh()
      d <- db()
      if (is.null(id) || is.null(d)) return(NULL)
      tryCatch(g3_load_game(d, id), error = function(e) e)
    })
    output$detail <- shiny::renderUI({
      g <- detail_game()
      l <- lang()
      if (is.null(g)) return(htmltools::p(class = "g3-empty", .g3_t("db.detail_empty", l)))
      if (inherits(g, "error")) return(htmltools::p(class = "g3-error", role = "alert", conditionMessage(g)))
      .g3_db_detail(g, ns, l)
    })
    shiny::observeEvent(input$resume_game, {
      g <- detail_game()
      if (is.null(g) || inherits(g, "error")) return()
      if (!isTRUE(g$catalogue$resumable)) {
        msg(list(type = "error", text = .g3_t("db.not_resumable", lang())))
        return()
      }
      ack <- app$resume_in_play(g)
      if (identical(ack$status, "committed")) app$go("play") else msg(list(type = "error", text = .g3_ack_text(ack, lang())))
    })
    shiny::observeEvent(input$analyse_game, {
      g <- detail_game()
      if (is.null(g) || inherits(g, "error")) return()
      app$open_analysis(g, label = .g3_t("db.branch_label", lang(), id = g$metadata$game_id))
      app$go("analyse")
    })

    # ---- delete / restore
    shiny::observeEvent(input$delete_sel, {
      ids <- selected()
      if (!length(ids)) {
        msg(list(type = "error", text = .g3_t("db.select_first", lang())))
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = .g3_t("db.delete_title", lang()), easyClose = TRUE,
        htmltools::p(.g3_t("db.delete_body", lang(), n = length(ids))),
        htmltools::tags$ul(lapply(ids, htmltools::tags$li)),
        footer = htmltools::tagList(
          shiny::modalButton(.g3_t("common.cancel", lang())),
          shiny::actionButton(ns("delete_confirm"), .g3_t("db.delete_confirm", lang()), class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$delete_confirm, {
      shiny::removeModal()
      res <- tryCatch(g3_delete_games(db(), selected(), confirm = TRUE), error = function(e) e)
      msg(if (inherits(res, "error")) list(type = "error", text = conditionMessage(res)) else
        list(type = "ok", text = .g3_t("db.deleted_report", lang(), n = sum(res$status == "deleted"),
                                       other = sum(res$status != "deleted"))))
      bump_refresh()
    })
    shiny::observeEvent(input$restore_sel, {
      ids <- selected()
      if (!length(ids)) {
        msg(list(type = "error", text = .g3_t("db.select_first", lang())))
        return()
      }
      res <- tryCatch(g3_restore_games(db(), ids), error = function(e) e)
      msg(if (inherits(res, "error")) list(type = "error", text = conditionMessage(res)) else
        list(type = "ok", text = .g3_t("db.restored_report", lang(), n = sum(res$status == "restored"),
                                       other = sum(res$status != "restored"))))
      bump_refresh()
    })
    output$sel_msg <- shiny::renderUI({
      m <- msg()
      if (is.null(m)) return(NULL)
      htmltools::p(class = if (m$type == "error") "g3-error" else "g3-ok",
                   role = if (m$type == "error") "alert" else "status", m$text)
    })

    # ---- import (background process with its own handle)
    shiny::observeEvent(input$import_files, {
      d <- db()
      if (is.null(d)) return()
      f <- input$import_files
      dir <- file.path(app$storage()$root, "imports", format(Sys.time(), "%Y%m%d-%H%M%OS3"))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      # The importer recognises formats by file name, so keep the names.
      targets <- file.path(dir, basename(f$name))
      file.copy(f$datapath, targets, overwrite = TRUE)
      import_bg$start(".g3_app_import_worker", list(db_path = d$path, paths = targets), tag = f$name)
    })
    output$import_report <- shiny::renderUI({
      import_bg$tick()
      .g3_db_import_report(import_bg$state, ns, lang())
    })
    shiny::observe({
      import_bg$tick()
      if (identical(import_bg$state$status, "succeeded")) shiny::isolate(bump_refresh())
    })
    shiny::observeEvent(input$import_cancel, import_bg$cancel())
    shiny::observeEvent(input$show_quarantine, quarantine_open(!quarantine_open()))
    output$quarantine <- shiny::renderUI({
      if (!quarantine_open()) return(NULL)
      refresh()
      import_bg$tick()
      d <- db()
      if (is.null(d)) return(NULL)
      q <- tryCatch(g3_quarantined_records(d, limit = 50L), error = function(e) e)
      if (inherits(q, "error")) return(htmltools::p(class = "g3-error", conditionMessage(q)))
      .g3_db_quarantine_table(q, lang())
    })

    # ---- export selection
    shiny::observeEvent(input$export_sel, {
      d <- db()
      ids <- selected()
      if (is.null(d)) return()
      if (!length(ids)) {
        msg(list(type = "error", text = .g3_t("db.select_first", lang())))
        return()
      }
      dir <- file.path(app$storage()$root, "exports")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      path <- file.path(dir, paste0("games-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".jsonl"))
      export_bg$start(".g3_app_export_worker", list(db_path = d$path, game_ids = ids, path = path), tag = path)
    })
    output$export_report <- shiny::renderUI({
      export_bg$tick()
      .g3_db_export_report(export_bg$state, ns, lang())
    })
    shiny::observeEvent(input$export_cancel, export_bg$cancel())
    output$export_download <- shiny::downloadHandler(
      filename = function() basename(export_bg$state$tag %||% "games.jsonl"),
      content = function(file) file.copy(export_bg$state$tag, file, overwrite = TRUE),
      contentType = "application/x-ndjson"
    )

    # ---- backup
    shiny::observeEvent(input$backup, {
      d <- db()
      if (is.null(d)) return()
      dir <- file.path(app$storage()$root, "backups")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      path <- file.path(dir, paste0("games-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".sqlite"))
      backup_res(tryCatch(g3_backup_game_db(d, path), error = function(e) e))
    })
    output$backup_report <- shiny::renderUI({
      b <- backup_res()
      l <- lang()
      if (is.null(b)) return(NULL)
      if (inherits(b, "error")) return(htmltools::p(class = "g3-error", role = "alert", conditionMessage(b)))
      htmltools::tagList(
        htmltools::p(class = "g3-ok", role = "status",
                     .g3_t("db.backup_done", l, path = b$path, n = b$n_games, bytes = b$bytes)),
        htmltools::p(class = "g3-small", .g3_t("db.backup_integrity", l, integrity = b$integrity),
                     " ", htmltools::tags$code(substr(b$sha256, 1L, 16L))),
        shiny::downloadButton(ns("backup_download"), .g3_t("db.backup_download", l), class = "g3-btn btn-outline-secondary",
                              icon = NULL)
      )
    })
    output$backup_download <- shiny::downloadHandler(
      filename = function() basename(backup_res()$path),
      content = function(file) file.copy(backup_res()$path, file, overwrite = TRUE),
      contentType = "application/vnd.sqlite3"
    )
    invisible(list(refresh = bump_refresh))
  })
}

# Background workers (run in a separate R process with their own handle).
#' @noRd
.g3_app_import_worker <- function(db_path, paths) {
  db <- g3_open_game_db(db_path)
  on.exit(g3_close_game_db(db), add = TRUE)
  g3_import_games(db, paths)
}

#' @noRd
.g3_app_export_worker <- function(db_path, game_ids, path) {
  db <- g3_open_game_db(db_path, read_only = TRUE)
  on.exit(g3_close_game_db(db), add = TRUE)
  g3_export_games(db, game_ids, path)
}

#' @noRd
.g3_db_table <- function(p, offset, ns, lang, current = NULL, checked = character()) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  total <- attr(p, "total") %||% nrow(p)
  if (!nrow(p)) return(htmltools::p(class = "g3-empty", t("db.no_games")))
  res_txt <- function(r) if (is.na(r)) t("files.unfinished") else .g3_result_short(r, lang)
  rows <- lapply(seq_len(nrow(p)), function(i) {
    r <- p[i, ]
    htmltools::tags$tr(
      class = if (identical(r$game_id, current)) "g3-row-current" else NULL,
      htmltools::tags$td(htmltools::tags$input(type = "checkbox", class = "g3-db-select", value = r$game_id,
                                               `data-g3-select` = ns("selected"),
                                               checked = if (r$game_id %in% checked) NA else NULL,
                                               `aria-label` = t("db.select_game", id = r$game_id))),
      htmltools::tags$td(htmltools::tags$button(type = "button", class = "g3-linkbtn", `data-g3-click` = ns("detail"),
                                                `data-g3-value` = r$game_id, r$game_id)),
      htmltools::tags$td(paste(r$white, "\u2013", r$black)),
      htmltools::tags$td(res_txt(r$result), if (isTRUE(r$deleted)) htmltools::span(class = "g3-chip", t("db.deleted_chip"))),
      htmltools::tags$td(t(paste0("db.status_", r$status))),
      htmltools::tags$td(class = "g3-num g3-tnum", r$plies),
      htmltools::tags$td(class = "g3-tnum", substr(r$updated_at, 1L, 16L))
    )
  })
  htmltools::tagList(
    htmltools::p(class = "g3-small g3-tnum", role = "status",
                 t("db.page_info", from = offset + 1L, to = offset + nrow(p), total = total)),
    htmltools::div(
      class = "g3-table-scroll",
      htmltools::tags$table(
        class = "g3-table g3-db-table",
        htmltools::tags$caption(class = "g3-sr-only", t("db.games")),
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(scope = "col", htmltools::span(class = "g3-sr-only", t("db.col_select"))),
          htmltools::tags$th(scope = "col", t("db.col_game")),
          htmltools::tags$th(scope = "col", t("db.col_players")),
          htmltools::tags$th(scope = "col", t("db.col_result")),
          htmltools::tags$th(scope = "col", t("db.col_status")),
          htmltools::tags$th(scope = "col", class = "g3-num", t("db.col_plies")),
          htmltools::tags$th(scope = "col", t("db.col_updated"))
        )),
        htmltools::tags$tbody(rows)
      )
    )
  )
}

#' @noRd
.g3_db_detail <- function(g, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  cat <- g$catalogue
  m <- g$metadata
  htmltools::tagList(
    htmltools::tags$dl(
      class = "g3-facts",
      htmltools::tags$dt(t("db.col_game")), htmltools::tags$dd(m$game_id),
      htmltools::tags$dt(t("db.col_players")),
      htmltools::tags$dd(paste(m$players$white %||% "?", "\u2013", m$players$black %||% "?")),
      htmltools::tags$dt(t("db.col_result")),
      htmltools::tags$dd(if (is.na(cat$result %||% NA)) t("files.unfinished") else .g3_result_short(cat$result, lang),
                         if (!is.na(cat$termination %||% NA)) paste0(" (", cat$termination, ")")),
      htmltools::tags$dt(t("db.col_status")), htmltools::tags$dd(t(paste0("db.status_", cat$status))),
      htmltools::tags$dt(t("db.origin")), htmltools::tags$dd(cat$origin, " \u00B7 ", t("db.sources", n = cat$n_sources)),
      htmltools::tags$dt(t("db.col_updated")), htmltools::tags$dd(cat$updated_at)
    ),
    htmltools::div(
      class = "g3-btn-row",
      if (isTRUE(cat$resumable)) shiny::actionButton(ns("resume_game"), t("db.resume"), class = "g3-btn btn-primary"),
      shiny::actionButton(ns("analyse_game"), t("db.open_analyse"), class = "g3-btn btn-outline-secondary")
    ),
    if (!isTRUE(cat$resumable)) htmltools::p(class = "g3-muted g3-small", t("db.finished_note")),
    htmltools::tags$h4(class = "g3-subtitle", t("play.moves")),
    .g3_move_list(g, lang)
  )
}

#' @noRd
.g3_db_import_report <- function(st, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (identical(st$status, "idle")) return(NULL)
  if (identical(st$status, "running")) {
    return(htmltools::div(
      htmltools::p(class = "g3-progress", role = "status", htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
                   t("db.importing", files = paste(st$tag, collapse = ", "))),
      shiny::actionButton(ns("import_cancel"), t("job.cancel"), class = "g3-btn btn-outline-secondary")
    ))
  }
  if (identical(st$status, "cancelled")) return(htmltools::p(role = "status", t("db.import_cancelled")))
  if (!is.null(st$error)) return(htmltools::p(class = "g3-error", role = "alert", st$error))
  r <- st$result
  if (!is.data.frame(r)) return(NULL)
  counts <- table(factor(r$status, levels = c("accepted", "duplicate", "quarantined", "error")))
  htmltools::tagList(
    htmltools::p(class = "g3-small", role = "status",
                 t("db.import_counts", accepted = counts[["accepted"]], duplicate = counts[["duplicate"]],
                   quarantined = counts[["quarantined"]], error = counts[["error"]])),
    htmltools::div(
      class = "g3-table-scroll",
      htmltools::tags$table(
        class = "g3-table",
        htmltools::tags$caption(class = "g3-sr-only", t("db.import_report")),
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(scope = "col", t("db.col_source")), htmltools::tags$th(scope = "col", t("db.col_record")),
          htmltools::tags$th(scope = "col", t("db.col_status")), htmltools::tags$th(scope = "col", t("db.col_game")),
          htmltools::tags$th(scope = "col", t("db.col_first_invalid")), htmltools::tags$th(scope = "col", t("db.col_message"))
        )),
        htmltools::tags$tbody(lapply(seq_len(nrow(r)), function(i) {
          x <- r[i, ]
          htmltools::tags$tr(
            class = paste0("g3-import-", x$status),
            htmltools::tags$td(x$source %||% basename(x$path)), htmltools::tags$td(class = "g3-tnum", x$record),
            htmltools::tags$td(htmltools::span(class = "g3-badge", t(paste0("db.import_status.", x$status)))),
            htmltools::tags$td(x$game_id),
            htmltools::tags$td(class = "g3-tnum",
                               if (!is.na(x$first_invalid_event)) t("db.first_invalid", event = x$first_invalid_event,
                                                                    ply = x$first_invalid_ply %||% NA) else ""),
            htmltools::tags$td(x$message)
          )
        }))
      )
    )
  )
}

#' @noRd
.g3_db_quarantine_table <- function(q, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (!nrow(q)) return(htmltools::p(class = "g3-empty", t("db.no_quarantine")))
  htmltools::div(
    class = "g3-table-scroll",
    htmltools::tags$table(
      class = "g3-table",
      htmltools::tags$caption(t("db.quarantine_caption")),
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(scope = "col", t("db.col_source")), htmltools::tags$th(scope = "col", t("db.col_record")),
        htmltools::tags$th(scope = "col", t("db.col_condition")), htmltools::tags$th(scope = "col", t("db.col_first_invalid")),
        htmltools::tags$th(scope = "col", t("db.col_message"))
      )),
      htmltools::tags$tbody(lapply(seq_len(nrow(q)), function(i) {
        x <- q[i, ]
        htmltools::tags$tr(
          htmltools::tags$td(x$source), htmltools::tags$td(class = "g3-tnum", x$record),
          htmltools::tags$td(x$condition),
          htmltools::tags$td(class = "g3-tnum", if (!is.na(x$first_invalid_event)) {
            t("db.first_invalid", event = x$first_invalid_event, ply = x$first_invalid_ply)
          } else ""),
          htmltools::tags$td(x$diagnostic)
        )
      }))
    )
  )
}

#' @noRd
.g3_db_export_report <- function(st, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (identical(st$status, "idle")) return(htmltools::p(class = "g3-muted g3-small", t("db.export_note")))
  if (identical(st$status, "running")) {
    return(htmltools::div(
      htmltools::p(class = "g3-progress", role = "status", htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
                   t("db.exporting")),
      shiny::actionButton(ns("export_cancel"), t("job.cancel"), class = "g3-btn btn-outline-secondary")
    ))
  }
  if (identical(st$status, "cancelled")) return(htmltools::p(role = "status", t("db.export_cancelled")))
  if (!is.null(st$error)) return(htmltools::p(class = "g3-error", role = "alert", st$error))
  m <- st$result
  if (is.null(m)) return(NULL)
  htmltools::tagList(
    htmltools::p(class = "g3-ok", role = "status", t("db.export_done", n = m$n_games, path = st$tag)),
    htmltools::tags$ul(class = "g3-small", lapply(seq_len(nrow(m$files)), function(i) {
      htmltools::tags$li(m$files$file[i], " (", m$files$role[i], ", ", m$files$bytes[i], " B, ",
                         htmltools::tags$code(substr(m$files$sha256[i], 1L, 12L)), ")")
    })),
    shiny::downloadButton(ns("export_download"), t("db.export_download"), class = "g3-btn btn-outline-secondary", icon = NULL)
  )
}
