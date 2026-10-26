# Models section: the model registry.
#
# Lists every model the registry knows (`g3_model_capabilities()`): the
# built-in search, the untrained transformer architecture, reference-only
# Hugging Face candidates and locally registered checkpoints, with their
# status, kind and source. A model's detail shows its capability per task
# with the reason when unavailable, the full `g3_validate_model()` check
# table, provenance, resources and artifacts. A model that validates for
# play can be chosen as the Play opponent (policy or MCTS mode). Reference
# candidates and the untrained architecture are shown, but their "use"
# action is disabled with the reason. Nothing here needs Python unless a
# model is actually played.

#' @noRd
.g3_models_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  card <- function(title_key, ...) {
    htmltools::tags$section(class = "g3-card", htmltools::tags$h3(class = "g3-card-title", t(title_key)), ...)
  }
  htmltools::div(
    class = "g3-section g3-models",
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", t("nav.models")),
    htmltools::p(class = "g3-lead", t("models.lead")),
    htmltools::div(
      class = "g3-analyse-grid",
      htmltools::div(class = "g3-analyse-main",
                     card("models.registry", shiny::uiOutput(ns("list")),
                          htmltools::div(class = "g3-btn-row",
                                         shiny::actionButton(ns("refresh"), t("models.refresh"),
                                                             class = "g3-btn btn-outline-secondary")))),
      htmltools::div(class = "g3-analyse-side", card("models.detail", shiny::uiOutput(ns("detail"))))
    )
  )
}

#' @noRd
.g3_models_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- app$lang
    refresh <- shiny::reactiveVal(0L)
    selected <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$refresh, refresh(refresh() + 1L))
    caps <- shiny::reactive({
      refresh()
      app$section()
      tryCatch(g3_model_capabilities(), error = function(e) e)
    })
    output$list <- shiny::renderUI({
      cp <- caps()
      l <- lang()
      if (inherits(cp, "error")) return(htmltools::p(class = "g3-error", role = "alert", conditionMessage(cp)))
      .g3_models_table(cp, selected(), ns, l)
    })
    shiny::observeEvent(input$pick, {
      v <- input$pick$value %||% input$pick
      if (is.character(v) && length(v) == 1L) selected(v)
    })
    detail <- shiny::reactive({
      id <- selected()
      refresh()
      if (is.null(id)) return(NULL)
      m <- tryCatch(.g3_model_resolve(id), error = function(e) e)
      if (inherits(m, "error")) return(m)
      v <- tryCatch(g3_validate_model(m, error = FALSE), error = function(e) e)
      cap <- tryCatch(g3_model_capabilities(m), error = function(e) e)
      list(model = m, validation = v, caps = cap)
    })
    output$detail <- shiny::renderUI({
      d <- detail()
      l <- lang()
      if (is.null(d)) return(htmltools::p(class = "g3-empty", .g3_t("models.pick", l)))
      if (inherits(d, "error")) return(htmltools::p(class = "g3-error", conditionMessage(d)))
      .g3_model_detail(d, ns, l)
    })
    shiny::observeEvent(input$use_model, {
      d <- detail()
      if (is.null(d) || inherits(d, "error") || !.g3_model_playable(d)) return()
      app$start_model_game(d$model$model_id, mode = input$use_mode %||% "policy")
    })
  })
}

# Can this model be chosen as an opponent? (Validated for play, and a real
# checkpoint; the search entry is offered as "Search" in Play already.)
#' @noRd
.g3_model_playable <- function(d) {
  m <- d$model
  v <- d$validation
  cp <- d$caps
  identical(m$manifest$kind, "trained_checkpoint") && !inherits(v, "error") && isTRUE(v$valid) &&
    !inherits(cp, "error") && any(cp$task == "play" & cp$status == "available")
}

#' @noRd
.g3_models_table <- function(cp, sel, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  ids <- unique(cp$model_id)
  rows <- lapply(ids, function(id) {
    r <- cp[cp$model_id == id, , drop = FALSE]
    play <- r$status[r$task == "play"][1L]
    htmltools::tags$tr(
      class = if (identical(id, sel)) "g3-row-current" else NULL,
      htmltools::tags$td(htmltools::tags$button(type = "button", class = "g3-linkbtn", `data-g3-click` = ns("pick"),
                                                `data-g3-value` = id, r$display_name[1L] %||% id),
                         htmltools::div(class = "g3-muted g3-small", htmltools::tags$code(id))),
      htmltools::tags$td(t(paste0("models.kind.", r$kind[1L]))),
      htmltools::tags$td(htmltools::span(class = "g3-chip", r$model_status[1L])),
      htmltools::tags$td(htmltools::span(class = paste0("g3-chip g3-cap-", play),
                                         t(paste0("models.cap.", play %||% "unavailable")))),
      htmltools::tags$td(r$source[1L])
    )
  })
  htmltools::div(
    class = "g3-table-scroll",
    htmltools::tags$table(
      class = "g3-table",
      htmltools::tags$caption(class = "g3-sr-only", t("models.registry")),
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(scope = "col", t("models.col_name")), htmltools::tags$th(scope = "col", t("models.col_kind")),
        htmltools::tags$th(scope = "col", t("models.col_status")), htmltools::tags$th(scope = "col", t("models.col_play")),
        htmltools::tags$th(scope = "col", t("models.col_source"))
      )),
      htmltools::tags$tbody(rows)
    )
  )
}

#' @noRd
.g3_model_detail <- function(d, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  m <- d$model
  man <- m$manifest
  cp <- d$caps
  v <- d$validation
  kv <- function(x) {
    if (is.null(x) || !length(x)) return(NULL)
    htmltools::tags$dl(class = "g3-facts", lapply(names(x), function(k) {
      val <- x[[k]]
      val <- if (is.list(val)) jsonlite::toJSON(val, auto_unbox = TRUE) else paste(format(val), collapse = ", ")
      htmltools::tagList(htmltools::tags$dt(k), htmltools::tags$dd(as.character(val)))
    }))
  }
  playable <- .g3_model_playable(d)
  reason <- if (playable) NULL else if (identical(man$kind, "reference_candidate")) {
    t("models.reason_reference")
  } else if (identical(man$kind, "architecture")) {
    t("models.reason_untrained")
  } else if (identical(man$kind, "search")) {
    t("models.reason_search")
  } else if (!inherits(v, "error") && !isTRUE(v$valid)) {
    paste(v$problems, collapse = "; ")
  } else {
    t("models.reason_runtime")
  }
  htmltools::tagList(
    htmltools::p(htmltools::strong(man$display_name %||% m$model_id), " ", htmltools::tags$code(m$model_id)),
    htmltools::p(class = "g3-small", t("models.kind_status", kind = man$kind %||% "", status = man$status %||% "")),
    htmltools::div(
      class = "g3-btn-row",
      shiny::selectInput(ns("use_mode"), t("models.mode"),
                         choices = stats::setNames(c("policy", "mcts"), c(t("models.mode_policy"), t("models.mode_mcts")))),
      htmltools::tags$button(type = "button", id = ns("use_model"), class = "btn btn-primary g3-btn action-button",
                             disabled = if (playable) NULL else NA, t("models.use"))
    ),
    if (!is.null(reason)) htmltools::p(class = "g3-muted g3-small", role = "note", t("models.not_usable", reason = reason)),
    htmltools::tags$h4(class = "g3-subtitle", t("models.capabilities")),
    if (inherits(cp, "error")) htmltools::p(class = "g3-error", conditionMessage(cp)) else htmltools::tags$table(
      class = "g3-table",
      htmltools::tags$caption(class = "g3-sr-only", t("models.capabilities")),
      htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th(scope = "col", t("models.col_task")),
                                               htmltools::tags$th(scope = "col", t("models.col_status")),
                                               htmltools::tags$th(scope = "col", t("models.col_reason")))),
      htmltools::tags$tbody(lapply(seq_len(nrow(cp)), function(i) {
        htmltools::tags$tr(htmltools::tags$td(cp$task[i]),
                           htmltools::tags$td(htmltools::span(class = paste0("g3-chip g3-cap-", cp$status[i]),
                                                              t(paste0("models.cap.", cp$status[i])))),
                           htmltools::tags$td(if (is.na(cp$reason[i])) "" else cp$reason[i]))
      }))
    ),
    htmltools::tags$details(
      class = "g3-disclosure",
      htmltools::tags$summary(t("models.validation", ok = if (!inherits(v, "error") && isTRUE(v$valid)) t("common.yes") else t("common.no"))),
      if (inherits(v, "error")) htmltools::p(class = "g3-error", conditionMessage(v)) else htmltools::tags$table(
        class = "g3-table",
        htmltools::tags$tbody(lapply(seq_len(nrow(v$checks)), function(i) {
          htmltools::tags$tr(htmltools::tags$td(v$checks$check[i]), htmltools::tags$td(v$checks$status[i]),
                             htmltools::tags$td(v$checks$detail[i]))
        }))
      )
    ),
    htmltools::tags$details(class = "g3-disclosure", htmltools::tags$summary(t("models.provenance")), kv(man$provenance)),
    htmltools::tags$details(class = "g3-disclosure", htmltools::tags$summary(t("models.resources")),
                            kv(c(man$resources %||% list(), list(parameters = man$parameters %||% NULL,
                                                                 weights_digest = man$weights_digest %||% NULL)))),
    if (length(man$artifacts)) htmltools::tags$details(
      class = "g3-disclosure", htmltools::tags$summary(t("models.artifacts")),
      htmltools::tags$ul(lapply(man$artifacts, function(a) htmltools::tags$li(a$role %||% "", ": ", a$path %||% "",
                                                                              " ", htmltools::tags$code(substr(a$sha256 %||% "", 1L, 12L)))))
    )
  )
}
