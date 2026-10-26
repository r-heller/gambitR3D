# Section shells: Database, Coach, Models and Training.
#
# These sections have stable module boundaries now and are filled by later
# phases (game database, coaching, model registry and training). Until a
# capability is connected the section says so precisely, names what it needs,
# and shows no button that would pretend to work. What is real today is
# shown: the local autosave folder (Database), deterministic engine facts
# about the last move (Coach), and the built-in opponents (Models).
#
# Availability is detected from this build's namespace, never assumed.

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
                 htmltools::span(class = "g3-badge g3-badge-off", htmltools::span(`aria-hidden` = "true", "○"),
                                 t("common.not_connected")),
                 " ", htmltools::strong(t(title_key))),
    htmltools::p(t(body_key, ...)),
    htmltools::p(class = "g3-muted g3-small", t("common.needs", what = paste(needs, collapse = ", ")))
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

# ---- Database

#' @noRd
.g3_database_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    output$body <- shiny::renderUI({
      lang <- app$lang()
      app$storage_tick()
      app$live_tick()
      st <- app$storage()
      t <- function(key, ...) .g3_t(key, lang, ...)
      n_files <- tryCatch(nrow(st$list_games()), error = function(e) NA_integer_)
      db_api <- .g3_api_available("g3_open_game_db") && .g3_api_available("g3_query_games")
      htmltools::tagList(
        htmltools::p(class = "g3-lead", t("database.lead")),
        htmltools::tags$section(
          class = "g3-card",
          htmltools::tags$h3(class = "g3-card-title", t("database.local_title")),
          htmltools::p(t("database.local_body", n = n_files, dir = st$games_dir)),
          htmltools::p(class = "g3-muted g3-small", t("database.local_hint"))
        ),
        if (db_api && !is.null(app$config$db_path)) {
          .g3_unavailable("database.collection", "database.api_present_body",
                          c("app integration of g3_import_games(), g3_query_games() and g3_export_games()"), lang,
                          path = app$config$db_path)
        } else if (db_api) {
          .g3_unavailable("database.collection", "database.api_no_path_body", c("g3_run_app(db_path = ...)"), lang)
        } else {
          .g3_unavailable("database.collection", "database.api_missing_body",
                          c("g3_open_game_db()", "g3_import_games()", "g3_query_games()"), lang)
        }
      )
    })
  })
}

# ---- Coach

#' @noRd
.g3_coach_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    output$body <- shiny::renderUI({
      lang <- app$lang()
      app$live_tick()
      t <- function(key, ...) .g3_t(key, lang, ...)
      coach_api <- .g3_api_available("g3_coach_game")
      htmltools::tagList(
        htmltools::p(class = "g3-lead", t("coach.lead")),
        htmltools::tags$section(
          class = "g3-card",
          htmltools::tags$h3(class = "g3-card-title", t("coach.facts_title")),
          htmltools::p(class = "g3-muted g3-small", t("coach.facts_note")),
          .g3_move_facts(app$live$game, lang)
        ),
        .g3_unavailable("coach.coaching", if (coach_api) "coach.api_present_body" else "coach.api_missing_body",
                        c("g3_coach_game()", "g3_create_exercises()", "g3_score_attempt()"), lang)
      )
    })
  })
}

# ---- Models

#' @noRd
.g3_models_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    output$body <- shiny::renderUI({
      lang <- app$lang()
      t <- function(key, ...) .g3_t(key, lang, ...)
      rows <- list(
        list(name = t("agent.search"), kind = t("models.kind_search"),
             status = t("models.ready"), detail = paste(.G3_SEARCH_VERSION, .G3_EVAL_VERSION)),
        list(name = t("agent.greedy"), kind = t("models.kind_baseline"),
             status = t("models.ready"), detail = .G3_EVAL_VERSION),
        list(name = t("agent.random"), kind = t("models.kind_baseline"),
             status = t("models.ready"), detail = t("models.random_detail"))
      )
      registry_api <- .g3_api_available("g3_register_model") && .g3_api_available("g3_model_capabilities")
      htmltools::tagList(
        htmltools::p(class = "g3-lead", t("models.lead")),
        htmltools::tags$section(
          class = "g3-card",
          htmltools::tags$h3(class = "g3-card-title", t("models.builtin")),
          htmltools::tags$table(
            class = "g3-table",
            htmltools::tags$caption(class = "g3-sr-only", t("models.builtin")),
            htmltools::tags$thead(htmltools::tags$tr(
              htmltools::tags$th(scope = "col", t("models.col_name")),
              htmltools::tags$th(scope = "col", t("models.col_kind")),
              htmltools::tags$th(scope = "col", t("models.col_status")),
              htmltools::tags$th(scope = "col", t("models.col_version"))
            )),
            htmltools::tags$tbody(lapply(rows, function(r) {
              htmltools::tags$tr(
                htmltools::tags$td(r$name), htmltools::tags$td(r$kind),
                htmltools::tags$td(htmltools::span(class = "g3-badge g3-badge-ok",
                                                   htmltools::span(`aria-hidden` = "true", "✓"), r$status)),
                htmltools::tags$td(htmltools::tags$code(r$detail))
              )
            }))
          ),
          htmltools::p(class = "g3-muted g3-small", t("models.choose_hint"))
        ),
        .g3_unavailable("models.trained", if (registry_api) "models.api_present_body" else "models.api_missing_body",
                        c("g3_register_model()", "g3_model_capabilities()", "g3_setup_ml()"), lang)
      )
    })
  })
}

# ---- Training

#' @noRd
.g3_training_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    output$body <- shiny::renderUI({
      lang <- app$lang()
      t <- function(key, ...) .g3_t(key, lang, ...)
      htmltools::tagList(
        htmltools::p(class = "g3-lead", t("training.lead")),
        htmltools::div(
          class = "g3-two-col",
          htmltools::tags$section(
            class = "g3-card",
            htmltools::tags$h3(class = "g3-card-title", t("training.practise")),
            htmltools::p(t("training.practise_body")),
            .g3_unavailable("training.practise_status", "training.practise_missing",
                            c("g3_create_exercises()", "g3_score_attempt()"), lang)
          ),
          htmltools::tags$section(
            class = "g3-card",
            htmltools::tags$h3(class = "g3-card-title", t("training.opponent")),
            htmltools::p(t("training.opponent_body")),
            .g3_unavailable("training.opponent_status", "training.opponent_missing",
                            c("g3_setup_ml()", "g3_train_model()"), lang)
          )
        )
      )
    })
  })
}
