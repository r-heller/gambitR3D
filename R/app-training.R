# Training section, with two separate modes.
#
# "Practise chess" is for the person: exercises from the authored catalogue,
# from the last Coach review, or generated from the analysed game
# (`g3_create_exercises()`); a practice plan (`g3_practice_plan()`);
# graduated hints (`g3_exercise_hint()`, levels 1-4 strictly in order, so a
# later hint never shows before an earlier one); attempts scored by the
# engine (`g3_score_attempt()`) and recorded in the local learner profile
# when the learner allows it; progress, export, delete and reset. The
# profile's data-use flags are shown and changed here: practice (on) and
# inclusion in a training dataset (off by default).
#
# "Train opponent" is for the computer opponent: the ML environment status
# (`g3_setup_ml()` inspecting an existing environment, never downloading
# here), a bounded CPU training job and a self-play job within the project's
# CPU smoke limits, with real progress, budget, stop reason, cancel and
# resume. "Play against this model" appears only when the run produced a
# registered checkpoint that passes `g3_validate_model()`.

.G3_TRAIN_LIMITS <- list(max_steps = 20L, max_positions = 512L, wall_seconds = 600, threads = 2L, max_games = 2L,
                         max_plies = 80L, grace_seconds = 120)

# The numeric fields of "Train opponent": input id, label key and the
# accepted whole-number range. Anything else (empty, not a number, a
# fraction, out of range) stops the start with a message.
.G3_TRAIN_FIELDS <- list(
  steps = list(label = "trainer.steps", min = 1L, max = .G3_TRAIN_LIMITS$max_steps),
  positions = list(label = "trainer.positions", min = 32L, max = .G3_TRAIN_LIMITS$max_positions),
  data_games = list(label = "trainer.data_games", min = 1L, max = 8L),
  seed = list(label = "trainer.seed", min = 1L, max = .Machine$integer.max),
  sp_games = list(label = "trainer.sp_games", min = 1L, max = .G3_TRAIN_LIMITS$max_games),
  sp_plies = list(label = "trainer.sp_plies", min = 4L, max = .G3_TRAIN_LIMITS$max_plies),
  sp_sims = list(label = "trainer.sp_sims", min = 1L, max = 64L)
)

# Reads whole-number inputs. Returns list(values, problems): `values` holds
# the integers, `problems` one translated sentence per invalid field.
#' @noRd
.g3_train_inputs <- function(input, ids, lang = "en") {
  values <- list()
  problems <- character()
  for (id in ids) {
    f <- .G3_TRAIN_FIELDS[[id]]
    x <- input[[id]]
    v <- if (is.numeric(x) && length(x) == 1L) as.numeric(x) else NA_real_
    if (is.finite(v) && v == round(v) && v >= f$min && v <= f$max) {
      values[[id]] <- as.integer(v)
    } else {
      problems <- c(problems, .g3_t("trainer.invalid_input", lang, field = .g3_t(f$label, lang),
                                    min = f$min, max = f$max))
    }
  }
  list(values = values, problems = problems)
}

#' @noRd
.g3_training_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  card <- function(title_key, ..., class = NULL) {
    htmltools::tags$section(class = paste("g3-card", class),
                            htmltools::tags$h3(class = "g3-card-title", t(title_key)), ...)
  }
  practise <- htmltools::div(
    class = "g3-analyse-grid",
    htmltools::div(
      class = "g3-analyse-main",
      card(
        "practice.exercise",
        shiny::uiOutput(ns("exercise_head")),
        .g3_board_ui(ns("board"), lang, "analysis"),
        htmltools::div(
          class = "g3-btn-row",
          shiny::actionButton(ns("hint"), t("practice.next_hint"), class = "g3-btn btn-outline-secondary"),
          shiny::actionButton(ns("retry"), t("practice.retry"), class = "g3-btn btn-outline-secondary"),
          shiny::actionButton(ns("reveal"), t("practice.reveal"), class = "g3-btn btn-outline-secondary")
        ),
        shiny::uiOutput(ns("hints")),
        shiny::uiOutput(ns("feedback"))
      )
    ),
    htmltools::div(
      class = "g3-analyse-side",
      card(
        "practice.exercises",
        shiny::uiOutput(ns("sources")),
        htmltools::div(
          class = "g3-btn-row",
          shiny::actionButton(ns("from_review"), t("practice.from_review"), class = "g3-btn btn-outline-secondary"),
          shiny::actionButton(ns("from_analysis"), t("practice.from_analysis"), class = "g3-btn btn-outline-secondary")
        ),
        shiny::uiOutput(ns("generate_msg")),
        shiny::uiOutput(ns("plan"))
      ),
      card("practice.progress", shiny::uiOutput(ns("progress"))),
      card(
        "practice.profile",
        shiny::checkboxInput(ns("use_for_practice"), t("practice.use_for_practice"), value = TRUE),
        shiny::checkboxInput(ns("training_dataset"), t("practice.training_dataset"), value = FALSE),
        htmltools::p(class = "g3-muted g3-small", t("practice.privacy_note")),
        shiny::uiOutput(ns("profile_path")),
        htmltools::div(
          class = "g3-btn-row",
          shiny::downloadButton(ns("export_profile"), t("practice.export"), class = "g3-btn btn-outline-secondary", icon = NULL),
          shiny::actionButton(ns("reset_profile"), t("practice.reset"), class = "g3-btn btn-outline-secondary"),
          shiny::actionButton(ns("delete_profile"), t("practice.delete"), class = "g3-btn btn-outline-secondary")
        ),
        shiny::uiOutput(ns("profile_msg"))
      )
    )
  )
  train <- htmltools::div(
    class = "g3-two-col",
    card(
      "trainer.environment",
      shiny::uiOutput(ns("env_status")),
      htmltools::div(class = "g3-btn-row",
                     shiny::actionButton(ns("env_check"), t("trainer.env_check"), class = "g3-btn btn-outline-secondary")),
      htmltools::p(class = "g3-muted g3-small", t("trainer.env_note"))
    ),
    card(
      "trainer.run",
      htmltools::p(class = "g3-small", t("trainer.limits")),
      htmltools::div(
        class = "g3-filter-grid",
        shiny::numericInput(ns("steps"), t("trainer.steps"), value = 8, min = 1, max = .G3_TRAIN_LIMITS$max_steps, step = 1),
        shiny::numericInput(ns("positions"), t("trainer.positions"), value = 128, min = 32,
                            max = .G3_TRAIN_LIMITS$max_positions, step = 32),
        shiny::numericInput(ns("data_games"), t("trainer.data_games"), value = 4, min = 1, max = 8, step = 1),
        shiny::numericInput(ns("seed"), t("trainer.seed"), value = 20261026, min = 1, step = 1)
      ),
      htmltools::div(class = "g3-btn-row",
                     shiny::actionButton(ns("train"), t("trainer.start"), class = "g3-btn btn-primary")),
      shiny::uiOutput(ns("train_msg")),
      shiny::uiOutput(ns("train_job")),
      shiny::uiOutput(ns("train_result"))
    ),
    card(
      "trainer.selfplay",
      htmltools::p(class = "g3-small", t("trainer.selfplay_note")),
      htmltools::div(
        class = "g3-filter-grid",
        shiny::numericInput(ns("sp_games"), t("trainer.sp_games"), value = 1, min = 1, max = .G3_TRAIN_LIMITS$max_games),
        shiny::numericInput(ns("sp_plies"), t("trainer.sp_plies"), value = 40, min = 4, max = .G3_TRAIN_LIMITS$max_plies),
        shiny::numericInput(ns("sp_sims"), t("trainer.sp_sims"), value = 8, min = 1, max = 64)
      ),
      htmltools::div(class = "g3-btn-row",
                     shiny::actionButton(ns("selfplay"), t("trainer.sp_start"), class = "g3-btn btn-outline-secondary")),
      shiny::uiOutput(ns("sp_msg")),
      shiny::uiOutput(ns("sp_job")),
      shiny::uiOutput(ns("sp_result"))
    )
  )
  htmltools::div(
    class = "g3-section g3-training",
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", t("nav.training")),
    htmltools::p(class = "g3-lead", t("training.lead")),
    shiny::tabsetPanel(
      id = ns("mode"), type = "pills",
      shiny::tabPanel(title = t("training.practise"), value = "practise",
                      htmltools::p(class = "g3-muted", t("training.practise_body")), practise),
      shiny::tabPanel(title = t("training.opponent"), value = "train",
                      htmltools::p(class = "g3-muted", t("training.opponent_body")), train)
    )
  )
}

#' @noRd
.g3_training_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- app$lang
    profile_tick <- shiny::reactiveVal(0L)
    bump_profile <- function() profile_tick(shiny::isolate(profile_tick()) + 1L)
    profile <- shiny::reactive({
      app$storage_tick()
      file.path(app$storage()$root, "learner", "profile.json")
    })
    generated <- shiny::reactiveVal(NULL)
    from_review <- shiny::reactiveVal(NULL)   # exercises added from a review
    gen_msg <- shiny::reactiveVal(NULL)
    active <- shiny::reactiveVal(NULL)       # the g3_exercise in progress
    hints <- shiny::reactiveVal(list())
    attempt <- shiny::reactiveVal(NULL)
    record_note <- shiny::reactiveVal(NULL)  # what happened to the attempt's record
    revealed <- shiny::reactiveVal(FALSE)
    profile_msg <- shiny::reactiveVal(NULL)

    catalogue <- shiny::reactive(tryCatch(g3_exercise_catalogue(), error = function(e) e))
    exercise_set <- shiny::reactive({
      cat <- catalogue()
      parts <- list()
      if (!inherits(cat, "error")) parts <- c(parts, cat$exercises)
      parts <- c(parts, from_review())
      g <- generated()
      if (!is.null(g)) parts <- c(parts, g$exercises)
      ids <- vapply(parts, function(e) e$exercise_id, "")
      .g3_exercise_set(parts[!duplicated(ids)], provenance = list(source = "app"))
    })

    last_review <- shiny::reactive(if (is.function(app$review)) app$review() else NULL)
    review_exercises <- function(rv) if (is.null(rv)) list() else rv$exercises$exercises %||% list()
    output$sources <- shiny::renderUI({
      l <- lang()
      cat <- catalogue()
      rv <- last_review()
      g <- generated()
      n_cat <- if (inherits(cat, "error")) 0L else length(cat$exercises)
      n_rv <- length(review_exercises(rv))
      htmltools::tagList(
        htmltools::p(class = "g3-small", .g3_t("practice.sources", l, catalogue = n_cat,
                                               review = length(from_review()),
                                               generated = if (is.null(g)) 0L else length(g$exercises))),
        # Why the review button is (un)available, in words.
        htmltools::p(class = "g3-muted g3-small",
                     if (is.null(rv)) .g3_t("practice.need_review", l)
                     else if (!n_rv) .g3_t("practice.review_none", l)
                     else .g3_t("practice.review_available", l, n = n_rv)),
        if (inherits(cat, "error")) htmltools::p(class = "g3-error", conditionMessage(cat))
      )
    })
    shiny::observe({
      shiny::updateActionButton(session, "from_review", disabled = !length(review_exercises(last_review())))
    })
    # The review's exercises join the list only on request; adding again
    # after a new review replaces the earlier review's exercises.
    shiny::observeEvent(input$from_review, {
      rv <- last_review()
      ex <- review_exercises(rv)
      if (is.null(rv)) {
        gen_msg(list(type = "error", text = .g3_t("practice.need_review", lang())))
      } else if (!length(ex)) {
        gen_msg(list(type = "error", text = .g3_t("practice.review_none", lang())))
      } else {
        from_review(ex)
        gen_msg(list(type = "ok", text = .g3_t("practice.review_added", lang(), n = length(ex))))
      }
    })
    shiny::observeEvent(input$from_analysis, {
      res <- if (is.function(app$analysis_result)) app$analysis_result() else NULL
      b <- if (is.function(app$analysis_branch)) app$analysis_branch() else NULL
      if (is.null(res) || is.null(b)) {
        gen_msg(list(type = "error", text = .g3_t("practice.need_analysis", lang())))
        return()
      }
      set <- shiny::withProgress(message = .g3_t("practice.generating", lang()), value = 0.3, {
        tryCatch(g3_create_exercises(res, max_n = 10L, game = b$game), error = function(e) e)
      })
      if (inherits(set, "error")) {
        gen_msg(list(type = "error", text = conditionMessage(set)))
      } else {
        generated(set)
        gen_msg(list(type = "ok", text = .g3_t("practice.generated", lang(), n = length(set$exercises),
                                               valid = sum(set$summary$status == "validated"))))
      }
    })
    output$generate_msg <- shiny::renderUI({
      m <- gen_msg()
      if (is.null(m)) return(NULL)
      htmltools::p(class = if (m$type == "error") "g3-error" else "g3-ok", role = "status", m$text)
    })

    plan <- shiny::reactive({
      profile_tick()
      set <- exercise_set()
      tryCatch(g3_practice_plan(profile(), set, record = FALSE, lang = lang(), include_candidates = FALSE),
               error = function(e) e)
    })
    output$plan <- shiny::renderUI({
      p <- plan()
      l <- lang()
      set <- exercise_set()
      if (inherits(p, "error")) return(htmltools::p(class = "g3-error", conditionMessage(p)))
      htmltools::tagList(
        htmltools::p(class = "g3-small", htmltools::strong(.g3_t("practice.plan_today", l, n = nrow(p)))),
        if (!nrow(p)) htmltools::p(class = "g3-empty", .g3_t("practice.plan_empty", l)),
        htmltools::tags$ol(class = "g3-plan", lapply(seq_len(nrow(p)), function(i) {
          ex <- set$exercises[[match(p$exercise_id[i], set$summary$exercise_id)]]
          htmltools::tags$li(
            htmltools::tags$button(type = "button", class = "g3-linkbtn", `data-g3-click` = ns("start"),
                                   `data-g3-value` = p$exercise_id[i], .g3_exercise_title(ex, l)),
            htmltools::span(class = "g3-muted g3-small", " \u2013 ", p$reason[i])
          )
        })),
        htmltools::tags$details(
          class = "g3-disclosure",
          htmltools::tags$summary(.g3_t("practice.all_exercises", l, n = length(set$exercises))),
          htmltools::tags$ul(lapply(seq_along(set$exercises), function(i) {
            ex <- set$exercises[[i]]
            htmltools::tags$li(
              htmltools::tags$button(type = "button", class = "g3-linkbtn", `data-g3-click` = ns("start"),
                                     `data-g3-value` = ex$exercise_id, .g3_exercise_title(ex, l)),
              htmltools::span(class = "g3-chip", .g3_t(paste0("practice.status.", ex$status), l))
            )
          }))
        )
      )
    })
    shiny::observeEvent(input$start, {
      id <- input$start$value %||% input$start
      set <- exercise_set()
      i <- match(id, set$summary$exercise_id)
      if (is.na(i)) return()
      active(set$exercises[[i]])
      hints(list())
      attempt(NULL)
      record_note(NULL)
      revealed(FALSE)
    })

    output$exercise_head <- shiny::renderUI({
      ex <- active()
      l <- lang()
      if (is.null(ex)) return(htmltools::p(class = "g3-empty", .g3_t("practice.pick", l)))
      htmltools::tagList(
        htmltools::p(class = "g3-panel-title", g3_exercise_objective(ex, lang = l)),
        htmltools::p(class = "g3-small",
                     htmltools::span(class = "g3-chip", .g3_t(paste0("practice.source.", ex$provenance$source %||% "authored"), l)),
                     htmltools::span(class = "g3-chip", .g3_t(paste0("practice.status.", ex$status), l)),
                     htmltools::span(class = "g3-muted", " ", .g3_t("practice.theme", l, theme = ex$theme)))
      )
    })
    hint_marks <- shiny::reactive({
      h <- hints()
      if (!length(h)) return(NULL)
      last <- h[[length(h)]]$highlight
      list(kind = "hint", boards = last$boards, squares = last$squares, platform = last$platform)
    })
    view <- shiny::reactive({
      ex <- active()
      if (is.null(ex)) return(NULL)
      st <- ex$start_state
      list(state = st, token = list(game_id = paste0("exercise_", substr(ex$exercise_id, 1L, 12L)), revision = 0L,
                                    state_hash = .g3_state_hash(st)),
           last = NULL, interactive = TRUE, kind = "analysis", note = .g3_t("practice.board_note", lang()),
           announce = NULL, highlight = hint_marks())
    })
    on_request <- function(req) {
      r <- .g3_request(req)
      ex <- shiny::isolate(active())
      l <- shiny::isolate(lang())
      if (is.null(ex)) return(.g3_ack(r, "rejected", "invalid_request", .g3_t("practice.pick", l)))
      att <- tryCatch(g3_score_attempt(ex, r$action_id, hints_used = length(shiny::isolate(hints())), lang = l),
                      error = function(e) e)
      if (inherits(att, "error")) return(.g3_ack(r, "rejected", "invalid_request", conditionMessage(att)))
      attempt(att)
      # The profile's own setting decides: with practice recording off,
      # g3_learner_record() records nothing and says so. A failure to save
      # is shown, never swallowed.
      path <- shiny::isolate(profile())
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      rec <- tryCatch(g3_learner_record(path, att), error = function(e) e)
      if (inherits(rec, "error")) {
        record_note(list(type = "error", key = "practice.record_failed", error = conditionMessage(rec)))
        app$notify(.g3_t("practice.record_failed", l, error = conditionMessage(rec)), "error")
      } else if (!isTRUE(rec$recorded)) {
        record_note(list(type = "note", key = "practice.record_off", error = NULL))
      } else {
        record_note(NULL)
      }
      bump_profile()
      .g3_ack(r, "info", NA_character_, .g3_t(paste0("practice.result.", att$status), l))
    }
    .g3_board_server("board", view, app$prefs, lang, on_request = on_request)

    shiny::observeEvent(input$hint, {
      ex <- active()
      if (is.null(ex)) return()
      n <- length(hints())
      if (n >= 4L) return()
      h <- tryCatch(g3_exercise_hint(ex, n + 1L, lang = lang()), error = function(e) e)
      if (inherits(h, "error")) {
        app$notify(conditionMessage(h), "error")
        return()
      }
      hints(c(hints(), list(h)))
    })
    shiny::observe({
      ex <- active()
      n <- length(hints())
      shiny::updateActionButton(session, "hint", disabled = is.null(ex) || n >= 4L,
                                label = .g3_t("practice.hint_n", lang(), n = min(4L, n + 1L)))
      shiny::updateActionButton(session, "retry", disabled = is.null(attempt()))
      shiny::updateActionButton(session, "reveal", disabled = is.null(ex))
    })
    shiny::observeEvent(input$retry, {
      attempt(NULL)
      record_note(NULL)
    })
    shiny::observeEvent(input$reveal, {
      ex <- active()
      if (is.null(ex)) return()
      # Revealing walks the hints in order up to the last level.
      while (length(hints()) < 4L) {
        h <- tryCatch(g3_exercise_hint(ex, length(hints()) + 1L, lang = lang()), error = function(e) NULL)
        if (is.null(h)) break
        hints(c(hints(), list(h)))
      }
      revealed(TRUE)
    })
    output$hints <- shiny::renderUI({
      h <- hints()
      l <- lang()
      ex <- active()
      if (is.null(ex)) return(NULL)
      htmltools::tagList(
        if (length(h)) htmltools::tags$ol(class = "g3-hints", lapply(h, function(x) {
          htmltools::tags$li(htmltools::span(class = "g3-chip", .g3_t("practice.hint_level", l, n = x$level)), " ", x$text)
        })) else htmltools::p(class = "g3-muted g3-small", .g3_t("practice.no_hints", l)),
        if (revealed()) {
          sol <- ex$solutions
          htmltools::div(
            class = "g3-note", role = "note",
            htmltools::p(htmltools::strong(.g3_t("practice.solutions", l))),
            htmltools::tags$ul(lapply(seq_len(nrow(sol)), function(i) {
              htmltools::tags$li(sol$label[i], " (", .g3_t(paste0("practice.grade.", sol$grade[i]), l), ")")
            }))
          )
        }
      )
    })
    output$feedback <- shiny::renderUI({
      a <- attempt()
      l <- lang()
      if (is.null(a)) return(NULL)
      cls <- if (a$status %in% c("solved", "acceptable")) "g3-ok" else "g3-error"
      htmltools::div(
        class = paste("g3-feedback", paste0("g3-feedback-", a$status)), role = "status",
        htmltools::p(class = cls, htmltools::span(class = "g3-badge", .g3_t(paste0("practice.result.", a$status), l)),
                     " ", paste(a$feedback, collapse = " ")),
        if (identical(a$feedback_kind, "illegal_input")) htmltools::p(class = "g3-small", .g3_t("practice.illegal_note", l)),
        if (nrow(a$claims %||% data.frame())) .g3_claim_list(a$claims, l),
        if (length(a$continuation$notation %||% NULL)) {
          htmltools::p(class = "g3-small", .g3_t("practice.continuation", l,
                                                 line = paste(a$continuation$notation, collapse = "  \u2192  ")))
        },
        if (!is.null(rn <- record_note())) {
          htmltools::p(class = if (rn$type == "error") "g3-error" else "g3-muted g3-small",
                       role = if (rn$type == "error") "alert", .g3_t(rn$key, l, error = rn$error))
        }
      )
    })

    # ---- profile
    settings <- shiny::reactive({
      profile_tick()
      tryCatch(g3_learner_settings(profile()), error = function(e) NULL)
    })
    shiny::observe({
      s <- settings()
      if (is.null(s)) return()
      shiny::updateCheckboxInput(session, "use_for_practice", value = isTRUE(s$use_for_practice))
      shiny::updateCheckboxInput(session, "training_dataset", value = isTRUE(s$include_in_training_dataset))
    })
    shiny::observeEvent(input$use_for_practice, {
      s <- shiny::isolate(settings())
      if (!is.null(s) && !identical(isTRUE(s$use_for_practice), isTRUE(input$use_for_practice))) {
        dir.create(dirname(profile()), recursive = TRUE, showWarnings = FALSE)
        g3_learner_settings(profile(), use_for_practice = isTRUE(input$use_for_practice))
        bump_profile()
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$training_dataset, {
      s <- shiny::isolate(settings())
      if (!is.null(s) && !identical(isTRUE(s$include_in_training_dataset), isTRUE(input$training_dataset))) {
        dir.create(dirname(profile()), recursive = TRUE, showWarnings = FALSE)
        g3_learner_settings(profile(), include_in_training_dataset = isTRUE(input$training_dataset))
        bump_profile()
      }
    }, ignoreInit = TRUE)
    output$profile_path <- shiny::renderUI({
      htmltools::p(class = "g3-muted g3-small", .g3_t("practice.profile_path", lang(), path = profile()))
    })
    output$progress <- shiny::renderUI({
      profile_tick()
      l <- lang()
      pr <- tryCatch(g3_learner_progress(profile()), error = function(e) e)
      if (inherits(pr, "error")) return(htmltools::p(class = "g3-error", conditionMessage(pr)))
      if (!isTRUE(pr$summary$exists) || !nrow(pr$attempts)) return(htmltools::p(class = "g3-empty", .g3_t("practice.no_progress", l)))
      th <- pr$themes
      htmltools::tagList(
        htmltools::p(class = "g3-small", .g3_t("practice.progress_summary", l, n = pr$summary$n_attempts,
                                               ex = pr$summary$n_exercises)),
        htmltools::tags$table(
          class = "g3-table",
          htmltools::tags$caption(class = "g3-sr-only", .g3_t("practice.progress", l)),
          htmltools::tags$thead(htmltools::tags$tr(
            htmltools::tags$th(scope = "col", .g3_t("practice.col_theme", l)),
            htmltools::tags$th(scope = "col", class = "g3-num", .g3_t("practice.col_attempts", l)),
            htmltools::tags$th(scope = "col", class = "g3-num", .g3_t("practice.col_solved", l)),
            htmltools::tags$th(scope = "col", .g3_t("practice.col_state", l))
          )),
          htmltools::tags$tbody(lapply(seq_len(nrow(th)), function(i) {
            htmltools::tags$tr(htmltools::tags$td(th$theme[i]), htmltools::tags$td(class = "g3-num g3-tnum", th$attempts[i]),
                               htmltools::tags$td(class = "g3-num g3-tnum", th$solved[i]),
                               htmltools::tags$td(th$state[i]))
          }))
        )
      )
    })
    output$export_profile <- shiny::downloadHandler(
      filename = function() "gambitR3D-learner-profile.json",
      content = function(file) {
        tmp <- tempfile(fileext = ".json")
        g3_learner_export(profile(), tmp, format = "json")
        file.copy(tmp, file, overwrite = TRUE)
      },
      contentType = "application/json"
    )
    confirm <- function(kind) {
      shiny::showModal(shiny::modalDialog(
        title = .g3_t(paste0("practice.", kind, "_title"), lang()), easyClose = TRUE,
        htmltools::p(.g3_t(paste0("practice.", kind, "_body"), lang(), path = profile())),
        footer = htmltools::tagList(
          shiny::modalButton(.g3_t("common.cancel", lang())),
          shiny::actionButton(ns(paste0(kind, "_confirm")), .g3_t(paste0("practice.", kind), lang()), class = "btn-primary")
        )
      ))
    }
    shiny::observeEvent(input$reset_profile, confirm("reset"))
    shiny::observeEvent(input$delete_profile, confirm("delete"))
    shiny::observeEvent(input$reset_confirm, {
      shiny::removeModal()
      res <- tryCatch(g3_learner_reset(profile(), confirm = TRUE), error = function(e) e)
      profile_msg(if (inherits(res, "error")) conditionMessage(res) else .g3_t("practice.reset_done", lang()))
      bump_profile()
    })
    shiny::observeEvent(input$delete_confirm, {
      shiny::removeModal()
      res <- tryCatch(g3_learner_delete(profile(), confirm = TRUE), error = function(e) e)
      profile_msg(if (inherits(res, "error")) conditionMessage(res) else .g3_t("practice.delete_done", lang()))
      bump_profile()
    })
    output$profile_msg <- shiny::renderUI({
      m <- profile_msg()
      if (is.null(m)) return(NULL)
      htmltools::p(class = "g3-small", role = "status", m)
    })

    # ---- Train opponent
    env_bg <- .g3_bg_controller(app, session, "ml_check", wall_seconds = .G3_BG_WALL$ml_check)
    env_path <- function() .g3_ml_env_path(required = FALSE) %||%
      normalizePath(file.path(tools::R_user_dir("gambitR3D", "data"), "ml-env"), mustWork = FALSE)
    shiny::observeEvent(input$env_check, {
      env_bg$start(".g3_app_ml_check", list(path = env_path()))
    })
    output$env_status <- shiny::renderUI({
      env_bg$tick()
      .g3_ml_status_panel(env_bg$state, env_path(), lang())
    })
    train_job <- .g3_job_controller(app, session, "training", wall_seconds = .G3_TRAIN_LIMITS$wall_seconds,
                                    grace_seconds = .G3_TRAIN_LIMITS$grace_seconds)
    train_msg <- shiny::reactiveVal(NULL)
    sp_msg <- shiny::reactiveVal(NULL)
    problem_list <- function(m) {
      if (!length(m)) return(NULL)
      htmltools::div(class = "g3-error", role = "alert",
                     htmltools::tags$ul(lapply(m, htmltools::tags$li)),
                     htmltools::p(.g3_t("trainer.nothing_started", lang())))
    }
    output$train_msg <- shiny::renderUI(problem_list(train_msg()))
    output$sp_msg <- shiny::renderUI(problem_list(sp_msg()))
    shiny::observeEvent(input$train, {
      chk <- .g3_train_inputs(input, c("steps", "positions", "data_games", "seed"), lang())
      train_msg(chk$problems)
      if (length(chk$problems)) return()
      steps <- chk$values$steps
      positions <- chk$values$positions
      games <- chk$values$data_games
      seed <- chk$values$seed
      env <- .g3_ml_env_path(required = FALSE)
      if (is.null(env)) {
        train_msg(.g3_t("trainer.no_env", lang()))
        app$notify(.g3_t("trainer.no_env", lang()), "error")
        return()
      }
      out <- file.path(app$storage()$root, "training", paste0("run-", format(Sys.time(), "%Y%m%d-%H%M%S")))
      train_job$start("training", list(
        config = list(profile = "tiny", steps = steps, threads = .G3_TRAIN_LIMITS$threads, seed = seed,
                      ml_env = env, registry_dir = .g3_registry_dir(), register = TRUE),
        dataset = list(games = games, positions = positions, seed = seed),
        output_dir = out
      ), tag = list(steps = steps, positions = positions, output_dir = out), label = "training")
    })
    shiny::observeEvent(input$train_cancel, train_job$cancel())
    shiny::observeEvent(input$train_resume, train_job$resume())
    output$train_job <- shiny::renderUI({
      train_job$tick()
      .g3_job_panel(train_job, ns, "train", lang())
    })
    trained_model <- shiny::reactive({
      train_job$tick()
      run <- train_job$state$result
      if (is.null(run) || is.null(run$model_id) || is.na(run$model_id)) return(NULL)
      v <- tryCatch(g3_validate_model(run$model_id, error = FALSE), error = function(e) NULL)
      list(id = run$model_id, valid = isTRUE(v$valid), validation = v)
    })
    output$train_result <- shiny::renderUI({
      train_job$tick()
      .g3_training_result(train_job$state$result, trained_model(), ns, lang())
    })
    shiny::observeEvent(input$play_trained, {
      m <- trained_model()
      if (is.null(m) || !m$valid) return()
      app$start_model_game(m$id, mode = "policy")
    })

    sp_job <- .g3_job_controller(app, session, "self_play", wall_seconds = .G3_TRAIN_LIMITS$wall_seconds,
                                 grace_seconds = .G3_TRAIN_LIMITS$grace_seconds)
    shiny::observeEvent(input$selfplay, {
      chk <- .g3_train_inputs(input, c("sp_games", "sp_plies", "sp_sims"), lang())
      sp_msg(chk$problems)
      if (length(chk$problems)) return()
      m <- trained_model()
      model_id <- if (!is.null(m) && m$valid) m$id else .g3_latest_trained_model()
      if (is.null(model_id)) {
        sp_msg(.g3_t("trainer.sp_need_model", lang()))
        app$notify(.g3_t("trainer.sp_need_model", lang()), "error")
        return()
      }
      out <- file.path(app$storage()$root, "self-play", paste0("run-", format(Sys.time(), "%Y%m%d-%H%M%S")))
      sp_job$start("self_play", list(
        config = list(model = model_id, games = chk$values$sp_games, max_plies = chk$values$sp_plies,
                      simulations = chk$values$sp_sims, fixtures = character(),
                      threads = .G3_TRAIN_LIMITS$threads, ml_env = .g3_ml_env_path(required = FALSE)),
        output_dir = out
      ), tag = list(model = model_id, output_dir = out), label = "self_play")
    })
    shiny::observeEvent(input$sp_cancel, sp_job$cancel())
    shiny::observeEvent(input$sp_resume, sp_job$resume())
    output$sp_job <- shiny::renderUI({
      sp_job$tick()
      .g3_job_panel(sp_job, ns, "sp", lang(), resumable = FALSE)
    })
    output$sp_result <- shiny::renderUI({
      sp_job$tick()
      run <- sp_job$state$result
      l <- lang()
      if (is.null(run) || is.null(run$games)) return(NULL)
      g <- run$games
      htmltools::tagList(
        htmltools::p(class = "g3-small", .g3_t("trainer.sp_done", l, n = nrow(g), model = run$model$model_id %||% "")),
        htmltools::tags$ul(lapply(seq_len(nrow(g)), function(i) {
          htmltools::tags$li(.g3_t("trainer.sp_game", l, plies = g$plies[i], status = g$status[i],
                                   result = g$result[i] %||% NA, termination = g$termination[i] %||% NA))
        })),
        htmltools::p(class = "g3-muted g3-small", .g3_t("trainer.sp_truncation", l))
      )
    })
  })
}

#' @noRd
.g3_app_ml_check <- function(path) {
  g3_setup_ml(path, offline = TRUE)
}

#' @noRd
.g3_latest_trained_model <- function() {
  caps <- tryCatch(g3_model_capabilities(), error = function(e) NULL)
  if (is.null(caps)) return(NULL)
  ok <- caps$kind == "trained_checkpoint" & caps$task == "play" & caps$status == "available"
  ids <- unique(caps$model_id[ok])
  if (length(ids)) utils::tail(ids, 1L) else NULL
}

#' @noRd
.g3_exercise_title <- function(ex, lang) {
  paste0(.g3_t(paste0("practice.theme_name.", ex$theme), lang), " \u00B7 ",
         .g3_t(paste0("practice.source.", ex$provenance$source %||% "authored"), lang),
         if (!is.null(ex$provenance$title) && !is.na(ex$provenance$title)) paste0(" \u00B7 ", ex$provenance$title))
}

#' @noRd
.g3_ml_status_panel <- function(st, path, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (identical(st$status, "running")) {
    return(htmltools::p(class = "g3-progress", role = "status", htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
                        t("trainer.env_checking", path = path)))
  }
  if (identical(st$status, "timed_out")) {
    return(htmltools::p(class = "g3-error", role = "alert", t("trainer.env_timed_out", s = st$wall_seconds)))
  }
  if (!is.null(st$error)) return(htmltools::p(class = "g3-error", role = "alert", st$error))
  r <- st$result
  if (is.null(r)) {
    found <- !is.null(.g3_ml_env_path(required = FALSE))
    return(htmltools::p(class = "g3-small", t(if (found) "trainer.env_found" else "trainer.env_missing", path = path)))
  }
  ready <- identical(r$status, "ready")
  htmltools::tagList(
    htmltools::p(class = if (ready) "g3-ok" else "g3-error", role = "status",
                 htmltools::span(class = "g3-badge", t(paste0("trainer.env_", if (ready) "ready" else "blocked"))),
                 " ", if (!ready) r$reason),
    if (ready) htmltools::tags$dl(
      class = "g3-facts",
      htmltools::tags$dt(t("trainer.env_path")), htmltools::tags$dd(r$path),
      htmltools::tags$dt("Python"), htmltools::tags$dd(r$python$version %||% ""),
      htmltools::tags$dt("PyTorch"), htmltools::tags$dd(paste(r$torch$version %||% "", r$torch$device %||% "")),
      htmltools::tags$dt(t("trainer.env_schema")), htmltools::tags$dd(if (isTRUE(r$schema_match)) t("common.yes") else t("common.no"))
    )
  )
}

#' @noRd
.g3_training_result <- function(run, model, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (is.null(run) || is.null(run$metrics)) return(NULL)
  m <- run$metrics
  tail_m <- utils::tail(m, 5L)
  htmltools::tagList(
    htmltools::p(class = "g3-small", t("trainer.run_done", status = run$status, reason = run$stop_reason %||% "",
                                       steps = nrow(m), loss1 = signif(m$loss[1L], 3), lossn = signif(m$loss[nrow(m)], 3))),
    htmltools::tags$table(
      class = "g3-table",
      htmltools::tags$caption(class = "g3-sr-only", t("trainer.metrics")),
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(scope = "col", class = "g3-num", t("trainer.col_step")),
        htmltools::tags$th(scope = "col", class = "g3-num", t("trainer.col_loss")),
        htmltools::tags$th(scope = "col", class = "g3-num", t("trainer.col_grad"))
      )),
      htmltools::tags$tbody(lapply(seq_len(nrow(tail_m)), function(i) {
        htmltools::tags$tr(htmltools::tags$td(class = "g3-num g3-tnum", tail_m$step[i]),
                           htmltools::tags$td(class = "g3-num g3-tnum", signif(tail_m$loss[i], 4)),
                           htmltools::tags$td(class = "g3-num g3-tnum", signif(tail_m$grad_norm[i], 3)))
      }))
    ),
    htmltools::p(class = "g3-muted g3-small", t("trainer.no_strength")),
    if (!is.null(run$checkpoint$path)) htmltools::p(class = "g3-small", t("trainer.checkpoint", path = run$checkpoint$path)),
    if (is.null(model)) {
      htmltools::p(class = "g3-muted g3-small", t("trainer.no_checkpoint"))
    } else if (!model$valid) {
      htmltools::p(class = "g3-error", t("trainer.invalid_model", id = model$id,
                                         problems = paste(model$validation$problems, collapse = "; ")))
    } else {
      htmltools::tagList(
        htmltools::p(class = "g3-ok", t("trainer.model_ready", id = model$id)),
        shiny::actionButton(ns("play_trained"), t("trainer.play_model"), class = "g3-btn btn-primary")
      )
    }
  )
}
