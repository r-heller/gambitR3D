# Coach section: post-game review and action explanations.
#
# A review (`g3_coach_game()`) runs as a background "coach" job over a copy
# of the live game or of the analysis branch, in the app's language. Its
# decisions are listed by ply; selecting a ply shows that ply's claims with
# their type and strength (exact rules facts, values exact at a stated
# depth, analysed lines, proofs, heuristics with their definitions) and the
# evidence id each claim rests on. A replayed line from the evidence can be
# previewed step by step on the coach board; the preview is computed with
# g3_apply_action() on a copy and is clearly labelled, and "Return to the
# game" drops it. The same highlight drives the 2D and 3D views.
#
# Any legal action of the position on the coach board can be explained
# (`g3_explain_action()`): pick it in the list, or select a piece and a
# destination on the board itself (the coach board never plays a move).

#' @noRd
.g3_coach_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  card <- function(title_key, ..., class = NULL) {
    htmltools::tags$section(class = paste("g3-card", class),
                            htmltools::tags$h3(class = "g3-card-title", t(title_key)), ...)
  }
  htmltools::div(
    class = "g3-section g3-coach",
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", t("nav.coach")),
    htmltools::p(class = "g3-lead", t("coach.lead")),
    htmltools::div(
      class = "g3-analyse-grid",
      htmltools::div(
        class = "g3-analyse-main",
        shiny::uiOutput(ns("board_info")),
        .g3_board_ui(ns("board"), lang, "analysis"),
        htmltools::div(
          class = "g3-btn-row", role = "group",
          shiny::actionButton(ns("line_prev"), t("coach.line_prev"), class = "g3-btn btn-outline-secondary"),
          shiny::actionButton(ns("line_next"), t("coach.line_next"), class = "g3-btn btn-outline-secondary"),
          shiny::actionButton(ns("line_return"), t("coach.line_return"), class = "g3-btn btn-outline-secondary")
        ),
        card(
          "coach.explain_title",
          htmltools::p(class = "g3-muted g3-small", t("coach.explain_note")),
          shiny::selectInput(ns("explain_action"), t("coach.explain_pick"), choices = c(), width = "100%"),
          htmltools::div(class = "g3-btn-row",
                         shiny::actionButton(ns("explain"), t("coach.explain_go"), class = "g3-btn btn-outline-secondary")),
          shiny::uiOutput(ns("explanation"))
        )
      ),
      htmltools::div(
        class = "g3-analyse-side",
        card(
          "coach.review_title",
          shiny::radioButtons(ns("source"), t("coach.review_source"),
                              choiceValues = list("live", "branch"),
                              choiceNames = list(t("coach.source_live"), t("coach.source_branch"))),
          shiny::selectInput(ns("budget"), t("analyse.budget_per_position"),
                             choices = stats::setNames(names(.G3_ANALYSIS_BUDGETS), vapply(names(.G3_ANALYSIS_BUDGETS), function(b) {
                               .g3_t(c(fast = "budget.fast", standard = "budget.standard", deep = "budget.deeper")[[b]], lang,
                                     n = .G3_ANALYSIS_BUDGETS[[b]])
                             }, "")), selected = "standard"),
          htmltools::div(class = "g3-btn-row",
                         shiny::actionButton(ns("review"), t("coach.review_go"), class = "g3-btn btn-primary")),
          shiny::uiOutput(ns("review_job")),
          shiny::uiOutput(ns("review_summary"))
        ),
        card("coach.decisions", shiny::uiOutput(ns("decisions"))),
        card("coach.claims", shiny::uiOutput(ns("claims"))),
        card(
          "coach.facts_title",
          htmltools::p(class = "g3-muted g3-small", t("coach.facts_note")),
          shiny::uiOutput(ns("facts"))
        )
      )
    )
  )
}

#' @noRd
.g3_coach_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- app$lang
    job <- .g3_job_controller(app, session, "coach")
    review <- shiny::reactiveVal(NULL)
    reviewed <- shiny::reactiveVal(NULL)    # a store over the reviewed game copy
    sel_ply <- shiny::reactiveVal(NA_integer_)
    preview <- shiny::reactiveVal(NULL)     # list(states, actions, step, evidence_id)
    explanation <- shiny::reactiveVal(NULL)
    app$review <- review

    source_game <- function() {
      if (identical(input$source, "branch")) {
        b <- app$analysis_branch()
        if (!is.null(b)) return(b$game)
      }
      app$live$game
    }

    shiny::observeEvent(input$review, {
      g <- source_game()
      if (!length(.g3_active_line(g$events))) {
        app$notify(.g3_t("coach.nothing_to_review", lang()), "error")
        return()
      }
      nodes <- .G3_ANALYSIS_BUDGETS[[input$budget %||% "standard"]]
      review(NULL)
      preview(NULL)
      st <- .g3_new_store(g, "analysis")
      st$states <- list()
      reviewed(st)
      job$start("coach", list(game = g, budget = list(nodes = nodes), lang = lang()),
                tag = list(game_hash = g3_game_hash(g), nodes = nodes), label = "coach")
    })
    shiny::observeEvent(input$review_cancel, job$cancel())
    shiny::observeEvent(input$review_resume, job$resume())
    shiny::observe({
      job$tick()
      r <- job$state$result
      if (inherits(r, "g3_coach_review") && !identical(shiny::isolate(review()), r)) {
        review(r)
        crit <- r$decisions$ply[r$decisions$critical]
        sel_ply(if (length(crit)) crit[1L] else utils::tail(r$decisions$ply, 1L))
      }
    })
    output$review_job <- shiny::renderUI({
      job$tick()
      .g3_job_panel(job, ns, "review", lang())
    })
    output$review_summary <- shiny::renderUI({
      r <- review()
      l <- lang()
      if (is.null(r)) return(NULL)
      htmltools::tagList(
        htmltools::p(class = "g3-small", .g3_t("coach.review_done", l, n = nrow(r$decisions),
                                               critical = sum(r$decisions$critical), evaluator = r$evaluator$id,
                                               nodes = r$budget$nodes %||% NA, language = r$lang)),
        htmltools::tags$details(
          class = "g3-disclosure",
          htmltools::tags$summary(.g3_t("coach.limitations", l)),
          htmltools::tags$ul(lapply(unlist(r$limitations), htmltools::tags$li))
        )
      )
    })

    output$decisions <- shiny::renderUI({
      r <- review()
      l <- lang()
      if (is.null(r)) return(htmltools::p(class = "g3-empty", .g3_t("coach.no_review", l)))
      .g3_coach_decisions(r, sel_ply(), ns, l)
    })
    shiny::observeEvent(input$pick_ply, {
      v <- suppressWarnings(as.integer(input$pick_ply$value %||% input$pick_ply))
      if (length(v) == 1L && !is.na(v)) {
        sel_ply(v)
        preview(NULL)
        explanation(NULL)
      }
    })
    output$claims <- shiny::renderUI({
      r <- review()
      l <- lang()
      if (is.null(r)) return(htmltools::p(class = "g3-empty", .g3_t("coach.no_review", l)))
      .g3_coach_claims(r, sel_ply(), ns, l)
    })

    # ---- line preview on a copy
    shiny::observeEvent(input$preview_line, {
      r <- review()
      st <- reviewed()
      id <- input$preview_line$value %||% input$preview_line
      ev <- r$evidence[[id]]
      if (is.null(ev) || is.null(ev$actions) || is.null(st)) return()
      start <- .g3_find_state(st, ev$start_state_hash, ev$ply)
      if (is.null(start)) {
        app$notify(.g3_t("coach.line_unavailable", lang()), "error")
        return()
      }
      states <- list(start)
      for (a in ev$actions) states[[length(states) + 1L]] <- g3_apply_action(states[[length(states)]], a)$state
      preview(list(states = states, actions = ev$actions, notation = ev$notation, step = 1L, evidence_id = id))
    })
    shiny::observeEvent(input$line_next, {
      p <- preview()
      if (is.null(p)) return()
      p$step <- min(length(p$actions), p$step + 1L)
      preview(p)
    })
    shiny::observeEvent(input$line_prev, {
      p <- preview()
      if (is.null(p)) return()
      p$step <- max(0L, p$step - 1L)
      preview(p)
    })
    shiny::observeEvent(input$line_return, preview(NULL))
    shiny::observe({
      p <- preview()
      shiny::updateActionButton(session, "line_prev", disabled = is.null(p) || p$step <= 0L)
      shiny::updateActionButton(session, "line_next", disabled = is.null(p) || p$step >= length(p$actions))
      shiny::updateActionButton(session, "line_return", disabled = is.null(p))
    })

    # The position the coach board shows.
    board_state <- shiny::reactive({
      p <- preview()
      if (!is.null(p)) return(p$states[[p$step + 1L]])
      st <- reviewed()
      ply <- sel_ply()
      if (!is.null(st) && !is.na(ply)) {
        # A decision at ply k is taken in the position after ply k - 1.
        lo <- st$game$initial_state$ply
        return(.g3_branch_state(st, max(lo, ply - 1L)))
      }
      app$live_tick()
      app$live$game$state
    })
    view <- shiny::reactive({
      st <- board_state()
      p <- preview()
      l <- lang()
      last <- NULL
      hl <- NULL
      note <- NULL
      if (!is.null(p)) {
        note <- .g3_t("coach.preview_note", l, step = p$step, n = length(p$actions))
        if (p$step >= 1L) {
          last <- .g3_action_marks(p$states[[p$step]], p$actions[[p$step]], p$notation[[p$step]])
        }
        if (p$step < length(p$actions)) {
          nxt <- .g3_action_marks(p$states[[p$step + 1L]], p$actions[[p$step + 1L]], p$notation[[p$step + 1L]])
          hl <- list(kind = "line", squares = unique(c(nxt$from, nxt$to)))
        }
      } else if (!is.null(reviewed()) && !is.na(sel_ply())) {
        note <- .g3_t("coach.board_decision", l, ply = sel_ply())
      }
      list(state = st, token = list(game_id = "coach", revision = 0L, state_hash = .g3_state_hash(st)),
           last = last, interactive = !identical(st$outcome$status, "terminal"), kind = "analysis",
           note = note, announce = NULL, highlight = hl)
    })
    explain <- function(state, action_id) {
      x <- tryCatch(g3_explain_action(state, action_id, lang = lang()), error = function(e) e)
      explanation(x)
      x
    }
    on_request <- function(req) {
      r <- .g3_request(req)
      st <- shiny::isolate(board_state())
      x <- explain(st, r$action_id)
      if (inherits(x, "error")) return(.g3_ack(r, "rejected", "illegal_action", conditionMessage(x)))
      a <- .g3_ack(r, "info", NA_character_, .g3_t("coach.explained_below", shiny::isolate(lang())))
      a
    }
    .g3_board_server("board", view, app$prefs, lang, on_request = on_request)

    output$board_info <- shiny::renderUI({
      p <- preview()
      l <- lang()
      if (!is.null(p)) {
        return(htmltools::div(
          class = "g3-banner g3-banner-preview", role = "status",
          htmltools::p(htmltools::strong(.g3_t("coach.preview_title", l)), " ",
                       .g3_t("coach.preview_line", l, line = paste(p$notation, collapse = "  \u2192  "))),
          htmltools::p(class = "g3-small", .g3_t("coach.preview_return", l))
        ))
      }
      NULL
    })

    shiny::observe({
      st <- board_state()
      acts <- g3_legal_actions(st)
      choices <- if (nrow(acts)) stats::setNames(acts$action_id, acts$label) else c()
      shiny::updateSelectInput(session, "explain_action", choices = choices)
    })
    shiny::observeEvent(input$explain, {
      a <- input$explain_action
      if (is.null(a) || !nzchar(a)) return()
      explain(board_state(), a)
    })
    output$explanation <- shiny::renderUI({
      x <- explanation()
      l <- lang()
      if (is.null(x)) return(NULL)
      if (inherits(x, "error")) return(htmltools::p(class = "g3-error", role = "alert", conditionMessage(x)))
      htmltools::tagList(
        htmltools::p(htmltools::strong(x$label)),
        .g3_claim_list(x$claims, l)
      )
    })
    output$facts <- shiny::renderUI({
      app$live_tick()
      .g3_move_facts(app$live$game, lang())
    })
    invisible(list(review = review))
  })
}

# Find the state with a given hash near a ply of a reviewed game.
#' @noRd
.g3_find_state <- function(store, hash, ply) {
  lo <- store$game$initial_state$ply
  hi <- store$game$state$ply
  for (p in unique(pmin(hi, pmax(lo, c(ply - 1L, ply, ply - 2L, ply + 1L))))) {
    st <- .g3_branch_state(store, p)
    if (identical(.g3_state_hash(st), hash)) return(st)
  }
  NULL
}

# Board marks for one action applied to `state` (from/to squares).
#' @noRd
.g3_action_marks <- function(state, action_id, notation = NULL) {
  acts <- g3_legal_actions(state)
  a <- acts[acts$action_id == action_id, , drop = FALSE]
  if (!nrow(a)) return(list(action_id = action_id, from = NULL, to = NULL))
  list(action_id = action_id, type = a$type, from = a$from_square,
       to = if (is.na(a$to_square)) NULL else a$to_square,
       platform = if (is.na(a$platform_id)) NULL else a$platform_id,
       notation = notation %||% a$label, check = FALSE)
}

#' @noRd
.g3_coach_decisions <- function(r, sel, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  d <- r$decisions
  if (!nrow(d)) return(htmltools::p(class = "g3-empty", t("coach.no_decisions")))
  htmltools::div(
    class = "g3-table-scroll",
    htmltools::tags$table(
      class = "g3-table",
      htmltools::tags$caption(class = "g3-sr-only", t("coach.decisions")),
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(scope = "col", t("analyse.col_ply")), htmltools::tags$th(scope = "col", t("coach.played")),
        htmltools::tags$th(scope = "col", t("coach.best_analysed")),
        htmltools::tags$th(scope = "col", class = "g3-num", t("coach.gap"))
      )),
      htmltools::tags$tbody(lapply(seq_len(nrow(d)), function(i) {
        x <- d[i, ]
        cur <- isTRUE(x$ply == sel)
        htmltools::tags$tr(
          class = paste(if (cur) "g3-row-current", if (isTRUE(x$critical)) "g3-row-critical"),
          `aria-current` = if (cur) "step" else NULL,
          htmltools::tags$td(htmltools::tags$button(type = "button", class = "g3-linkbtn g3-tnum",
                                                    `data-g3-click` = ns("pick_ply"), `data-g3-value` = x$ply,
                                                    `aria-label` = t("analyse.goto_ply", ply = x$ply), x$ply),
                             if (isTRUE(x$critical)) htmltools::span(class = "g3-chip g3-chip-critical",
                                                                     htmltools::span(`aria-hidden` = "true", "!"),
                                                                     t("coach.critical"))),
          htmltools::tags$td(x$played_label), htmltools::tags$td(x$best_label),
          htmltools::tags$td(class = "g3-num g3-tnum", if (is.na(x$gap)) "\u2013" else format(round(x$gap)))
        )
      }))
    )
  )
}

#' @noRd
.g3_claim_list <- function(claims, lang, ns = NULL, evidence = NULL) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (!nrow(claims)) return(htmltools::p(class = "g3-empty", t("coach.no_claims")))
  htmltools::tags$ul(
    class = "g3-claims",
    lapply(seq_len(nrow(claims)), function(i) {
      x <- claims[i, ]
      ev <- if (!is.null(evidence) && !is.na(x$evidence_id)) evidence[[x$evidence_id]] else NULL
      htmltools::tags$li(
        htmltools::span(class = paste0("g3-chip g3-claim-", x$type), t(paste0("coach.type.", x$type))),
        htmltools::span(class = paste0("g3-chip g3-strength-", x$strength), t(paste0("coach.strength.", x$strength))),
        " ", x$text,
        if (!is.na(x$definition %||% NA) && nzchar(x$definition)) {
          htmltools::tags$details(class = "g3-disclosure", htmltools::tags$summary(t("coach.definition")), x$definition)
        },
        if (!is.na(x$evidence_id %||% NA)) htmltools::span(class = "g3-muted g3-small", " ", t("coach.evidence", id = x$evidence_id)),
        if (!is.null(ns) && !is.null(ev$actions) && length(ev$actions)) {
          htmltools::tags$button(type = "button", class = "g3-btn btn btn-sm btn-outline-secondary",
                                 `data-g3-click` = ns("preview_line"), `data-g3-value` = x$evidence_id,
                                 t("coach.preview_go", n = length(ev$actions)))
        }
      )
    })
  )
}

#' @noRd
.g3_coach_claims <- function(r, ply, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  cl <- r$claims
  if (is.na(ply)) return(htmltools::p(class = "g3-empty", t("coach.pick_ply")))
  cl <- cl[!is.na(cl$ply) & cl$ply == ply, , drop = FALSE]
  ev <- r$evidence
  htmltools::tagList(
    htmltools::p(class = "g3-small", htmltools::strong(t("coach.claims_for", ply = ply))),
    .g3_claim_list(cl, lang),
    {
      lines <- Filter(function(e) identical(e$kind, "line") && identical(as.integer(e$ply), as.integer(ply)), ev)
      if (length(lines)) {
        htmltools::div(class = "g3-btn-row", lapply(names(lines), function(id) {
          htmltools::tags$button(type = "button", class = "g3-btn btn btn-sm btn-outline-primary",
                                 `data-g3-click` = ns("preview_line"), `data-g3-value` = id,
                                 t("coach.preview_named", id = id, line = paste(lines[[id]]$notation, collapse = " ")))
        }))
      }
    }
  )
}
