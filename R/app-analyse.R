# Analyse section: a separate branch of the live game.
#
# "Open the current game" copies the live game into a new analysis store
# with its own id (`analysis_*`), recording the live game id and ply it came
# from. The live store is never touched: moves, takebacks and engine runs
# here act on the branch only, and the branch is not saved. Plies are
# navigated with states that equal g3_replay_game(branch, ply); they are
# cached per ply and extended through g3_apply_action(), the same transition
# replay uses. Moves are played at the end of the branch; to explore from an
# earlier ply the user explicitly continues from it, which records takeback
# events in the branch (its history is kept).
#
# "Analyse position" runs g3_analyse_position() in a background R process
# with a declared node budget. Results are tagged with the state hash they
# describe and are shown only for that position.

#' @noRd
.g3_analyse_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  nav_btn <- function(input_id, key) {
    shiny::actionButton(ns(input_id), label = t(key), class = "g3-btn btn-outline-secondary")
  }
  htmltools::div(
    class = "g3-section g3-analyse",
    htmltools::tags$h2(class = "g3-section-title", tabindex = "-1", t("nav.analyse")),
    htmltools::p(class = "g3-lead", t("analyse.lead")),
    htmltools::div(
      class = "g3-btn-row",
      shiny::actionButton(ns("open"), t("analyse.open"), class = "g3-btn btn-primary")
    ),
    shiny::uiOutput(ns("branch_info")),
    htmltools::div(
      class = "g3-analyse-grid",
      htmltools::div(
        class = "g3-analyse-main",
        htmltools::div(
          class = "g3-btn-row g3-plynav", role = "group", !!!.g3_i18n_attr(c("aria-label" = "analyse.navigation"), lang),
          nav_btn("first", "analyse.first"), nav_btn("prev", "analyse.prev"),
          shiny::uiOutput(ns("ply_label"), inline = TRUE),
          nav_btn("next_ply", "analyse.next"), nav_btn("last", "analyse.last")
        ),
        .g3_board_ui(ns("board"), lang, "analysis"),
        htmltools::div(
          class = "g3-btn-row",
          nav_btn("branch_here", "analyse.branch_here"),
          nav_btn("undo", "analyse.undo")
        )
      ),
      htmltools::div(
        class = "g3-analyse-side",
        htmltools::tags$section(
          class = "g3-card",
          htmltools::tags$h3(class = "g3-card-title", t("analyse.engine")),
          shiny::selectInput(ns("budget"), label = t("analyse.budget"),
                             choices = stats::setNames(names(.G3_APP_BUDGETS),
                                                       vapply(names(.G3_APP_BUDGETS), function(b) {
                                                         .g3_t(paste0("budget.", b), lang, n = .G3_APP_BUDGETS[[b]])
                                                       }, "")),
                             selected = "quick"),
          htmltools::div(
            class = "g3-btn-row",
            nav_btn("run", "analyse.run"),
            nav_btn("cancel", "analyse.cancel")
          ),
          shiny::uiOutput(ns("engine"))
        ),
        htmltools::tags$section(
          class = "g3-card",
          htmltools::tags$h3(class = "g3-card-title", t("analyse.branch_moves")),
          shiny::uiOutput(ns("moves"))
        )
      )
    )
  )
}

#' @noRd
.g3_analyse_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    lang <- app$lang
    branch <- shiny::reactiveVal(NULL)
    tick <- shiny::reactiveVal(0L)
    bump <- function() tick(shiny::isolate(tick()) + 1L)
    viewed <- shiny::reactiveVal(NA_integer_)
    task <- new.env(parent = emptyenv())
    task$proc <- NULL
    task$tag <- NULL
    task$result <- NULL
    task$error <- NULL
    task_rv <- shiny::reactiveVal(0L)
    bump_task <- function() task_rv(shiny::isolate(task_rv()) + 1L)

    end_ply <- function(b) b$game$state$ply
    app$analysis_branch <- branch
    app$analysis_viewed <- viewed

    shiny::observeEvent(input$open, {
      live <- app$live$game
      b <- .g3_new_store(.g3_branch_from(live), kind = "analysis", saver = NULL)
      b$states <- list()
      branch(b)
      viewed(end_ply(b))
      bump()
    })
    app$open_analysis <- function() {
      shiny::isolate({
        live <- app$live$game
        b <- .g3_new_store(.g3_branch_from(live), kind = "analysis", saver = NULL)
        b$states <- list()
        branch(b)
        viewed(end_ply(b))
        bump()
      })
    }

    state_at <- function(b, ply) .g3_branch_state(b, ply)

    view <- shiny::reactive({
      tick()
      b <- branch()
      if (is.null(b)) return(NULL)
      p <- viewed()
      at_end <- identical(as.integer(p), as.integer(end_ply(b)))
      st <- state_at(b, p)
      list(
        state = st,
        token = c(.g3_store_token(b)[c("game_id", "revision")], list(state_hash = .g3_state_hash(st))),
        last = .g3_last_action(b$game, p),
        interactive = at_end,
        kind = "analysis",
        note = if (!at_end) .g3_t("analyse.viewing_earlier", lang(), ply = p) else NULL,
        announce = NULL
      )
    })
    dispatch <- function(req) {
      b <- shiny::isolate(branch())
      if (is.null(b)) return(.g3_ack(.g3_request(req) %||% list(), "rejected", "invalid_request", "No branch."))
      ack <- .g3_dispatch(b, req)
      if (identical(ack$status, "committed")) {
        viewed(end_ply(b))
        bump()
      }
      ack
    }
    .g3_board_server("board", view, app$prefs, lang, on_request = dispatch)

    output$branch_info <- shiny::renderUI({
      tick()
      b <- branch()
      if (is.null(b)) {
        return(htmltools::div(class = "g3-empty-state", htmltools::p(.g3_t("analyse.empty", lang()))))
      }
      m <- b$game$metadata
      htmltools::p(class = "g3-muted g3-small",
                   .g3_t("analyse.branch_of", lang(), branch = m$game_id, game = m$branch_of, ply = m$branch_ply))
    })
    output$ply_label <- shiny::renderUI({
      tick()
      b <- branch()
      if (is.null(b)) return(htmltools::span(class = "g3-ply g3-tnum", "–"))
      htmltools::span(class = "g3-ply g3-tnum", role = "status",
                      .g3_t("analyse.ply_of", lang(), ply = viewed(), n = end_ply(b)))
    })
    step <- function(to) {
      b <- branch()
      if (is.null(b)) return()
      lo <- b$game$initial_state$ply
      viewed(as.integer(max(lo, min(end_ply(b), to))))
    }
    shiny::observeEvent(input$first, step(-Inf))
    shiny::observeEvent(input$prev, step(viewed() - 1L))
    shiny::observeEvent(input$next_ply, step(viewed() + 1L))
    shiny::observeEvent(input$last, step(Inf))

    shiny::observeEvent(input$branch_here, {
      b <- branch()
      if (is.null(b)) return()
      n <- end_ply(b) - viewed()
      if (n < 1L) return()
      dispatch(.g3_store_request(b, "event", event = "takeback", n = n))
    })
    shiny::observeEvent(input$undo, {
      b <- branch()
      if (is.null(b) || !length(.g3_active_line(b$game$events))) return()
      dispatch(.g3_store_request(b, "event", event = "takeback", n = 1L))
    })
    shiny::observe({
      tick()
      b <- branch()
      p <- viewed()
      has <- !is.null(b)
      shiny::updateActionButton(session, "branch_here", disabled = !has || p >= end_ply(b))
      shiny::updateActionButton(session, "undo", disabled = !has || !length(.g3_active_line(b$game$events)) || p != end_ply(b))
      shiny::updateActionButton(session, "run", disabled = !has)
      for (nm in c("first", "prev", "next_ply", "last")) shiny::updateActionButton(session, nm, disabled = !has)
    })

    output$moves <- shiny::renderUI({
      tick()
      b <- branch()
      if (is.null(b)) return(htmltools::p(class = "g3-empty", .g3_t("analyse.no_branch", lang())))
      .g3_move_list(b$game, lang(), current_ply = viewed())
    })

    # ---- background engine analysis
    shiny::observeEvent(input$run, {
      b <- branch()
      if (is.null(b)) return()
      if (!is.null(task$proc)) try(task$proc$kill_tree(), silent = TRUE)
      st <- state_at(b, viewed())
      nodes <- .G3_APP_BUDGETS[[input$budget %||% "quick"]]
      task$tag <- list(state_hash = .g3_state_hash(st), ply = viewed(), nodes = nodes, started = Sys.time())
      task$result <- NULL
      task$error <- NULL
      task$proc <- tryCatch(.g3_analysis_start(st, nodes), error = function(e) {
        task$error <- conditionMessage(e)
        NULL
      })
      app$set_job("analysis", !is.null(task$proc))
      bump_task()
    })
    shiny::observeEvent(input$cancel, {
      if (!is.null(task$proc)) {
        try(task$proc$kill_tree(), silent = TRUE)
        task$proc <- NULL
        task$error <- .g3_t("analyse.cancelled", lang())
        app$set_job("analysis", FALSE)
        bump_task()
      }
    })
    shiny::observe({
      task_rv()
      if (is.null(task$proc)) return()
      shiny::invalidateLater(300)
      shiny::isolate({
        if (task$proc$is_alive()) return()
        res <- tryCatch(task$proc$get_result(), error = function(e) e)
        task$proc <- NULL
        app$set_job("analysis", FALSE)
        if (inherits(res, "error")) {
          task$error <- conditionMessage(res)
        } else {
          task$result <- res
        }
        bump_task()
      })
    })
    shiny::observe({
      task_rv()
      shiny::updateActionButton(session, "cancel", disabled = is.null(task$proc))
    })
    session$onSessionEnded(function() {
      if (!is.null(task$proc)) try(task$proc$kill_tree(), silent = TRUE)
    })
    output$engine <- shiny::renderUI({
      task_rv()
      tick()
      b <- branch()
      cur_hash <- if (is.null(b)) NULL else .g3_state_hash(state_at(b, viewed()))
      .g3_analysis_panel(task, cur_hash, lang())
    })
    invisible(list(branch = branch))
  })
}

# A branch copy of a game: same events and states, a new id, and a record of
# where it came from.
#' @noRd
.g3_branch_from <- function(game) {
  b <- game
  b$metadata$branch_of <- game$metadata$game_id
  b$metadata$branch_ply <- game$state$ply
  b$metadata$game_id <- .g3_new_id("analysis")
  b$metadata$mode <- "analysis"
  b$metadata$opponent <- NULL
  b
}

# State of a branch at `ply`, equal to g3_replay_game(branch$game, ply).
# Cached per ply and keyed by the branch's event count, so a takeback or a
# new move invalidates the cache.
#' @noRd
.g3_branch_state <- function(b, ply) {
  game <- b$game
  key <- paste(nrow(game$events), game$state$ply, sep = ":")
  if (!identical(b$states_key, key)) {
    b$states <- list()
    b$states_key <- key
  }
  lo <- game$initial_state$ply
  ply <- as.integer(ply)
  if (ply == game$state$ply) return(game$state)
  hit <- b$states[[as.character(ply)]]
  if (!is.null(hit)) return(hit)
  line <- .g3_active_line(game$events)
  # Start from the nearest cached ply below, else from the initial state.
  cached <- as.integer(names(b$states))
  cached <- cached[cached < ply]
  from <- if (length(cached)) max(cached) else lo
  st <- if (from == lo) game$initial_state else b$states[[as.character(from)]]
  if (from < ply) {
    for (i in line[(from - lo + 1L):(ply - lo)]) {
      st <- g3_apply_action(st, game$events$action_id[i])$state
      b$states[[as.character(st$ply)]] <- st
    }
  }
  st
}

#' @noRd
.g3_analysis_panel <- function(task, cur_hash, lang = "en") {
  t <- function(key, ...) .g3_t(key, lang, ...)
  if (!is.null(task$proc)) {
    return(htmltools::p(class = "g3-progress", role = "status",
                        htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
                        t("analyse.running", ply = task$tag$ply, nodes = task$tag$nodes)))
  }
  if (!is.null(task$error)) return(htmltools::p(class = "g3-error", role = "alert", task$error))
  r <- task$result
  if (is.null(r)) return(htmltools::p(class = "g3-muted g3-small", t("analyse.not_run")))
  stale <- !identical(task$tag$state_hash, cur_hash)
  alt <- r$alternatives
  alt <- alt[order(alt$rank), , drop = FALSE]
  top <- utils::head(alt, 8L)
  fmt <- function(i) {
    if (is.na(top$score[i])) return(t("analyse.unknown"))
    if (identical(top$score_kind[i], "mate")) return(t("analyse.mate_in", n = top$mate_in[i]))
    format(round(top$score[i]), big.mark = ",")
  }
  htmltools::tagList(
    if (stale) htmltools::p(class = "g3-note", role = "note", t("analyse.stale", ply = task$tag$ply)),
    htmltools::p(class = "g3-small", t("analyse.summary", depth = r$depth, completeness = r$completeness,
                                       nodes = r$cost$nodes, ms = round(r$cost$elapsed_ms))),
    htmltools::p(class = "g3-muted g3-small", t("analyse.perspective", side = t(paste0("side.", r$side)))),
    htmltools::tags$table(
      class = "g3-table",
      htmltools::tags$caption(class = "g3-sr-only", t("analyse.table_caption")),
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(scope = "col", "#"), htmltools::tags$th(scope = "col", t("analyse.col_move")),
        htmltools::tags$th(scope = "col", class = "g3-num", t("analyse.col_score")),
        htmltools::tags$th(scope = "col", t("analyse.col_bound"))
      )),
      htmltools::tags$tbody(lapply(seq_len(nrow(top)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(class = "g3-tnum", top$rank[i]),
          htmltools::tags$td(top$label[i]),
          htmltools::tags$td(class = "g3-num g3-tnum", fmt(i)),
          htmltools::tags$td(top$bound[i] %||% "")
        )
      }))
    ),
    if (length(r$pv_labels)) htmltools::p(class = "g3-small", htmltools::strong(t("analyse.pv")), " ",
                                          paste(r$pv_labels, collapse = "  →  "))
  )
}
