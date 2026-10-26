# Analyse section: a separate branch of a game, with full analysis.
#
# "Open the current game" (or "Open in Analyse" from the Database) copies the
# game into a new analysis store with its own id (`analysis_*`), recording
# the source game id and ply. The live store and the catalogue are never
# touched: moves, takebacks and engine runs here act on the branch only, and
# the branch is not saved. Plies are navigated with states that equal
# g3_replay_game(branch, ply). Playing on from an earlier ply ("Continue from
# this ply") takes back later branch moves (kept in the branch log) and turns
# the branch into a labelled variation.
#
# Whole-game analysis (g3_analyse_game() over the branch) runs as a
# background "analysis" job; its result drives the evaluation plot, the move
# and event table, tactics and the heatmap. Board, table and plot share one
# selected ply: a plot click, a table row or previous/next select the same
# ply everywhere. A one-position search (g3_analyse_position()) runs in a
# background R process; results are tagged with the state they describe.

.G3_ANALYSIS_BUDGETS <- c(fast = 2000L, standard = 20000L, deep = 100000L)

#' @noRd
.g3_analyse_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  nav_btn <- function(input_id, key) {
    shiny::actionButton(ns(input_id), label = t(key), class = "g3-btn btn-outline-secondary")
  }
  card <- function(title_key, ..., class = NULL) {
    htmltools::tags$section(class = paste("g3-card", class),
                            htmltools::tags$h3(class = "g3-card-title", t(title_key)), ...)
  }
  budget_select <- function(input_id, label_key, budgets, keys, selected) {
    shiny::selectInput(ns(input_id), label = t(label_key),
                       choices = stats::setNames(names(budgets), vapply(names(budgets), function(b) {
                         .g3_t(keys[[b]], lang, n = budgets[[b]])
                       }, "")), selected = selected)
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
        ),
        card(
          "analyse.trajectory",
          shiny::selectInput(ns("metric"), t("analyse.metric"),
                             choices = stats::setNames(
                               c("expected_score", "score", "move_loss", "material", "mobility"),
                               vapply(paste0("analyse.metric.", c("expected_score", "score", "move_loss", "material",
                                                                  "mobility")), .g3_t, "", lang = lang)
                             )),
          shiny::uiOutput(ns("plot_note")),
          shiny::plotOutput(ns("trajectory"), height = "280px", click = ns("plot_click"))
        ),
        card("analyse.moves_events", shiny::uiOutput(ns("move_table"))),
        card(
          "analyse.heatmap",
          htmltools::div(
            class = "g3-filter-grid",
            shiny::selectInput(ns("heat_metric"), t("analyse.heat_metric"),
                               choices = stats::setNames(c("occupancy", "attacks", "visits", "captures"),
                                                         vapply(paste0("analyse.heat.", c("occupancy", "attacks", "visits",
                                                                                          "captures")), .g3_t, "",
                                                                lang = lang))),
            shiny::selectInput(ns("heat_color"), t("analyse.heat_color"),
                               choices = stats::setNames(c("both", "white", "black"),
                                                         vapply(c("analyse.heat.both", "side.white", "side.black"),
                                                                .g3_t, "", lang = lang)))
          ),
          shiny::uiOutput(ns("heat_note")),
          shiny::plotOutput(ns("heatmap"), height = "420px")
        )
      ),
      htmltools::div(
        class = "g3-analyse-side",
        card(
          "analyse.game_analysis",
          htmltools::p(class = "g3-muted g3-small", t("analyse.game_analysis_note")),
          budget_select("game_budget", "analyse.budget_per_position", .G3_ANALYSIS_BUDGETS,
                        c(fast = "budget.fast", standard = "budget.standard", deep = "budget.deeper"), "fast"),
          htmltools::div(class = "g3-btn-row", nav_btn("run_game", "analyse.run_game")),
          shiny::uiOutput(ns("game_job")),
          shiny::uiOutput(ns("game_summary")),
          htmltools::div(class = "g3-btn-row",
                         shiny::downloadButton(ns("report"), t("analyse.report"), class = "g3-btn btn-outline-secondary",
                                               icon = NULL)),
          htmltools::p(class = "g3-muted g3-small", t("analyse.report_note"))
        ),
        card("analyse.tactics", shiny::uiOutput(ns("tactics"))),
        card(
          "analyse.engine",
          budget_select("budget", "analyse.budget", .G3_APP_BUDGETS,
                        c(quick = "budget.quick", standard = "budget.standard", deeper = "budget.deeper"), "quick"),
          htmltools::div(class = "g3-btn-row", nav_btn("run", "analyse.run"), nav_btn("cancel", "analyse.cancel")),
          shiny::uiOutput(ns("engine"))
        ),
        card("analyse.branch_moves", shiny::uiOutput(ns("moves")))
      )
    )
  )
}

#' @noRd
.g3_analyse_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
    game_job <- .g3_job_controller(app, session, "analysis")
    result <- shiny::reactiveVal(NULL)

    end_ply <- function(b) b$game$state$ply
    app$analysis_branch <- branch
    app$analysis_viewed <- viewed
    app$analysis_result <- result

    # Open a branch of `game` (default: the live game).
    open_branch <- function(game = NULL, label = NULL) {
      shiny::isolate({
        src <- game %||% app$live$game
        b <- .g3_new_store(.g3_branch_from(src), kind = "analysis", saver = NULL)
        b$states <- list()
        b$origin_ids <- b$game$events$action_id[.g3_active_line(b$game$events)]
        b$label <- label
        branch(b)
        viewed(end_ply(b))
        result(NULL)
        bump()
      })
    }
    app$open_analysis <- function(game = NULL, label = NULL) open_branch(game, label)
    shiny::observeEvent(input$open, open_branch())

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
      l <- lang()
      if (is.null(b)) {
        return(htmltools::div(class = "g3-empty-state", htmltools::p(.g3_t("analyse.empty", l))))
      }
      m <- b$game$metadata
      div_ply <- .g3_variation_ply(b)
      htmltools::p(
        class = "g3-muted g3-small",
        .g3_t("analyse.branch_of", l, branch = m$game_id, game = m$branch_of, ply = m$branch_ply),
        if (!is.null(b$label)) htmltools::span(class = "g3-chip", b$label),
        if (!is.na(div_ply)) htmltools::span(class = "g3-chip g3-chip-variation",
                                             .g3_t("analyse.variation_from", l, ply = div_ply))
      )
    })
    output$ply_label <- shiny::renderUI({
      tick()
      b <- branch()
      if (is.null(b)) return(htmltools::span(class = "g3-ply g3-tnum", "\u2013"))
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
    # A move-table row or a plot point selects the same ply everywhere.
    shiny::observeEvent(input$goto, {
      v <- suppressWarnings(as.integer(input$goto$value %||% input$goto))
      if (length(v) == 1L && !is.na(v)) step(v)
    })
    shiny::observeEvent(input$plot_click, {
      x <- input$plot_click$x
      if (is.numeric(x) && length(x) == 1L && is.finite(x)) step(round(x))
    })

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
      shiny::updateActionButton(session, "run_game", disabled = !has || game_job$running())
      for (nm in c("first", "prev", "next_ply", "last")) shiny::updateActionButton(session, nm, disabled = !has)
    })

    output$moves <- shiny::renderUI({
      tick()
      b <- branch()
      if (is.null(b)) return(htmltools::p(class = "g3-empty", .g3_t("analyse.no_branch", lang())))
      .g3_move_list(b$game, lang(), current_ply = viewed())
    })

    # ---- whole-game analysis job
    shiny::observeEvent(input$run_game, {
      b <- branch()
      if (is.null(b)) return()
      nodes <- .G3_ANALYSIS_BUDGETS[[input$game_budget %||% "fast"]]
      result(NULL)
      game_job$start("analysis", list(db = list(b$game), evaluator = "search", budget = list(nodes = nodes)),
                     tag = list(game_hash = g3_game_hash(b$game), plies = end_ply(b), nodes = nodes),
                     label = "analysis")
    })
    shiny::observeEvent(input$game_cancel, game_job$cancel())
    shiny::observeEvent(input$game_resume, game_job$resume())
    shiny::observe({
      game_job$tick()
      st <- game_job$state
      if (is.null(st$result) || !is.null(shiny::isolate(result()))) return()
      res <- .g3_read_analysis_job(st)
      if (inherits(res, "g3_analysis")) result(res) else if (is.character(res)) st$error <- res
    })
    output$game_job <- shiny::renderUI({
      game_job$tick()
      .g3_job_panel(game_job, ns, "game", lang())
    })
    stale <- shiny::reactive({
      tick()
      r <- result()
      b <- branch()
      !is.null(r) && !is.null(b) && !identical(r$game$game_content_hash, g3_game_hash(b$game))
    })
    output$game_summary <- shiny::renderUI({
      r <- result()
      l <- lang()
      if (is.null(r)) return(NULL)
      s <- r$summary
      pv <- r$provenance
      htmltools::tagList(
        if (stale()) htmltools::p(class = "g3-note", role = "note", .g3_t("analyse.result_stale", l)),
        htmltools::p(class = "g3-small", .g3_t("analyse.game_summary", l, plies = r$game$n_plies,
                                               evaluator = pv$evaluator, nodes = pv$budget$nodes %||% NA,
                                               ms = round(sum(unlist(pv$timings_ms)), 0))),
        htmltools::p(class = "g3-muted g3-small", .g3_t("analyse.expected_note", l))
      )
    })

    # ---- trajectory plot, linked to the selected ply
    plot_obj <- shiny::reactive({
      r <- result()
      if (is.null(r)) return(NULL)
      tryCatch(g3_plot_game(r, metric = input$metric %||% "expected_score"), error = function(e) e)
    })
    output$plot_note <- shiny::renderUI({
      l <- lang()
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        return(.g3_unavailable("analyse.trajectory", "analyse.needs_ggplot", "ggplot2", l))
      }
      p <- plot_obj()
      if (is.null(p)) return(htmltools::p(class = "g3-empty", .g3_t("analyse.no_result", l)))
      if (inherits(p, "error")) return(htmltools::p(class = "g3-error", conditionMessage(p)))
      meta <- attr(p, "g3_plot_meta")
      htmltools::p(class = "g3-muted g3-small",
                   if (isTRUE(meta$empty)) meta$reason else .g3_t("analyse.plot_hint", l))
    })
    output$trajectory <- shiny::renderPlot({
      p <- plot_obj()
      shiny::req(!is.null(p), !inherits(p, "error"))
      v <- viewed()
      if (!isTRUE(attr(p, "g3_plot_meta")$empty) && !is.na(v)) {
        p <- p + ggplot2::geom_vline(xintercept = v, linetype = "dashed", colour = "#E69F00", linewidth = 0.8)
      }
      p
    }, alt = shiny::reactive({
      .g3_t("analyse.plot_alt", lang(), metric = input$metric %||% "expected_score", ply = viewed())
    }))

    output$move_table <- shiny::renderUI({
      r <- result()
      l <- lang()
      if (is.null(r)) return(htmltools::p(class = "g3-empty", .g3_t("analyse.no_result", l)))
      .g3_analysis_move_table(r, viewed(), ns, l)
    })
    output$tactics <- shiny::renderUI({
      r <- result()
      l <- lang()
      if (is.null(r)) return(htmltools::p(class = "g3-empty", .g3_t("analyse.no_result", l)))
      .g3_analysis_tactics(r, viewed(), ns, l)
    })

    heat_obj <- shiny::reactive({
      r <- result()
      if (is.null(r)) return(NULL)
      tryCatch(g3_plot_board_heatmap(r, metric = input$heat_metric %||% "occupancy",
                                     color = input$heat_color %||% "both"), error = function(e) e)
    })
    output$heat_note <- shiny::renderUI({
      h <- heat_obj()
      l <- lang()
      if (is.null(h)) return(htmltools::p(class = "g3-empty", .g3_t("analyse.no_result", l)))
      if (inherits(h, "error")) return(htmltools::p(class = "g3-error", conditionMessage(h)))
      meta <- attr(h, "g3_plot_meta")
      htmltools::p(class = "g3-muted g3-small", if (isTRUE(meta$empty)) meta$reason else .g3_t("analyse.heat_note", l))
    })
    output$heatmap <- shiny::renderPlot({
      h <- heat_obj()
      shiny::req(!is.null(h), !inherits(h, "error"))
      h
    }, alt = shiny::reactive(.g3_t("analyse.heat_alt", lang(), metric = input$heat_metric %||% "occupancy")))

    output$report <- shiny::downloadHandler(
      filename = function() paste0("gambitR3D-analysis-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".zip"),
      content = function(file) {
        r <- result()
        if (is.null(r)) stop(.g3_t("analyse.no_result", lang()))
        dir <- file.path(tempfile("g3report-"), "report")
        dir.create(dirname(dir), recursive = TRUE)
        g3_export_analysis_report(r, dir, title = paste("gambitR3D", r$game$game_id))
        old <- setwd(dirname(dir))
        on.exit(setwd(old), add = TRUE)
        utils::zip(file, "report", flags = "-r9Xq")
      },
      contentType = "application/zip"
    )
    shiny::observe({
      shiny::updateActionButton(session, "report", disabled = is.null(result()))
    })

    # ---- one-position engine analysis
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
      app$set_job("position", !is.null(task$proc))
      bump_task()
    })
    shiny::observeEvent(input$cancel, {
      if (!is.null(task$proc)) {
        try(task$proc$kill_tree(), silent = TRUE)
        task$proc <- NULL
        task$error <- .g3_t("analyse.cancelled", lang())
        app$set_job("position", FALSE)
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
        app$set_job("position", FALSE)
        if (inherits(res, "error")) task$error <- conditionMessage(res) else task$result <- res
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

# The g3_analysis written by an "analysis" job for its single game.
#' @noRd
.g3_read_analysis_job <- function(st) {
  r <- st$result
  if (is.null(r) || is.null(r$completed) || !nrow(r$completed)) {
    if (!is.null(r$errors) && nrow(r$errors)) return(paste(r$errors$message, collapse = "; "))
    return(NULL)
  }
  out_dir <- r$manifest$output_dir %||% file.path(st$job$dir, "analysis")
  path <- file.path(out_dir, r$completed$result_file[1L])
  tryCatch(readRDS(path), error = function(e) conditionMessage(e))
}

# First ply at which the branch's active line departs from the line it was
# copied with (NA while it has not departed).
#' @noRd
.g3_variation_ply <- function(b) {
  if (is.null(b$origin_ids)) return(NA_integer_)
  cur <- b$game$events$action_id[.g3_active_line(b$game$events)]
  n <- min(length(cur), length(b$origin_ids))
  diff <- which(cur[seq_len(n)] != b$origin_ids[seq_len(n)])
  base <- b$game$initial_state$ply
  if (length(diff)) return(as.integer(base + diff[1L]))
  if (length(cur) > length(b$origin_ids)) return(as.integer(base + length(b$origin_ids) + 1L))
  NA_integer_
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
  b$catalogue <- NULL
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
.g3_analysis_move_table <- function(r, viewed, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  mv <- r$moves
  ev <- r$evaluations
  if (!nrow(mv)) return(htmltools::p(class = "g3-empty", t("moves.empty")))
  es <- ev$expected_score_white[match(mv$ply, ev$ply)]
  loss <- ev$move_loss_display_clipped[match(mv$ply - 1L, ev$ply)] %||% rep(NA_real_, nrow(mv))
  tac <- r$tactics
  rows <- lapply(seq_len(nrow(mv)), function(i) {
    m <- mv[i, ]
    cur <- isTRUE(m$ply == viewed)
    tags_i <- if (nrow(tac)) unique(tac$label[tac$ply == m$ply]) else character()
    htmltools::tags$tr(
      class = if (cur) "g3-row-current" else NULL, `aria-current` = if (cur) "step" else NULL,
      htmltools::tags$td(htmltools::tags$button(type = "button", class = "g3-linkbtn g3-tnum",
                                                `data-g3-click` = ns("goto"), `data-g3-value` = m$ply,
                                                `aria-label` = t("analyse.goto_ply", ply = m$ply), m$ply)),
      htmltools::tags$td(t(paste0("side.", m$actor))),
      htmltools::tags$td(m$notation),
      htmltools::tags$td(gsub("_", " ", m$action_type)),
      htmltools::tags$td(class = "g3-num g3-tnum", if (is.na(es[i])) "\u2013" else sprintf("%.2f", es[i])),
      htmltools::tags$td(class = "g3-num g3-tnum", if (is.na(loss[i])) "\u2013" else format(round(loss[i]))),
      htmltools::tags$td(paste(tags_i, collapse = ", "))
    )
  })
  ses <- r$events
  htmltools::tagList(
    htmltools::div(
      class = "g3-table-scroll",
      htmltools::tags$table(
        class = "g3-table",
        htmltools::tags$caption(class = "g3-sr-only", t("analyse.moves_events")),
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(scope = "col", t("analyse.col_ply")), htmltools::tags$th(scope = "col", t("coach.fact_side")),
          htmltools::tags$th(scope = "col", t("analyse.col_move")), htmltools::tags$th(scope = "col", t("coach.fact_type")),
          htmltools::tags$th(scope = "col", class = "g3-num", t("analyse.col_expected")),
          htmltools::tags$th(scope = "col", class = "g3-num", t("analyse.col_loss")),
          htmltools::tags$th(scope = "col", t("analyse.tactics"))
        )),
        htmltools::tags$tbody(rows)
      )
    ),
    if (nrow(ses)) htmltools::p(class = "g3-small", htmltools::strong(t("analyse.session_events")), " ",
                                paste(paste0(ses$type, " (", t("analyse.col_ply"), " ", ses$ply, ")"), collapse = "; "))
  )
}

#' @noRd
.g3_analysis_tactics <- function(r, viewed, ns, lang) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  tac <- r$tactics
  if (!nrow(tac)) return(htmltools::p(class = "g3-empty", t("analyse.no_tactics")))
  htmltools::tagList(
    htmltools::p(class = "g3-muted g3-small", t("analyse.strength_note")),
    htmltools::tags$ul(
      class = "g3-claims",
      lapply(seq_len(nrow(tac)), function(i) {
        x <- tac[i, ]
        htmltools::tags$li(
          class = if (isTRUE(x$ply == viewed)) "g3-row-current" else NULL,
          htmltools::tags$button(type = "button", class = "g3-linkbtn g3-tnum", `data-g3-click` = ns("goto"),
                                 `data-g3-value` = x$ply, t("analyse.ply_short", ply = x$ply)),
          " ", htmltools::strong(gsub("_", " ", x$label)), " ",
          htmltools::span(class = paste0("g3-chip g3-strength-", x$claim_strength), x$claim_strength),
          " ", x$evidence %||% ""
        )
      })
    )
  )
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
                                          paste(r$pv_labels, collapse = "  \u2192  "))
  )
}
