# Play section: the live game, its controls and the computer opponent.
#
# The section owns no game state of its own. It reads the session's live
# store (see app-store.R), renders derived views (status bar, controls,
# board message) and sends every change through `.g3_dispatch()`: board
# actions from the widget, resignation, agreed draws, takebacks, new games
# and loaded games. Computer moves arrive through the same dispatcher.
#
# Modes: "hotseat" (both sides at one device) and "computer" (one side is
# played by the built-in search, the greedy one-ply baseline or the
# explicitly named random baseline). Clocks are not offered: untimed play is
# the only mode in this release.

#' @noRd
.g3_play_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(key) .g3_txt(key, lang)
  btn <- function(input_id, key, class = "btn-outline-secondary") {
    shiny::actionButton(ns(input_id), label = t(key), class = paste("g3-btn", class))
  }
  card <- function(title_key, ..., class = NULL) {
    htmltools::tags$section(
      class = paste("g3-card", class),
      htmltools::tags$h3(class = "g3-card-title", t(title_key)),
      ...
    )
  }
  htmltools::div(
    class = "g3-section g3-play",
    htmltools::tags$h2(class = "g3-section-title g3-sr-only", tabindex = "-1", t("nav.play")),
    htmltools::div(
      class = "g3-play-grid",
      htmltools::div(
        class = "g3-play-main",
        shiny::uiOutput(ns("gamebar"), class = "g3-gamebar-wrap"),
        shiny::uiOutput(ns("resume"), class = "g3-resume-wrap"),
        .g3_board_ui(ns("board"), lang, "live")
      ),
      htmltools::div(
        class = "g3-play-side",
        card(
          "play.game",
          htmltools::div(
            class = "g3-btn-row",
            btn("new_game", "play.new_game", "btn-primary"),
            btn("undo", "play.undo"),
            btn("draw", "play.draw"),
            btn("resign", "play.resign"),
            btn("analyse_copy", "play.analyse_copy")
          ),
          shiny::uiOutput(ns("control_notes")),
          shiny::uiOutput(ns("opponent")),
          htmltools::p(class = "g3-muted g3-small", t("play.untimed"))
        ),
        card("play.moves", shiny::uiOutput(ns("moves")), class = "g3-card-moves"),
        card("files.title", .g3_files_ui(ns("files"), lang)),
        card("rules.title", shiny::uiOutput(ns("rules")))
      )
    )
  )
}

#' @noRd
.g3_play_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    store <- app$live
    opp <- .g3_opponent_new()
    opp_rv <- shiny::reactiveVal(0L)
    bump_opp <- function() opp_rv(shiny::isolate(opp_rv()) + 1L)
    app$opponent <- opp
    lang <- app$lang

    game_r <- shiny::reactive({
      app$live_tick()
      store$game
    })

    # Every change to the live game goes through here.
    dispatch <- function(req) {
      ack <- .g3_dispatch(store, req)
      if (identical(ack$status, "committed")) app$live_bump()
      ack
    }
    app$dispatch_live <- dispatch

    human_may_act <- function(game) {
      if (!is.na(game$metadata$result) || identical(game$state$outcome$status, "terminal")) return(FALSE)
      !.g3_computer_to_act(game)
    }

    # ---- board
    board_view <- shiny::reactive({
      game <- game_r()
      opp_rv()
      list(
        state = game$state,
        token = .g3_store_token(store),
        last = .g3_last_action(game),
        interactive = human_may_act(game),
        kind = "live",
        note = if (!is.na(game$metadata$result)) {
          .g3_result_text(game, lang())
        } else if (.g3_computer_to_act(game)) {
          .g3_t("play.computer_turn", lang())
        },
        announce = .g3_announcement(store, lang())
      )
    })
    board <- .g3_board_server("board", board_view, app$prefs, lang, on_request = dispatch)
    app$live_selection <- board$selection

    # ---- status bar
    output$gamebar <- shiny::renderUI({
      opp_rv()
      .g3_gamebar(game_r(), opp, lang())
    })

    # ---- controls
    shiny::observe({
      game <- game_r()
      opp_rv()
      ab <- .g3_control_state(game)
      shiny::updateActionButton(session, "undo", disabled = !ab$undo)
      shiny::updateActionButton(session, "draw", disabled = !ab$draw)
      shiny::updateActionButton(session, "resign", disabled = !ab$resign)
    })
    output$control_notes <- shiny::renderUI({
      game <- game_r()
      ab <- .g3_control_state(game)
      notes <- character()
      if (identical(game$metadata$mode, "computer")) notes <- c(notes, .g3_t("play.draw_computer", lang()))
      if (!ab$undo && length(.g3_active_line(game$events)) && game$metadata$termination %in% c("resign", "draw_agreed")) {
        notes <- c(notes, .g3_t("play.undo_after_session_end", lang()))
      }
      if (!length(notes)) return(NULL)
      htmltools::tags$ul(class = "g3-notes", lapply(notes, htmltools::tags$li))
    })

    output$moves <- shiny::renderUI(.g3_move_list(game_r(), lang()))
    output$rules <- shiny::renderUI(.g3_rules_panel(lang()))

    # ---- new game
    shiny::observeEvent(input$new_game, {
      shiny::showModal(.g3_new_game_modal(ns, lang(), store$game))
    })
    shiny::observeEvent(input$start_game, {
      game <- tryCatch(.g3_build_new_game(input$mode, input$human_color, input$agent, input$budget, lang(),
                                          model_mode = input$model_mode %||% "policy"), error = function(e) e)
      if (inherits(game, "error")) {
        shiny::removeModal()
        app$notify(conditionMessage(game), "error")
        return()
      }
      shiny::removeModal()
      .g3_opponent_cancel(opp, "idle")
      ack <- dispatch(.g3_store_request(store, "replace", game = game))
      if (!identical(ack$status, "committed")) app$notify(.g3_ack_text(ack, lang()), "error")
      bump_opp()
    })

    # ---- undo / draw / resign
    shiny::observeEvent(input$undo, {
      game <- store$game
      n <- .g3_undo_count(game)
      if (n < 1L) return()
      .g3_opponent_cancel(opp, "idle")
      ack <- dispatch(.g3_store_request(store, "event", event = "takeback", n = n))
      if (!identical(ack$status, "committed")) app$notify(.g3_ack_text(ack, lang()), "error")
      bump_opp()
    })
    shiny::observeEvent(input$draw, {
      if (identical(store$game$metadata$mode, "computer")) return()
      shiny::showModal(.g3_confirm_modal(ns("draw_confirm"), "play.draw_confirm_title", "play.draw_confirm_body",
                                         "play.draw_confirm_yes", lang()))
    })
    shiny::observeEvent(input$draw_confirm, {
      shiny::removeModal()
      ack <- dispatch(.g3_store_request(store, "event", event = "draw_agreed", actor = "system"))
      if (!identical(ack$status, "committed")) app$notify(.g3_ack_text(ack, lang()), "error")
    })
    shiny::observeEvent(input$resign, {
      who <- .g3_resigning_side(store$game)
      shiny::showModal(.g3_confirm_modal(ns("resign_confirm"), "play.resign_confirm_title", "play.resign_confirm_body",
                                         "play.resign_confirm_yes", lang(), side = .g3_t(paste0("side.", who), lang())))
    })
    shiny::observeEvent(input$resign_confirm, {
      shiny::removeModal()
      .g3_opponent_cancel(opp, "idle")
      ack <- dispatch(.g3_store_request(store, "event", event = "resign", actor = .g3_resigning_side(store$game)))
      if (!identical(ack$status, "committed")) app$notify(.g3_ack_text(ack, lang()), "error")
      bump_opp()
    })

    # ---- computer opponent
    shiny::observe({
      game <- game_r()
      opp_rv()
      shiny::isolate({
        if (!.g3_computer_to_act(game)) {
          # Leaving computer mode or the computer's turn invalidates any job.
          if (!is.null(opp$job)) {
            .g3_opponent_cancel(opp, "idle")
            bump_opp()
          }
          return()
        }
        if (!is.null(opp$job)) {
          if (!.g3_tag_current(opp$tag, store)) {
            .g3_opponent_cancel(opp, "idle")
            bump_opp()
          }
          return()
        }
        if (identical(opp$for_revision, store$revision)) return()
        .g3_opponent_start(opp, store, file.path(app$storage()$root, "jobs"))
        app$set_job("opponent", !is.null(opp$job))
        bump_opp()
      })
    })
    shiny::observe({
      opp_rv()
      if (is.null(opp$job) && !length(opp$draining)) return()
      shiny::invalidateLater(250)
      shiny::isolate({
        .g3_drain_jobs(opp)
        res <- .g3_opponent_poll(opp, store)
        if (is.null(res)) return()
        app$set_job("opponent", FALSE)
        if (identical(res$status, "decision")) {
          ack <- dispatch(.g3_opponent_request(res))
          if (identical(ack$status, "committed")) {
            opp$status <- "idle"
          } else {
            opp$status <- "failed"
            opp$message <- .g3_ack_text(ack, lang())
          }
        } else if (identical(res$status, "stale")) {
          opp$status <- "idle"
        } else {
          opp$status <- "failed"
          opp$message <- res$message
        }
        bump_opp()
      })
    })
    output$opponent <- shiny::renderUI({
      opp_rv()
      game <- game_r()
      .g3_opponent_panel(game, opp, ns, lang())
    })
    shiny::observeEvent(input$opp_cancel, {
      .g3_opponent_cancel(opp, "cancelled")
      app$set_job("opponent", FALSE)
      bump_opp()
    })
    shiny::observeEvent(input$opp_retry, {
      opp$for_revision <- NA_integer_
      opp$status <- "idle"
      bump_opp()
    })
    session$onSessionEnded(function() {
      .g3_opponent_cancel(opp, "idle")
      .g3_drain_jobs(opp, force = TRUE)
    })

    # ---- files and resume
    files <- .g3_files_server("files", app, dispatch, before_replace = function() {
      .g3_opponent_cancel(opp, "idle")
      bump_opp()
    })
    dismissed <- shiny::reactiveVal(FALSE)
    output$resume <- shiny::renderUI({
      latest <- files$latest()
      game <- game_r()
      if (dismissed() || is.null(latest) || nrow(game$events) > 0L ||
          identical(latest$game_id, game$metadata$game_id)) {
        return(NULL)
      }
      htmltools::div(
        class = "g3-banner", role = "region", `aria-label` = .g3_t("resume.label", lang()),
        htmltools::p(.g3_t("resume.text", lang(), when = format(latest$modified, "%Y-%m-%d %H:%M"),
                           ply = latest$ply, white = latest$white, black = latest$black)),
        htmltools::div(
          class = "g3-btn-row",
          shiny::actionButton(ns("resume_latest"), .g3_t("resume.go", lang()), class = "g3-btn btn-primary"),
          shiny::actionButton(ns("resume_dismiss"), .g3_t("resume.dismiss", lang()), class = "g3-btn btn-outline-secondary")
        )
      )
    })
    shiny::observeEvent(input$resume_latest, {
      latest <- files$latest()
      dismissed(TRUE)
      if (!is.null(latest)) files$resume_path(latest$path)
    })
    shiny::observeEvent(input$resume_dismiss, dismissed(TRUE))

    # Opening an analysis branch from Play.
    shiny::observeEvent(input$analyse_copy, {
      if (is.function(app$open_analysis)) app$open_analysis()
      app$go("analyse")
    })
    invisible(list(game = game_r))
  })
}

# Which game controls are enabled.
#' @noRd
.g3_control_state <- function(game) {
  ended_by_session <- isTRUE(game$metadata$termination %in% c("resign", "draw_agreed"))
  ongoing <- is.na(game$metadata$result)
  list(
    undo = .g3_undo_count(game) > 0L,
    draw = ongoing && !identical(game$metadata$mode, "computer"),
    resign = ongoing,
    ended_by_session = ended_by_session
  )
}

# How many actions an undo takes back. Hotseat: one. Against the computer:
# back to the human's previous turn (two actions when the computer has
# replied, one while it is still to move). A resignation or agreed draw is
# not undone; start a new game instead.
#' @noRd
.g3_undo_count <- function(game) {
  if (isTRUE(game$metadata$termination %in% c("resign", "draw_agreed"))) return(0L)
  n_line <- length(.g3_active_line(game$events))
  if (!n_line) return(0L)
  o <- .g3_opponent_settings(game)
  if (is.null(o)) return(1L)
  if (identical(game$state$side, o$color)) return(1L)
  # The human is to act: undo the computer's reply and the human's move,
  # unless the only action so far is the computer's opening move.
  if (n_line >= 2L) 2L else 0L
}

#' @noRd
.g3_resigning_side <- function(game) {
  o <- .g3_opponent_settings(game)
  if (is.null(o)) game$state$side else setdiff(.G3_COLORS, o$color)
}

#' @noRd
.g3_build_new_game <- function(mode, human_color, agent, budget, lang = "en", model_mode = "policy") {
  mode <- if (identical(mode, "computer")) "computer" else "hotseat"
  if (mode == "hotseat") {
    game <- g3_new_game(players = list(white = .g3_t("side.white", lang), black = .g3_t("side.black", lang)))
    game$metadata$mode <- "hotseat"
    # Catalogue convention (database handoff): who played each side.
    game$metadata$agents <- list(white = "human", black = "human")
    return(game)
  }
  human <- if (identical(human_color, "black")) "black" else "white"
  computer <- setdiff(.G3_COLORS, human)
  is_model <- is.character(agent) && length(agent) == 1L && startsWith(agent, "model:")
  if (!is_model) agent <- if (isTRUE(agent %in% names(.G3_APP_AGENTS))) agent else "search"
  nodes <- unname(.G3_APP_BUDGETS[if (isTRUE(budget %in% names(.G3_APP_BUDGETS))) budget else "standard"])
  model_mode <- if (identical(model_mode, "mcts")) "mcts" else "policy"
  model <- if (is_model) .g3_model_resolve(agent) else NULL
  label <- if (is_model) .g3_model_label(model, model_mode) else .g3_agent_label(agent, nodes, "en")
  players <- list()
  players[[human]] <- "Human"
  players[[computer]] <- label
  game <- g3_new_game(players = players[c("white", "black")])
  game$metadata$mode <- "computer"
  agents <- list()
  agents[[human]] <- "human"
  agents[[computer]] <- list(agent = if (is_model) agent else agent,
                             model_revision = if (is_model) model$manifest$weights_digest %||% NA_character_ else NA_character_)
  game$metadata$agents <- agents[c("white", "black")]
  game$metadata$opponent <- list(
    agent = agent, color = computer, human = human, nodes = nodes, label = label,
    mode = if (is_model) model_mode else NULL, simulations = if (is_model) 32L else NULL,
    model_revision = if (is_model) model$manifest$weights_digest %||% NA_character_ else NULL,
    engine = if (is_model) "model" else .G3_SEARCH_VERSION, evaluator = if (is_model) NULL else .G3_EVAL_VERSION,
    rules_version = game$state$rules_version, rules_hash = game$state$rules_hash,
    package_version = as.character(utils::packageVersion("gambitR3D"))
  )
  game
}

#' @noRd
.g3_agent_label <- function(agent, nodes, lang = "en") {
  if (startsWith(agent, "model:")) return(agent)
  base <- .g3_t(.G3_APP_AGENTS[[agent]], lang)
  if (identical(agent, "search")) paste0(base, ", ", .g3_t("agent.nodes", lang, n = nodes)) else base
}

#' @noRd
.g3_model_label <- function(model, mode) {
  paste0(model$manifest$display_name %||% model$model_id, " [", model$model_id, ", ", mode, "]")
}

# Trained checkpoints that validate for play, as select choices.
#' @noRd
.g3_playable_models <- function() {
  cp <- tryCatch(g3_model_capabilities(), error = function(e) NULL)
  if (is.null(cp)) return(character())
  ok <- cp$kind == "trained_checkpoint" & cp$task == "play" & cp$status == "available"
  ids <- unique(cp$model_id[ok])
  if (!length(ids)) return(character())
  names_ <- cp$display_name[match(ids, cp$model_id)]
  stats::setNames(paste0("model:", ids), paste0(names_, " (", ids, ")"))
}

#' @noRd
.g3_new_game_modal <- function(ns, lang, current) {
  t <- function(key) .g3_t(key, lang)
  o <- .g3_opponent_settings(current)
  shiny::modalDialog(
    title = t("newgame.title"),
    easyClose = TRUE,
    shiny::radioButtons(ns("mode"), t("newgame.mode"),
                        choiceNames = list(t("mode.hotseat"), t("mode.computer")),
                        choiceValues = list("hotseat", "computer"),
                        selected = current$metadata$mode %||% "hotseat"),
    shiny::conditionalPanel(
      "input.mode == 'computer'", ns = ns,
      shiny::radioButtons(ns("human_color"), t("newgame.your_side"),
                          choiceNames = list(t("side.white"), t("side.black")),
                          choiceValues = list("white", "black"), selected = o$human %||% "white", inline = TRUE),
      shiny::selectInput(ns("agent"), t("newgame.opponent"),
                         choices = c(stats::setNames(names(.G3_APP_AGENTS), vapply(.G3_APP_AGENTS, t, "")),
                                     .g3_playable_models()),
                         selected = o$agent %||% "search"),
      shiny::conditionalPanel(
        "input.agent && input.agent.indexOf('model:') === 0", ns = ns,
        shiny::radioButtons(ns("model_mode"), t("models.mode"),
                            choiceNames = list(t("models.mode_policy"), t("models.mode_mcts")),
                            choiceValues = list("policy", "mcts"), selected = o$mode %||% "policy", inline = TRUE),
        htmltools::p(class = "g3-muted g3-small", t("newgame.model_note"))
      ),
      shiny::selectInput(ns("budget"), t("newgame.budget"),
                         choices = stats::setNames(names(.G3_APP_BUDGETS),
                                                   vapply(names(.G3_APP_BUDGETS), function(b) {
                                                     .g3_t(paste0("budget.", b), lang, n = .G3_APP_BUDGETS[[b]])
                                                   }, "")),
                         selected = "standard"),
      htmltools::p(class = "g3-muted g3-small", t("newgame.random_note"))
    ),
    htmltools::p(class = "g3-muted g3-small", t("newgame.replace_note")),
    footer = htmltools::tagList(
      shiny::modalButton(t("common.cancel")),
      shiny::actionButton(ns("start_game"), t("newgame.start"), class = "btn-primary")
    )
  )
}

#' @noRd
.g3_confirm_modal <- function(confirm_id, title_key, body_key, yes_key, lang, ...) {
  shiny::modalDialog(
    title = .g3_t(title_key, lang),
    easyClose = TRUE,
    htmltools::p(.g3_t(body_key, lang, ...)),
    footer = htmltools::tagList(
      shiny::modalButton(.g3_t("common.cancel", lang)),
      shiny::actionButton(confirm_id, .g3_t(yes_key, lang), class = "btn-primary")
    )
  )
}

# Text for the live region after a committed board action: the move, check,
# and the rule result the move produced. Nothing else is announced (no
# camera, selection or presentation changes). Returned with the revision so
# the browser announces it once.
#' @noRd
.g3_announcement <- function(store, lang = "en") {
  if (!store$commits || is.null(store$last) || !identical(store$last$kind, "action")) return(NULL)
  game <- store$game
  la <- .g3_last_action(game)
  if (is.null(la)) return(NULL)
  txt <- paste0(
    .g3_t(paste0("side.", la$actor), lang), ": ", la$notation,
    if (la$check && !identical(game$state$outcome$reason, "checkmate")) paste0(". ", .g3_t("status.check", lang)) else ""
  )
  res <- .g3_result_text(game, lang)
  if (!is.null(res)) txt <- paste(c(txt, res), collapse = ". ")
  list(revision = store$revision, text = txt)
}

# "Checkmate. Black wins." and similar; NULL while the game goes on.
#' @noRd
.g3_result_text <- function(game, lang = "en") {
  res <- game$metadata$result
  if (is.na(res)) return(NULL)
  term <- game$metadata$termination
  reason <- .g3_t(paste0("result.reason.", term), lang)
  winner <- switch(res,
    white = .g3_t("result.white_wins", lang),
    black = .g3_t("result.black_wins", lang),
    draw = .g3_t("result.draw", lang),
    .g3_t("result.truncated", lang)
  )
  paste0(reason, ". ", winner)
}

#' @noRd
.g3_gamebar <- function(game, opp, lang = "en") {
  t <- function(key, ...) .g3_t(key, lang, ...)
  st <- game$state
  res <- .g3_result_text(game, lang)
  prof <- g3_rules_profile(st$rules_version)
  tabs <- .g3_tables()
  core <- .g3_core(st, tabs)
  in_check <- is.null(res) && .g3_in_check(core, core$side, .g3_ctx(core, tabs), tabs)
  o <- .g3_opponent_settings(game)
  mode_txt <- if (is.null(o)) t("mode.hotseat") else
    t("mode.vs", side = t(paste0("side.", o$human)), opponent = if (startsWith(o$agent, "model:")) o$label %||% o$agent else .g3_agent_label(o$agent, o$nodes, lang))
  htmltools::div(
    class = "g3-gamebar",
    htmltools::div(
      class = "g3-turn",
      if (is.null(res)) {
        htmltools::tagList(
          htmltools::span(class = paste0("g3-swatch g3-swatch-", st$side), `aria-hidden` = "true"),
          htmltools::strong(t("status.to_act", side = t(paste0("side.", st$side)))),
          if (in_check) htmltools::span(class = "g3-badge g3-badge-check", htmltools::span(`aria-hidden` = "true", "+"), t("status.check"))
        )
      } else {
        htmltools::tagList(
          htmltools::span(class = "g3-badge g3-badge-result", htmltools::span(`aria-hidden` = "true", "\u25A0"), t("status.finished")),
          htmltools::strong(res)
        )
      }
    ),
    htmltools::div(
      class = "g3-gamebar-meta",
      htmltools::span(mode_txt),
      htmltools::span(class = "g3-tnum", t("status.ply", n = st$ply)),
      htmltools::span(title = prof$rules_hash,
                      t("status.rules", name = prof$profile_id, hash = substr(prof$rules_hash, 1L, 12L)))
    )
  )
}

#' @noRd
.g3_opponent_panel <- function(game, opp, ns, lang = "en") {
  t <- function(key, ...) .g3_t(key, lang, ...)
  o <- .g3_opponent_settings(game)
  if (is.null(o)) return(NULL)
  status <- opp$status
  body <- switch(status,
    thinking = htmltools::tagList(
      htmltools::p(class = "g3-progress", role = "status",
                   htmltools::span(class = "g3-spinner", `aria-hidden` = "true"),
                   t("opponent.thinking", opponent = o$label %||% o$agent)),
      htmltools::p(class = "g3-muted g3-small", t("opponent.progress_note")),
      shiny::actionButton(ns("opp_cancel"), t("opponent.cancel"), class = "g3-btn btn-outline-secondary")
    ),
    failed = htmltools::tagList(
      htmltools::p(class = "g3-error", role = "alert", t("opponent.failed", message = opp$message %||% "")),
      shiny::actionButton(ns("opp_retry"), t("opponent.retry"), class = "g3-btn btn-outline-secondary")
    ),
    cancelled = if (.g3_computer_to_act(game)) htmltools::tagList(
      htmltools::p(role = "status", t("opponent.cancelled")),
      shiny::actionButton(ns("opp_retry"), t("opponent.ask_again"), class = "g3-btn btn-outline-secondary")
    ),
    NULL
  )
  htmltools::div(
    class = "g3-opponent",
    htmltools::p(class = "g3-small", htmltools::strong(t("opponent.title")), " ",
                 t("opponent.identity", opponent = o$label %||% o$agent, side = t(paste0("side.", o$color)),
                   engine = paste(o$engine %||% "", o$evaluator %||% ""))),
    body
  )
}
