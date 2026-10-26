# Board widget, server side.
#
# One widget shows one game position in two equal views: seven native HTML
# grids (2D) and a WebGL scene (3D), both drawn by inst/app/www/g3-app.js and
# g3-scene.js from the same message. The server is the only authority:
#
#   server -> browser  "g3-board-state"  the committed position, its legal
#                       actions from g3_legal_actions(), the revision token
#                       and exact scene coordinates from display-geometry.R
#   browser -> server  input `<id>-request`  an intention: action id plus
#                       game id, revision, request id and expected state hash
#   server -> browser  "g3-board-ack"    committed / duplicate / rejected
#   browser -> server  input `<id>-pref`  a presentation change (view,
#                       Neutral height, exploded, x-ray, level, camera)
#   browser -> server  input `<id>-selection`  the selected square (for the
#                       inspector panels; never part of the game)
#
# The browser filters the server's legal actions to show destinations; it
# never decides legality and never sends a state. See dev/handoff/05-07-app.md
# for the full schema.

#' @noRd
.g3_board_ui <- function(id, lang = "en", kind = c("live", "analysis")) {
  kind <- match.arg(kind)
  t <- function(key) .g3_txt(key, lang)
  seg_button <- function(pref, value, key, pressed = FALSE) {
    htmltools::tags$button(
      type = "button", class = "g3-seg-btn", `data-pref` = pref, `data-value` = value,
      `aria-pressed` = if (pressed) "true" else "false", t(key)
    )
  }
  toggle_button <- function(pref, key) {
    htmltools::tags$button(type = "button", class = "g3-toggle", `data-pref` = pref,
                           `aria-pressed` = "false", t(key))
  }
  select_box <- function(pref, label_key, choices) {
    sid <- paste0(id, "-", pref)
    htmltools::tags$label(
      class = "g3-select", `for` = sid,
      htmltools::span(class = "g3-select-label", t(label_key)),
      htmltools::tags$select(
        id = sid, `data-pref` = pref,
        lapply(names(choices), function(v) {
          htmltools::tags$option(value = v, `data-i18n` = choices[[v]], .g3_t(choices[[v]], lang))
        })
      )
    )
  }
  htmltools::div(
    id = id, class = "g3-board", `data-kind` = kind, `data-view` = "2d",
    htmltools::div(
      class = "g3-board-toolbar", role = "toolbar",
      !!!.g3_i18n_attr(c("aria-label" = "board.toolbar"), lang),
      htmltools::div(
        class = "g3-seg", role = "group", !!!.g3_i18n_attr(c("aria-label" = "board.view"), lang),
        seg_button("view", "2d", "board.view2d", TRUE),
        seg_button("view", "3d", "board.view3d")
      ),
      htmltools::div(
        class = "g3-seg", role = "group", !!!.g3_i18n_attr(c("aria-label" = "board.neutral"), lang),
        seg_button("neutral_height", "highest", "board.neutral_highest", TRUE),
        seg_button("neutral_height", "lowest", "board.neutral_lowest")
      ),
      htmltools::div(
        class = "g3-tools-3d",
        select_box("camera", "board.camera", c(white = "board.camera_white", black = "board.camera_black",
                                               top = "board.camera_top", side = "board.camera_side")),
        htmltools::tags$button(type = "button", class = "g3-tool", `data-cmd` = "reset", t("board.reset")),
        select_box("level", "board.level", c(all = "board.level_all", W = "board.level_W",
                                             N = "board.level_N", B = "board.level_B")),
        toggle_button("xray", "board.xray"),
        toggle_button("exploded", "board.exploded")
      )
    ),
    htmltools::div(class = "g3-board-notices"),
    htmltools::div(
      class = "g3-board-stage",
      htmltools::div(class = "g3-view-2d", role = "application",
                     !!!.g3_i18n_attr(c("aria-roledescription" = "board.roledesc"), lang),
                     htmltools::p(class = "g3-loading",
                                  t(if (kind == "analysis") "analyse.no_board" else "common.loading"))),
      htmltools::div(class = "g3-view-3d", hidden = NA)
    ),
    htmltools::div(class = "g3-action-panel")
  )
}

# Description of the last action of a game's active line up to `ply`.
#' @noRd
.g3_last_action <- function(game, ply = NULL) {
  line <- .g3_active_line(game$events)
  n <- if (is.null(ply)) length(line) else as.integer(ply) - game$initial_state$ply
  if (n < 1L || n > length(line)) return(NULL)
  ev <- game$events[line[n], ]
  p <- jsonlite::fromJSON(ev$payload, simplifyVector = FALSE)
  a <- p$action %||% list()
  na <- function(x) if (is.null(x)) NULL else x
  list(
    action_id = ev$action_id, ply = ev$ply, actor = ev$actor, type = na(a$type),
    from = na(a$from_square), to = na(a$to_square), platform = na(a$platform_id),
    dock = na(a$to_dock), rotation = na(a$rotation_delta), capture = na(a$capture_id),
    notation = p$notation %||% ev$action_id, check = isTRUE(p$check)
  )
}

# The full JSON-ready message for one widget.
#' @noRd
.g3_board_payload <- function(state, token, prefs, last = NULL, interactive = TRUE,
                              kind = "live", note = NULL, announce = NULL, lang = "en", highlight = NULL) {
  tabs <- .g3_tables()
  nh <- prefs$neutral_height %||% "highest"
  ex <- isTRUE(prefs$exploded)
  sq <- .g3_scene_squares(state, nh, ex)
  bd <- .g3_scene_boards(state, nh, ex, squares = sq)
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  pieces <- state$pieces
  pmap <- match(sq$square_id, pieces$square_id)
  squares <- lapply(seq_len(nrow(sq)), function(i) {
    p <- pmap[i]
    list(
      id = sq$square_id[i], board = sq$board[i], u = sq$u[i], v = sq$v[i],
      parent = sq$parent[i], file = sq$file[i], rank = sq$rank[i], coord = sq$coord[i],
      shade = sq$shade[i], x = sq$x[i], y = sq$y[i], z = sq$z[i],
      piece = if (is.na(p)) NULL else list(id = pieces$id[p], type = pieces$type[p], color = pieces$color[p])
    )
  })
  boards <- lapply(seq_len(nrow(bd)), function(i) {
    r <- bd[i, ]
    list(id = r$board, type = r$type, parent = r$parent, dock = if (is.na(r$dock)) NULL else r$dock,
         orientation = r$orientation, x = r$x, y = r$y, z = r$z,
         x_min = r$x_min, x_max = r$x_max, z_min = r$z_min, z_max = r$z_max)
  })
  # Centre of a platform at each dock, for platform-move previews.
  dparent <- c("W", "N", "B")[tabs$dock_parent]
  dxyz <- .g3_scene_transform(tabs$dock_min_file + 0.5, tabs$dock_min_rank + 0.5, dparent, TRUE, nh, ex)
  docks <- lapply(seq_along(tabs$dock_ids), function(d) {
    list(id = tabs$dock_ids[d], parent = dparent[d], x = dxyz$x[d], y = dxyz$y[d], z = dxyz$z[d],
         files = .g3_coord_label(tabs$dock_min_file[d], tabs$dock_min_rank[d]))
  })
  acts <- g3_legal_actions(state)
  actions <- lapply(seq_len(nrow(acts)), function(i) {
    a <- acts[i, ]
    out <- list(
      id = a$action_id, type = a$type, piece = a$piece_id, from = a$from_square,
      to = if (is.na(a$to_square)) NULL else a$to_square,
      platform = if (is.na(a$platform_id)) NULL else a$platform_id,
      dock = if (is.na(a$to_dock)) NULL else a$to_dock,
      rotation = if (is.na(a$rotation_delta)) NULL else a$rotation_delta,
      promotion = if (is.na(a$promotion)) NULL else a$promotion,
      castle = if (is.na(a$castle_side)) NULL else a$castle_side,
      capture = if (is.na(a$capture_id)) NULL else a$capture_id,
      en_passant = isTRUE(a$en_passant), label = a$label
    )
    if (identical(a$type, "platform")) {
      # Where the carried piece will stand: same square id, new projected
      # coordinate (dock and turn applied).
      p <- match(a$platform_id, .G3_PLATFORMS)
      dock <- core$dock
      orient <- core$orient
      dock[p] <- match(a$to_dock, tabs$dock_ids)
      orient[p] <- (orient[p] + as.integer(a$rotation_delta)) %% 360L
      co <- .g3_square_coords(dock, orient, tabs)
      s <- match(a$from_square, tabs$sq_id)
      out$result_coord <- .g3_coord_label(co$file[s], co$rank[s])
      out$result_orientation <- orient[p]
    }
    out
  })
  in_check <- !identical(state$outcome$status, "terminal") && .g3_in_check(core, core$side, ctx, tabs)
  king <- if (in_check) {
    pieces$square_id[pieces$type == "K" & pieces$color == state$side][1L]
  } else if (identical(state$outcome$reason, "checkmate")) {
    pieces$square_id[pieces$type == "K" & pieces$color == state$side][1L]
  } else NULL
  list(
    kind = kind,
    game_id = token$game_id,
    revision = token$revision,
    state_hash = token$state_hash,
    position_key = state$repetition$keys[length(state$repetition$keys)],
    side = state$side,
    ply = state$ply,
    outcome = state$outcome,
    check = in_check || identical(state$outcome$reason, "checkmate"),
    check_square = king,
    interactive = isTRUE(interactive) && !identical(state$outcome$status, "terminal"),
    note = note,
    announce = announce,
    geometry = list(neutral_height = nh, exploded = ex, heights = as.list(.g3_parent_heights(nh)),
                    edge_gap = .g3_scene_edge_gap(bd)),
    boards = boards,
    docks = docks,
    squares = squares,
    actions = actions,
    last = last,
    # Extra marks (hints, previewed lines): `kind` and physical square ids;
    # `boards` marks every square of those boards as an area.
    highlight = .g3_highlight_squares(highlight, sq)
  )
}

# Expand a highlight (`squares`, `boards`, `platform`) into square ids.
#' @noRd
.g3_highlight_squares <- function(h, sq) {
  if (is.null(h)) return(NULL)
  area <- character()
  if (length(h$boards)) area <- sq$square_id[sq$board %in% h$boards[!is.na(h$boards)]]
  if (length(h$platform) && !is.na(h$platform[1L])) area <- c(area, sq$square_id[sq$board %in% h$platform])
  focus <- h$squares[!is.na(h$squares)]
  list(kind = h$kind %||% "hint", focus = as.list(unique(focus)), area = as.list(setdiff(unique(area), focus)))
}

#' Board widget module server
#'
#' @param id Module id (the widget element id is the namespaced id).
#' @param view A reactive returning `NULL` or a list with `state`, `token`
#'   (`game_id`, `revision`, `state_hash`), `last`, `interactive`, `kind`,
#'   `note` and `announce`.
#' @param prefs Shared presentation `reactiveValues`.
#' @param lang Reactive language code.
#' @param on_request Function `(request)` returning an acknowledgement, or
#'   `NULL` for a read-only widget.
#' @return A list with reactive `selection` (square id or `NULL`).
#' @noRd
.g3_board_server <- function(id, view, prefs, lang, on_request = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    widget <- session$ns(NULL)
    # The browser keeps its selection while the revision and state are
    # unchanged, so presentation updates re-send the same position safely.
    shiny::observe({
      v <- view()
      if (is.null(v)) return()
      p <- list(neutral_height = prefs$neutral_height, exploded = prefs$exploded)
      payload <- .g3_board_payload(
        v$state, v$token, p, last = v$last, interactive = v$interactive %||% TRUE,
        kind = v$kind %||% "live", note = v$note, announce = v$announce, lang = lang(),
        highlight = v$highlight
      )
      payload$widget <- widget
      session$sendCustomMessage("g3-board-state", payload)
    })
    shiny::observeEvent(input$request, {
      req <- input$request
      ack <- if (is.function(on_request)) {
        on_request(req)
      } else {
        .g3_ack(.g3_request(req) %||% list(), "rejected", "read_only", .g3_t("board.read_only", lang()))
      }
      ack$widget <- widget
      ack$message_text <- .g3_ack_text(ack, lang())
      session$sendCustomMessage("g3-board-ack", ack)
    })
    # Presentation preferences from the toolbar. Only known names and
    # values are accepted; none of them reaches the game.
    shiny::observeEvent(input$pref, {
      .g3_apply_pref(prefs, input$pref$name, input$pref$value)
    })
    list(selection = shiny::reactive(input$selection))
  })
}

# Accept one presentation preference change.
#' @noRd
.g3_apply_pref <- function(prefs, name, value) {
  allowed <- list(
    view = c("2d", "3d"), neutral_height = c("highest", "lowest"),
    exploded = c(TRUE, FALSE), xray = c(TRUE, FALSE), level = c("all", "W", "N", "B"),
    camera = c("white", "black", "top", "side"), reduced_motion = c(TRUE, FALSE),
    theme = c("light", "dark")
  )
  if (!is.character(name) || length(name) != 1L || !name %in% names(allowed)) return(invisible(FALSE))
  if (is.logical(allowed[[name]])) value <- isTRUE(value)
  if (length(value) != 1L || !value %in% allowed[[name]]) return(invisible(FALSE))
  prefs[[name]] <- value
  invisible(TRUE)
}

# Text shown beside a rejected request.
#' @noRd
.g3_ack_text <- function(ack, lang = "en") {
  if (identical(ack$status, "committed")) return(.g3_t("ack.committed", lang))
  if (identical(ack$status, "info")) return(ack$message %||% "")
  if (identical(ack$status, "duplicate")) return(.g3_t("ack.duplicate", lang))
  key <- paste0("ack.", ack$reason %||% "invalid_request")
  txt <- .g3_t(key, lang)
  if (identical(txt, key)) txt <- .g3_t("ack.invalid_request", lang)
  if (identical(ack$reason, "save_failed") || identical(ack$reason, "invalid_request")) {
    txt <- paste(txt, ack$message %||% "")
  }
  txt
}
