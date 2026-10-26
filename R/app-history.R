# Move history and engine move facts.
#
# The move list shows the active line of the game with the engine's own
# notation, which names the physical board of every square ("P W:b2-W:b3",
# "WQ(R) to N:L:lo turned"), so overlapping coordinates are never
# ambiguous. Takebacks keep their actions in the game log; the list says how
# many were recorded. Session results (resignation, agreed draw) follow the
# last move.

#' @noRd
.g3_move_list <- function(game, lang = "en", current_ply = NULL) {
  t <- function(key, ...) .g3_t(key, lang, ...)
  mv <- g3_game_moves(game)
  ev <- game$events
  n_takeback <- sum(ev$type == "takeback")
  if (!nrow(mv)) {
    return(htmltools::tagList(
      htmltools::p(class = "g3-empty", t("moves.empty")),
      if (n_takeback) htmltools::p(class = "g3-muted g3-small", t("moves.takebacks", n = n_takeback))
    ))
  }
  base_ply <- game$initial_state$ply
  items <- lapply(seq_len(nrow(mv)), function(i) {
    m <- mv[i, ]
    is_current <- !is.null(current_ply) && identical(as.integer(current_ply), as.integer(m$ply))
    htmltools::tags$li(
      class = paste("g3-move", paste0("g3-move-", m$actor), if (is_current) "g3-move-current"),
      `aria-current` = if (is_current) "step" else NULL,
      htmltools::span(class = "g3-move-num g3-tnum", paste0(m$ply - base_ply, ".")),
      htmltools::span(class = paste0("g3-swatch g3-swatch-", m$actor), `aria-hidden` = "true"),
      htmltools::span(class = "g3-sr-only", paste0(t(paste0("side.", m$actor)), ": ")),
      htmltools::span(class = "g3-move-text", m$notation)
    )
  })
  res <- .g3_result_text(game, lang)
  htmltools::tagList(
    htmltools::tags$ol(class = "g3-move-list", `aria-label` = t("moves.list_label"), items),
    if (!is.null(res)) htmltools::p(class = "g3-move-result", htmltools::strong(res)),
    if (n_takeback) htmltools::p(class = "g3-muted g3-small", t("moves.takebacks", n = n_takeback))
  )
}

# Deterministic facts about the last committed action, straight from the
# engine's event record. Used by the Coach section until real coaching is
# connected; nothing here is evaluated or guessed.
#' @noRd
.g3_move_facts <- function(game, lang = "en") {
  t <- function(key, ...) .g3_t(key, lang, ...)
  la <- .g3_last_action(game)
  if (is.null(la)) return(htmltools::p(class = "g3-empty", t("coach.no_move")))
  rows <- list(
    c(t("coach.fact_move"), la$notation),
    c(t("coach.fact_side"), t(paste0("side.", la$actor))),
    c(t("coach.fact_type"), t(paste0("action.type.", la$type %||% "move")))
  )
  if (!is.null(la$from)) rows[[length(rows) + 1L]] <- c(t("coach.fact_from"), la$from)
  if (!is.null(la$to)) rows[[length(rows) + 1L]] <- c(t("coach.fact_to"), la$to)
  if (!is.null(la$platform)) {
    rows[[length(rows) + 1L]] <- c(t("coach.fact_platform"),
                                   paste0(la$platform, " \u2192 ", la$dock,
                                          if (identical(as.integer(la$rotation), 180L)) paste0(" (", t("board.turned"), ")") else ""))
  }
  rows[[length(rows) + 1L]] <- c(t("coach.fact_capture"), la$capture %||% t("common.none"))
  rows[[length(rows) + 1L]] <- c(t("coach.fact_check"), if (la$check) t("common.yes") else t("common.no"))
  htmltools::tags$dl(
    class = "g3-facts",
    lapply(rows, function(r) htmltools::tagList(htmltools::tags$dt(r[1]), htmltools::tags$dd(r[2])))
  )
}
