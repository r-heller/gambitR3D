# Reproducible static plots of analyses.
#
# Every plot is built from a data table first; the table is attached to the
# returned ggplot as attribute `g3_plot_data`, and the plot's semantics
# (metric, perspective, scale, aggregation, denominator) as `g3_plot_meta`,
# so plots can be checked without image snapshots and every graphic can be
# inspected as a table.
#
# Design rules:
#   * colours from viridisLite, always doubled by shape or line type;
#   * labelled score scale and perspective; heuristic expected scores are
#     named as a logistic display map, never centipawns or probabilities;
#   * missing scores leave gaps (separate line groups), never zeros;
#   * different score scales are never joined into one series, and raw
#     scores in different units are refused;
#   * an empty selection gives an informative empty plot;
#   * board heatmaps keep physical boards apart (board-local mode) or state
#     their aggregation and denominator (projected mode).

.G3_PLOT_BOARDS <- c("W", "N", "B", "WQ", "WK", "BQ", "BK")

# Column references inside aes() use the .data pronoun.
utils::globalVariables(".data")

#' @noRd
.g3_plot_need <- function() {
  for (p in c("ggplot2", "viridisLite")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      .g3_abort("g3_io_error", paste0("Plotting needs the '", p, "' package: install.packages('", p, "')."))
    }
  }
}

#' @noRd
.g3_plot_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30", size = ggplot2::rel(0.8)),
      plot.subtitle = ggplot2::element_text(colour = "grey20", size = ggplot2::rel(0.9)),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' @noRd
.g3_plot_viridis <- function(n, begin = 0.1, end = 0.75) viridisLite::viridis(n, begin = begin, end = end)

#' @noRd
.g3_plot_wrap <- function(x, width = 95) paste(strwrap(x, width = width), collapse = "\n")

#' @noRd
.g3_plot_finish <- function(p, data, meta) {
  attr(p, "g3_plot_data") <- data
  attr(p, "g3_plot_meta") <- meta
  p
}

# An explicit empty plot that says why nothing is drawn.
#' @noRd
.g3_plot_empty <- function(title, reason, meta = list()) {
  p <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = .g3_plot_wrap(reason, 60), size = 4, colour = "grey20") +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  .g3_plot_finish(p, data.frame(), utils::modifyList(list(empty = TRUE, reason = reason), meta))
}

# Line groups that break at missing values, per scale.
#' @noRd
.g3_plot_segments <- function(value, scale) {
  run <- cumsum(c(TRUE, diff(is.na(value)) != 0 | scale[-1L] != scale[-length(scale)]))
  paste(scale, run, sep = "#")
}

#' Plot a game's evaluation or feature trajectory
#'
#' Plots one metric of a [g3_analyse_game()] result against ply, with the
#' score scale and perspective in the labels, captures, checks and platform
#' actions marked by shape and colour, and gaps where scores are missing.
#' Expected scores from the search are a documented logistic display map of
#' heuristic units (scale 400), uncalibrated and not probabilities.
#'
#' @param result A `g3_analysis`.
#' @param metric `"expected_score"` (default), `"score"` (heuristic units;
#'   forced mates are drawn as off-scale markers), `"move_loss"` (matched
#'   best-versus-played loss per action), `"material"` (provisional piece
#'   values) or `"mobility"` (legal actions and distinct destinations).
#' @param perspective `"white"` (default) or `"mover"` (the side to act at
#'   each position). Ignored for `"move_loss"` (always the mover's loss) and
#'   for `"mobility"`.
#' @param events Event markers to show: any of `"capture"`, `"check"`,
#'   `"platform"`; `character()` for none.
#' @return A `ggplot` object. Its attribute `g3_plot_data` holds the plotted
#'   table and `g3_plot_meta` a list with `metric`, `perspective`, `y_label`,
#'   `scale_ids`, `n_missing`, `n_points` and `empty`. Without compatible
#'   scores an informative empty plot is returned (`empty = TRUE`, with
#'   `reason`).
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("viridisLite", quietly = TRUE)
#' # Analyse two opening actions, then plot White's expected score.
#' game <- gambitR3D::g3_new_game()
#' game <- gambitR3D::g3_game_apply(game, "m:W:0:1-W:0:2")
#' game <- gambitR3D::g3_game_apply(game, "m:B:2:2-B:2:1")
#' result <- gambitR3D::g3_analyse_game(game, budget = base::list(nodes = 1000L))
#' trajectory <- gambitR3D::g3_plot_game(result, metric = "expected_score", perspective = "white")
#' # Save a static figure with an explicit output size and resolution.
#' # ggplot2::ggsave(base::file.path(base::tempdir(), "game-evaluation.png"),
#' #                 trajectory, width = 8, height = 4, units = "in", dpi = 300)
#' attr(trajectory, "g3_plot_meta")$y_label
#' @export
g3_plot_game <- function(result, metric = "expected_score", perspective = "white",
                         events = c("capture", "check", "platform")) {
  .g3_plot_need()
  if (!inherits(result, "g3_analysis")) .g3_abort("g3_invalid_state", "`result` must be a g3_analysis (see g3_analyse_game()).")
  metric <- match.arg(metric, c("expected_score", "score", "move_loss", "material", "mobility"))
  perspective <- match.arg(perspective, c("white", "mover"))
  events <- if (length(events)) match.arg(events, c("capture", "check", "platform"), several.ok = TRUE) else character()
  pv <- result$provenance
  gid <- result$game$game_id
  title <- paste0("Game ", gid)
  persp_lab <- if (perspective == "white") "White's perspective" else "the side to act's perspective"
  budget <- .g3_an_budget_text(pv$budget)
  if (metric %in% c("material", "mobility")) {
    return(.g3_plot_game_features(result, metric, events, title))
  }
  ev <- result$evaluations
  if (metric == "move_loss") {
    return(.g3_plot_game_loss(result, title, budget))
  }
  if (metric == "expected_score") {
    val <- if (perspective == "white") ev$expected_score_white else ev$expected_score_mover
    line_rows <- !is.na(ev$expected_score_kind) & ev$expected_score_kind != "terminal_exact"
    y_lab <- paste0("Expected score, ", persp_lab)
  } else {
    units <- unique(stats::na.omit(ev$score_unit[!is.na(ev$score_kind) & ev$score_kind == "heuristic"]))
    if (length(units) > 1L) {
      .g3_abort("g3_incompatible_model", paste0(
        "The evaluations mix score units (", paste(units, collapse = ", "),
        "); raw scores in different units cannot share an axis. Plot metric = 'expected_score' or one evaluator."
      ))
    }
    val <- if (perspective == "white") ev$score_white else ev$score_mover
    line_rows <- !is.na(ev$score_kind) & ev$score_kind == "heuristic"
    y_lab <- paste0("Search score (heuristic units, not centipawns), ", persp_lab)
  }
  # Without any score for an unfinished position there is no trajectory;
  # exact terminal results alone are not plotted as one.
  if (!any(!is.na(val) & !is.na(ev$score_kind) & ev$score_kind != "terminal")) {
    reasons <- unique(stats::na.omit(ev$missing_reason))
    why <- if (length(reasons)) paste(reasons, collapse = "; ") else "no compatible scores"
    return(.g3_plot_empty(title, paste0("No ", gsub("_", " ", metric), " to plot: ", why, ". Evaluator '",
                                        pv$evaluator, "'. Features remain available (metric = 'material' or 'mobility')."),
                          list(metric = metric, perspective = perspective, y_label = y_lab)))
  }
  scale <- ifelse(is.na(ev$scale_id), "none", ev$scale_id)
  d <- data.frame(ply = ev$ply, value = ifelse(line_rows, val, NA_real_), scale_id = scale,
                  score_kind = ev$score_kind, side = ev$side, stringsAsFactors = FALSE)
  d$segment <- .g3_plot_segments(d$value, d$scale_id)
  line_scales <- unique(d$scale_id[!is.na(d$value)])
  d$series <- d$scale_id
  # Off-line markers: terminal results (exact) and forced mates (off scale).
  special <- data.frame(ply = integer(), y = double(), marker = character(), stringsAsFactors = FALSE)
  term <- which(!is.na(ev$score_kind) & ev$score_kind == "terminal")
  mate <- which(!is.na(ev$score_kind) & ev$score_kind == "mate")
  if (metric == "expected_score") {
    if (length(term)) special <- rbind(special, data.frame(ply = ev$ply[term], y = val[term], marker = "terminal rule result (exact)"))
    if (length(mate)) special <- rbind(special, data.frame(ply = ev$ply[mate], y = val[mate], marker = "forced mate within search"))
  } else {
    mw <- if (perspective == "white") ev$mate_in_white else ev$mate_in_mover
    if (length(mate)) special <- rbind(special, data.frame(ply = ev$ply[mate], y = ifelse(mw[mate] > 0, Inf, -Inf),
                                                          marker = "forced mate within search (off scale)"))
    if (length(term)) {
      tv <- ev$expected_score_white[term]
      if (perspective == "mover") tv <- ev$expected_score_mover[term]
      special <- rbind(special, data.frame(ply = ev$ply[term], y = ifelse(tv == 0.5, 0, ifelse(tv > 0.5, Inf, -Inf)),
                                           marker = "terminal rule result (exact)"))
    }
  }
  # Event markers at the value after the action.
  mk <- .g3_plot_event_markers(result$moves, events)
  if (nrow(mk)) {
    mk$y <- d$value[match(mk$ply, d$ply)]
    mk <- mk[!is.na(mk$y), , drop = FALSE]
  }
  n_missing <- sum(is.na(ev$raw_score))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$ply, y = .data$value))
  if (metric == "expected_score") {
    p <- p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey55", linewidth = 0.4)
  } else {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4)
  }
  pal <- .g3_plot_viridis(max(1L, length(line_scales)), 0.15, 0.55)
  p <- p + ggplot2::geom_line(ggplot2::aes(group = .data$segment, colour = .data$series, linetype = .data$series),
                              linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$series), size = 1.1, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = stats::setNames(pal, line_scales), name = "Score scale", drop = TRUE,
                                 breaks = line_scales) +
    ggplot2::scale_linetype_manual(values = stats::setNames(rep(c("solid", "longdash", "dotdash"), length.out = length(line_scales)), line_scales),
                                   name = "Score scale", breaks = line_scales)
  if (nrow(mk)) {
    ev_types <- unique(mk$event)
    p <- p + ggplot2::geom_point(data = mk, ggplot2::aes(x = .data$ply, y = .data$y, shape = .data$event, fill = .data$event),
                                 size = 2.8, colour = "grey10", stroke = 0.6, inherit.aes = FALSE) +
      ggplot2::scale_shape_manual(values = c(capture = 21, check = 24, platform = 22)[ev_types], name = "After the action") +
      ggplot2::scale_fill_manual(values = stats::setNames(viridisLite::viridis(3, begin = 0.35, end = 0.95), c("capture", "check", "platform"))[ev_types],
                                 name = "After the action")
  }
  if (nrow(special)) {
    p <- p + ggplot2::geom_point(data = special, ggplot2::aes(x = .data$ply, y = .data$y), shape = 23, size = 3.4,
                                 fill = "white", colour = "grey10", stroke = 1, inherit.aes = FALSE)
  }
  scale_note <- if (length(line_scales) > 1L) " Different score scales are drawn as separate series, never joined." else ""
  if (metric == "expected_score") {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = ggplot2::expansion(mult = 0.02))
    sub <- paste0("Heuristic expected score = 1 / (1 + exp(-score / ", .G3_LOGISTIC_SCALE,
                  ")): an uncalibrated display map of search scores, not a probability.", scale_note)
  } else {
    sub <- paste0("Raw search scores in heuristic units (provisional weights); forced mates are off-scale markers.", scale_note)
  }
  cap <- paste0(
    "Evaluator ", pv$evaluator, " (", pv$evaluator_version %||% "-", "), budget ", budget, " per position. ",
    "Gaps: missing scores (", n_missing, " of ", nrow(ev), " positions). ",
    if (nrow(special)) "Diamonds: exact terminal results and forced mates (not joined to the line). " else "",
    "Rules ", substr(pv$rules_hash, 1L, 12L), "."
  )
  p <- p + ggplot2::labs(title = title, subtitle = .g3_plot_wrap(sub), x = "Ply (position after this many actions)", y = .g3_plot_wrap(y_lab, 26),
                         caption = .g3_plot_wrap(cap, 110)) +
    .g3_plot_theme() + ggplot2::guides(colour = if (length(line_scales) > 1L) "legend" else "none",
                                  linetype = if (length(line_scales) > 1L) "legend" else "none")
  .g3_plot_finish(p, d, list(
    metric = metric, perspective = perspective, y_label = y_lab, scale_ids = line_scales,
    n_missing = n_missing, n_points = sum(!is.na(d$value)), empty = FALSE, markers = mk, special = special
  ))
}

#' @noRd
.g3_plot_event_markers <- function(moves, events) {
  out <- data.frame(ply = integer(), event = character(), stringsAsFactors = FALSE)
  if (!nrow(moves) || !length(events)) return(out)
  if ("capture" %in% events) out <- rbind(out, data.frame(ply = moves$ply[moves$capture], event = rep("capture", sum(moves$capture))))
  if ("check" %in% events) out <- rbind(out, data.frame(ply = moves$ply[moves$check], event = rep("check", sum(moves$check))))
  if ("platform" %in% events) out <- rbind(out, data.frame(ply = moves$ply[moves$platform_action], event = rep("platform", sum(moves$platform_action))))
  out
}

#' @noRd
.g3_plot_game_loss <- function(result, title, budget) {
  ev <- result$evaluations
  d <- ev[!is.na(ev$played_action_id), c("ply", "side", "move_loss_raw", "played_action_id"), drop = FALSE]
  d$action_ply <- d$ply + 1L
  have <- !is.na(d$move_loss_raw)
  y_lab <- "Move loss (heuristic units, mover's perspective)"
  if (!any(have)) {
    return(.g3_plot_empty(title, paste0(
      "No heuristic move losses: positions are unscored, finished, or their best/played scores are forced mates. ",
      "Evaluator '", result$provenance$evaluator, "'."
    ), list(metric = "move_loss", y_label = y_lab)))
  }
  d <- d[have, , drop = FALSE]
  d$actor <- factor(d$side, levels = c("white", "black"), labels = c("White", "Black"))
  pal <- stats::setNames(.g3_plot_viridis(2L, 0.15, 0.6), c("White", "Black"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$action_ply, y = .data$move_loss_raw, colour = .data$actor)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$action_ply, y = 0, yend = .data$move_loss_raw), linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$actor), size = 2.2) +
    ggplot2::scale_colour_manual(values = pal, name = "Actor", drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(White = 21, Black = 17), name = "Actor", drop = FALSE) +
    ggplot2::labs(
      title = title, x = "Ply of the action", y = .g3_plot_wrap(y_lab, 26),
      subtitle = .g3_plot_wrap("Best alternative minus played action, both scored by the same completed root search (raw, unclipped)."),
      caption = .g3_plot_wrap(paste0(nrow(d), " of ", sum(!is.na(ev$played_action_id)), " actions have a heuristic loss; ",
                                "others are unscored or involve forced-mate scores. Budget ", budget, " per position."), 110)
    ) + .g3_plot_theme()
  .g3_plot_finish(p, d, list(metric = "move_loss", perspective = "mover", y_label = y_lab,
                             n_points = nrow(d), n_missing = sum(!have), empty = FALSE))
}

#' @noRd
.g3_plot_game_features <- function(result, metric, events, title) {
  pos <- result$positions
  if (metric == "material") {
    d <- data.frame(ply = pos$ply, value = pos$material_balance_white, series = "White minus Black", stringsAsFactors = FALSE)
    y_lab <- "Material balance, White minus Black (provisional piece values)"
    sub <- "Provisional values P 100, N 300, B 300, R 500, Q 900 (ordinary chess values, not measured for this variant)."
  } else {
    d <- rbind(
      data.frame(ply = pos$ply, value = pos$white_actions, series = "White actions", stringsAsFactors = FALSE),
      data.frame(ply = pos$ply, value = pos$black_actions, series = "Black actions", stringsAsFactors = FALSE),
      data.frame(ply = pos$ply, value = pos$white_destinations, series = "White destinations", stringsAsFactors = FALSE),
      data.frame(ply = pos$ply, value = pos$black_destinations, series = "Black destinations", stringsAsFactors = FALSE)
    )
    y_lab <- "Mobility (count)"
    sub <- "Side to act: legal actions; side not to act: null-move legal actions (a counterfactual). Destinations remove promotion and orientation duplicates."
  }
  d$segment <- d$series
  lv <- unique(d$series)
  pal <- stats::setNames(.g3_plot_viridis(length(lv), 0.15, 0.7), lv)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$ply, y = .data$value, colour = .data$series, linetype = .data$series,
                                       group = .data$segment)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$series), size = 1.4) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_linetype_manual(values = stats::setNames(rep(c("solid", "longdash", "dotted", "dotdash"), length.out = length(lv)), lv), name = NULL) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(16, 17, 1, 2)[seq_along(lv)], lv), name = NULL) +
    ggplot2::labs(title = title, subtitle = .g3_plot_wrap(sub), x = "Ply", y = .g3_plot_wrap(y_lab, 26),
                  caption = "Engine-derived features; no evaluator needed.") +
    .g3_plot_theme()
  .g3_plot_finish(p, d, list(metric = metric, perspective = "white", y_label = y_lab,
                             n_points = nrow(d), n_missing = 0L, empty = FALSE))
}

#' Board heatmap of occupancy, visits, captures or attacks
#'
#' Summarises where pieces stood, arrived, captured or attacked. In
#' `"board_local"` mode each of the seven physical boards is its own facet
#' in its local `(u, v)` frame, so overlapping projected coordinates on
#' different boards never merge and a platform square keeps its identity
#' wherever the platform travelled. `"projected"` mode answers a different
#' question: it aggregates all physical squares that share a projected
#' file/rank and states that aggregation and its denominator in the plot.
#'
#' @param result A `g3_analysis`, a `g3_database_analysis` or a list of
#'   `g3_analysis` (pooled).
#' @param metric `"occupancy"` (share of selected positions in which the
#'   square holds a piece), `"attacks"` (share of selected positions in
#'   which the square's coordinate is attacked under the engine's attack
#'   semantics), `"visits"` (share of piece moves arriving on the square;
#'   transport is not a visit) or `"captures"` (share of captures landing
#'   on the square).
#' @param coordinate_space `"board_local"` (default) or `"projected"`.
#' @param color `"both"` (default), `"white"` or `"black"`: whose pieces,
#'   attacks or actions are counted. For attacks, `"both"` means attacked by
#'   either side.
#' @param plies Optional integer vector of plies to include (positions for
#'   occupancy/attacks, action plies for visits/captures).
#' @return A `ggplot` object with attributes `g3_plot_data` (one row per
#'   physical square or projected coordinate with `numerator`,
#'   `denominator` and `value`) and `g3_plot_meta` (`metric`,
#'   `coordinate_space`, `aggregation`, `denominator_label`, `n_games`,
#'   `empty`).
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("viridisLite", quietly = TRUE)
#' # Features only: no evaluator is needed for occupancy.
#' game <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' result <- gambitR3D::g3_analyse_game(game, evaluator = "none")
#' # Keep overlapping boards separate in an occupancy visualisation.
#' board_plot <- gambitR3D::g3_plot_board_heatmap(result, metric = "occupancy",
#'                                                coordinate_space = "board_local")
#' attr(board_plot, "g3_plot_meta")$denominator_label
#' @export
g3_plot_board_heatmap <- function(result, metric = "occupancy", coordinate_space = "board_local",
                                  color = "both", plies = NULL) {
  .g3_plot_need()
  metric <- match.arg(metric, c("occupancy", "attacks", "visits", "captures"))
  coordinate_space <- match.arg(coordinate_space, c("board_local", "projected"))
  color <- match.arg(color, c("both", "white", "black"))
  res <- .g3_an_list(result)
  n_games <- length(res)
  title <- paste0(.g3_heat_title(metric), if (color != "both") paste0(" (", color, ")") else "",
                  if (n_games == 1L) paste0(", game ", res[[1L]]$game$game_id) else paste0(", ", n_games, " games"))
  meta0 <- list(metric = metric, coordinate_space = coordinate_space, color = color, n_games = n_games)
  if (!n_games) return(.g3_plot_empty(title, "No analysed games in the selection.", meta0))
  if (metric %in% c("occupancy", "attacks")) {
    sq <- do.call(rbind, lapply(res, function(r) r$squares))
    if (!is.null(plies)) sq <- sq[sq$ply %in% plies, , drop = FALSE]
    pos_id <- paste(sq$game_id, sq$ply)
    n_pos <- length(unique(pos_id))
    if (!n_pos) return(.g3_plot_empty(title, "No positions match the selected plies.", meta0))
    hit <- if (metric == "occupancy") {
      if (color == "both") !is.na(sq$occupant_id) else !is.na(sq$occupant_color) & sq$occupant_color == color
    } else {
      switch(color, both = sq$attacked_by_white | sq$attacked_by_black, white = sq$attacked_by_white, black = sq$attacked_by_black)
    }
    unit_lab <- "positions"
  } else {
    mv <- do.call(rbind, lapply(res, function(r) r$moves))
    if (!is.null(plies)) mv <- mv[mv$ply %in% plies, , drop = FALSE]
    if (color != "both") mv <- mv[mv$actor == color, , drop = FALSE]
    mv <- mv[!is.na(mv$to_square), , drop = FALSE]
    if (metric == "captures") mv <- mv[mv$capture, , drop = FALSE]
    if (!nrow(mv)) {
      return(.g3_plot_empty(title, paste0("No ", if (metric == "captures") "captures" else "piece moves",
                                          " in the selection (platform transport is not a visit)."), meta0))
    }
    unit_lab <- if (metric == "captures") "captures" else "piece moves"
  }
  if (coordinate_space == "board_local") {
    tabs <- .g3_tables()
    base <- data.frame(square_id = tabs$sq_id, board = tabs$board_ids[tabs$sq_board], u = tabs$sq_u, v = tabs$sq_v,
                       stringsAsFactors = FALSE)
    if (metric %in% c("occupancy", "attacks")) {
      num <- tapply(hit, sq$square_id, sum)
      base$numerator <- as.integer(num[base$square_id])
      base$denominator <- n_pos
    } else {
      num <- table(mv$to_square)
      base$numerator <- as.integer(ifelse(is.na(num[base$square_id]), 0L, num[base$square_id]))
      base$denominator <- nrow(mv)
    }
    base$value <- base$numerator / base$denominator
    base$board <- factor(base$board, levels = .G3_PLOT_BOARDS)
    den_lab <- paste0("share of ", base$denominator[1L], " selected ", unit_lab)
    agg <- "none: each physical square is a separate tile in its board's local (u, v) frame"
    base$label <- .g3_plot_pct(base$value)
    p <- ggplot2::ggplot(base, ggplot2::aes(x = .data$u, y = .data$v, fill = .data$value)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = .data$label, colour = .data$value > 0.6), size = 3, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "grey10", `FALSE` = "white"), guide = "none") +
      ggplot2::facet_wrap(~board, nrow = 2L, drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = 0:3, limits = c(-0.5, 3.5), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = 0:3, limits = c(-0.5, 3.5), expand = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "Local u (board's own frame)", y = "Local v")
    data <- base
  } else {
    if (metric %in% c("occupancy", "attacks")) {
      key <- paste(sq$file, sq$rank)
      if (metric == "occupancy") {
        num <- tapply(hit, key, sum)
        den <- rep(n_pos, length(num))
        agg <- "occupied physical squares summed over all boards sharing the projected coordinate (overlapping boards combined), divided by the number of selected positions; values can exceed 1"
        den_lab <- paste0("mean occupied squares per selected position (", n_pos, " positions)")
      } else {
        # Attack status belongs to the coordinate: count each position once
        # among the positions in which the coordinate has a physical square.
        pk <- paste(sq$game_id, sq$ply, key)
        first <- !duplicated(pk)
        num <- tapply(hit[first], key[first], sum)
        den <- tapply(rep(1L, sum(first)), key[first], sum)
        agg <- "attack status is per projected coordinate (all boards there share it); share of the selected positions in which the coordinate had at least one physical square"
        den_lab <- "share of positions in which the coordinate exists"
      }
      ks <- strsplit(names(num), " ", fixed = TRUE)
      data <- data.frame(file = as.integer(vapply(ks, `[`, "", 1L)), rank = as.integer(vapply(ks, `[`, "", 2L)),
                         numerator = as.integer(num), denominator = as.integer(den), stringsAsFactors = FALSE)
      nsq <- tapply(sq$square_id, key, function(x) length(unique(x)))
      data$physical_squares <- as.integer(nsq[paste(data$file, data$rank)])
    } else {
      key <- paste(mv$to_file, mv$to_rank)
      num <- table(key)
      ks <- strsplit(names(num), " ", fixed = TRUE)
      data <- data.frame(file = as.integer(vapply(ks, `[`, "", 1L)), rank = as.integer(vapply(ks, `[`, "", 2L)),
                         numerator = as.integer(num), denominator = nrow(mv), stringsAsFactors = FALSE)
      data$physical_squares <- NA_integer_
      agg <- paste0(unit_lab, " landing on any physical square at the projected coordinate (boards combined), divided by all selected ", unit_lab)
      den_lab <- paste0("share of ", nrow(mv), " selected ", unit_lab)
    }
    data$value <- data$numerator / data$denominator
    data$label <- if (metric == "occupancy") formatC(data$value, format = "f", digits = 2) else .g3_plot_pct(data$value)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$file, y = .data$rank, fill = .data$value)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = .data$label), colour = "grey5", size = 2.8) +
      ggplot2::scale_x_continuous(breaks = 0:5, labels = .G3_FILES, limits = c(-0.5, 5.5), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = 0:9, limits = c(-0.5, 9.5), expand = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "Projected file", y = "Projected rank")
  }
  p <- p + ggplot2::scale_fill_gradientn(colours = viridisLite::viridis(256), name = .g3_plot_wrap(den_lab, 30),
                                         limits = c(0, max(1, max(data$value, na.rm = TRUE))), na.value = "grey85") +
    ggplot2::labs(
      title = title,
      subtitle = .g3_plot_wrap(if (coordinate_space == "board_local") {
        "Physical squares in each board's own local frame; overlapping coordinates on different boards stay separate."
      } else {
        paste0("Projected coordinates. Aggregation: ", agg, ".")
      }),
      caption = .g3_plot_wrap(paste0(.g3_heat_definition(metric), " Display settings (view, Neutral height, camera) never enter."), 110)
    ) + .g3_plot_theme() + ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")
  .g3_plot_finish(p, data, c(meta0, list(aggregation = agg, denominator_label = den_lab, empty = FALSE)))
}

#' @noRd
.g3_plot_pct <- function(x) ifelse(is.na(x), "", paste0(round(100 * x), "%"))

#' @noRd
.g3_heat_title <- function(metric) {
  switch(metric, occupancy = "Square occupancy", attacks = "Attacked squares", visits = "Piece arrivals",
         captures = "Capture squares")
}

#' @noRd
.g3_heat_definition <- function(metric) {
  switch(metric,
         occupancy = "Occupancy: a square counts in a position when a piece stands on it.",
         attacks = "Attacks: a square counts when its current projected coordinate is attacked under the engine's attack semantics.",
         visits = "Visits: destination squares of piece moves and castling (transport moves no piece between squares).",
         captures = "Captures: landing squares of capturing piece moves.")
}

# Analyses from a single result, a database run or a list.
#' @noRd
.g3_an_list <- function(x) {
  if (inherits(x, "g3_analysis")) return(list(x))
  if (inherits(x, "g3_database_analysis")) return(unname(x$results))
  if (is.list(x) && all(vapply(x, inherits, TRUE, "g3_analysis"))) return(unname(x))
  .g3_abort("g3_invalid_state", "Expected a g3_analysis, a g3_database_analysis or a list of g3_analysis.")
}

#' Plot a game collection
#'
#' Outcome, termination, game-length and platform-use views of a set of
#' analysed games, always with counts and denominators. Truncated and
#' unfinished games are separate categories, never draws.
#'
#' @param x A `g3_database_analysis` or a list of `g3_analysis`.
#' @param metric `"outcome"`, `"termination"`, `"length"` or
#'   `"platform_use"` (share of actions that are platform actions, by game
#'   phase).
#' @return A `ggplot` object with attributes `g3_plot_data` and
#'   `g3_plot_meta` (`metric`, `n_games`, `denominator_label`, `empty`).
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("viridisLite", quietly = TRUE)
#' # A one-game collection: the outcome plot keeps its denominator.
#' g1 <- gambitR3D::g3_game_apply(gambitR3D::g3_new_game(), "m:W:0:1-W:0:2")
#' run <- gambitR3D::g3_analyse_database(base::list(g1), evaluator = "none")
#' p <- gambitR3D::g3_plot_collection(run, metric = "outcome")
#' attr(p, "g3_plot_data")
#' @export
g3_plot_collection <- function(x, metric = "outcome") {
  .g3_plot_need()
  metric <- match.arg(metric, c("outcome", "termination", "length", "platform_use"))
  if (inherits(x, "g3_database_analysis")) {
    sm <- x$summary
    filt <- x$manifest$filters
    extra <- paste0(" Scheduled ", x$manifest$n_scheduled, ", errors ", x$manifest$n_errors, ", pending ", x$manifest$n_pending, ".")
  } else {
    res <- .g3_an_list(x)
    sm <- .g3_dbrun_summary(res, list(), length(res), 0L, 0L)
    filt <- "none"
    extra <- ""
  }
  n <- sm$counts$n_completed
  title <- paste0(switch(metric, outcome = "Outcomes", termination = "Termination causes", length = "Game lengths",
                         platform_use = "Platform actions by game phase"), " (", n, " analysed games)")
  if (!n) return(.g3_plot_empty(title, paste0("No analysed games in this selection (filters: ", filt, ").", extra),
                                list(metric = metric, n_games = 0L)))
  cap <- .g3_plot_wrap(paste0("Filters: ", filt, ".", extra, " Truncated = computational cutoff, not a draw."), 110)
  if (metric %in% c("outcome", "termination")) {
    d <- if (metric == "outcome") {
      data.frame(category = sm$outcomes$result, n_games = sm$outcomes$n_games, denominator = n, stringsAsFactors = FALSE)
    } else {
      data.frame(category = sm$terminations$termination, n_games = sm$terminations$n_games, denominator = n,
                 group = sm$terminations$outcome_class, stringsAsFactors = FALSE)
    }
    d$share <- d$n_games / d$denominator
    d$category <- factor(d$category, levels = rev(d$category))
    d$label <- paste0(d$n_games, " / ", d$denominator)
    fill_var <- if (metric == "outcome") "category" else "group"
    lv <- unique(as.character(d[[fill_var]]))
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$n_games, y = .data$category, fill = .data[[fill_var]])) +
      ggplot2::geom_col(width = 0.7, colour = "grey20", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = .data$label), hjust = -0.15, size = 3.2, colour = "grey10") +
      ggplot2::scale_fill_manual(values = stats::setNames(.g3_plot_viridis(length(lv), 0.1, 0.9), lv),
                                 name = if (metric == "outcome") NULL else "Outcome class",
                                 guide = if (metric == "outcome") "none" else "legend") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
      ggplot2::labs(title = title, x = "Games (label: games / analysed games)", y = NULL, caption = cap) + .g3_plot_theme()
    den <- "analysed games"
  } else if (metric == "length") {
    d <- sm$games[, c("game_id", "n_plies", "outcome_class")]
    d$outcome_class <- factor(d$outcome_class, levels = unique(c("rule_decisive", "rule_draw", "resignation",
                                                                   "agreed_draw", "truncated", "unfinished")))
    lv <- levels(d$outcome_class)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$n_plies, y = .data$outcome_class, colour = .data$outcome_class,
                                         shape = .data$outcome_class)) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0, height = 0.15, seed = 1L), size = 2.2) +
      ggplot2::stat_summary(fun = stats::median, fun.min = stats::median, fun.max = stats::median, geom = "crossbar",
                            width = 0.5, colour = "grey15", linewidth = 0.3) +
      ggplot2::scale_colour_manual(values = stats::setNames(.g3_plot_viridis(length(lv), 0.1, 0.85), lv), drop = FALSE, guide = "none") +
      ggplot2::scale_shape_manual(values = stats::setNames(c(16, 17, 15, 18, 1, 2)[seq_along(lv)], lv), drop = FALSE, guide = "none") +
      ggplot2::scale_y_discrete(drop = FALSE) +
      ggplot2::labs(title = title, x = "Plies (actions in the active line)", y = NULL,
                    subtitle = "One point per game (vertical jitter only); bar = median.", caption = cap) + .g3_plot_theme()
    den <- "games per outcome class"
  } else {
    d <- sm$platform_use_by_phase
    d$phase <- factor(d$phase, levels = d$phase)
    d$label <- paste0(d$platform_actions, " / ", d$actions)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$phase, y = .data$share)) +
      ggplot2::geom_col(fill = .g3_plot_viridis(1L, 0.35, 0.35), width = 0.6, na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = .data$label, y = ifelse(is.na(.data$share), 0, .data$share)),
                         vjust = -0.4, size = 3.2) +
      ggplot2::scale_y_continuous(limits = c(0, max(0.05, max(d$share, na.rm = TRUE) * 1.25)),
                                  labels = function(v) paste0(round(100 * v), "%")) +
      ggplot2::labs(title = title, x = NULL, y = "Platform actions / all actions in the phase",
                    subtitle = paste0("Overall: ", sm$platform_use$platform_actions, " of ", sm$platform_use$total_actions,
                                      " actions; ", sm$platform_use$games_with_platform_action, " of ", n,
                                      " games use a platform."), caption = cap) + .g3_plot_theme()
    den <- "actions in each phase"
  }
  .g3_plot_finish(p, d, list(metric = metric, n_games = n, denominator_label = den, empty = FALSE))
}

#' Plot a paired agent comparison
#'
#' @param x A `g3_comparison` from [g3_compare_agents()].
#' @param metric `"score"` (agent 1's completed-game expected score with its
#'   paired-block bootstrap interval and the scheduled-game bounds),
#'   `"outcomes"` (counts of every scheduled game, by agent 1's colour) or
#'   `"latency"` (decision wall time per agent, median and 95th percentile).
#' @return A `ggplot` object with attributes `g3_plot_data` and
#'   `g3_plot_meta`.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("viridisLite", quietly = TRUE)
#' # A tiny capped comparison; every scheduled game is counted.
#' cmp <- gambitR3D::g3_compare_agents(c("greedy", "random"), budget = base::list(nodes = 50L),
#'                                     seeds = 1L, max_plies = 4L)
#' p <- gambitR3D::g3_plot_comparison(cmp, metric = "outcomes")
#' attr(p, "g3_plot_data")
#' @export
g3_plot_comparison <- function(x, metric = "score") {
  .g3_plot_need()
  if (!inherits(x, "g3_comparison")) .g3_abort("g3_invalid_state", "`x` must be a g3_comparison.")
  metric <- match.arg(metric, c("score", "outcomes", "latency"))
  s <- x$summary
  title <- paste0(s$agent1, " vs ", s$agent2)
  cap <- .g3_plot_wrap(paste0(
    s$scheduled, " scheduled games: ", s$completed, " completed, ", s$truncated, " truncated, ", s$failed, " failed, ",
    s$cancelled, " cancelled. Budget ", .g3_an_budget_text(x$provenance$budget), " per decision for both agents; ",
    "max ", x$provenance$max_plies, " plies. Colour-swapped pairs (role swap, no mirroring). No rating is fitted."
  ), 110)
  if (!s$scheduled) return(.g3_plot_empty(title, "No scheduled games.", list(metric = metric)))
  if (metric == "score") {
    u <- x$uncertainty
    iv <- u$intervals
    d <- data.frame(
      row = c("All games", paste0(s$agent1, " as White"), paste0(s$agent1, " as Black")),
      estimate = c(s$completed_score, x$by_colour$completed_score),
      lower_bound = c(s$scheduled_lower_bound, x$by_colour$scheduled_lower_bound),
      upper_bound = c(s$scheduled_upper_bound, x$by_colour$scheduled_upper_bound),
      ci_low = c(if (nrow(iv)) iv$lower[iv$statistic == "completed_score"] else NA_real_, NA_real_, NA_real_),
      ci_high = c(if (nrow(iv)) iv$upper[iv$statistic == "completed_score"] else NA_real_, NA_real_, NA_real_),
      completed = c(s$completed, x$by_colour$completed), scheduled = c(s$scheduled, x$by_colour$scheduled),
      stringsAsFactors = FALSE
    )
    d$row <- factor(d$row, levels = rev(d$row))
    bounds <- rbind(data.frame(row = d$row, value = d$lower_bound, bound = "unknown games as losses"),
                    data.frame(row = d$row, value = d$upper_bound, bound = "unknown games as wins"))
    p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$row)) +
      ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey55") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high), width = 0.2, na.rm = TRUE,
                             colour = "grey15", orientation = "y") +
      ggplot2::geom_point(data = bounds, ggplot2::aes(x = .data$value, y = .data$row, shape = .data$bound, fill = .data$bound),
                          size = 2.6, colour = "grey15", na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(x = .data$estimate), shape = 21, size = 3.6, fill = .g3_plot_viridis(1L, 0.3, 0.3),
                          colour = "grey5", na.rm = TRUE) +
      ggplot2::scale_shape_manual(values = c(`unknown games as losses` = 25, `unknown games as wins` = 24),
                                  name = "Scheduled-game bound") +
      ggplot2::scale_fill_manual(values = stats::setNames(.g3_plot_viridis(2L, 0.55, 0.85), c("unknown games as losses", "unknown games as wins")),
                                 name = "Scheduled-game bound") +
      ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      ggplot2::labs(
        title = title,
        subtitle = .g3_plot_wrap(paste0(
          "Circle: completed-game expected score of ", s$agent1, " (wins + draws / 2) / completed. Bar: ",
          round(100 * u$level), "% ", "paired-block bootstrap interval (", u$replicates, " replicates, seed ", u$seed,
          ", ", u$n_blocks, " starting positions). ", u$note
        )),
        x = paste0("Expected score of ", s$agent1), y = NULL, caption = cap
      ) + .g3_plot_theme()
    data <- d
  } else if (metric == "outcomes") {
    g <- x$games
    cat_of <- ifelse(g$status != "completed", g$status,
                     ifelse(g$agent1_score == 1, "win", ifelse(g$agent1_score == 0.5, "draw", "loss")))
    lv <- c("win", "draw", "loss", "truncated", "failed", "cancelled")
    d <- do.call(rbind, lapply(c("white", "black"), function(col) {
      data.frame(colour = paste0(s$agent1, " as ", if (col == "white") "White" else "Black"),
                 category = lv, n_games = vapply(lv, function(l) sum(cat_of == l & g$agent1_colour == col), 0L, USE.NAMES = FALSE),
                 denominator = sum(g$agent1_colour == col), stringsAsFactors = FALSE)
    }))
    d$category <- factor(d$category, levels = lv)
    d$colour <- factor(d$colour, levels = unique(d$colour))
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$category, y = .data$n_games, fill = .data$category)) +
      ggplot2::geom_col(width = 0.7, colour = "grey20", linewidth = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(.data$n_games, "/", .data$denominator)), vjust = -0.3, size = 3) +
      ggplot2::facet_wrap(~colour) +
      ggplot2::scale_fill_manual(values = stats::setNames(.g3_plot_viridis(6L, 0.05, 0.95), lv), guide = "none") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
      ggplot2::labs(title = title, subtitle = paste0("Every scheduled game from ", s$agent1, "'s side (label: games / scheduled)."),
                    x = NULL, y = "Games", caption = cap) + .g3_plot_theme()
    data <- d
  } else {
    dd <- x$decisions
    if (!nrow(dd)) return(.g3_plot_empty(title, "No decisions were recorded.", list(metric = metric)))
    co <- x$costs
    dd$agent <- factor(dd$agent, levels = co$agent)
    lv <- levels(dd$agent)
    summ <- rbind(data.frame(agent = co$agent, value = co$latency_median_ms, stat = "median"),
                  data.frame(agent = co$agent, value = co$latency_p95_ms, stat = "95th percentile"))
    summ$agent <- factor(summ$agent, levels = lv)
    p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data$latency_ms, y = .data$agent)) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$agent), position = ggplot2::position_jitter(width = 0, height = 0.15, seed = 1L),
                          alpha = 0.45, size = 1.2) +
      ggplot2::geom_point(data = summ, ggplot2::aes(x = .data$value, y = .data$agent, shape = .data$stat), size = 3.2,
                          colour = "grey5", fill = "white", na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = stats::setNames(.g3_plot_viridis(length(lv), 0.15, 0.6), lv), guide = "none") +
      ggplot2::scale_shape_manual(values = c(median = 21, `95th percentile` = 24), name = NULL) +
      ggplot2::scale_x_log10() +
      ggplot2::labs(title = title, subtitle = "Decision wall time per agent call (log scale), including R overhead.",
                    x = "Latency (ms, log scale)", y = NULL, caption = cap) + .g3_plot_theme()
    data <- dd
  }
  .g3_plot_finish(p, data, list(metric = metric, empty = FALSE, n_scheduled = s$scheduled))
}
