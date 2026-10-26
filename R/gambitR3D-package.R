#' gambitR3D: multilevel chess for play, analysis and training
#'
#' An experimental multilevel chess variant on three fixed 4x4 boards (W, N,
#' B) and four movable 2x2 platforms (WQ, WK, BQ, BK). Every one of the 64
#' physical squares keeps its identity `board:u:v` when platforms move; the
#' projected file and rank are derived from the board's current dock.
#'
#' The rules engine in R is the only authority on legality. The Shiny app,
#' search, analysis, coaching and the optional Python training worker all
#' use [g3_legal_actions()] and [g3_apply_action()].
#'
#' The rules profile `experimental-v0.1` is a transparent project
#' convention, not an official rulebook. See [g3_rules_profile()] and
#' `vignette("rules", package = "gambitR3D")`.
#'
#' @keywords internal
"_PACKAGE"
