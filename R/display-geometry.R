# Display geometry: where squares are drawn, never what they mean.
#
# This file is the R implementation of docs/GEOMETRY_CONTRACT.md. The app
# computes every scene coordinate here and sends the numbers to the browser,
# so the 3D renderer, the spatial overlays and the numeric tests share one
# transform. Nothing in this file is read by the rules engine: changing the
# Neutral height, the exploded spacing or the camera cannot change a square
# id, a legal action, a position key or a state hash.
#
# One square has unit side length. For a square centre at projected file `f`
# (0..5) and rank `r` (0..9) whose board hangs on parent P (W, N or B):
#
#   x = f - 2.5
#   z = 4.5 - r + offset(P)          offset: W +1, N 0, B -1
#   y = height(P)                    main boards
#   y = height(P) + 0.9              platforms, P = parent of the CURRENT dock
#
# Heights: "highest" puts W and B at y = 0 and N at y = 3.4; "lowest" puts W
# and B at 3.4 and N at 0. The exploded inspection view adds
# 2.25 * (1 - parent_index) to z (W 0, N 1, B 2), moving whole parent
# assemblies along z only; turning it off restores the two-unit gap between
# the facing W and B edges. A platform always uses its current dock's parent,
# never the board it started on.

.G3_DISPLAY_PARENTS <- c("W", "N", "B")
.G3_PARENT_OFFSET <- c(W = 1, N = 0, B = -1)
.G3_PARENT_INDEX <- c(W = 0L, N = 1L, B = 2L)
.G3_NEUTRAL_RAISED <- 3.4
.G3_PLATFORM_LIFT <- 0.9
.G3_EXPLODE_STEP <- 2.25
.G3_TILE <- 1

# Parent main-board heights for a Neutral height preference.
#' @noRd
.g3_parent_heights <- function(neutral_height = c("highest", "lowest")) {
  neutral_height <- match.arg(neutral_height)
  if (neutral_height == "highest") {
    c(W = 0, N = .G3_NEUTRAL_RAISED, B = 0)
  } else {
    c(W = .G3_NEUTRAL_RAISED, N = 0, B = .G3_NEUTRAL_RAISED)
  }
}

# z shift of each parent assembly: the default offset plus, in the exploded
# inspection view, 2.25 * (1 - parent_index).
#' @noRd
.g3_parent_z_shift <- function(exploded = FALSE) {
  shift <- .G3_PARENT_OFFSET
  if (isTRUE(exploded)) {
    shift <- shift + .G3_EXPLODE_STEP * (1 - .G3_PARENT_INDEX[names(shift)])
  }
  shift
}

# Scene transform of logical square centres. `file`, `rank` are projected
# coordinates; `parent` the parent main board (W/N/B) of each square's
# board; `platform` whether the square is on a platform.
#' @noRd
.g3_scene_transform <- function(file, rank, parent, platform, neutral_height = "highest", exploded = FALSE) {
  heights <- .g3_parent_heights(neutral_height)
  shift <- .g3_parent_z_shift(exploded)
  data.frame(
    x = file - 2.5,
    y = unname(heights[parent]) + ifelse(platform, .G3_PLATFORM_LIFT, 0),
    z = 4.5 - rank + unname(shift[parent]),
    stringsAsFactors = FALSE
  )
}

# Parent main board (W/N/B) of every board in a state: main boards are
# their own parent; a platform's parent is its current dock's parent.
#' @noRd
.g3_board_parents <- function(state) {
  b <- state$boards
  parent <- b$id
  plat <- b$type == "platform"
  parent[plat] <- substr(b$dock[plat], 1L, 1L)
  stats::setNames(parent, b$id)
}

#' Scene coordinates of the 64 physical squares
#'
#' @param state A `g3_state`.
#' @param neutral_height `"highest"` or `"lowest"`.
#' @param exploded Logical; the labelled exploded inspection view.
#' @return A data frame with one row per physical square in canonical
#'   order: `square_id`, `board`, `u`, `v`, `parent`, `platform`, `file`,
#'   `rank`, `coord` (projected label such as `"b4"`), `shade` (`"light"`
#'   or `"dark"`, from projected parity), `x`, `y`, `z` (tile centre; `y`
#'   is the tile's top surface) and `size` (tile side length, always 1).
#' @noRd
.g3_scene_squares <- function(state, neutral_height = "highest", exploded = FALSE) {
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  co <- .g3_square_coords(core$dock, core$orient, tabs)
  board <- tabs$board_ids[tabs$sq_board]
  parents <- .g3_board_parents(state)
  parent <- unname(parents[board])
  platform <- tabs$sq_board > 3L
  xyz <- .g3_scene_transform(co$file, co$rank, parent, platform, neutral_height, exploded)
  data.frame(
    square_id = tabs$sq_id,
    board = board,
    u = tabs$sq_u,
    v = tabs$sq_v,
    parent = parent,
    platform = platform,
    file = co$file,
    rank = co$rank,
    coord = .g3_coord_label(co$file, co$rank),
    # Square colour follows projected geometry: (file + rank) odd is dark,
    # so b1 is light as on an ordinary board. Platform docks and 180-degree
    # turns preserve this parity, so a square keeps its colour when moved.
    shade = ifelse((co$file + co$rank) %% 2L == 1L, "dark", "light"),
    x = xyz$x,
    y = xyz$y,
    z = xyz$z,
    size = .G3_TILE,
    stringsAsFactors = FALSE
  )
}

#' Scene placement of the seven boards
#'
#' @inheritParams .g3_scene_squares
#' @return A data frame with one row per board (`W`, `N`, `B`, `WQ`, `WK`,
#'   `BQ`, `BK`): `board`, `type`, `parent`, `dock`, `orientation`, centre
#'   `x`, `y`, `z`, and tile-edge extents `x_min`, `x_max`, `z_min`,
#'   `z_max`.
#' @noRd
.g3_scene_boards <- function(state, neutral_height = "highest", exploded = FALSE,
                             squares = .g3_scene_squares(state, neutral_height, exploded)) {
  b <- state$boards
  half <- .G3_TILE / 2
  rows <- lapply(b$id, function(id) {
    s <- squares[squares$board == id, ]
    data.frame(
      board = id,
      type = b$type[b$id == id],
      parent = s$parent[1L],
      dock = b$dock[b$id == id],
      orientation = as.integer(b$orientation[b$id == id]),
      x = mean(s$x), y = s$y[1L], z = mean(s$z),
      x_min = min(s$x) - half, x_max = max(s$x) + half,
      z_min = min(s$z) - half, z_max = max(s$z) + half,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Horizontal edge-to-edge gap between the facing W and B main boards.
#' @noRd
.g3_scene_edge_gap <- function(boards) {
  boards$z_min[boards$board == "W"] - boards$z_max[boards$board == "B"]
}
