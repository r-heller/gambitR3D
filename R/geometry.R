# Engine geometry tables.
#
# Squares are numbered 1..64 in the canonical order W, N, B, WQ, WK, BQ, BK
# and, within a board, by local v then u (`board:u:v`). Main-board squares
# have fixed projected coordinates. A platform square's projected file and
# rank follow its platform's current dock and orientation:
#
#   orientation 0:   file = min_file + u,     rank = min_rank + v
#   orientation 180: file = min_file + 1 - u, rank = min_rank + 1 - v
#
# Projected coordinates are packed as `ci = file * 10 + rank + 1` (1..60).
# Docks are numbered 1..12 in the order of the profile's adjacency table.
# Piece types: 1 P, 2 N, 3 B, 4 R, 5 Q, 6 K. Colours: 1 white, 2 black.

.G3_TYPES <- c("P", "N", "B", "R", "Q", "K")
.G3_COLORS <- c("white", "black")
.G3_FILES <- c("a", "b", "c", "d", "e", "f")
.G3_PLATFORMS <- c("WQ", "WK", "BQ", "BK")

# Build (once) and return the engine tables for the supported profile.
#' @noRd
.g3_tables <- function() {
  if (!is.null(.g3_env$tables)) {
    return(.g3_env$tables)
  }
  prof <- g3_rules_profile("experimental-v0.1")
  s <- prof$semantics
  board_ids <- unlist(s$square_order)
  sizes <- c(vapply(s$main_boards, function(b) b$size, 0L), vapply(s$platforms, function(p) p$size, 0L))
  sq_board <- integer()
  sq_u <- integer()
  sq_v <- integer()
  for (b in seq_along(board_ids)) {
    n <- sizes[b]
    sq_board <- c(sq_board, rep(b, n * n))
    sq_u <- c(sq_u, rep(0:(n - 1L), times = n))
    sq_v <- c(sq_v, rep(0:(n - 1L), each = n))
  }
  sq_id <- paste(board_ids[sq_board], sq_u, sq_v, sep = ":")

  # Fixed coordinates of the 48 main-board squares.
  main_min_file <- vapply(s$main_boards, function(b) b$min_file, 0L)
  main_min_rank <- vapply(s$main_boards, function(b) b$min_rank, 0L)
  main_file <- main_min_file[sq_board[1:48]] + sq_u[1:48]
  main_rank <- main_min_rank[sq_board[1:48]] + sq_v[1:48]

  # Docks.
  dock_ids <- names(s$dock_adjacency)
  parts <- strsplit(dock_ids, ":", fixed = TRUE)
  dock_parent <- match(vapply(parts, `[`, "", 1L), c("W", "N", "B"))
  dock_side <- match(vapply(parts, `[`, "", 2L), c("L", "R"))
  dock_end <- match(vapply(parts, `[`, "", 3L), c("lo", "hi"))
  pmf <- unlist(s$docks$platform_min_file)
  pro <- unlist(s$docks$platform_min_rank_offset)
  dock_min_file <- unname(pmf[c("L", "R")][dock_side])
  dock_min_rank <- main_min_rank[dock_parent] + unname(pro[c("lo", "hi")][dock_end])
  dock_adj <- lapply(dock_ids, function(d) match(unlist(s$dock_adjacency[[d]]), dock_ids))

  # Platform cell coordinates per dock and orientation (cells in v-then-u order).
  cu <- c(0L, 1L, 0L, 1L)
  cv <- c(0L, 0L, 1L, 1L)
  plat_file <- array(0L, c(12L, 2L, 4L))
  plat_rank <- array(0L, c(12L, 2L, 4L))
  for (d in 1:12) {
    plat_file[d, 1L, ] <- dock_min_file[d] + cu
    plat_rank[d, 1L, ] <- dock_min_rank[d] + cv
    plat_file[d, 2L, ] <- dock_min_file[d] + 1L - cu
    plat_rank[d, 2L, ] <- dock_min_rank[d] + 1L - cv
  }

  # Setup.
  st <- s$setup
  setup <- data.frame(
    id = vapply(st, function(p) p$id, ""),
    type = match(vapply(st, function(p) p$type, ""), .G3_TYPES),
    color = match(vapply(st, function(p) p$color, ""), .G3_COLORS),
    sq = match(vapply(st, function(p) p$square, ""), sq_id),
    stringsAsFactors = FALSE
  )
  init_dock <- match(vapply(s$platforms, function(p) p$initial_dock, ""), dock_ids)

  # Promotion endpoints.
  promo <- list(
    white = list(
      board = match(s$promotion$white$main_board, board_ids), rank = s$promotion$white$main_rank,
      docks = match(unlist(s$promotion$white$platform_docks), dock_ids), prank = s$promotion$white$platform_rank
    ),
    black = list(
      board = match(s$promotion$black$main_board, board_ids), rank = s$promotion$black$main_rank,
      docks = match(unlist(s$promotion$black$platform_docks), dock_ids), prank = s$promotion$black$platform_rank
    )
  )

  # Castling definitions as square indices; index [color][side], side 1 king, 2 queen.
  castle <- lapply(.G3_COLORS, function(col) {
    cs <- s$castling[[col]]
    lapply(c("kingside", "queenside"), function(side) {
      x <- cs[[side]]
      list(
        king = cs$king, rook = x$rook,
        king_from = match(x$king_from, sq_id), rook_from = match(x$rook_from, sq_id),
        king_to = match(x$king_to, sq_id), rook_to = match(x$rook_to, sq_id),
        home_platforms = match(names(x$home_docks), .G3_PLATFORMS),
        home_docks = match(unlist(x$home_docks), dock_ids),
        clear = .g3_coord_list(x$clear_coordinates),
        probe = .g3_coord_list(x$probe_coordinates),
        empty = match(unlist(x$empty_squares), sq_id)
      )
    })
  })

  # Static lookup tables in projected-coordinate space (ci = file*10 + rank + 1):
  # main-board squares at each coordinate, knight and king targets, and the
  # coordinates along each of the eight rays (orthogonal first, then diagonal).
  main_ci <- main_file * 10L + main_rank + 1L
  main_by_ci <- lapply(1:60, function(ci) which(main_ci == ci))
  cf <- (0:59) %/% 10L
  cr <- (0:59) %% 10L
  offsets_ci <- function(off) {
    lapply(1:60, function(ci) {
      tf <- cf[ci] + off[, 1L]
      tr <- cr[ci] + off[, 2L]
      ok <- tf >= 0L & tf <= 5L & tr >= 0L & tr <= 9L
      as.integer(tf[ok] * 10L + tr[ok] + 1L)
    })
  }
  knight_off <- matrix(c(1L, 2L, 2L, 1L, 2L, -1L, 1L, -2L, -1L, -2L, -2L, -1L, -2L, 1L, -1L, 2L), ncol = 2L, byrow = TRUE)
  king_off <- matrix(c(-1L, -1L, -1L, 0L, -1L, 1L, 0L, -1L, 0L, 1L, 1L, -1L, 1L, 0L, 1L, 1L), ncol = 2L, byrow = TRUE)
  dirs <- matrix(c(1L, 0L, -1L, 0L, 0L, 1L, 0L, -1L, 1L, 1L, 1L, -1L, -1L, 1L, -1L, -1L), ncol = 2L, byrow = TRUE)
  ray_ci <- lapply(1:60, function(ci) {
    lapply(1:8, function(k) {
      out <- integer()
      f <- cf[ci]
      r <- cr[ci]
      repeat {
        f <- f + dirs[k, 1L]
        r <- r + dirs[k, 2L]
        if (f < 0L || f > 5L || r < 0L || r > 9L) break
        out <- c(out, as.integer(f * 10L + r + 1L))
      }
      out
    })
  })

  tabs <- list(
    rules_version = prof$rules_version,
    rules_hash = prof$rules_hash,
    board_ids = board_ids,
    sq_board = sq_board, sq_u = sq_u, sq_v = sq_v, sq_id = sq_id,
    main_file = main_file, main_rank = main_rank,
    dock_ids = dock_ids, dock_parent = dock_parent, dock_side = dock_side, dock_end = dock_end,
    dock_min_file = dock_min_file, dock_min_rank = dock_min_rank, dock_adj = dock_adj,
    plat_file = plat_file, plat_rank = plat_rank,
    plat_squares = lapply(1:4, function(p) 48L + (p - 1L) * 4L + 1:4),
    setup = setup, init_dock = init_dock,
    promo = promo, castle = castle,
    main_by_ci = main_by_ci, knight_ci = offsets_ci(knight_off), king_ci = offsets_ci(king_off),
    ray_ci = ray_ci
  )
  .g3_env$tables <- tabs
  tabs
}

#' @noRd
.g3_coord_list <- function(x) {
  if (!length(x)) {
    return(matrix(integer(), ncol = 2L))
  }
  do.call(rbind, lapply(x, function(p) as.integer(unlist(p))))
}

# Projected file and rank of all 64 squares for given platform docks and
# orientations (integer vectors of length 4).
#' @noRd
.g3_square_coords <- function(dock, orient, tabs = .g3_tables()) {
  f <- c(tabs$main_file, integer(16L))
  r <- c(tabs$main_rank, integer(16L))
  for (p in 1:4) {
    idx <- 48L + (p - 1L) * 4L + 1:4
    o <- if (orient[p] == 180L) 2L else 1L
    f[idx] <- tabs$plat_file[dock[p], o, ]
    r[idx] <- tabs$plat_rank[dock[p], o, ]
  }
  list(file = f, rank = r, ci = f * 10L + r + 1L)
}

#' @noRd
.g3_coord_label <- function(file, rank) {
  paste0(.G3_FILES[file + 1L], rank)
}
