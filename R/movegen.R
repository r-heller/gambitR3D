# Move generation and attack detection.
#
# Actions are carried internally as rows of an integer matrix with columns:
#
#   type   1 piece move, 2 platform action, 3 castling
#   piece  moving (or carried) piece index
#   from   source square (piece moves and castling: king source)
#   to     destination square (piece moves and castling: king destination)
#   cap    captured piece index, 0 when none
#   promo  promotion type code (2..5), 0 when none
#   plat   platform 1..4 for platform actions
#   dock   destination dock 1..12 for platform actions
#   rot    rotation delta 0 or 180 for platform actions
#   cside  castling side: 1 king side, 2 queen side
#   flag   1 pawn double step, 2 en passant
#
# Legality follows the profile: generate pseudo-legal candidates, apply each
# to a light copy of the position and reject those leaving the mover's king
# attacked. Attack detection is geometric and never calls move generation.

.G3_ACOLS <- c("type", "piece", "from", "to", "cap", "promo", "plat", "dock", "rot", "cside", "flag")
.G3_KNIGHT <- matrix(c(1L, 2L, 2L, 1L, 2L, -1L, 1L, -2L, -1L, -2L, -2L, -1L, -2L, 1L, -1L, 2L), ncol = 2L, byrow = TRUE)
.G3_KING <- matrix(c(-1L, -1L, -1L, 0L, -1L, 1L, 0L, -1L, 0L, 1L, 1L, -1L, 1L, 0L, 1L, 1L), ncol = 2L, byrow = TRUE)
.G3_ORTHO <- matrix(c(1L, 0L, -1L, 0L, 0L, 1L, 0L, -1L), ncol = 2L, byrow = TRUE)
.G3_DIAG <- matrix(c(1L, 1L, 1L, -1L, -1L, 1L, -1L, -1L), ncol = 2L, byrow = TRUE)
.G3_PAWN_DIR <- c(1L, -1L)

# Coordinates, occupancy counts per projected coordinate and square colours.
#' @noRd
.g3_ctx <- function(core, tabs) {
  co <- .g3_square_coords(core$dock, core$orient, tabs)
  occupied <- core$occ > 0L
  sqcol <- integer(64L)
  sqcol[occupied] <- core$pcolor[core$occ[occupied]]
  list(
    file = co$file, rank = co$rank, ci = co$ci,
    cnt = tabulate(co$ci[occupied], 60L), sqcol = sqcol
  )
}

#' @noRd
.g3_in_grid <- function(f, r) f >= 0L & f <= 5L & r >= 0L & r <= 9L

# Is projected coordinate (f, r) attacked by colour `by`? Works for virtual
# coordinates with no physical square. `file`, `rank` are square coordinates,
# `cnt` the occupant count per coordinate, `psq`/`ptype`/`pcolor` the pieces.
#' @noRd
.g3_attacked <- function(f, r, by, file, rank, cnt, psq, ptype, pcolor) {
  idx <- which(pcolor == by & psq > 0L)
  if (!length(idx)) {
    return(FALSE)
  }
  s <- psq[idx]
  t <- ptype[idx]
  df <- f - file[s]
  dr <- r - rank[s]
  adf <- abs(df)
  adr <- abs(dr)
  # Pawns attack one file aside, one rank forward in their colour's direction.
  if (any(t == 1L & dr == .G3_PAWN_DIR[by] & adf == 1L)) return(TRUE)
  if (any(t == 2L & ((adf == 1L & adr == 2L) | (adf == 2L & adr == 1L)))) return(TRUE)
  if (any(t == 6L & pmax(adf, adr) == 1L)) return(TRUE)
  ortho <- (t == 4L | t == 5L) & xor(df == 0L, dr == 0L)
  diag <- (t == 3L | t == 5L) & adf == adr & adf > 0L
  cand <- which(ortho | diag)
  for (k in cand) {
    n <- max(adf[k], adr[k])
    if (n == 1L) return(TRUE)
    sf <- sign(df[k])
    sr <- sign(dr[k])
    steps <- seq_len(n - 1L)
    ci <- (file[s[k]] + steps * sf) * 10L + rank[s[k]] + steps * sr + 1L
    if (all(cnt[ci] == 0L)) return(TRUE)
  }
  FALSE
}

#' @noRd
.g3_king_sq <- function(core, color) {
  k <- which(core$ptype == 6L & core$pcolor == color & core$psq > 0L)
  if (length(k) != 1L) NA_integer_ else core$psq[k]
}

#' @noRd
.g3_in_check <- function(core, color, ctx, tabs) {
  ks <- .g3_king_sq(core, color)
  if (is.na(ks)) {
    return(FALSE)
  }
  .g3_attacked(ctx$file[ks], ctx$rank[ks], 3L - color, ctx$file, ctx$rank, ctx$cnt, core$psq, core$ptype, core$pcolor)
}

# Does a pawn of `color` arriving on square `sq` promote? `dock` gives the
# current platform docks (the destination dock for a platform action).
#' @noRd
.g3_promotes <- function(color, sq, rank, dock, tabs) {
  pr <- tabs$promo[[color]]
  b <- tabs$sq_board[sq]
  main <- b <= 3L
  plat_dock <- dock[pmax(b - 3L, 1L)]
  ifelse(main, b == pr$board & rank == pr$rank, plat_dock %in% pr$docks & rank == pr$prank)
}

# Pseudo-legal piece and platform actions for the side to move (castling is
# generated separately and is always fully legal).
#' @noRd
.g3_pseudo <- function(core, ctx, tabs) {
  side <- core$side
  rows <- vector("list", 64L)
  nr <- 0L
  file <- ctx$file
  rank <- ctx$rank
  ci <- ctx$ci
  sqcol <- ctx$sqcol
  occ <- core$occ
  # Destination squares at coordinate (tf, tr) that are empty or hold an enemy.
  dests_at <- function(tf, tr) {
    d <- which(ci == tf * 10L + tr + 1L)
    d[sqcol[d] != side]
  }
  movers <- which(core$pcolor == side & core$psq > 0L)
  for (p in movers) {
    s <- core$psq[p]
    f <- file[s]
    r <- rank[s]
    t <- core$ptype[p]
    to <- integer()
    flag <- integer()
    if (t == 2L || t == 6L) {
      off <- if (t == 2L) .G3_KNIGHT else .G3_KING
      tf <- f + off[, 1L]
      tr <- r + off[, 2L]
      ok <- .g3_in_grid(tf, tr)
      tci <- tf[ok] * 10L + tr[ok] + 1L
      d <- which(ci %in% tci)
      to <- d[sqcol[d] != side]
      flag <- integer(length(to))
    } else if (t >= 3L && t <= 5L) {
      dirs <- switch(t - 2L, .G3_DIAG, .G3_ORTHO, rbind(.G3_ORTHO, .G3_DIAG))
      for (k in seq_len(nrow(dirs))) {
        tf <- f
        tr <- r
        repeat {
          tf <- tf + dirs[k, 1L]
          tr <- tr + dirs[k, 2L]
          if (!.g3_in_grid(tf, tr)) break
          to <- c(to, dests_at(tf, tr))
          if (ctx$cnt[tf * 10L + tr + 1L] > 0L) break
        }
      }
      flag <- integer(length(to))
    } else {
      dir <- .G3_PAWN_DIR[side]
      tr <- r + dir
      if (tr >= 0L && tr <= 9L) {
        d <- which(ci == f * 10L + tr + 1L)
        d <- d[occ[d] == 0L]
        to <- c(to, d)
        flag <- c(flag, integer(length(d)))
        tr2 <- r + 2L * dir
        if (!core$pmoved[p] && tr2 >= 0L && tr2 <= 9L && ctx$cnt[f * 10L + tr + 1L] == 0L) {
          d2 <- which(ci == f * 10L + tr2 + 1L)
          d2 <- d2[occ[d2] == 0L]
          to <- c(to, d2)
          flag <- c(flag, rep(1L, length(d2)))
        }
        for (tf in c(f - 1L, f + 1L)) {
          if (tf < 0L || tf > 5L) next
          d <- which(ci == tf * 10L + tr + 1L)
          d <- d[sqcol[d] == 3L - side]
          to <- c(to, d)
          flag <- c(flag, integer(length(d)))
          # En passant: land on an empty square at the crossed coordinate.
          if (length(core$ep) && core$ep[3L] == tf && core$ep[4L] == tr &&
              core$pcolor[core$ep[1L]] != side && core$psq[core$ep[1L]] == core$ep[2L]) {
            d <- which(ci == tf * 10L + tr + 1L)
            d <- d[occ[d] == 0L]
            to <- c(to, d)
            flag <- c(flag, rep(2L, length(d)))
          }
        }
      }
    }
    if (!length(to)) next
    cap <- occ[to]
    cap[flag == 2L] <- core$ep[1L]
    m <- cbind(1L, p, s, to, cap, 0L, 0L, 0L, 0L, 0L, flag)
    if (t == 1L) {
      pro <- .g3_promotes(side, to, rank[to], core$dock, tabs)
      if (any(pro)) {
        keep <- m[!pro, , drop = FALSE]
        pm <- m[pro, , drop = FALSE]
        pm <- pm[rep(seq_len(nrow(pm)), each = 4L), , drop = FALSE]
        pm[, 6L] <- rep(c(5L, 4L, 3L, 2L), times = sum(pro))
        m <- rbind(keep, pm)
      }
    }
    nr <- nr + 1L
    rows[[nr]] <- m
  }

  # Platform actions: exactly one occupant, owned by the mover; move to an
  # adjacent free dock, keeping or turning the orientation.
  for (pl in 1:4) {
    sqs <- tabs$plat_squares[[pl]]
    occp <- occ[sqs][occ[sqs] > 0L]
    if (length(occp) != 1L || core$pcolor[occp] != side) next
    free <- setdiff(tabs$dock_adj[[core$dock[pl]]], core$dock[-pl])
    if (!length(free)) next
    psq <- core$psq[occp]
    cell <- psq - sqs[1L] + 1L
    for (nd in free) {
      for (rot in c(0L, 180L)) {
        promo <- 0L
        if (core$ptype[occp] == 1L) {
          no <- (core$orient[pl] + rot) %% 360L
          nrk <- tabs$plat_rank[nd, if (no == 180L) 2L else 1L, cell]
          dk <- core$dock
          dk[pl] <- nd
          if (.g3_promotes(side, psq, nrk, dk, tabs)) promo <- c(5L, 4L, 3L, 2L)
        }
        m <- cbind(2L, occp, psq, 0L, 0L, promo, pl, nd, rot, 0L, 0L)
        nr <- nr + 1L
        rows[[nr]] <- m
      }
    }
  }
  if (nr == 0L) {
    out <- matrix(integer(), 0L, 11L)
  } else {
    out <- do.call(rbind, rows[seq_len(nr)])
  }
  storage.mode(out) <- "integer"
  colnames(out) <- .G3_ACOLS
  out
}

# Is the mover's king safe after pseudo-legal action `a`? Uses a light copy:
# only squares, piece squares and platform docks change.
#' @noRd
.g3_safe_after <- function(core, ctx, a, tabs) {
  side <- core$side
  psq <- core$psq
  file <- ctx$file
  rank <- ctx$rank
  cnt <- ctx$cnt
  if (a[1L] == 1L) {
    p <- a[2L]
    from <- a[3L]
    to <- a[4L]
    cnt[ctx$ci[from]] <- cnt[ctx$ci[from]] - 1L
    if (a[5L] > 0L) {
      vs <- psq[a[5L]]
      cnt[ctx$ci[vs]] <- cnt[ctx$ci[vs]] - 1L
      psq[a[5L]] <- 0L
    }
    cnt[ctx$ci[to]] <- cnt[ctx$ci[to]] + 1L
    psq[p] <- to
  } else {
    dock <- core$dock
    orient <- core$orient
    pl <- a[7L]
    dock[pl] <- a[8L]
    orient[pl] <- (orient[pl] + a[9L]) %% 360L
    co <- .g3_square_coords(dock, orient, tabs)
    file <- co$file
    rank <- co$rank
    cnt <- tabulate(co$ci[core$occ > 0L], 60L)
  }
  k <- which(core$ptype == 6L & core$pcolor == side)
  ks <- psq[k]
  !.g3_attacked(file[ks], rank[ks], 3L - side, file, rank, cnt, psq, core$ptype, core$pcolor)
}

# Fully legal castling actions for the side to move.
#' @noRd
.g3_castles <- function(core, ctx, tabs) {
  side <- core$side
  out <- matrix(integer(), 0L, 11L)
  if (!core$first[side]) {
    return(out)
  }
  enemy <- 3L - side
  for (cs in 1:2) {
    if (!core$castle[(side - 1L) * 2L + cs]) next
    def <- tabs$castle[[side]][[cs]]
    k <- match(def$king, core$pid)
    rk <- match(def$rook, core$pid)
    if (is.na(k) || is.na(rk)) next
    if (core$psq[k] != def$king_from || core$psq[rk] != def$rook_from) next
    if (core$ptype[k] != 6L || core$ptype[rk] != 4L) next
    if (any(core$dock[def$home_platforms] != def$home_docks) || any(core$orient[def$home_platforms] != 0L)) next
    if (length(def$empty) && any(core$occ[def$empty] != 0L)) next
    if (nrow(def$clear) && any(ctx$cnt[def$clear[, 1L] * 10L + def$clear[, 2L] + 1L] > 0L)) next
    # The king may not castle out of check.
    if (.g3_attacked(ctx$file[def$king_from], ctx$rank[def$king_from], enemy, ctx$file, ctx$rank,
                     ctx$cnt, core$psq, core$ptype, core$pcolor)) next
    # Transit probes with the king lifted from its source, rook in place.
    if (nrow(def$probe)) {
      cnt <- ctx$cnt
      cnt[ctx$ci[def$king_from]] <- cnt[ctx$ci[def$king_from]] - 1L
      psq <- core$psq
      psq[k] <- 0L
      hit <- FALSE
      for (i in seq_len(nrow(def$probe))) {
        if (.g3_attacked(def$probe[i, 1L], def$probe[i, 2L], enemy, ctx$file, ctx$rank, cnt, psq,
                         core$ptype, core$pcolor)) {
          hit <- TRUE
          break
        }
      }
      if (hit) next
    }
    # Final compound position must leave the king safe.
    psq <- core$psq
    psq[k] <- def$king_to
    psq[rk] <- def$rook_to
    occ <- core$occ
    occ[c(def$king_from, def$rook_from)] <- 0L
    occ[def$king_to] <- k
    occ[def$rook_to] <- rk
    cnt <- tabulate(ctx$ci[occ > 0L], 60L)
    if (.g3_attacked(ctx$file[def$king_to], ctx$rank[def$king_to], enemy, ctx$file, ctx$rank, cnt, psq,
                     core$ptype, core$pcolor)) next
    out <- rbind(out, c(3L, k, def$king_from, def$king_to, 0L, 0L, 0L, 0L, 0L, cs, 0L))
  }
  storage.mode(out) <- "integer"
  colnames(out) <- .G3_ACOLS
  out
}

# All legal actions as an integer matrix, sorted by action id.
#' @noRd
.g3_legal <- function(core, tabs, ctx = .g3_ctx(core, tabs)) {
  ps <- .g3_pseudo(core, ctx, tabs)
  if (nrow(ps)) {
    keep <- vapply(seq_len(nrow(ps)), function(i) .g3_safe_after(core, ctx, ps[i, ], tabs), TRUE)
    ps <- ps[keep, , drop = FALSE]
  }
  m <- rbind(ps, .g3_castles(core, ctx, tabs))
  if (nrow(m) > 1L) {
    ids <- .g3_action_ids(core, m, tabs)
    m <- m[order(ids, method = "radix"), , drop = FALSE]
  }
  m
}

# Is there a legal en-passant capture? Used by the repetition key; it does
# not call full move generation.
#' @noRd
.g3_ep_effective <- function(core, ctx, tabs) {
  if (!length(core$ep)) {
    return(FALSE)
  }
  side <- core$side
  v <- core$ep[1L]
  if (core$psq[v] != core$ep[2L] || core$pcolor[v] == side) {
    return(FALSE)
  }
  tf <- core$ep[3L]
  tr <- core$ep[4L]
  dir <- .G3_PAWN_DIR[side]
  dest <- which(ctx$ci == tf * 10L + tr + 1L)
  dest <- dest[core$occ[dest] == 0L]
  if (!length(dest)) {
    return(FALSE)
  }
  pawns <- which(core$pcolor == side & core$ptype == 1L & core$psq > 0L)
  for (p in pawns) {
    s <- core$psq[p]
    if (ctx$rank[s] + dir == tr && abs(ctx$file[s] - tf) == 1L) {
      for (d in dest) {
        a <- c(1L, p, s, d, v, 0L, 0L, 0L, 0L, 0L, 2L)
        if (.g3_safe_after(core, ctx, a, tabs)) return(TRUE)
      }
    }
  }
  FALSE
}

# Apply a legal action to a core (no legality check). Returns the new core
# with side, ply, rights, en passant, halfmove counter and repetition keys
# updated. Terminal detection is separate.
#' @noRd
.g3_make <- function(core, a, tabs) {
  a <- unname(a)
  side <- core$side
  p <- a[2L]
  core$ep <- integer(0L)
  irreversible <- FALSE
  reset_half <- FALSE
  if (a[1L] == 1L) {
    from <- a[3L]
    to <- a[4L]
    if (a[5L] > 0L) {
      v <- a[5L]
      core$occ[core$psq[v]] <- 0L
      core$castle <- .g3_revoke_rook(core, v, tabs)
      core$psq[v] <- 0L
      irreversible <- TRUE
      reset_half <- TRUE
    }
    core$occ[from] <- 0L
    core$occ[to] <- p
    core$psq[p] <- to
    core$pmoved[p] <- TRUE
    if (core$ptype[p] == 1L) {
      reset_half <- TRUE
      if (a[11L] == 1L) {
        co <- .g3_square_coords(core$dock, core$orient, tabs)
        core$ep <- c(p, to, co$file[from], co$rank[from] + .G3_PAWN_DIR[side])
      }
    }
    if (a[6L] > 0L) {
      core$ptype[p] <- a[6L]
      irreversible <- TRUE
    }
    core$castle <- .g3_revoke_mover(core, p, side, tabs)
  } else if (a[1L] == 2L) {
    pl <- a[7L]
    core$dock[pl] <- a[8L]
    core$orient[pl] <- (core$orient[pl] + a[9L]) %% 360L
    core$pmoved[p] <- TRUE
    if (core$ptype[p] == 1L) reset_half <- TRUE
    if (a[6L] > 0L) {
      core$ptype[p] <- a[6L]
      irreversible <- TRUE
    }
    core$castle <- .g3_revoke_mover(core, p, side, tabs)
  } else {
    def <- tabs$castle[[side]][[a[10L]]]
    rk <- match(def$rook, core$pid)
    core$occ[c(def$king_from, def$rook_from)] <- 0L
    core$occ[def$king_to] <- p
    core$occ[def$rook_to] <- rk
    core$psq[p] <- def$king_to
    core$psq[rk] <- def$rook_to
    core$pmoved[c(p, rk)] <- TRUE
    core$castle[(side - 1L) * 2L + 1:2] <- FALSE
  }
  core$halfmove <- if (reset_half) 0L else core$halfmove + 1L
  core$first[side] <- TRUE
  core$side <- 3L - side
  core$ply <- core$ply + 1L
  key <- .g3_key_core(core, tabs)
  core$rep <- if (irreversible) key else c(core$rep, key)
  core
}

# Castling rights lost when piece `p` of `side` moves (or is carried).
#' @noRd
.g3_revoke_mover <- function(core, p, side, tabs) {
  cr <- core$castle
  id <- core$pid[p]
  defs <- tabs$castle[[side]]
  if (id == defs[[1L]]$king) cr[(side - 1L) * 2L + 1:2] <- FALSE
  for (cs in 1:2) if (id == defs[[cs]]$rook) cr[(side - 1L) * 2L + cs] <- FALSE
  cr
}

# Castling rights lost when piece `v` is captured.
#' @noRd
.g3_revoke_rook <- function(core, v, tabs) {
  cr <- core$castle
  col <- core$pcolor[v]
  defs <- tabs$castle[[col]]
  for (cs in 1:2) if (core$pid[v] == defs[[cs]]$rook) cr[(col - 1L) * 2L + cs] <- FALSE
  cr
}
