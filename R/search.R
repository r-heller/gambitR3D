# Search: negamax with alpha-beta, iterative deepening and a bounded
# quiescence phase.
#
# Budgets: `nodes` (positions visited, per root position), `time_ms`,
# `depth` (maximum full-width depth). Only a completed root iteration is
# published; if none completes, a documented fallback is used (the first
# action in the static ordering) and the status says so.
#
# Scores are heuristic units from the side to act's perspective; mates are
# MATE - plies. The transposition table never shares scores between
# different draw contexts: its key includes the whole repetition record
# and the halfmove counter, plus evaluation version and budget kind.

.G3_MATE <- 1000000L
.G3_MATE_BOUND <- .G3_MATE - 1000L
.G3_SEARCH_VERSION <- "g3-search/1"
.G3_QDEPTH <- 4L

#' @noRd
.g3_budget <- function(budget) {
  b <- list(nodes = 2000L, time_ms = Inf, depth = 8L)
  if (!is.null(budget)) {
    if (!is.list(budget)) .g3_abort("g3_budget_exceeded", "`budget` must be a list, e.g. list(nodes = 2000L).")
    unknown <- setdiff(names(budget), names(b))
    if (length(unknown)) .g3_abort("g3_budget_exceeded", paste0("Unknown budget field: ", unknown[1L]))
    b[names(budget)] <- budget
  }
  b$nodes <- as.numeric(b$nodes)
  b$time_ms <- as.numeric(b$time_ms)
  b$depth <- as.integer(b$depth)
  if (is.na(b$nodes) || b$nodes < 0 || is.na(b$time_ms) || b$time_ms < 0 || is.na(b$depth) || b$depth < 1L) {
    .g3_abort("g3_budget_exceeded", "Budgets must be non-negative (depth at least 1).")
  }
  b
}

# Order candidate actions: TT move, promotions, captures (most valuable
# victim, least valuable attacker), castling, platform actions, the rest.
# Stable within a class, so ties keep action-id order.
#' @noRd
.g3_order <- function(core, m, tt_move = NA_integer_) {
  if (nrow(m) <= 1L) return(m)
  victim <- ifelse(m[, "cap"] > 0L, .G3_PIECE_VALUE[core$ptype[pmax(m[, "cap"], 1L)]], 0L)
  attacker <- .G3_PIECE_VALUE[core$ptype[m[, "piece"]]]
  key <- victim * 10L - attacker / 100 + ifelse(m[, "promo"] > 0L, 5000L, 0L) +
    ifelse(m[, "type"] == 3L, 50L, 0L) + ifelse(m[, "type"] == 2L, 20L, 0L)
  if (!is.na(tt_move)) key[tt_move] <- 1e7
  m[order(-key, seq_len(nrow(m)), method = "radix"), , drop = FALSE]
}

# Draw by the rules before move generation (repetition, counter, kings).
#' @noRd
.g3_rule_draw <- function(core) {
  key <- core$rep[length(core$rep)]
  if (sum(core$rep == key) >= 3L || core$halfmove >= 100L) return(TRUE)
  alive <- core$psq > 0L
  sum(alive) == 2L && all(core$ptype[alive] == 6L)
}

#' @noRd
.g3_search_env <- function(budget, tabs) {
  e <- new.env(parent = emptyenv())
  e$nodes <- 0
  e$budget <- budget
  e$t0 <- proc.time()[["elapsed"]]
  e$stop <- FALSE
  e$tt <- new.env(hash = TRUE, parent = emptyenv())
  e$tt_n <- 0L
  e$tabs <- tabs
  # An external stop check (set by background jobs) is polled every 64 nodes.
  e$stop_check <- getOption("gambitR3D.stop_check")
  e
}

#' @noRd
.g3_tick <- function(e) {
  e$nodes <- e$nodes + 1
  if (e$nodes > e$budget$nodes) e$stop <- TRUE
  if (is.finite(e$budget$time_ms) && (e$nodes %% 16 == 0) &&
      (proc.time()[["elapsed"]] - e$t0) * 1000 > e$budget$time_ms) e$stop <- TRUE
  if (!is.null(e$stop_check) && (e$nodes %% 64 == 0) && isTRUE(e$stop_check())) e$stop <- TRUE
  e$stop
}

#' @noRd
.g3_tt_key <- function(core) {
  paste(core$rep[length(core$rep)], length(core$rep), .g3_sha256(paste(core$rep, collapse = "")),
        core$halfmove, sep = "|")
}

#' @noRd
.g3_negamax <- function(e, core, depth, alpha, beta, ply) {
  if (.g3_tick(e)) return(0)
  tabs <- e$tabs
  if (ply > 0L && .g3_rule_draw(core)) return(0)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  if (!nrow(legal)) {
    return(if (.g3_in_check(core, core$side, ctx, tabs)) -(.G3_MATE - ply) else 0)
  }
  if (depth <= 0L) return(.g3_qsearch(e, core, alpha, beta, ply, 0L, ctx))
  ttk <- .g3_tt_key(core)
  ent <- e$tt[[ttk]]
  tt_move <- NA_integer_
  if (!is.null(ent)) {
    tt_move <- match(ent$move, .g3_action_ids(core, legal, tabs))
    if (ent$depth >= depth) {
      s <- .g3_tt_unadjust(ent$score, ply)
      if (ent$flag == 0L) return(s)
      if (ent$flag == 1L && s >= beta) return(s)
      if (ent$flag == 2L && s <= alpha) return(s)
    }
  }
  legal <- .g3_order(core, legal, tt_move)
  alpha0 <- alpha
  best <- -Inf
  best_i <- 1L
  for (i in seq_len(nrow(legal))) {
    child <- .g3_make(core, legal[i, ], tabs)
    s <- -.g3_negamax(e, child, depth - 1L, -beta, -alpha, ply + 1L)
    if (e$stop) return(0)
    if (s > best) {
      best <- s
      best_i <- i
    }
    if (s > alpha) alpha <- s
    if (alpha >= beta) break
  }
  flag <- if (best <= alpha0) 2L else if (best >= beta) 1L else 0L
  if (e$tt_n > 200000L) {
    e$tt <- new.env(hash = TRUE, parent = emptyenv())
    e$tt_n <- 0L
  }
  e$tt[[ttk]] <- list(depth = depth, score = .g3_tt_adjust(best, ply), flag = flag,
                      move = .g3_action_ids(core, legal[best_i, , drop = FALSE], tabs))
  e$tt_n <- e$tt_n + 1L
  best
}

#' @noRd
.g3_tt_adjust <- function(s, ply) {
  if (s > .G3_MATE_BOUND) s + ply else if (s < -.G3_MATE_BOUND) s - ply else s
}

#' @noRd
.g3_tt_unadjust <- function(s, ply) {
  if (s > .G3_MATE_BOUND) s - ply else if (s < -.G3_MATE_BOUND) s + ply else s
}

# Quiescence: all evasions when in check; otherwise captures and
# promotions only, at most .G3_QDEPTH plies deep. Stalemate is not
# detected here (a documented horizon limitation).
#' @noRd
.g3_qsearch <- function(e, core, alpha, beta, ply, qd, ctx = .g3_ctx(core, e$tabs)) {
  if (.g3_tick(e)) return(0)
  tabs <- e$tabs
  if (qd > 0L && .g3_rule_draw(core)) return(0)
  in_check <- .g3_in_check(core, core$side, ctx, tabs)
  if (in_check) {
    moves <- .g3_legal(core, tabs, ctx)
    if (!nrow(moves)) return(-(.G3_MATE - ply))
    if (qd >= .G3_QDEPTH) return(.g3_eval(core, ctx, tabs))
    best <- -Inf
  } else {
    stand <- .g3_eval(core, ctx, tabs)
    if (stand >= beta || qd >= .G3_QDEPTH) return(stand)
    if (stand > alpha) alpha <- stand
    best <- stand
    ps <- .g3_pseudo(core, ctx, tabs)
    ps <- ps[ps[, "cap"] > 0L | ps[, "promo"] > 0L, , drop = FALSE]
    if (!nrow(ps)) return(stand)
    keep <- vapply(seq_len(nrow(ps)), function(i) .g3_safe_after(core, ctx, ps[i, ], tabs), TRUE)
    moves <- ps[keep, , drop = FALSE]
    if (!nrow(moves)) return(stand)
  }
  moves <- .g3_order(core, moves)
  for (i in seq_len(nrow(moves))) {
    child <- .g3_make(core, moves[i, ], tabs)
    s <- -.g3_qsearch(e, child, -beta, -alpha, ply + 1L, qd + 1L)
    if (e$stop) return(0)
    if (s > best) best <- s
    if (s > alpha) alpha <- s
    if (alpha >= beta) break
  }
  best
}

# Iterative-deepening root search. Returns the best action row, the
# completed depth, per-root-action scores of the last completed iteration
# with bound type, node count and elapsed time. With `exact_alternatives`,
# every root action is searched with a full window (exact scores).
#' @noRd
.g3_root_search <- function(core, tabs, budget, exact_alternatives = FALSE, restrict = NULL) {
  e <- .g3_search_env(budget, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- .g3_legal(core, tabs, ctx)
  ids <- .g3_action_ids(core, legal, tabs)
  if (!is.null(restrict)) {
    keep <- ids %in% restrict
    legal <- legal[keep, , drop = FALSE]
    ids <- ids[keep]
  }
  n <- nrow(legal)
  res <- list(legal = legal, ids = ids, depth = 0L, best = NA_integer_, scores = rep(NA_real_, n),
              bounds = rep("unknown", n), status = "terminal", nodes = 0, elapsed = 0, pv = character())
  if (!n) return(res)
  order0 <- .g3_order(core, legal)
  ord_ids <- .g3_action_ids(core, order0, tabs)
  children <- lapply(seq_len(n), function(i) .g3_make(core, order0[i, ], tabs))
  best_completed <- NA_integer_
  for (d in seq_len(budget$depth)) {
    alpha <- -Inf
    beta <- Inf
    sc <- rep(NA_real_, n)
    bd <- rep("unknown", n)
    best_i <- NA_integer_
    for (i in seq_len(n)) {
      if (exact_alternatives) {
        s <- -.g3_negamax(e, children[[i]], d - 1L, -Inf, Inf, 1L)
        b <- "exact"
      } else {
        s <- -.g3_negamax(e, children[[i]], d - 1L, -beta, -alpha, 1L)
        b <- if (is.na(best_i) || s > alpha) "exact" else "upper"
      }
      if (e$stop) break
      sc[i] <- s
      bd[i] <- b
      if (is.na(best_i) || s > sc[best_i]) best_i <- i
      if (s > alpha) alpha <- s
    }
    if (e$stop) break
    # Iteration complete: publish it and reorder so the best goes first.
    res$depth <- d
    best_completed <- best_i
    res$scores <- sc[match(ids, ord_ids)]
    res$bounds <- bd[match(ids, ord_ids)]
    res$best <- match(ord_ids[best_i], ids)
    o <- c(best_i, setdiff(order(-sc, seq_len(n)), best_i))
    order0 <- order0[o, , drop = FALSE]
    ord_ids <- ord_ids[o]
    children <- children[o]
    if (abs(sc[best_i]) > .G3_MATE_BOUND) break
  }
  res$nodes <- e$nodes
  res$elapsed <- (proc.time()[["elapsed"]] - e$t0) * 1000
  if (is.na(best_completed)) {
    res$status <- "fallback"
    res$best <- match(.g3_action_ids(core, .g3_order(core, legal)[1L, , drop = FALSE], tabs), ids)
  } else {
    res$status <- if (e$stop) "budget" else "completed"
    res$pv <- .g3_extract_pv(e, core, ids[res$best], tabs, res$depth)
  }
  res
}

# Principal variation: the best root action, then transposition-table best
# moves while they remain legal.
#' @noRd
.g3_extract_pv <- function(e, core, first_id, tabs, depth) {
  pv <- first_id
  cur <- core
  id <- first_id
  for (k in seq_len(max(depth, 1L) + 2L)) {
    legal <- .g3_legal(cur, tabs)
    i <- match(id, .g3_action_ids(cur, legal, tabs))
    if (is.na(i)) break
    cur <- .g3_make(cur, legal[i, ], tabs)
    ent <- e$tt[[.g3_tt_key(cur)]]
    if (is.null(ent) || k >= depth) break
    id <- ent$move
    pv <- c(pv, id)
  }
  pv
}
