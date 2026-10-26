# Monte Carlo tree search owned by R (PUCT).
#
# The tree, every transition and every terminal decision belong to the R
# engine; an evaluator only supplies priors over a node's canonical legal
# actions and a value from the side to act's perspective. Selection uses
#
#   score(a) = Q(s, a) + c_puct * P(s, a) * sqrt(sum_b N(s, b)) / (1 + N(s, a))
#
# with Q from the selecting player's perspective (unvisited edges use the
# first-play value `fpu`, default 0). Exact ties go to the higher prior,
# then to the earlier canonical action. A leaf value is from the leaf's
# side to act; it is negated exactly once per edge while backing up,
# because every action passes the turn. Terminal leaves (checkmate,
# stalemate, rule draws) are valued by R: +1/0/-1 for the side to act,
# never by the network.
#
# Nodes are path specific: a child belongs to one parent edge and keeps
# its own repetition record, so positions reached through different
# histories are never merged. Dirichlet noise at the root and sampling
# from visit counts happen only in training mode, under an explicit seed;
# evaluation mode is deterministic. If a hard stop leaves the root
# unvisited, the documented fallback is the legal prior argmax and the
# policy target is marked unavailable (visits are never divided by zero).
# A terminal root returns no action.

.G3_MCTS_VERSION <- "g3-mcts/1"

#' @noRd
.g3_mcts_settings <- function(settings = list()) {
  s <- list(simulations = 32L, c_puct = 1.5, time_ms = Inf, training = FALSE, dirichlet_alpha = 0.3,
            dirichlet_epsilon = 0.25, temperature = 1, fpu = 0, seed = NULL)
  unknown <- setdiff(names(settings), names(s))
  if (length(unknown)) .g3_abort("g3_budget_exceeded", paste0("Unknown MCTS setting: ", unknown[1L]))
  s[names(settings)] <- settings
  s$simulations <- as.integer(s$simulations)
  if (is.na(s$simulations) || s$simulations < 0L) .g3_abort("g3_budget_exceeded", "simulations must be >= 0.")
  if (!is.numeric(s$c_puct) || s$c_puct < 0) .g3_abort("g3_budget_exceeded", "c_puct must be >= 0.")
  s$training <- isTRUE(s$training)
  if (!s$training) {
    # Evaluation is deterministic: no noise, no sampling.
    s$dirichlet_epsilon <- 0
    s$temperature <- 0
  }
  s
}

# A tree node for one core position.
#' @noRd
.g3_mcts_node <- function(core, tabs) {
  n <- new.env(parent = emptyenv())
  n$core <- core
  n$ctx <- .g3_ctx(core, tabs)
  n$legal <- .g3_legal(core, tabs, n$ctx)
  n$ids <- .g3_action_ids(core, n$legal, tabs)
  oc <- .g3_outcome_core(core, tabs, n$ctx, n$legal)
  n$outcome <- oc
  n$terminal <- identical(oc$status, "terminal")
  n$terminal_value <- if (!n$terminal) NA_real_ else if (identical(oc$result, "draw")) 0 else
    if (identical(oc$result, .G3_COLORS[core$side])) 1 else -1
  k <- length(n$ids)
  n$expanded <- FALSE
  n$P <- NULL
  n$value <- NA_real_
  n$N <- integer(k)
  n$W <- numeric(k)
  n$children <- vector("list", k)
  n
}

#' @noRd
.g3_mcts_expand <- function(node, ev) {
  p <- as.numeric(ev$prior)
  if (length(p) != length(node$ids) || any(!is.finite(p)) || any(p < 0) || sum(p) <= 0) {
    .g3_abort("g3_incompatible_model", "The evaluator returned invalid priors for the legal actions.")
  }
  v <- as.numeric(ev$value)
  if (length(v) != 1L || !is.finite(v) || abs(v) > 1 + 1e-9) {
    .g3_abort("g3_incompatible_model", "The evaluator returned an invalid value (expected one number in [-1, 1]).")
  }
  node$P <- p / sum(p)
  node$value <- v
  node$expanded <- TRUE
  invisible(node)
}

#' @noRd
.g3_mcts_select <- function(node, s) {
  N <- node$N
  q <- ifelse(N > 0L, node$W / pmax(N, 1L), s$fpu)
  u <- s$c_puct * node$P * sqrt(sum(N)) / (1 + N)
  score <- q + u
  order(-score, -node$P, seq_along(score))[1L]
}

# One simulation: select to a leaf, value it, back up.
#' @noRd
.g3_mcts_simulate <- function(root, evaluator, s, tabs, stats) {
  node <- root
  path <- list()
  acts <- integer()
  repeat {
    if (node$terminal) {
      v <- node$terminal_value
      stats$terminal_leaves <- stats$terminal_leaves + 1L
      break
    }
    if (!node$expanded) {
      ev <- evaluator(list(node))[[1L]]
      stats$evaluations <- stats$evaluations + 1L
      .g3_mcts_expand(node, ev)
      v <- node$value
      break
    }
    a <- .g3_mcts_select(node, s)
    path[[length(path) + 1L]] <- node
    acts <- c(acts, a)
    child <- node$children[[a]]
    if (is.null(child)) {
      child <- .g3_mcts_node(.g3_make(node$core, node$legal[a, ], tabs), tabs)
      node$children[[a]] <- child
      stats$nodes <- stats$nodes + 1L
    }
    node <- child
  }
  stats$max_depth <- max(stats$max_depth, length(path))
  # v is from the leaf's side to act; the edge into it belongs to the other
  # player, so negate once per edge on the way up.
  for (j in rev(seq_along(path))) {
    v <- -v
    p <- path[[j]]
    a <- acts[j]
    p$N[a] <- p$N[a] + 1L
    p$W[a] <- p$W[a] + v
  }
  invisible(NULL)
}

# Run a search from a core position. `evaluator(nodes)` receives a list of
# node environments (fields core, ctx, legal, ids) and returns, per node,
# list(prior = <numeric aligned with ids>, value = <side-to-act value>).
#' @noRd
.g3_mcts <- function(core, evaluator, settings = list(), tabs = .g3_tables(), should_stop = NULL) {
  s <- .g3_mcts_settings(settings)
  t0 <- proc.time()[["elapsed"]]
  root <- .g3_mcts_node(core, tabs)
  stats <- new.env(parent = emptyenv())
  stats$evaluations <- 0L
  stats$terminal_leaves <- 0L
  stats$nodes <- 1L
  stats$max_depth <- 0L
  empty_root <- data.frame(action_id = character(), prior = numeric(), prior_searched = numeric(),
                           visits = integer(), q = numeric(), policy_target = numeric(), stringsAsFactors = FALSE)
  out <- list(
    action_id = NA_character_, index = NA_integer_, status = "terminal", simulations = 0L,
    root = empty_root, root_value = root$terminal_value, root_outcome = root$outcome,
    policy_target_available = FALSE, stop_reason = "terminal", settings = s,
    version = .G3_MCTS_VERSION, elapsed_ms = 0, stats = list()
  )
  if (root$terminal) return(out)
  run <- function() {
    ev <- evaluator(list(root))[[1L]]
    stats$evaluations <- stats$evaluations + 1L
    .g3_mcts_expand(root, ev)
    prior_raw <- root$P
    noise_applied <- FALSE
    if (s$training && s$dirichlet_epsilon > 0 && length(root$P) > 1L) {
      g <- stats::rgamma(length(root$P), shape = s$dirichlet_alpha)
      if (sum(g) > 0) {
        root$P <- (1 - s$dirichlet_epsilon) * root$P + s$dirichlet_epsilon * g / sum(g)
        noise_applied <- TRUE
      }
    }
    done <- 0L
    stop_reason <- "simulations"
    while (done < s$simulations) {
      if (is.finite(s$time_ms) && (proc.time()[["elapsed"]] - t0) * 1000 >= s$time_ms) {
        stop_reason <- "time"
        break
      }
      if (is.function(should_stop) && isTRUE(should_stop())) {
        stop_reason <- "stopped"
        break
      }
      .g3_mcts_simulate(root, evaluator, s, tabs, stats)
      done <- done + 1L
    }
    visits <- root$N
    total <- sum(visits)
    if (total == 0L) {
      # Zero-visit fallback: legal prior argmax; no policy target.
      i <- order(-prior_raw, seq_along(prior_raw))[1L]
      status <- "fallback"
      target <- rep(NA_real_, length(visits))
    } else {
      target <- visits / total
      if (s$training && s$temperature > 0) {
        w <- visits^(1 / s$temperature)
        i <- if (sum(w) > 0) sample.int(length(w), 1L, prob = w) else which.max(visits)
      } else {
        i <- order(-visits, -prior_raw, seq_along(visits))[1L]
      }
      status <- if (stop_reason == "simulations") "completed" else "budget"
    }
    list(i = i, status = status, target = target, prior_raw = prior_raw, done = done,
         stop_reason = stop_reason, noise_applied = noise_applied)
  }
  r <- if (s$training) .g3_with_seed(s$seed, run()) else run()
  visits <- root$N
  out$action_id <- root$ids[r$i]
  out$index <- r$i
  out$status <- r$status
  out$simulations <- r$done
  out$root <- data.frame(
    action_id = root$ids, prior = r$prior_raw, prior_searched = root$P, visits = visits,
    q = ifelse(visits > 0L, root$W / pmax(visits, 1L), NA_real_), policy_target = r$target,
    stringsAsFactors = FALSE
  )
  out$root_value <- root$value
  # Backed-up value of the root for the side to act (visit-weighted Q).
  out$search_value <- if (sum(visits) > 0L) sum(root$W) / sum(visits) else NA_real_
  out$policy_target_available <- sum(visits) > 0L
  out$stop_reason <- r$stop_reason
  out$noise_applied <- r$noise_applied
  out$temperature <- if (s$training) s$temperature else 0
  out$elapsed_ms <- (proc.time()[["elapsed"]] - t0) * 1000
  out$stats <- list(evaluations = stats$evaluations, terminal_leaves = stats$terminal_leaves,
                    nodes = stats$nodes, max_depth = stats$max_depth)
  out
}

# Evaluators -----------------------------------------------------------------

# Uniform priors with a caller-supplied value function (pure R; used for
# tests and as a model-free baseline).
#' @noRd
.g3_mcts_uniform_evaluator <- function(value_fn = function(node) 0) {
  function(nodes) {
    lapply(nodes, function(n) list(prior = rep(1 / length(n$ids), length(n$ids)), value = value_fn(n)))
  }
}

# Network evaluator through the worker: R encodes the node's own legal
# actions, the worker scores them, R validates and normalises.
#' @noRd
.g3_mcts_model_evaluator <- function(w, model_hash, tabs = .g3_tables(), counter = NULL) {
  function(nodes) {
    encs <- lapply(nodes, function(n) .g3_encode_core(n$core, tabs, n$ctx, n$legal))
    outs <- .g3_ml_infer(w, encs, model_hash = model_hash)
    if (!is.null(counter)) counter$calls <- counter$calls + 1L
    lapply(seq_along(nodes), function(i) {
      o <- outs[[i]]
      if (!identical(o$action_ids, nodes[[i]]$ids)) {
        .g3_abort("g3_incompatible_model", "Network outputs are not aligned with the node's legal actions.")
      }
      list(prior = o$probability, value = o$value, wdl = o$wdl)
    })
  }
}
