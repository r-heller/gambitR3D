# Paired agent comparisons and leakage-aware data splits.
#
# Comparison design (docs/EXPERIMENT_PROTOCOL.md): every starting position
# is played in colour-swapped pairs (a role swap, never a geometric mirror),
# both agents get the same per-decision budget, and a ply cap ends a game as
# a `truncated` session event, never a draw. Every scheduled game is
# accounted for: completed, truncated, failed (an error or an illegal
# output, which is never silently replaced) or cancelled. The completed-game
# expected score (W + D/2) / completed is reported with scheduled-game
# bounds that score the unknown games as all losses and as all wins.
# Uncertainty comes from a seeded base-R percentile bootstrap over starting
# positions, each resampled together with its nested seeds and colour-swapped
# pairs, so plies and paired games are never treated as independent.

.G3_COMPARE_SCHEMA <- "g3-comparison/1"

#' Compare two agents in paired games
#'
#' Plays colour-swapped pairs of games between two agents from each
#' starting position and seed, with equal per-decision budgets and a ply
#' cap, and summarises outcomes, scores, costs and uncertainty.
#'
#' @param agents Two agents: a character vector such as `c("search",
#'   "random")` (names, if given, are used as labels) or a list of two
#'   agents accepted by [g3_choose_action()]. The first agent is the
#'   reference for scores.
#' @param positions Starting positions: `NULL` (the standard start), a
#'   `g3_state`, or a (named) list of `g3_state` objects. Finished positions
#'   are rejected.
#' @param budget Per-decision budget given identically to both agents (see
#'   [g3_choose_action()]).
#' @param seeds Integer seeds. Each seed replicates every position; seeds
#'   drive agents that sample (`"random"`) and are recorded.
#' @param pairs Colour-swapped pairs per position and seed.
#' @param max_plies Ply cap per game; reaching it records a `truncated`
#'   session event.
#' @param bootstrap List with `replicates` (default 2000), `seed` (default
#'   20261026) and `level` (default 0.95).
#' @param stop_check Optional function returning `TRUE` to stop; remaining
#'   games are reported as cancelled.
#' @param progress Optional function `function(done, total, unit)`.
#' @param keep_games Keep the `g3_game` records (for saving or analysis).
#' @return A `g3_comparison` list:
#'   * `games`: one row per scheduled game: `game_index`, `position_id`,
#'     `seed`, `pair`, `white_agent`, `black_agent`, `agent1_colour`,
#'     `status` (`completed`, `truncated`, `failed`, `cancelled`), `result`
#'     (`white`, `black`, `draw`, or `NA` when unknown), `termination`,
#'     `plies`, `agent1_score` (1, 0.5, 0 or `NA`), `illegal_actions`,
#'     `error`, `game_id`, `game_hash`.
#'   * `decisions`: one row per decision with `latency_ms` (wall time around
#'     the agent call), `engine_ms`, `nodes`, `depth` and `status`.
#'   * `pair_results`: one row per position, seed and pair.
#'   * `summary`: one row with scheduled/completed/truncated/failed/cancelled
#'     counts, wins/draws/losses of agent 1, `completed_score`,
#'     `scheduled_lower_bound`, `scheduled_upper_bound` and illegal counts.
#'   * `by_colour`: the same counts split by agent 1's colour.
#'   * `costs`: per agent decisions, latency median/p95/mean, nodes.
#'   * `uncertainty`: list with `method`, `unit`, `n_blocks`, `replicates`,
#'     `seed`, `level`, `intervals` (data frame) and `note`.
#'   * `provenance`: agents, budget, seeds, pairs, max_plies, positions,
#'     policies, rules hash, versions, hardware and timings.
#'   * `game_records`: list of `g3_game` (when `keep_games`).
#' @examples
#' # One colour-swapped pair from the start, capped at six plies.
#' cmp <- gambitR3D::g3_compare_agents(
#'   c("greedy", "random"), positions = NULL, budget = base::list(nodes = 50L),
#'   seeds = 1L, pairs = 1L, max_plies = 6L
#' )
#' cmp$games[, c("white_agent", "black_agent", "status", "agent1_score")]
#' cmp$summary[, c("scheduled", "completed", "truncated", "completed_score",
#'                 "scheduled_lower_bound", "scheduled_upper_bound")]
#' @export
g3_compare_agents <- function(agents, positions = NULL, budget = list(nodes = 2000L), seeds = 1L, pairs = 1L,
                              max_plies = 120L, bootstrap = list(), stop_check = NULL, progress = NULL,
                              keep_games = TRUE) {
  t0 <- proc.time()[["elapsed"]]
  ag <- .g3_compare_agent_specs(agents)
  b <- .g3_budget(budget)
  pos <- .g3_compare_positions(positions)
  seeds <- as.integer(seeds)
  pairs <- as.integer(pairs)
  max_plies <- as.integer(max_plies)
  if (!length(seeds) || anyNA(seeds)) .g3_abort("g3_invalid_state", "`seeds` must be non-missing integers.")
  if (length(pairs) != 1L || is.na(pairs) || pairs < 1L) .g3_abort("g3_invalid_state", "`pairs` must be a positive integer.")
  if (length(max_plies) != 1L || is.na(max_plies) || max_plies < 1L) .g3_abort("g3_invalid_state", "`max_plies` must be a positive integer.")
  boot <- utils::modifyList(list(replicates = 2000L, seed = 20261026L, level = 0.95), bootstrap)

  sched <- expand.grid(role = 1:2, pair = seq_len(pairs), seed = seeds, position = seq_along(pos$states),
                       KEEP.OUT.ATTRS = FALSE)
  sched <- sched[, c("position", "seed", "pair", "role")]
  n_games <- nrow(sched)
  game_rows <- vector("list", n_games)
  dec_rows <- list()
  records <- list()
  stopped <- FALSE
  for (gi in seq_len(n_games)) {
    s <- sched[gi, ]
    white <- ag[[if (s$role == 1L) 1L else 2L]]
    black <- ag[[if (s$role == 1L) 2L else 1L]]
    base <- list(game_index = gi, position_id = pos$ids[s$position], seed = s$seed, pair = s$pair,
                 white_agent = white$label, black_agent = black$label,
                 agent1_colour = if (s$role == 1L) "white" else "black")
    if (!stopped && !is.null(stop_check) && isTRUE(stop_check())) stopped <- TRUE
    if (stopped) {
      game_rows[[gi]] <- c(base, list(status = "cancelled", result = NA_character_, termination = NA_character_,
                                      plies = 0L, illegal_actions = 0L, error = "cancelled before start"))
      next
    }
    seed_base <- c(s$seed, s$position, s$pair, s$role)
    played <- .g3_compare_play(pos$states[[s$position]], white, black, b, max_plies, seed_base, stop_check)
    if (identical(played$status, "cancelled")) stopped <- TRUE
    game_rows[[gi]] <- c(base, played$row)
    if (length(played$decisions)) {
      dec_rows <- c(dec_rows, lapply(played$decisions, function(d) c(list(game_index = gi), d)))
    }
    if (keep_games) records[[gi]] <- played$game
    if (is.function(progress)) progress(gi, n_games, "games")
  }
  games <- .g3_an_rows_df(game_rows, .g3_compare_schema("games"))
  games$agent1_score <- .g3_compare_score(games)
  games <- games[, names(.g3_compare_schema("games")), drop = FALSE]
  decisions <- .g3_an_rows_df(dec_rows, .g3_compare_schema("decisions"))
  meta <- list(
    agents = lapply(ag, function(a) list(label = a$label, agent = if (is.character(a$agent)) a$agent else "model")),
    budget = b, seeds = seeds, pairs = pairs, max_plies = max_plies,
    positions = data.frame(position_id = pos$ids, position_key = vapply(pos$states, g3_position_key, ""),
                           state_hash = vapply(pos$states, g3_state_hash, ""), stringsAsFactors = FALSE),
    elapsed_ms = (proc.time()[["elapsed"]] - t0) * 1000
  )
  out <- .g3_compare_result(games, decisions, meta, boot, ag[[1L]]$label, ag[[2L]]$label)
  out$game_records <- if (keep_games) records else NULL
  out
}

#' @export
print.g3_comparison <- function(x, ...) {
  s <- x$summary
  cat("<g3_comparison> ", s$agent1, " vs ", s$agent2, ": ", s$scheduled, " scheduled games (", s$completed,
      " completed, ", s$truncated, " truncated, ", s$failed, " failed, ", s$cancelled, " cancelled)\n", sep = "")
  cat(s$agent1, " W/D/L ", s$wins, "/", s$draws, "/", s$losses, "; completed-game score ",
      format(round(s$completed_score, 3)), ", scheduled bounds [", format(round(s$scheduled_lower_bound, 3)), ", ",
      format(round(s$scheduled_upper_bound, 3)), "]\n", sep = "")
  iv <- x$uncertainty$intervals
  if (nrow(iv)) {
    cat(x$uncertainty$method, ", ", x$uncertainty$replicates, " replicates, seed ", x$uncertainty$seed, "\n", sep = "")
    print(iv, row.names = FALSE)
  }
  if (nzchar(x$uncertainty$note)) cat(x$uncertainty$note, "\n")
  invisible(x)
}

#' @noRd
.g3_compare_agent_specs <- function(agents) {
  if (is.character(agents)) {
    if (length(agents) != 2L || anyNA(agents)) .g3_abort("g3_invalid_state", "`agents` must name exactly two agents.")
    labels <- if (!is.null(names(agents)) && all(nzchar(names(agents)))) names(agents) else unname(agents)
    agents <- as.list(unname(agents))
  } else if (is.list(agents) && length(agents) == 2L) {
    labels <- if (!is.null(names(agents)) && all(nzchar(names(agents)))) names(agents) else
      vapply(agents, function(a) if (is.character(a)) a else paste0("model:", a$model_id %||% a$id %||% "model"), "")
  } else {
    .g3_abort("g3_invalid_state", "`agents` must be two agents, e.g. c('search', 'random').")
  }
  if (identical(labels[1L], labels[2L])) labels <- paste0(labels, c(" (A)", " (B)"))
  for (a in agents) {
    ok <- (is.character(a) && length(a) == 1L && (a %in% .G3_BUILTIN_AGENTS || grepl("^model", a))) || inherits(a, "g3_model")
    if (!ok) .g3_abort("g3_incompatible_model", paste0("Unknown agent: ", format(a)[1L], "."))
  }
  list(list(agent = agents[[1L]], label = labels[1L]), list(agent = agents[[2L]], label = labels[2L]))
}

#' @noRd
.g3_compare_positions <- function(positions) {
  if (is.null(positions)) positions <- list(start = g3_new_game()$state)
  if (inherits(positions, "g3_state")) positions <- list(positions)
  if (!is.list(positions) || !length(positions)) .g3_abort("g3_invalid_state", "`positions` must be g3_state objects.")
  positions <- lapply(positions, function(p) if (inherits(p, "g3_game")) p$state else p)
  for (p in positions) {
    .g3_check_state(p)
    if (identical(p$outcome$status, "terminal")) .g3_abort("g3_invalid_state", "A starting position is already finished.")
  }
  ids <- names(positions)
  if (is.null(ids) || any(!nzchar(ids))) ids <- sprintf("pos%02d", seq_along(positions))
  if (anyDuplicated(ids)) .g3_abort("g3_invalid_state", "Starting position names must be unique.")
  list(states = unname(positions), ids = ids)
}

# A reproducible per-decision seed from the game's coordinates and the ply.
#' @noRd
.g3_compare_seed <- function(parts) {
  strtoi(substr(.g3_sha256(paste(parts, collapse = ":")), 1L, 7L), 16L)
}

# Play one scheduled game. Illegal or failed outputs end the game as
# failed; nothing is substituted.
#' @noRd
.g3_compare_play <- function(state, white, black, budget, max_plies, seed_base, stop_check) {
  game <- .g3_game_new(state, players = list(white = white$label, black = black$label))
  decisions <- list()
  illegal <- 0L
  status <- NA_character_
  err <- NA_character_
  failed_agent <- NA_character_
  if (!is.null(stop_check)) {
    op <- options(gambitR3D.stop_check = stop_check)
    on.exit(options(op), add = TRUE)
  }
  for (k in seq_len(max_plies)) {
    if (identical(game$state$outcome$status, "terminal")) break
    if (!is.null(stop_check) && isTRUE(stop_check())) {
      status <- "cancelled"
      err <- "cancelled during play"
      break
    }
    side <- game$state$side
    a <- if (side == "white") white else black
    seed <- .g3_compare_seed(c(seed_base, game$state$ply))
    t1 <- proc.time()[["elapsed"]]
    d <- tryCatch(g3_choose_action(game$state, a$agent, budget, seed = seed), error = function(e) e)
    lat <- (proc.time()[["elapsed"]] - t1) * 1000
    if (inherits(d, "error")) {
      status <- "failed"
      failed_agent <- a$label
      err <- paste0(a$label, ": ", conditionMessage(d))
      break
    }
    decisions[[length(decisions) + 1L]] <- list(
      ply = as.integer(game$state$ply + 1L), side = side, agent = a$label, action_id = d$action_id,
      latency_ms = lat, engine_ms = as.numeric(d$elapsed_ms %||% NA_real_), nodes = as.numeric(d$nodes %||% NA_real_),
      depth = as.integer(d$depth %||% NA_integer_), status = d$status
    )
    if (is.na(d$action_id)) {
      status <- "failed"
      failed_agent <- a$label
      err <- paste0(a$label, ": no action in an ongoing position")
      break
    }
    step <- tryCatch(g3_game_apply(game, d$action_id), g3_illegal_action = function(e) e)
    if (inherits(step, "error")) {
      illegal <- illegal + 1L
      status <- "failed"
      failed_agent <- a$label
      err <- paste0(a$label, ": illegal action ", d$action_id)
      break
    }
    game <- step
  }
  if (is.na(status)) {
    if (identical(game$state$outcome$status, "terminal")) {
      status <- "completed"
    } else {
      game <- g3_game_event(game, "truncated", reason = "max_plies")
      status <- "truncated"
    }
  }
  res <- if (status == "completed") game$state$outcome$result else NA_character_
  term <- if (status == "completed") game$state$outcome$reason else if (status == "truncated") "truncated" else NA_character_
  list(
    row = list(status = status, result = res, termination = term,
               plies = as.integer(game$state$ply - state$ply), illegal_actions = illegal,
               failed_agent = failed_agent, error = err,
               game_id = game$metadata$game_id, game_hash = g3_game_hash(game)),
    decisions = decisions, game = game
  )
}

#' @noRd
.g3_compare_schema <- function(which) {
  switch(which,
    games = c(game_index = "integer", position_id = "character", seed = "integer", pair = "integer",
              white_agent = "character", black_agent = "character", agent1_colour = "character",
              status = "character", result = "character", termination = "character", plies = "integer",
              agent1_score = "double", illegal_actions = "integer", failed_agent = "character", error = "character",
              game_id = "character", game_hash = "character"),
    decisions = c(game_index = "integer", ply = "integer", side = "character", agent = "character",
                  action_id = "character", latency_ms = "double", engine_ms = "double", nodes = "double",
                  depth = "integer", status = "character")
  )
}

#' @noRd
.g3_compare_score <- function(games) {
  ifelse(games$status != "completed", NA_real_,
         ifelse(games$result == "draw", 0.5, ifelse(games$result == games$agent1_colour, 1, 0)))
}

# Summary, costs and uncertainty from the games and decisions tables.
#' @noRd
.g3_compare_result <- function(games, decisions, meta, boot, label1, label2) {
  acc <- function(g) {
    comp <- g$status == "completed"
    sc <- g$agent1_score[comp]
    pts <- sum(sc)
    n <- nrow(g)
    unknown <- sum(!comp)
    data.frame(
      scheduled = n, completed = sum(comp), truncated = sum(g$status == "truncated"),
      failed = sum(g$status == "failed"), cancelled = sum(g$status == "cancelled"),
      wins = sum(sc == 1), draws = sum(sc == 0.5), losses = sum(sc == 0),
      completed_score = if (sum(comp)) pts / sum(comp) else NA_real_,
      scheduled_lower_bound = if (n) pts / n else NA_real_,
      scheduled_upper_bound = if (n) (pts + unknown) / n else NA_real_,
      illegal_actions = sum(g$illegal_actions), stringsAsFactors = FALSE
    )
  }
  summary <- cbind(data.frame(agent1 = label1, agent2 = label2, stringsAsFactors = FALSE), acc(games))
  summary$illegal_actions_agent1 <- sum(games$illegal_actions[!is.na(games$failed_agent) & games$failed_agent == label1])
  summary$illegal_actions_agent2 <- sum(games$illegal_actions[!is.na(games$failed_agent) & games$failed_agent == label2])
  by_colour <- do.call(rbind, lapply(c("white", "black"), function(col) {
    cbind(data.frame(agent1_colour = col, stringsAsFactors = FALSE), acc(games[games$agent1_colour == col, , drop = FALSE]))
  }))
  pr_key <- paste(games$position_id, games$seed, games$pair, sep = "|")
  u <- unique(pr_key)
  pair_results <- do.call(rbind, lapply(u, function(k) {
    g <- games[pr_key == k, , drop = FALSE]
    a <- acc(g)
    data.frame(position_id = g$position_id[1L], seed = g$seed[1L], pair = g$pair[1L], games = nrow(g),
               completed = a$completed, truncated = a$truncated, failed = a$failed, cancelled = a$cancelled,
               agent1_points = sum(g$agent1_score, na.rm = TRUE), agent1_completed_score = a$completed_score,
               agent1_lower_bound = a$scheduled_lower_bound, agent1_upper_bound = a$scheduled_upper_bound,
               stringsAsFactors = FALSE)
  }))
  if (is.null(pair_results)) pair_results <- data.frame()
  costs <- do.call(rbind, lapply(c(label1, label2), function(lb) {
    d <- decisions[decisions$agent == lb, , drop = FALSE]
    lat <- d$latency_ms
    data.frame(
      agent = lb, decisions = nrow(d),
      latency_median_ms = if (nrow(d)) stats::median(lat) else NA_real_,
      latency_p95_ms = if (nrow(d)) stats::quantile(lat, 0.95, names = FALSE, type = 7) else NA_real_,
      latency_mean_ms = if (nrow(d)) mean(lat) else NA_real_,
      nodes_total = if (nrow(d)) sum(d$nodes, na.rm = TRUE) else 0,
      nodes_median = if (nrow(d)) stats::median(d$nodes, na.rm = TRUE) else NA_real_,
      nodes_per_second = if (nrow(d) && sum(d$engine_ms, na.rm = TRUE) > 0) sum(d$nodes, na.rm = TRUE) / (sum(d$engine_ms, na.rm = TRUE) / 1000) else NA_real_,
      fallback_decisions = sum(d$status == "fallback"), stringsAsFactors = FALSE
    )
  }))
  unc <- .g3_compare_bootstrap(games, boot$replicates, boot$seed, boot$level)
  prov <- c(meta, list(
    schema = .G3_COMPARE_SCHEMA,
    budget_semantics = "per decision, identical for both agents",
    colour_policy = "role swap: each pair plays the same starting position twice with the agents' colours exchanged; no geometric mirroring",
    truncation_policy = "reaching max_plies records a 'truncated' session event; truncated games have no result and are never draws",
    illegal_policy = "an illegal or missing action ends the game as failed and is counted; it is never replaced by another action",
    score_definition = "completed_score = (wins + 0.5 * draws) / completed games, conditional on completion; scheduled bounds score unknown games 0 and 1",
    rating = "no relative rating is fitted",
    rules_hash = .g3_tables()$rules_hash,
    search_version = .G3_SEARCH_VERSION, eval_version = .G3_EVAL_VERSION,
    package_version = as.character(utils::packageVersion("gambitR3D")),
    r_version = R.version.string,
    hardware = list(sysname = Sys.info()[["sysname"]], release = Sys.info()[["release"]],
                    machine = Sys.info()[["machine"]]),
    created = .g3_now()
  ))
  structure(
    list(games = games, decisions = decisions, pair_results = pair_results, summary = summary,
         by_colour = by_colour, costs = costs, uncertainty = unc, provenance = prov),
    class = "g3_comparison"
  )
}

# Percentile bootstrap over starting positions (blocks). Each block keeps
# its nested seeds and colour-swapped pairs together.
#' @noRd
.g3_compare_bootstrap <- function(games, replicates, seed, level) {
  method <- "paired-block percentile bootstrap (base R): starting positions resampled with replacement, each with its nested seeds and colour-swapped pairs"
  empty <- data.frame(statistic = character(), estimate = double(), lower = double(), upper = double(),
                      stringsAsFactors = FALSE)
  blocks <- unique(games$position_id)
  nb <- length(blocks)
  comp <- games$status == "completed"
  agg <- data.frame(
    points = vapply(blocks, function(bk) sum(games$agent1_score[games$position_id == bk & comp]), 0),
    completed = vapply(blocks, function(bk) sum(games$position_id == bk & comp), 0),
    scheduled = vapply(blocks, function(bk) sum(games$position_id == bk), 0)
  )
  agg$unknown <- agg$scheduled - agg$completed
  est <- function(i) {
    p <- sum(agg$points[i])
    c(completed_score = if (sum(agg$completed[i]) > 0) p / sum(agg$completed[i]) else NA_real_,
      scheduled_lower_bound = p / sum(agg$scheduled[i]),
      scheduled_upper_bound = (p + sum(agg$unknown[i])) / sum(agg$scheduled[i]))
  }
  out <- list(method = method, unit = "starting position (nested seeds and colour-swapped pairs kept together)",
              n_blocks = nb, replicates = as.integer(replicates), seed = as.integer(seed), level = level,
              intervals = empty, n_undefined_replicates = 0L, note = "")
  if (!nrow(games) || nb == 0L) {
    out$note <- "No games: no interval."
    return(out)
  }
  point <- est(seq_len(nb))
  if (nb < 2L) {
    out$intervals <- data.frame(statistic = names(point), estimate = unname(point), lower = NA_real_,
                                upper = NA_real_, stringsAsFactors = FALSE)
    out$note <- "Fewer than two independent starting positions: descriptive only, no interval."
    return(out)
  }
  reps <- .g3_with_seed(seed, vapply(seq_len(replicates), function(r) est(sample.int(nb, nb, replace = TRUE)), numeric(3L)))
  alpha <- (1 - level) / 2
  q <- function(x) stats::quantile(x, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE, type = 7)
  ci <- t(apply(reps, 1L, q))
  out$intervals <- data.frame(statistic = names(point), estimate = unname(point), lower = ci[, 1L], upper = ci[, 2L],
                              stringsAsFactors = FALSE)
  out$n_undefined_replicates <- sum(is.na(reps[1L, ]))
  if (nb < 5L) out$note <- paste0("Only ", nb, " starting positions: the interval is unstable and descriptive.")
  out
}

# Combine comparisons of the same agents and budget (for example run in
# parallel per starting position) and recompute summaries and intervals.
#' @noRd
.g3_compare_combine <- function(parts, bootstrap = list()) {
  boot <- utils::modifyList(list(replicates = 2000L, seed = 20261026L, level = 0.95), bootstrap)
  off <- 0L
  games <- list()
  decs <- list()
  recs <- list()
  pos <- list()
  for (p in parts) {
    g <- p$games
    d <- p$decisions
    g$game_index <- g$game_index + off
    d$game_index <- d$game_index + off
    games[[length(games) + 1L]] <- g
    decs[[length(decs) + 1L]] <- d
    recs <- c(recs, p$game_records)
    pos[[length(pos) + 1L]] <- p$provenance$positions
    off <- off + nrow(p$games)
  }
  first <- parts[[1L]]
  meta <- first$provenance[c("agents", "budget", "seeds", "pairs", "max_plies")]
  meta$positions <- do.call(rbind, pos)
  meta$elapsed_ms <- sum(vapply(parts, function(p) p$provenance$elapsed_ms, 0))
  out <- .g3_compare_result(do.call(rbind, games), do.call(rbind, decs), meta, boot,
                               first$summary$agent1, first$summary$agent2)
  out$game_records <- recs
  out
}

# Starting positions reached by seeded uniformly random legal actions from
# the standard start (a documented way to obtain varied, held-out starts).
#' @noRd
.g3_compare_openings <- function(n, plies, seed) {
  out <- list()
  k <- 0L
  attempt <- 0L
  while (length(out) < n && attempt < 50L * n) {
    attempt <- attempt + 1L
    st <- g3_new_game()$state
    ok <- TRUE
    for (i in seq_len(plies)) {
      acts <- g3_legal_actions(st)
      if (!nrow(acts)) {
        ok <- FALSE
        break
      }
      j <- .g3_with_seed(.g3_compare_seed(c(seed, attempt, i)), sample.int(nrow(acts), 1L))
      st <- g3_apply_action(st, acts$action_id[j])$state
      if (identical(st$outcome$status, "terminal")) {
        ok <- FALSE
        break
      }
    }
    key <- g3_position_key(st)
    if (ok && !key %in% vapply(out, g3_position_key, "")) {
      k <- k + 1L
      out[[sprintf("open%02d", k)]] <- st
    }
  }
  out
}

# Background-job type "match": a paired agent comparison.
#' @noRd
.g3_job_match <- function(args, ctx) {
  out <- g3_compare_agents(
    args$agents, positions = args$positions, budget = args$budget %||% list(nodes = 2000L),
    seeds = args$seeds %||% 1L, pairs = args$pairs %||% 1L, max_plies = args$max_plies %||% 120L,
    bootstrap = args$bootstrap %||% list(), stop_check = ctx$should_stop,
    progress = function(done, total, unit) ctx$progress(done, total, unit),
    keep_games = isTRUE(args$keep_games %||% TRUE)
  )
  if (any(out$games$status == "cancelled")) attr(out, "stop_reason") <- if (ctx$timed_out()) "timed_out" else "cancelled"
  out
}

# ---- splits -------------------------------------------------------------------

#' Split games into partitions without leakage
#'
#' Assigns complete games, grouped by a provenance group (for example a
#' source, player or tournament), to partitions such as training and
#' evaluation, then audits identical position keys across partitions. Plies
#' of one game never end up in different partitions, and a whole group stays
#' together. Overlap is reported, not silently removed: the audit separates
#' shared starting positions (expected when games share a start) from
#' positions reached in play.
#'
#' @param games A list of `g3_game` objects (or a `g3_database_analysis`, or
#'   a list of `g3_analysis`, whose position keys are used).
#' @param proportions Named numeric vector of partition shares, for example
#'   `c(train = 0.8, test = 0.2)`; normalised to sum to one.
#' @param group Grouping: `NULL` (each game is its own group), a character
#'   vector with one group per game, or the name of a metadata field of the
#'   games (for example `"source"`).
#' @param seed Integer seed for the group shuffle (the caller's random
#'   stream is restored).
#' @return A `g3_split` list: `assignments` (`game_id`, `group`,
#'   `partition`), `sizes` (`partition`, `n_games`, `n_groups`,
#'   `n_position_keys`), `audit` (one row per partition pair: distinct keys
#'   in each, `shared_keys`, `shared_start_keys`, `shared_play_keys`,
#'   `shared_history_contexts`, `share_of_b`), `shared_keys` (the shared
#'   keys with their partitions), `policy` and `seed`.
#' @examples
#' # Three one-action games split by game; the shared start is audited.
#' g <- gambitR3D::g3_new_game()
#' games <- base::lapply(c("m:W:0:1-W:0:2", "m:W:3:1-W:3:2", "m:W:1:1-W:1:2"), function(a) {
#'   gambitR3D::g3_game_apply(g, a)
#' })
#' games <- base::lapply(seq_along(games), function(i) {
#'   games[[i]]$metadata$game_id <- base::paste0("g", i)
#'   games[[i]]
#' })
#' sp <- gambitR3D::g3_split_games(games, c(train = 2 / 3, test = 1 / 3), seed = 1L)
#' sp$assignments
#' sp$audit
#' @export
g3_split_games <- function(games, proportions = c(train = 0.8, test = 0.2), group = NULL, seed = 20261026L) {
  keysets <- .g3_split_keys(games)
  ids <- keysets$ids
  n <- length(ids)
  if (!n) .g3_abort("g3_invalid_state", "No games to split.")
  if (is.null(names(proportions)) || any(!nzchar(names(proportions))) || any(proportions < 0) || sum(proportions) <= 0) {
    .g3_abort("g3_invalid_state", "`proportions` must be a named non-negative vector, e.g. c(train = 0.8, test = 0.2).")
  }
  prop <- proportions / sum(proportions)
  meta_field <- is.character(group) && length(group) == 1L && !is.null(keysets$meta) &&
    any(vapply(keysets$meta, function(m) !is.null(m[[group]]), TRUE))
  grp <- if (is.null(group)) {
    ids
  } else if (meta_field) {
    vapply(keysets$meta, function(m) as.character(m[[group]] %||% NA_character_)[1L], "")
  } else if (length(group) == n) {
    as.character(group)
  } else {
    .g3_abort("g3_invalid_state", "`group` must be NULL, one group per game, or a metadata field name.")
  }
  if (anyNA(grp)) .g3_abort("g3_invalid_state", "Every game needs a group (missing group values found).")
  ug <- unique(grp)
  order_g <- .g3_with_seed(seed, sample(ug))
  size <- table(grp)[order_g]
  target <- prop * n
  part_n <- stats::setNames(numeric(length(prop)), names(prop))
  assign_g <- stats::setNames(character(length(ug)), order_g)
  for (gname in order_g) {
    deficit <- target - part_n
    p <- names(prop)[which.max(deficit)]
    assign_g[[gname]] <- p
    part_n[[p]] <- part_n[[p]] + size[[gname]]
  }
  part <- unname(assign_g[grp])
  assignments <- data.frame(game_id = ids, group = grp, partition = part, stringsAsFactors = FALSE)
  pk <- keysets$position
  hk <- keysets$history
  starts <- keysets$start
  sizes <- do.call(rbind, lapply(names(prop), function(p) {
    sel <- part == p
    data.frame(partition = p, n_games = sum(sel), n_groups = length(unique(grp[sel])),
               n_position_keys = length(unique(unlist(pk[sel]))), stringsAsFactors = FALSE)
  }))
  pn <- names(prop)
  audit <- list()
  shared_rows <- list()
  if (length(pn) >= 2L) {
    for (i in seq_len(length(pn) - 1L)) {
      for (j in (i + 1L):length(pn)) {
        a <- part == pn[i]
        b <- part == pn[j]
        ka <- unique(unlist(pk[a]))
        kb <- unique(unlist(pk[b]))
        sh <- intersect(ka, kb)
        st <- intersect(sh, unique(c(unlist(starts[a]), unlist(starts[b]))))
        ha <- unique(unlist(hk[a]))
        hb <- unique(unlist(hk[b]))
        audit[[length(audit) + 1L]] <- data.frame(
          partition_a = pn[i], partition_b = pn[j], keys_a = length(ka), keys_b = length(kb),
          shared_keys = length(sh), shared_start_keys = length(st), shared_play_keys = length(setdiff(sh, st)),
          shared_history_contexts = if (is.null(hk)) NA_integer_ else length(intersect(ha, hb)),
          share_of_b = if (length(kb)) length(sh) / length(kb) else NA_real_, stringsAsFactors = FALSE
        )
        if (length(sh)) {
          shared_rows[[length(shared_rows) + 1L]] <- data.frame(
            position_key = sh, partition_a = pn[i], partition_b = pn[j], start_position = sh %in% st,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  audit <- if (length(audit)) do.call(rbind, audit) else
    data.frame(partition_a = character(), partition_b = character(), keys_a = integer(), keys_b = integer(),
               shared_keys = integer(), shared_start_keys = integer(), shared_play_keys = integer(),
               shared_history_contexts = integer(), share_of_b = double(), stringsAsFactors = FALSE)
  shared <- if (length(shared_rows)) do.call(rbind, shared_rows) else
    data.frame(position_key = character(), partition_a = character(), partition_b = character(),
               start_position = logical(), stringsAsFactors = FALSE)
  structure(
    list(
      assignments = assignments, sizes = sizes, audit = audit, shared_keys = shared,
      policy = paste(
        "Complete games are assigned by group; a group never spans partitions.",
        "Position keys (board-only repetition identity) and history contexts (key plus repetition record and",
        "halfmove counter) shared across partitions are reported, not removed. Shared starting positions are",
        "expected when games share a start and are listed separately; exclude them from novel-position claims.",
        "Filter shared play keys before training if the evaluation must be position-disjoint."
      ),
      seed = as.integer(seed), proportions = prop
    ),
    class = "g3_split"
  )
}

# Position keys (and history contexts) per game, by one replay per game.
#' @noRd
.g3_split_keys <- function(games) {
  if (inherits(games, "g3_database_analysis")) games <- games$results
  if (inherits(games, "g3_game")) games <- list(games)
  if (!is.list(games) || !length(games)) return(list(ids = character()))
  if (all(vapply(games, inherits, TRUE, "g3_analysis"))) {
    return(list(
      ids = vapply(games, function(a) a$game$game_id, "", USE.NAMES = FALSE),
      position = lapply(games, function(a) a$positions$position_key),
      start = lapply(games, function(a) a$positions$position_key[1L]),
      history = NULL, meta = NULL
    ))
  }
  if (!all(vapply(games, inherits, TRUE, "g3_game"))) {
    .g3_abort("g3_invalid_state", "`games` must be g3_game objects, g3_analysis objects or a g3_database_analysis.")
  }
  tabs <- .g3_tables()
  ids <- vapply(games, function(g) as.character(g$metadata$game_id %||% NA_character_), "", USE.NAMES = FALSE)
  if (anyNA(ids) || anyDuplicated(ids)) .g3_abort("g3_invalid_state", "Games need unique, non-missing game ids.")
  pos <- vector("list", length(games))
  hist <- vector("list", length(games))
  for (i in seq_along(games)) {
    g <- games[[i]]
    core <- .g3_core(g$initial_state, tabs)
    line <- .g3_active_line(g$events)
    keys <- core$rep[length(core$rep)]
    hk <- .g3_sha256(paste(c(core$rep, core$halfmove), collapse = "|"))
    for (e in line) {
      legal <- .g3_legal(core, tabs)
      j <- match(g$events$action_id[e], .g3_action_ids(core, legal, tabs))
      if (is.na(j)) .g3_abort("g3_illegal_action", paste0("Game ", ids[i], ": illegal action in its log."), game_id = ids[i])
      core <- .g3_make(core, legal[j, ], tabs)
      keys <- c(keys, core$rep[length(core$rep)])
      hk <- c(hk, .g3_sha256(paste(c(core$rep, core$halfmove), collapse = "|")))
    }
    pos[[i]] <- keys
    hist[[i]] <- hk
  }
  list(ids = ids, position = pos, start = lapply(pos, `[`, 1L), history = hist,
       meta = lapply(games, `[[`, "metadata"))
}
