# Phase 12: R-owned MCTS semantics with pure-R stub evaluators.

mcts_net <- c("wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3")

mates_after <- function(st, ids) {
  vapply(ids, function(id) identical(g3_apply_action(st, id)$state$outcome$reason, "checkmate"), TRUE)
}

test_that("mate in one: terminal leaves are valued by the rules and found", {
  tabs <- .g3_tables()
  st <- g3_setup_position(pcs(mcts_net), side = "black")
  r <- .g3_mcts(.g3_core(st, tabs), .g3_mcts_uniform_evaluator(), list(simulations = 150L))
  expect_identical(r$status, "completed")
  expect_true(mates_after(st, r$action_id))
  expect_equal(r$root$q[r$root$action_id == r$action_id], 1)
  # Every visited mating edge has Q = +1 for Black, the selecting player.
  mating <- r$root$visits > 0L & mates_after(st, r$root$action_id)
  expect_true(all(r$root$q[mating] == 1))
  expect_gt(r$search_value, 0)
  expect_equal(sum(r$root$policy_target), 1)
})

test_that("a forced loss gives negative values to every root action", {
  tabs <- .g3_tables()
  st <- g3_setup_position(pcs(mcts_net), side = "white")
  expect_identical(nrow(g3_legal_actions(st)), 3L)
  r <- .g3_mcts(.g3_core(st, tabs), .g3_mcts_uniform_evaluator(), list(simulations = 90L))
  expect_true(all(r$root$visits > 0L))
  expect_true(all(r$root$q < 0))
  expect_lt(r$search_value, 0)
})

test_that("the leaf value is negated exactly once per edge", {
  tabs <- .g3_tables()
  st <- g3_new_game()$state
  core <- .g3_core(st, tabs)
  n <- nrow(g3_legal_actions(st))
  # Every non-terminal position is worth +0.5 to its side to act.
  ev <- .g3_mcts_uniform_evaluator(function(node) 0.5)
  r <- .g3_mcts(core, ev, list(simulations = n))
  # With uniform priors each root action is expanded once at depth 1.
  expect_true(all(r$root$visits == 1L))
  expect_true(all(r$root$q == -0.5))
  # One more simulation reaches depth 2: +0.5 from the grandchild's side,
  # which is the root's side, so that edge's average moves towards 0.
  r2 <- .g3_mcts(core, ev, list(simulations = n + 1L))
  i <- which(r2$root$visits == 2L)
  expect_length(i, 1L)
  expect_equal(r2$root$q[i], 0)
})

test_that("a terminal root returns no action and never calls the evaluator", {
  tabs <- .g3_tables()
  st <- g3_apply_action(g3_setup_position(pcs(mcts_net), side = "black"), "m:B:0:0-B:3:0")$state
  expect_true(g3_is_terminal(st))
  boom <- function(nodes) stop("evaluator must not be called")
  r <- .g3_mcts(.g3_core(st, tabs), boom, list(simulations = 10L))
  expect_identical(r$status, "terminal")
  expect_true(is.na(r$action_id))
  expect_identical(nrow(r$root), 0L)
  expect_identical(r$root_value, -1)
})

test_that("zero root visits fall back to the legal prior argmax without a target", {
  tabs <- .g3_tables()
  st <- g3_new_game()$state
  core <- .g3_core(st, tabs)
  ids <- g3_legal_actions(st)$action_id
  ev <- function(nodes) lapply(nodes, function(n) list(prior = seq_along(n$ids), value = 0))
  for (settings in list(list(simulations = 0L), list(simulations = 50L, time_ms = 0))) {
    r <- .g3_mcts(core, ev, settings)
    expect_identical(r$status, "fallback")
    expect_identical(r$action_id, ids[length(ids)])
    expect_false(r$policy_target_available)
    expect_true(all(is.na(r$root$policy_target)))
    expect_true(all(r$root$visits == 0L))
  }
})

test_that("evaluation is deterministic; noise and sampling exist only in training mode", {
  tabs <- .g3_tables()
  core <- .g3_core(g3_new_game()$state, tabs)
  ev <- .g3_mcts_uniform_evaluator(function(node) 0.1)
  a <- .g3_mcts(core, ev, list(simulations = 40L, dirichlet_epsilon = 0.5, temperature = 1))
  b <- .g3_mcts(core, ev, list(simulations = 40L, dirichlet_epsilon = 0.5, temperature = 1))
  expect_identical(a$root, b$root)
  expect_false(a$noise_applied)
  expect_identical(a$temperature, 0)
  expect_identical(a$root$prior, a$root$prior_searched)
  set.seed(1)
  x0 <- stats::runif(1)
  set.seed(1)
  t1 <- .g3_mcts(core, ev, list(simulations = 40L, training = TRUE, seed = 7L))
  x1 <- stats::runif(1)
  expect_identical(x0, x1)
  t2 <- .g3_mcts(core, ev, list(simulations = 40L, training = TRUE, seed = 7L))
  expect_true(t1$noise_applied)
  expect_identical(t1$root, t2$root)
  expect_identical(t1$action_id, t2$action_id)
  expect_false(isTRUE(all.equal(t1$root$prior, t1$root$prior_searched)))
})

test_that("nodes are path specific and repetition is adjudicated by the rules", {
  tabs <- .g3_tables()
  st <- g3_setup_position(pcs("wK K white W:0:0", "wN N white W:3:0", "bK K black B:3:3", "bN N black B:0:3"))
  s <- st
  # After one knight cycle, repeating it once more is a threefold draw.
  for (id in c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3")) s <- g3_apply_action(s, id)$state
  core <- .g3_core(s, tabs)
  root <- .g3_mcts_node(core, tabs)
  path <- c("m:W:3:0-W:2:2", "m:B:0:3-B:1:1", "m:W:2:2-W:3:0", "m:B:1:1-B:0:3")
  # A guiding evaluator puts most prior mass on the cycle's actions.
  ev <- function(nodes) lapply(nodes, function(n) list(prior = ifelse(n$ids %in% path, 100, 1), value = 0))
  .g3_mcts_expand(root, ev(list(root))[[1L]])
  stats <- new.env()
  stats$evaluations <- 0L
  stats$terminal_leaves <- 0L
  stats$nodes <- 1L
  stats$max_depth <- 0L
  set <- .g3_mcts_settings(list(c_puct = 1.5))
  for (i in 1:60) .g3_mcts_simulate(root, ev, set, tabs, stats)
  # Follow the knight cycle down the tree.
  node <- root
  for (id in path) {
    child <- node$children[[match(id, node$ids)]]
    if (is.null(child)) break
    node <- child
  }
  expect_false(is.null(child))
  expect_true(node$terminal)
  expect_identical(node$outcome$reason, "repetition")
  expect_identical(node$terminal_value, 0)
  # Same board as the root, different history: a distinct node.
  expect_identical(node$core$rep[length(node$core$rep)], core$rep[length(core$rep)])
  expect_false(identical(node, root))
  expect_gt(length(node$core$rep), length(core$rep))
})

test_that("invalid evaluator output is rejected", {
  tabs <- .g3_tables()
  core <- .g3_core(g3_new_game()$state, tabs)
  bad_prior <- function(nodes) lapply(nodes, function(n) list(prior = rep(NaN, length(n$ids)), value = 0))
  bad_value <- function(nodes) lapply(nodes, function(n) list(prior = rep(1, length(n$ids)), value = 3))
  short <- function(nodes) lapply(nodes, function(n) list(prior = 1, value = 0))
  for (ev in list(bad_prior, bad_value, short)) {
    expect_error(.g3_mcts(core, ev, list(simulations = 2L)), class = "g3_incompatible_model")
  }
  expect_error(.g3_mcts(core, .g3_mcts_uniform_evaluator(), list(simulations = -1L)), class = "g3_budget_exceeded")
  expect_error(.g3_mcts(core, .g3_mcts_uniform_evaluator(), list(rollouts = 2L)), class = "g3_budget_exceeded")
})
