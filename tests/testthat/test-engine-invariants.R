# Phase 02: API contracts, immutability and randomized invariants.

test_that("API return types and typed conditions", {
  game <- g3_new_game()
  expect_s3_class(game, "g3_game")
  expect_named(game, c("initial_state", "state", "events", "metadata"))
  expect_s3_class(game$state, "g3_state")
  a <- g3_legal_actions(game$state)
  expect_s3_class(a, "g3_actions")
  expect_identical(
    names(a)[1:10],
    c("action_id", "type", "piece_id", "from_square", "to_square", "platform_id",
      "to_dock", "rotation_delta", "promotion", "castle_side")
  )
  step <- g3_apply_action(game$state, a$action_id[[1L]])
  expect_s3_class(step, "g3_transition")
  expect_match(g3_position_key(game$state), "^[0-9a-f]{64}$")
  expect_true(isTRUE(g3_validate_state(step$state)))
  expect_error(g3_apply_action(game$state, "m:W:0:0-B:0:0"), class = "g3_illegal_action")
  expect_error(g3_apply_action(game$state, a$action_id[[1L]], expected_state_hash = "0"),
               class = "g3_stale_revision")
  expect_error(g3_legal_actions(list()), class = "g3_invalid_state")
  # A one-row action table is accepted; extra fields are not trusted.
  forged <- a[1, ]
  forged$to_square <- "B:0:0"
  s2 <- g3_apply_action(game$state, forged)
  expect_identical(s2$event$action$to_square, a$to_square[[1L]])
})

test_that("illegal actions leave the input state and its hashes unchanged", {
  st <- g3_new_game()$state
  h <- g3_state_hash(st)
  k <- g3_position_key(st)
  try(g3_apply_action(st, "t:WQ>N:L:lo@0"), silent = TRUE)
  expect_identical(g3_state_hash(st), h)
  expect_identical(g3_position_key(st), k)
})

test_that("validation catches structural and logical errors", {
  st <- g3_new_game()$state
  bad <- st
  bad$pieces$square_id[2] <- bad$pieces$square_id[1]
  expect_error(g3_validate_state(bad), class = "g3_invalid_state")
  expect_false(g3_validate_state(bad, error = FALSE)$valid)
  nok <- st
  nok$pieces <- nok$pieces[nok$pieces$id != "bK", ]
  expect_false(g3_validate_state(nok, error = FALSE)$valid)
  expect_true(g3_validate_state(nok, mode = "editor", error = FALSE)$valid)
  dock <- st
  dock$boards$dock[5] <- dock$boards$dock[4]
  expect_match(paste(g3_validate_state(dock, error = FALSE)$problems, collapse = " "), "two platforms")
})

test_that("random legal trajectories keep every transition invariant", {
  set.seed(20261026)
  for (g in 1:6) {
    st <- g3_new_game()$state
    for (ply in 1:40) {
      if (g3_is_terminal(st)) break
      a <- g3_legal_actions(st)
      pick <- a$action_id[sample.int(nrow(a), 1L)]
      before <- st
      step <- g3_apply_action(st, pick, expected_state_hash = g3_state_hash(st))
      s2 <- step$state
      expect_identical(before, st) # input unchanged
      expect_identical(s2$ply, st$ply + 1L)
      expect_false(identical(s2$side, st$side))
      expect_true(isTRUE(g3_validate_state(s2)))
      # Pieces conserved except the captured one; ids never reappear.
      lost <- setdiff(st$pieces$id, s2$pieces$id)
      expect_identical(lost, if (is.na(step$event$action$capture_id)) character() else step$event$action$capture_id)
      expect_true(all(s2$pieces$id %in% st$pieces$id))
      # No king is ever captured.
      expect_identical(sum(s2$pieces$type == "K"), 2L)
      expect_identical(step$event$post_state_hash, g3_state_hash(s2))
      expect_identical(step$event$position_key, g3_position_key(s2))
      st <- s2
    }
  }
})

test_that("replaying the same actions reproduces the same checksum", {
  set.seed(7)
  st <- g3_new_game()$state
  ids <- character()
  for (i in 1:30) {
    if (g3_is_terminal(st)) break
    a <- g3_legal_actions(st)
    ids <- c(ids, a$action_id[sample.int(nrow(a), 1L)])
    st <- g3_apply_action(st, ids[length(ids)])$state
  }
  st2 <- g3_new_game()$state
  for (id in ids) st2 <- g3_apply_action(st2, id)$state
  expect_identical(g3_state_hash(st2), g3_state_hash(st))
})

test_that("action ids are deterministic and order-independent of generation", {
  st <- g3_new_game()$state
  expect_identical(g3_legal_actions(st)$action_id, g3_legal_actions(st)$action_id)
  ids <- g3_legal_actions(st)$action_id
  expect_identical(ids, sort(ids, method = "radix"))
})
