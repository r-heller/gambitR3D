# The direct state serialiser must reproduce the canonical JSON exactly.

test_that("fast state checksum equals the canonical-JSON reference", {
  set.seed(31)
  states <- list(g3_new_game()$state)
  st <- states[[1]]
  for (i in 1:80) {
    if (g3_is_terminal(st)) break
    a <- g3_legal_actions(st)
    st <- g3_apply_action(st, a$action_id[sample.int(nrow(a), 1L)])$state
    states[[length(states) + 1L]] <- st
  }
  # An en-passant record, a finished game and unusual piece ids.
  ep <- g3_apply_action(g3_setup_position(transform(data.frame(
    id = c("wP", "wK", "bP", "bK"), type = c("P", "K", "P", "K"),
    color = c("white", "white", "black", "black"),
    square_id = c("W:1:1", "W:0:0", "N:2:1", "B:3:3")
  ), has_moved = c(FALSE, TRUE, TRUE, TRUE))), "m:W:1:1-W:1:3")$state
  odd <- g3_setup_position(data.frame(
    id = c("w \"K\"", "b\\K", "x/y"), type = c("K", "K", "R"),
    color = c("white", "black", "white"), square_id = c("W:0:0", "B:3:3", "W:2:0")
  ))
  uni <- g3_setup_position(data.frame(
    id = c("König", "bK"), type = c("K", "K"), color = c("white", "black"),
    square_id = c("W:0:0", "B:3:3")
  ))
  for (s in c(states, list(ep, odd, uni))) {
    expect_identical(.g3_state_hash(s), .g3_state_hash_ref(s))
  }
  expect_null(.g3_state_json(uni)) # non-ASCII text uses the reference path
})
