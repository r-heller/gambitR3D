# The compiled backend must reproduce the R reference exactly: legal
# actions (with order), repetition-key texts, evaluation and full root
# searches (scores, bounds, depth, node counts, principal variation).

test_that("compiled and R backends agree on random positions", {
  skip_if_not(identical(.g3_engine(), "cpp"), "compiled backend not available")
  tabs <- .g3_tables()
  ptr <- .g3_cpp_ptr()
  expect_identical(.g3_cpp_sha256("abc"), digest::digest("abc", algo = "sha256", serialize = FALSE))
  set.seed(20261026)
  for (g in 1:4) {
    st <- g3_new_game()$state
    for (ply in 1:40) {
      if (g3_is_terminal(st)) break
      core <- .g3_core(st, tabs)
      cc <- .g3_cpp_core(core)
      ref <- .g3_legal_r(core, tabs)
      fast <- .g3_cpp_legal(ptr, cc)
      expect_identical(unname(fast), unname(ref))
      expect_identical(.g3_cpp_key_text(ptr, cc), .g3_key_text(core, tabs))
      expect_identical(.g3_cpp_eval(ptr, cc), as.numeric(.g3_eval(core, .g3_ctx(core, tabs), tabs)))
      if (ply %% 10 == 0) {
        for (exact in c(FALSE, TRUE)) {
          b <- .g3_budget(list(nodes = 120, depth = 3))
          a <- .g3_root_search_r(core, tabs, b, exact)
          f <- .g3_root_search(core, tabs, b, exact)
          for (field in c("depth", "best", "scores", "bounds", "status", "nodes", "pv")) {
            expect_identical(unname(f[[field]]), unname(a[[field]]), info = field)
          }
        }
      }
      acts <- g3_legal_actions(st)
      st <- g3_apply_action(st, acts$action_id[sample.int(nrow(acts), 1L)])$state
    }
  }
})

test_that("the R reference can be selected and gives the same decision", {
  st <- g3_new_game()$state
  a <- withr::with_options(list(gambitR3D.engine = "r"), g3_choose_action(st, budget = list(nodes = 150L)))
  b <- g3_choose_action(st, budget = list(nodes = 150L))
  expect_identical(a$action_id, b$action_id)
  expect_identical(a$score, b$score)
  expect_identical(a$nodes, b$nodes)
})
