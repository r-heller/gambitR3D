# Phase 13: optional tutor contract and offline operation.

tutor_fixture <- function(level = 4L) {
  ex <- Filter(function(e) identical(e$provenance$definition_id, "checkmate-e5-twins"),
               g3_exercise_catalogue()$exercises)[[1L]]
  h <- g3_exercise_hint(ex, level)
  list(ex = ex, hint = h, ctx = g3_tutor_context(h, exercise = ex))
}

test_that("a deliberately false tutor move is rejected and the deterministic text is kept", {
  f <- tutor_fixture(4L)
  # Legal but not verified by the engine: a quiet queen move.
  bad <- g3_validate_tutor_output(
    list(text = "Better is the quiet queen move to B:b6.", evidence_refs = "hint", actions = "m:B:0:0-B:0:1"),
    f$ctx
  )
  expect_false(bad$valid)
  expect_identical(bad$source, "deterministic")
  expect_identical(bad$text, f$ctx$deterministic_text)
  expect_true(any(grepl("not one the engine verified", bad$problems, fixed = TRUE)))
  # Illegal outright (a white move when Black is to act).
  ill <- g3_validate_tutor_output(
    list(text = "Play m:WK:0:0-WK:1:0.", evidence_refs = "hint"), f$ctx
  )
  expect_false(ill$valid)
  expect_true(any(grepl("not legal", ill$problems, fixed = TRUE)))
  ill2 <- g3_validate_tutor_output(
    list(text = "Mate follows.", evidence_refs = "hint", actions = c("m:B:0:0-B:3:0", "m:WK:0:0-WK:1:0")), f$ctx
  )
  expect_false(ill2$valid)
  # A faithful paraphrase that cites evidence passes.
  ok <- g3_validate_tutor_output(
    list(text = "The queen goes from B:b5 to B:e5 and that is mate.", evidence_refs = "hint",
         actions = "m:B:0:0-B:3:0"),
    f$ctx
  )
  expect_true(ok$valid)
  expect_identical(ok$source, "tutor")
})

test_that("missing evidence, unknown squares and forbidden wording fall back", {
  f <- tutor_fixture(1L)
  none <- g3_validate_tutor_output(list(text = "Think about the king."), f$ctx)
  expect_false(none$valid)
  expect_true(any(grepl("missing evidence", none$problems, fixed = TRUE)))
  unknown <- g3_validate_tutor_output(list(text = "Think about the king.", evidence_refs = "elsewhere"), f$ctx)
  expect_false(unknown$valid)
  # Naming the destination at level 1 would leak level 3.
  leak <- g3_validate_tutor_output(list(text = "Look at B:e5.", evidence_refs = "hint"), f$ctx)
  expect_false(leak$valid)
  words <- g3_validate_tutor_output(list(text = "This gains about 300 centipawns.", evidence_refs = "hint"), f$ctx)
  expect_false(words$valid)
  # "Forces mate" needs an exhaustive proof in the cited evidence.
  forced <- g3_validate_tutor_output(list(text = "Black forces mate here.", evidence_refs = "hint"), f$ctx)
  expect_false(forced$valid)
  f4 <- tutor_fixture(4L)
  expect_true(g3_validate_tutor_output(list(text = "Black forces mate here.", evidence_refs = "hint"), f4$ctx)$valid)
})

test_that("a missing tutor model gives the ordinary deterministic hint", {
  f <- tutor_fixture(1L)
  h <- g3_exercise_hint(f$ex, 1L, tutor = "model:offline-tutor-not-installed")
  expect_identical(h$source, "deterministic")
  expect_identical(h$text, f$hint$text)
  expect_match(h$fallback_reason, "no offline tutor model runtime", fixed = TRUE)
  # A local paraphraser that fails or lies is also replaced.
  h2 <- g3_exercise_hint(f$ex, 1L, tutor = function(ctx) stop("model crashed"))
  expect_identical(h2$source, "deterministic")
  expect_identical(h2$text, f$hint$text)
  h3 <- g3_exercise_hint(f$ex, 1L, tutor = function(ctx) list(text = "Play Q B:b5-N:e5.", evidence_refs = "hint"))
  expect_identical(h3$source, "deterministic")
  # A grounded paraphrase is accepted and labelled as tutor text.
  h4 <- g3_exercise_hint(f$ex, 1L, tutor = function(ctx) {
    list(text = "Count the escape squares of the king on WK:e0.", evidence_refs = "hint")
  })
  expect_identical(h4$source, "tutor")
  expect_identical(h4$deterministic_text, f$hint$text)
})

test_that("tutor contexts can be built for explanations and attempts", {
  st <- g3_new_game()$state
  e <- g3_explain_action(st, "m:W:0:0-N:1:0")
  ctx <- g3_tutor_context(e, state = st)
  expect_identical(names(ctx$evidence), "facts")
  expect_true(g3_validate_tutor_output(list(text = "The knight jumps to N:c3, not W:c3.", evidence_refs = "facts",
                                            actions = "m:W:0:0-N:1:0"), ctx)$valid)
  expect_false(g3_validate_tutor_output(list(text = "Jump to W:c3.", evidence_refs = "facts",
                                             actions = "m:W:0:0-W:1:2"), ctx)$valid)
  expect_error(g3_tutor_context(e, state = g3_apply_action(st, "m:W:0:0-N:1:0")$state), class = "g3_stale_revision")
  f <- tutor_fixture(1L)
  at <- g3_score_attempt(f$ex, "m:B:0:0-B:0:1")
  actx <- g3_tutor_context(at, exercise = f$ex)
  expect_identical(names(actx$evidence), "attempt")
})

test_that("coaching works offline and the code references no remote endpoint", {
  src_dir <- test_path("..", "..", "R")
  files <- list.files(src_dir, pattern = "^(coach.*|exercises|learner)\\.R$", full.names = TRUE)
  skip_if(!length(files), "package sources not available")
  json <- list.files(test_path("..", "..", "inst", "coach"), full.names = TRUE)
  code <- unlist(lapply(c(files, json), readLines, warn = FALSE))
  pat <- "https?://|\\burl\\(|download\\.file|curl|httr|socketConnection|socketAccept|make_socket|websocket|requests\\."
  expect_identical(grep(pat, code, value = TRUE), character())
  # Running the whole teaching pipeline opens no URL or socket connection.
  before <- getAllConnections()
  ex <- g3_exercise_catalogue()$exercises[[2]]
  for (l in 1:4) g3_exercise_hint(ex, l, tutor = "model:remote-tutor")
  g3_score_attempt(ex, ex$primary_action_id)
  g <- g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2")
  g3_coach_game(g, budget = list(nodes = 50L), max_exercises = 1L, exercise_budget = list(nodes = 0L))
  path <- file.path(withr::local_tempdir(), "profile.json")
  g3_learner_record(path, g3_score_attempt(ex, ex$primary_action_id))
  new <- setdiff(getAllConnections(), before)
  cls <- vapply(new, function(i) summary(getConnection(i))$class, "")
  expect_false(any(cls %in% c("url", "url-libcurl", "url-wininet", "sockconn", "servsockconn")))
})
