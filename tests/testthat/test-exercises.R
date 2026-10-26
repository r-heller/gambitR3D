# Phase 13: exercises, graduated hints and attempt scoring.

catalogue <- function() g3_exercise_catalogue()
ex_by <- function(set, id) Filter(function(e) identical(e$provenance$definition_id, id), set$exercises)[[1L]]

test_that("the authored catalogue loads, revalidates and carries its provenance", {
  set <- catalogue()
  expect_s3_class(set, "g3_exercise_set")
  expect_gte(length(set$exercises), 8L)
  expect_true(all(set$summary$status == "validated"))
  expect_true(all(set$summary$source == "authored"))
  expect_setequal(set$summary$theme, c("checkmate", "escape_check", "platform", "fork", "capture",
                                       "coordinate_levels", "defend"))
  rh <- g3_rules_profile()$rules_hash
  for (e in set$exercises) {
    expect_identical(e$provenance$label, "authored, engine-verified")
    expect_identical(e$rules_hash, rh)
    expect_true(g3_validate_exercise(e))
    expect_identical(e$exercise_id, .g3_exercise_id(g3_state_hash(e$start_state), e$objective))
  }
})

test_that("rebuilding every authored definition reproduces the shipped exercise", {
  idx <- jsonlite::fromJSON(system.file("extdata", "exercises", "index.json", package = "gambitR3D"),
                            simplifyVector = FALSE)
  set <- catalogue()
  b <- idx$build$budget
  for (f in idx$files) {
    rebuilt <- .g3_authored_exercise(f$definition, budget = list(nodes = b$nodes, depth = b$depth),
                                     tolerance = idx$build$tolerance)
    shipped <- ex_by(set, f$definition$id)
    expect_identical(rebuilt$exercise_id, shipped$exercise_id)
    expect_identical(rebuilt$solutions[, c("action_id", "grade", "basis", "reason")],
                     shipped$solutions[, c("action_id", "grade", "basis", "reason")])
  }
})

test_that("multiple accepted solutions are all solved, including twins at one coordinate", {
  mate <- ex_by(catalogue(), "checkmate-e5-twins")
  solved <- mate$solutions$action_id[mate$solutions$grade == "solved"]
  expect_true(all(c("m:B:0:0-B:3:0", "m:B:0:0-N:3:2") %in% solved))
  for (a in solved) {
    at <- g3_score_attempt(mate, a)
    expect_identical(at$status, "solved")
    expect_true(g3_is_terminal(g3_apply_action(mate$start_state, a)$state))
  }
  fork <- ex_by(catalogue(), "fork-d6-twins")
  expect_setequal(fork$solutions$action_id, c("m:W:3:3-B:2:1", "m:W:3:3-N:2:3"))
  expect_false(fork$validation$unique_solution_proven)
})

test_that("attempts are graded solved, acceptable, incorrect or illegal", {
  set <- catalogue()
  cap <- ex_by(set, "capture-queen")
  expect_identical(g3_score_attempt(cap, "m:W:0:0-W:0:3")$status, "solved")
  acc <- g3_score_attempt(cap, "m:W:0:0-W:1:0")
  expect_identical(acc$status, "acceptable")
  expect_identical(acc$evidence$reason, "lesser_capture")
  inc <- g3_score_attempt(cap, "m:W:0:0-W:0:2")
  expect_identical(inc$status, "incorrect")
  expect_identical(inc$feedback_kind, "objective")
  # Illegal input is reported separately from strategic mistakes.
  ill <- g3_score_attempt(cap, "m:W:0:0-B:3:3")
  expect_identical(ill$status, "illegal")
  expect_identical(ill$feedback_kind, "illegal_input")
  expect_false(ill$evidence$legal)
  expect_identical(ill$evidence$diagnosis, "not_reachable")
  expect_identical(g3_score_attempt(cap, "nonsense")$evidence$diagnosis, "malformed")
  expect_identical(g3_score_attempt(cap, "m:N:0:0-N:0:1")$evidence$diagnosis, "no_piece")
  expect_identical(g3_score_attempt(cap, "m:W:0:3-W:0:2")$evidence$diagnosis, "wrong_side")
  # A refuting reply is named and verified by replay.
  esc <- ex_by(set, "escape-platform-block")
  bad <- g3_score_attempt(esc, "m:W:0:0-WQ:0:0")
  expect_identical(bad$status, "incorrect")
  expect_identical(bad$evidence$reason, "mate_reply")
  expect_true(bad$evidence$reply$verified)
  after <- g3_apply_action(g3_apply_action(esc$start_state, "m:W:0:0-WQ:0:0")$state, bad$evidence$reply$action_id)$state
  expect_identical(after$outcome$reason, "checkmate")
  # Moving into check is illegal input, not a strategic mistake.
  sc <- g3_score_attempt(esc, "m:W:0:0-W:0:1")
  expect_identical(sc$status, "illegal")
  expect_identical(sc$evidence$diagnosis, "self_check")
})

test_that("an illegal continuation is reported without changing the first grade", {
  mate <- ex_by(catalogue(), "checkmate-e5-twins")
  at <- g3_score_attempt(mate, c("m:B:0:0-B:3:0", "m:WK:0:0-WK:1:0"))
  expect_identical(at$status, "solved")
  expect_false(at$continuation$ok)
  expect_identical(at$continuation$failed_at, 2L)
  expect_true(any(grepl("Continuation action 2", at$feedback, fixed = TRUE)))
})

test_that("coordinate-level exercises explain the twin square", {
  ex <- ex_by(catalogue(), "coordinate-d4-levels")
  twin <- g3_score_attempt(ex, "m:W:2:0-W:2:3")
  expect_identical(twin$status, "incorrect")
  expect_identical(twin$evidence$reason, "twin_square")
  expect_match(twin$feedback[2], "W:d4 has the same coordinate d4 as N:d4", fixed = TRUE)
  expect_identical(g3_score_attempt(ex, "m:W:2:0-N:2:1")$status, "solved")
  expect_identical(g3_score_attempt(ex, "m:W:0:1-N:2:1")$status, "solved")
})

test_that("graduated hints reveal one layer at a time and never leak later levels", {
  set <- catalogue()
  tabs <- .g3_tables()
  for (ex in set$exercises) {
    h <- lapply(1:4, function(l) g3_exercise_hint(ex, l))
    st <- ex$start_state
    core <- .g3_core(st, tabs)
    ctx <- .g3_ctx(core, tabs)
    legal <- .g3_legal(core, tabs, ctx)
    i <- .g3_find_action(core, legal, ex$primary_action_id, tabs)
    a <- legal[i, ]
    label <- .g3_action_labels(core, legal[i, , drop = FALSE], tabs, ctx)
    from <- .g3_sq_name(a[["from"]], ctx, tabs)
    obj <- .g3_objective_text(ex)
    txt <- vapply(h, function(x) paste(x$text, collapse = " "), "")
    for (l in 1:3) {
      expect_false(grepl(ex$primary_action_id, txt[l], fixed = TRUE))
      expect_false(grepl(label, txt[l], fixed = TRUE))
    }
    if (!grepl(from, obj, fixed = TRUE)) expect_false(grepl(from, txt[1], fixed = TRUE))
    if (a[["type"]] == 1L) {
      to <- .g3_sq_name(a[["to"]], ctx, tabs)
      expect_false(grepl(to, txt[1], fixed = TRUE))
      expect_false(grepl(to, txt[2], fixed = TRUE))
      expect_true(grepl(to, txt[3], fixed = TRUE))
    } else if (a[["type"]] == 2L) {
      dock <- tabs$dock_ids[a[["dock"]]]
      expect_false(grepl(dock, txt[1], fixed = TRUE))
      expect_false(grepl(dock, txt[2], fixed = TRUE))
      expect_false(grepl(paste0("platform ", .G3_PLATFORMS[a[["plat"]]]), txt[1], fixed = TRUE))
      expect_true(grepl(dock, txt[3], fixed = TRUE))
    }
    expect_true(grepl(label, txt[4], fixed = TRUE))
    expect_identical(lapply(h, `[[`, "reveals")[[4]], c("area", "piece", "destination", "solution"))
    # Highlights are physical square ids, identical in 2D and 3D.
    sq <- unlist(lapply(h, function(x) x$highlight$squares))
    expect_true(all(sq %in% tabs$sq_id))
    expect_identical(h[[4]]$highlight$action_id, ex$primary_action_id)
    # Pure: asking again gives the same hint.
    expect_identical(g3_exercise_hint(ex, 2L)$text, h[[2]]$text)
  }
  expect_error(g3_exercise_hint(set$exercises[[1]], 5L), class = "g3_invalid_state")
})

test_that("a level-4 hint shows only a continuation replayed through the engine", {
  for (ex in catalogue()$exercises) {
    cont <- ex$continuation
    expect_true(cont$verified)
    st <- ex$start_state
    for (k in seq_along(cont$actions)) {
      step <- g3_apply_action(st, cont$actions[[k]])
      expect_identical(step$event$notation, cont$notation[[k]])
      st <- step$state
    }
    expect_identical(g3_state_hash(st), cont$final_state_hash)
    expect_identical(cont$actions[[1]], ex$primary_action_id)
  }
})

test_that("rules-hash mismatches and stale positions are rejected", {
  ex <- catalogue()$exercises[[1]]
  foreign <- ex
  foreign$rules_hash <- strrep("b", 64)
  expect_error(g3_score_attempt(foreign, ex$primary_action_id), class = "g3_unsupported_rules")
  expect_error(g3_exercise_hint(foreign, 1L), class = "g3_unsupported_rules")
  stale <- ex
  stale$start_state_hash <- strrep("0", 64)
  expect_error(g3_score_attempt(stale, ex$primary_action_id), class = "g3_stale_revision")
  # A file written under another rules hash is refused on read.
  path <- file.path(withr::local_tempdir(), "ex.json")
  g3_write_exercises(catalogue(), path)
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  raw$exercises[[1]]$rules_hash <- strrep("c", 64)
  writeLines(jsonlite::toJSON(raw, auto_unbox = TRUE, null = "null", na = "null", digits = NA), path)
  expect_error(g3_read_exercises(path), class = "g3_unsupported_rules")
  # Tampered solutions are caught by validation and by scoring.
  bad <- ex
  bad$solutions <- bad$solutions[-1, ]
  expect_false(g3_validate_exercise(bad, error = FALSE)$valid)
  expect_error(g3_score_attempt(bad, ex$solutions$action_id[1]), class = "g3_invalid_state")
})

test_that("exercises round-trip through JSON unchanged", {
  set <- catalogue()
  path <- file.path(withr::local_tempdir(), "set.json")
  g3_write_exercises(set, path)
  back <- g3_read_exercises(path)
  expect_identical(back$summary$exercise_id, set$summary$exercise_id)
  for (k in seq_along(set$exercises)) {
    expect_identical(back$exercises[[k]]$solutions, set$exercises[[k]]$solutions)
    expect_identical(g3_exercise_hint(back$exercises[[k]], 3L)$text, g3_exercise_hint(set$exercises[[k]], 3L)$text)
  }
  expect_error(g3_write_exercises(set, path), class = "g3_io_error")
})

test_that("neutral height and 2D/3D view cannot change an exercise, its answers or hints", {
  st <- g3_setup_position(pcs("wN N white W:3:3", "wK K white WK:0:0", "bK K black B:3:3", "bR R black B:1:3"))
  g <- .g3_game_new(st)
  dir <- withr::local_tempdir()
  p1 <- file.path(dir, "low-2d.g3.json")
  p2 <- file.path(dir, "high-3d.g3.json")
  g3_write_game(g, p1, ui = list(view = "2d", neutral_height = "lowest"))
  g3_write_game(g, p2, ui = list(view = "3d", neutral_height = "highest", camera = list(theta = 30)))
  g1 <- g3_read_game(p1)
  g2 <- g3_read_game(p2)
  expect_false(identical(g1$ui, g2$ui))
  e1 <- g3_create_exercises(g1$state, themes = "fork", budget = list(nodes = 50L))$exercises[[1]]
  e2 <- g3_create_exercises(g2$state, themes = "fork", budget = list(nodes = 50L))$exercises[[1]]
  expect_identical(e1$exercise_id, e2$exercise_id)
  expect_identical(e1$solutions, e2$solutions)
  for (l in 1:4) expect_identical(g3_exercise_hint(e1, l)$text, g3_exercise_hint(e2, l)$text)
  for (a in g3_legal_actions(g1$state)$action_id) {
    expect_identical(g3_score_attempt(e1, a)$status, g3_score_attempt(e2, a)$status)
  }
})

test_that("search-based exercises stay candidates and never become proven unique", {
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  g <- g3_game_apply(.g3_game_new(st), "m:B:0:0-B:0:1")
  rev <- g3_coach_game(g, budget = list(nodes = 300L, depth = 2L), max_exercises = 0L)
  set <- g3_create_exercises(rev, max_n = 5L, budget = list(nodes = 200L, depth = 2L))
  imp <- Filter(function(e) e$theme == "improve_decision", set$exercises)
  expect_identical(length(imp), 1L)
  imp <- imp[[1]]
  expect_identical(imp$status, "candidate")
  expect_false(imp$validation$unique_solution_proven)
  expect_true(all(imp$solutions$basis == "searched"))
  expect_identical(g3_score_attempt(imp, "m:B:0:0-B:0:1")$evidence$reason, "played_move")
  # Searched tiering within an exact criterion is never "proven unique".
  esc <- ex_by(catalogue(), "escape-platform-block")
  expect_identical(esc$grading$basis, "searched")
  expect_false(esc$validation$unique_solution_proven)
  # The candidate exercise is excluded from practice plans by default.
  plan <- g3_practice_plan(file.path(withr::local_tempdir(), "p.json"), set, record = FALSE)
  expect_false(imp$exercise_id %in% plan$exercise_id)
})

test_that("a zero budget disables searched grading and continuations", {
  st <- g3_setup_position(pcs(
    "wPb2 P white W:0:3", "wK K white W:0:0", "bB B black W:3:1", "bQ Q black N:0:0", "bK K black BK:0:1"
  ))
  set <- g3_create_exercises(st, themes = "escape_check", budget = list(nodes = 0L))
  skip_if(!length(set$exercises), "fixture has no escape exercise")
  ex <- set$exercises[[1]]
  expect_identical(ex$grading$basis, "exact")
  expect_true(all(ex$solutions$grade == "solved"))
  expect_identical(length(ex$continuation$actions), 1L)
})

test_that("objectives are rendered in both languages without naming the solution", {
  for (ex in catalogue()$exercises) {
    en <- g3_exercise_objective(ex)
    de <- g3_exercise_objective(ex, lang = "de")
    expect_true(is.character(en) && length(en) == 1L && nzchar(en))
    expect_true(is.character(de) && length(de) == 1L && nzchar(de))
    expect_false(grepl(ex$primary_action_id, en, fixed = TRUE))
  }
  foreign <- catalogue()$exercises[[1]]
  foreign$rules_hash <- strrep("d", 64)
  expect_error(g3_exercise_objective(foreign), class = "g3_unsupported_rules")
})
