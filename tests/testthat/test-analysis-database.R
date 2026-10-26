# Phase 09: batch analysis, resumable runs, collection summaries, splits.

random_game <- function(seed, plies, id) {
  g <- g3_new_game()
  g$metadata$game_id <- id
  set.seed(seed)
  for (i in seq_len(plies)) {
    acts <- g3_legal_actions(g$state)
    if (!nrow(acts)) break
    g <- g3_game_apply(g, acts$action_id[sample.int(nrow(acts), 1L)])
  }
  g
}

collection <- function() {
  gs <- list(random_game(1, 6, "g1"), random_game(2, 8, "g2"), random_game(3, 5, "g3"), random_game(4, 7, "g4"))
  gs[[4]] <- g3_game_event(gs[[4]], "truncated", reason = "max_plies")
  gs[[3]] <- g3_game_event(gs[[3]], "draw_agreed")
  gs
}

no_timing <- function(df) df[, setdiff(names(df), c("elapsed_ms", "cache_hit")), drop = FALSE]

test_that("batch analysis equals single-game analysis", {
  gs <- collection()
  run <- g3_analyse_database(gs, evaluator = "search", budget = list(nodes = 80L), chunk_size = 3L)
  expect_s3_class(run, "g3_database_analysis")
  expect_identical(run$manifest$status, "completed")
  expect_identical(run$completed$game_id, c("g1", "g2", "g3", "g4"))
  expect_identical(run$completed$chunk, c(1L, 1L, 1L, 2L))
  for (i in seq_along(gs)) {
    single <- g3_analyse_game(gs[[i]], budget = list(nodes = 80L))
    batch <- run$results[[gs[[i]]$metadata$game_id]]
    for (tb in c("moves", "positions", "tactics", "events", "summary")) expect_identical(batch[[tb]], single[[tb]])
    expect_identical(no_timing(batch$evaluations), no_timing(single$evaluations))
  }
  # The shared start position is evaluated once and reused under the same context.
  expect_gt(sum(run$completed$cache_hits), 0L)
})

test_that("summaries keep denominators and separate truncation from draws", {
  run <- g3_analyse_database(collection(), evaluator = "none")
  s <- run$summary
  expect_identical(s$counts$n_completed, 4L)
  expect_identical(sum(s$outcomes$n_games), 4L)
  expect_true(all(s$outcomes$denominator == 4L))
  oc <- stats::setNames(s$outcomes$n_games, s$outcomes$result)
  expect_identical(unname(oc[c("draw", "truncated", "unfinished")]), c(1L, 1L, 2L))
  tm <- stats::setNames(s$terminations$n_games, s$terminations$termination)
  expect_identical(unname(tm[c("draw_agreed", "truncated", "unfinished")]), c(1L, 1L, 2L))
  expect_identical(s$terminations$outcome_class[s$terminations$termination == "truncated"], "truncated")
  expect_identical(s$platform_use$total_actions, sum(vapply(run$results, function(r) nrow(r$moves), 0L)))
  expect_identical(sum(s$platform_use_by_phase$actions), s$platform_use$total_actions)
  expect_identical(s$lengths$n_games[s$lengths$outcome_class == "all"], 4L)
  expect_true(all(s$motifs$denominator_games == 4L))
  n_pos <- sum(vapply(run$results, function(r) nrow(r$positions), 0L))
  expect_identical(s$evaluation_missingness$status, c("scored", "not_requested"))
  expect_identical(s$evaluation_missingness$n_positions, c(0L, n_pos))
  expect_true(all(s$evaluation_missingness$denominator == n_pos))
  expect_identical(s$repetition$distinct_start_positions, 1L)
  expect_identical(sum(s$opening_groups$n_games), 4L)
  expect_true(all(grepl("^Standard start: ", s$opening_groups$name)))
  expect_identical(s$filters$filters, "none")
  # Start positions are not counted as positions recurring in play.
  expect_false(run$results[[1]]$positions$position_key[1] %in% s$repeated_positions$position_key)
  empty <- g3_analyse_database(list(), evaluator = "none")
  expect_identical(empty$summary$counts$n_completed, 0L)
  expect_identical(nrow(empty$completed), 0L)
  expect_identical(names(empty$completed), names(run$completed))
})

test_that("failures are audited and do not stop the batch", {
  gs <- collection()
  gs[[2]]$events$action_id[3] <- "m:W:0:0-W:3:3"
  run <- g3_analyse_database(gs, evaluator = "none")
  expect_identical(run$manifest$status, "completed_with_errors")
  expect_identical(run$errors$game_id, "g2")
  expect_identical(run$errors$error_class, "g3_illegal_action")
  expect_identical(run$errors$first_invalid_ply, 3L)
  expect_identical(run$completed$game_id, c("g1", "g3", "g4"))
  expect_identical(run$summary$counts$n_errors, 1L)
  expect_error(g3_analyse_database(gs, filters = list(result = "white"), evaluator = "none"), class = "g3_invalid_state")
  sel <- g3_analyse_database(gs, filters = list(game_id = c("g1", "g4")), evaluator = "none")
  expect_identical(sel$completed$game_id, c("g1", "g4"))
})

test_that("a cancelled run resumes from its output directory without recomputing", {
  gs <- collection()
  dir <- withr::local_tempdir()
  done <- character()
  prog <- function(d, total, unit) done <<- c(done, paste(d, total, unit))
  # Cancel once two games have been completed.
  stopper <- function() length(done) >= 2L
  run1 <- g3_analyse_database(gs, evaluator = "search", budget = list(nodes = 60L), output_dir = dir,
                              chunk_size = 1L, stop_check = stopper, progress = prog)
  expect_identical(run1$manifest$status, "cancelled")
  expect_identical(run1$completed$game_id, c("g1", "g2"))
  expect_identical(run1$pending, c("g3", "g4"))
  expect_identical(done, c("1 4 games", "2 4 games"))
  expect_true(file.exists(file.path(dir, "manifest.json")))
  man <- jsonlite::fromJSON(file.path(dir, "manifest.json"))
  expect_identical(man$status, "cancelled")
  expect_identical(man$n_pending, 2L)
  f1 <- file.path(dir, run1$completed$result_file[1])
  mtime <- file.mtime(f1)
  run2 <- g3_analyse_database(gs, evaluator = "search", budget = list(nodes = 60L), output_dir = dir, chunk_size = 1L)
  expect_identical(run2$manifest$status, "completed")
  expect_identical(run2$completed$game_id, c("g1", "g2", "g3", "g4"))
  expect_identical(file.mtime(f1), mtime)
  expect_identical(run2$completed$elapsed_ms[1:2], run1$completed$elapsed_ms)
  expect_identical(digest::digest(file = f1, algo = "sha256"), run2$completed$result_sha256[1])
  # The resumed results equal an uninterrupted run.
  full <- g3_analyse_database(gs, evaluator = "search", budget = list(nodes = 60L))
  for (id in names(full$results)) {
    expect_identical(no_timing(run2$results[[id]]$evaluations), no_timing(full$results[[id]]$evaluations))
  }
  # Another configuration may not reuse the directory.
  expect_error(g3_analyse_database(gs, evaluator = "search", budget = list(nodes = 61L), output_dir = dir),
               class = "g3_io_error")
})

test_that("the analysis job type runs, reports progress and honours cancellation", {
  gs <- collection()
  dir <- withr::local_tempdir()
  prog <- list()
  ctx <- new.env()
  ctx$dir <- dir
  ctx$resume <- FALSE
  ctx$should_stop <- function() length(prog) >= 1L
  ctx$timed_out <- function() FALSE
  ctx$progress <- function(done, total, unit) prog[[length(prog) + 1L]] <<- c(done, total)
  out <- .g3_job_analysis(list(db = gs, evaluator = "none"), ctx)
  expect_identical(out$manifest$status, "cancelled")
  expect_identical(attr(out, "stop_reason"), "cancelled")
  expect_identical(nrow(out$completed), 1L)
  ctx$should_stop <- function() FALSE
  out2 <- .g3_job_analysis(list(db = gs, evaluator = "none"), ctx)
  expect_identical(out2$manifest$status, "completed")
  expect_identical(nrow(out2$completed), 4L)
  expect_null(attr(out2, "stop_reason"))
})

test_that("games are analysed straight from a game database", {
  skip_if_not_installed("RSQLite")
  skip_if(is.null(get0("g3_open_game_db", envir = asNamespace("gambitR3D"))), "database API not available")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "games.sqlite")
  .g3_example_game_db(path)
  db <- g3_open_game_db(path, read_only = TRUE)
  on.exit(g3_close_game_db(db), add = TRUE)
  all_ids <- g3_query_games(db, limit = 1000L)$game_id
  fin <- g3_query_games(db, filters = list(status = "finished"), limit = 1000L)$game_id
  run <- g3_analyse_database(db, filters = list(status = "finished"), evaluator = "none")
  expect_identical(run$manifest$n_scheduled, length(fin))
  expect_setequal(c(run$completed$game_id, run$errors$game_id), fin)
  expect_identical(nrow(run$errors), 0L)
  expect_match(run$manifest$filters, "finished")
  # The same game analysed from the database and from memory agree.
  g <- g3_load_game(db, fin[1])
  single <- g3_analyse_game(g, evaluator = "none")
  expect_identical(run$results[[fin[1]]]$positions, single$positions)
  # A path is opened read-only and the database file is not modified.
  before <- digest::digest(file = path, algo = "sha256")
  run2 <- g3_analyse_database(path, evaluator = "none")
  expect_identical(run2$manifest$n_completed + run2$manifest$n_errors, length(all_ids))
  expect_identical(digest::digest(file = path, algo = "sha256"), before)
})

test_that("splits keep games and groups whole and audit shared positions", {
  gs <- lapply(1:8, function(i) random_game(i, 6, sprintf("s%02d", i)))
  for (i in seq_along(gs)) gs[[i]]$metadata$source <- if (i <= 4) "batch-a" else if (i <= 6) "batch-b" else "batch-c"
  sp <- g3_split_games(gs, c(train = 0.75, test = 0.25), group = "source", seed = 3L)
  expect_s3_class(sp, "g3_split")
  a <- sp$assignments
  expect_identical(a$game_id, vapply(gs, function(g) g$metadata$game_id, ""))
  expect_true(all(tapply(a$partition, a$group, function(x) length(unique(x))) == 1L))
  expect_identical(sum(sp$sizes$n_games), 8L)
  au <- sp$audit
  expect_identical(nrow(au), 1L)
  # Every game shares the standard start: it is reported as a start key.
  expect_gte(au$shared_start_keys, 1L)
  expect_identical(au$shared_keys, au$shared_start_keys + au$shared_play_keys)
  expect_true(any(sp$shared_keys$start_position))
  expect_identical(g3_split_games(gs, c(train = 0.75, test = 0.25), group = "source", seed = 3L)$assignments, a)
  # Game-level groups by default; analyses can be audited by key too.
  sp2 <- g3_split_games(gs, c(train = 0.5, test = 0.5), seed = 1L)
  expect_identical(sp2$sizes$n_games, c(4L, 4L))
  ans <- lapply(gs[1:4], g3_analyse_game, evaluator = "none")
  sp3 <- g3_split_games(ans, c(a = 0.5, b = 0.5), seed = 1L)
  expect_true(is.na(sp3$audit$shared_history_contexts))
  expect_error(g3_split_games(gs, c(0.5, 0.5)), class = "g3_invalid_state")
})
