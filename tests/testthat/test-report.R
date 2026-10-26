# Phase 10: self-contained offline reports with escaped text and correct
# totals.

skip_if_not_installed("ggplot2")
skip_if_not_installed("viridisLite")

hostile_game <- function() {
  g <- g3_new_game(players = list(white = "<b>Ada</b>", black = "Ben & 'Co' <script>alert(1)</script>"))
  set.seed(8)
  for (i in 1:10) {
    acts <- g3_legal_actions(g$state)
    if (!nrow(acts)) break
    g <- g3_game_apply(g, acts$action_id[sample.int(nrow(acts), 1L)])
  }
  g$annotations <- list(list(ply = 2L, text = "<script>alert('x')</script> `r file.create('PWNED')` <img src=x onerror=alert(1)> https://example.org/track.png"))
  g3_game_event(g, "truncated", reason = "<iframe src=\"http://example.org\"></iframe>")
}

html_links <- function(html) {
  m <- regmatches(html, gregexpr("(href|src)=\"[^\"]*\"", html))[[1]]
  sub("^(href|src)=\"", "", sub("\"$", "", m))
}

test_that("a game report is self-contained, escaped and consistent with its tables", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  r <- g3_analyse_game(hostile_game(), budget = list(nodes = 500L))
  out <- g3_export_analysis_report(r, file.path(dir, "rep"))
  html <- paste(readLines(file.path(dir, "rep", "report.html"), warn = FALSE), collapse = "\n")
  # Offline: no scripts, no remote references, images are data URIs.
  expect_false(grepl("<script", html, ignore.case = TRUE))
  expect_false(grepl("<iframe", html, ignore.case = TRUE))
  expect_false(grepl("<img src=x", html, fixed = TRUE))
  links <- html_links(html)
  expect_true(all(grepl("^(#|tables/|data:image/png;base64,)", links)))
  expect_gte(sum(grepl("^data:image/png;base64,", links)), 4L)
  rel <- links[grepl("^tables/", links)]
  expect_true(all(file.exists(file.path(dir, "rep", rel))))
  expect_false(grepl("@import|url\\(", html))
  # Escaped annotations, names and reasons; nothing was executed.
  expect_match(html, "&lt;script&gt;alert(&#39;x&#39;)", fixed = TRUE)
  expect_match(html, "&lt;b&gt;Ada&lt;/b&gt;", fixed = TRUE)
  expect_match(html, "Ben &amp; &#39;Co&#39;", fixed = TRUE)
  expect_match(html, "&lt;iframe", fixed = TRUE)
  expect_false(file.exists(file.path(dir, "PWNED")))
  # Totals in the report equal the table rows.
  mv <- utils::read.csv(file.path(dir, "rep", "tables", "moves.csv"))
  pos <- utils::read.csv(file.path(dir, "rep", "tables", "positions.csv"))
  expect_identical(out$totals$actions, nrow(mv))
  expect_identical(out$totals$positions, nrow(pos))
  expect_match(html, paste0("Actions (rows in moves.csv)</th><td>", nrow(mv), "</td>"), fixed = TRUE)
  expect_match(html, paste0("Positions without a score</th><td>", r$summary$n_eval_missing, "</td>"), fixed = TRUE)
  # Metadata: rules hash, evaluator, budget, perspective, generation time.
  expect_match(html, r$provenance$rules_hash, fixed = TRUE)
  expect_match(html, "side_to_act", fixed = TRUE)
  expect_match(html, "uncalibrated", fixed = TRUE)
  expect_match(html, "500 nodes", fixed = TRUE)
  expect_match(html, "truncated", fixed = TRUE)
  # The manifest lists every file with its checksum.
  f <- out$files
  expect_true(all(c("report.html", "tables/moves.csv", "tables/evaluations.csv", "tables/feature_dictionary.csv") %in% f$file))
  for (i in seq_len(nrow(f))) {
    expect_identical(digest::digest(file = file.path(dir, "rep", f$file[i]), algo = "sha256"), f$sha256[i])
  }
  mj <- jsonlite::fromJSON(file.path(dir, "rep", "manifest.json"))
  expect_identical(mj$files$sha256, f$sha256)
  expect_true(mj$offline)
  # Exported paths are relative to the report directory: no local paths in
  # the manifest or any exported file (the returned list keeps the local one).
  expect_identical(mj$report, "report.html")
  expect_true(is.null(mj$pdf) || is.na(mj$pdf))
  expect_identical(normalizePath(out$report), normalizePath(file.path(dir, "rep", "report.html")))
  local <- c(normalizePath(dir), dir, normalizePath(tempdir()), path.expand("~"))
  for (ff in c("manifest.json", f$file)) {
    txt <- paste(readLines(file.path(dir, "rep", ff), warn = FALSE), collapse = "\n")
    for (l in unique(local)) expect_false(grepl(l, txt, fixed = TRUE), info = paste(ff, l))
  }
  expect_error(g3_export_analysis_report(r, file.path(dir, "rep")), class = "g3_io_error")
  expect_error(g3_export_analysis_report(list(), file.path(dir, "x")), class = "g3_invalid_state")
})

test_that("collection and comparison reports render with their counts", {
  dir <- withr::local_tempdir()
  gs <- list(hostile_game(), g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2"))
  gs[[2]]$metadata$game_id <- "second"
  bad <- gs[[2]]
  bad$metadata$game_id <- "broken"
  bad$events$action_id[1] <- "m:W:0:1-W:3:3"
  run <- g3_analyse_database(c(gs, list(bad)), evaluator = "none")
  out <- g3_export_analysis_report(run, file.path(dir, "db"))
  html <- paste(readLines(file.path(dir, "db", "report.html"), warn = FALSE), collapse = "\n")
  expect_false(grepl("<script", html, ignore.case = TRUE))
  expect_identical(out$totals$completed, 2L)
  expect_identical(out$totals$errors, 1L)
  expect_match(html, "Scheduled games</th><td>3</td>", fixed = TRUE)
  err <- utils::read.csv(file.path(dir, "db", "tables", "errors.csv"))
  expect_identical(err$game_id, "broken")
  oc <- utils::read.csv(file.path(dir, "db", "tables", "outcomes.csv"))
  expect_identical(sum(oc$n_games), 2L)
  cmp <- g3_compare_agents(c("greedy", "random"), budget = list(nodes = 30L), seeds = 1:2, max_plies = 3L,
                           bootstrap = list(replicates = 50L))
  oc2 <- g3_export_analysis_report(cmp, file.path(dir, "cmp"))
  h2 <- paste(readLines(file.path(dir, "cmp", "report.html"), warn = FALSE), collapse = "\n")
  expect_match(h2, "role swap", fixed = TRUE)
  expect_identical(oc2$totals$scheduled, 4L)
  expect_identical(nrow(utils::read.csv(file.path(dir, "cmp", "tables", "games.csv"))), 4L)
  expect_true(all(grepl("^(#|tables/|data:image/png;base64,)", html_links(h2))))
})

test_that("PDF output is optional and explains a missing browser", {
  dir <- withr::local_tempdir()
  r <- g3_analyse_game(g3_game_apply(g3_new_game(), "m:W:0:1-W:0:2"), evaluator = "none")
  withr::local_envvar(PATH = dir)
  expect_warning(out <- g3_export_analysis_report(r, file.path(dir, "p"), format = "pdf"), class = "g3_warning")
  expect_true(is.na(out$pdf))
  expect_match(out$pdf_status, "Chromium")
  expect_true(file.exists(file.path(dir, "p", "report.html")))
})
