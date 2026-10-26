# Phase 13: deterministic explanation layer (engine facts only).

test_that("overlapping coordinates are named with their board identity", {
  st <- g3_new_game()$state
  n <- g3_explain_action(st, "m:W:0:0-N:1:0")
  w <- g3_explain_action(st, "m:W:0:0-W:1:2")
  expect_identical(n$facts$to_square, "N:1:0")
  expect_identical(n$facts$to_name, "N:c3")
  expect_identical(n$facts$twin_names, "W:c3")
  expect_identical(w$facts$to_name, "W:c3")
  expect_identical(w$facts$twin_names, "N:c3")
  expect_match(n$text[1], "from W:b1 to N:c3", fixed = TRUE)
  expect_match(w$text[1], "from W:b1 to W:c3", fixed = TRUE)
  twin <- n$claims[n$claims$template == "fact.twin_squares", ]
  expect_identical(twin$type, "legal_fact")
  expect_match(twin$text, "physical square N:c3 on board N", fixed = TRUE)
  # Same coordinate, different physical square, different sentence.
  expect_false(identical(n$text, w$text))
})

test_that("platform transport is explained with carried piece, docks, rotation and coordinates", {
  st <- g3_setup_position(pcs("wR R white WQ:0:0", "wK K white WK:0:0", "bK K black B:0:3", "bP P black B:3:2"))
  turned <- g3_explain_action(st, "t:WQ>N:L:lo@180")
  kept <- g3_explain_action(st, "t:WQ>N:L:lo@0")
  f <- turned$facts
  expect_identical(f$platform, "WQ")
  expect_identical(c(f$from_dock, f$to_dock), c("W:L:lo", "N:L:lo"))
  expect_identical(c(f$orient_before, f$orient_after), c(0L, 180L))
  # The rook keeps its physical square; only the coordinate changes.
  expect_identical(f$from_square, "WQ:0:0")
  expect_identical(c(f$coord_before, f$coord_after), c("a0", "b3"))
  expect_identical(kept$facts$coord_after, "a2")
  expect_true(f$gives_check)
  expect_false(kept$facts$gives_check)
  tpl <- turned$claims$template
  expect_true(all(c("action.platform_turn", "fact.platform_coords", "fact.platform_orientation",
                    "fact.check_given") %in% tpl))
  expect_false("fact.platform_orientation" %in% kept$claims$template)
  expect_match(paste(turned$text, collapse = " "), "turns it by 180 degrees", fixed = TRUE)
  expect_match(paste(turned$text, collapse = " "), "physical square WQ:0:0; its coordinate changes from a0 to b3",
               fixed = TRUE)
  expect_match(paste(turned$text, collapse = " "), "king on B:b8", fixed = TRUE)
  # Carrying a rook off its home platform gives up nothing here (no rights),
  # but the claim set stays typed.
  expect_true(all(turned$claims$type %in% c("legal_fact", "heuristic")))
})

test_that("check evasions are classified from engine facts", {
  # Platform carries the king out of check.
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bN N black W:2:1", "bR1 R black W:0:0", "bR2 R black BK:1:1", "bK K black B:0:3"
  ))
  e <- g3_explain_action(st, "t:WK>N:R:lo@0")
  expect_true(e$facts$was_in_check)
  expect_identical(e$facts$escape, "platform_king")
  expect_identical(e$facts$checker_names, "W:d2")
  expect_true("fact.escape.platform_king" %in% e$claims$template)
  expect_match(paste(e$text, collapse = " "), "in check from Black's knight on W:d2", fixed = TRUE)
  # A rook captures the checking rook.
  st2 <- g3_setup_position(pcs("wK K white WK:0:0", "wR R white W:0:2", "bR R black W:3:2", "bK K black B:3:3"))
  e2 <- g3_explain_action(st2, "m:W:0:2-W:3:2")
  expect_identical(e2$facts$escape, "capture_checker")
  expect_true(all(c("fact.capture", "fact.escape.capture_checker") %in% e2$claims$template))
  # The king steps aside.
  e3 <- g3_explain_action(st2, "m:WK:0:0-W:2:0")
  expect_identical(e3$facts$escape, "king_moves")
  # Moves that do not resolve the check are illegal, with a precise reason.
  err <- tryCatch(g3_explain_action(st2, "m:WK:0:0-WK:0:1"), g3_illegal_action = function(e) e)
  expect_s3_class(err, "g3_illegal_action")
  expect_identical(err$context$reason, "self_check")
})

test_that("checkmate, captures and heuristics carry the right claim types", {
  st <- g3_setup_position(pcs(
    "wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  e <- g3_explain_action(st, "m:B:0:0-N:3:2")
  expect_identical(e$facts$outcome$reason, "checkmate")
  mate <- e$claims[e$claims$template == "fact.checkmate", ]
  expect_identical(mate$type, "legal_fact")
  expect_identical(mate$strength, "exact")
  expect_match(mate$text, "White's king on WK:e0", fixed = TRUE)
  # Coordinate e5 exists on N and B; the explanation says which one.
  expect_identical(e$facts$twin_names, "B:e5")
  heur <- e$claims[e$claims$type == "heuristic", ]
  expect_true(all(!is.na(heur$definition) & nzchar(heur$definition)))
  expect_false(any(grepl("centipawn|\\bElo\\b|rating", e$text, ignore.case = TRUE)))
  # Capture of a queen.
  st2 <- g3_setup_position(pcs("wR R white W:0:0", "wK K white WK:0:0", "bQ Q black W:0:3", "bK K black B:3:3"))
  e2 <- g3_explain_action(st2, "m:W:0:0-W:0:3")
  expect_true(e2$facts$capture)
  expect_identical(e2$facts$victim, "Q")
  expect_match(paste(e2$text, collapse = " "), "captures Black's queen on W:b4", fixed = TRUE)
})

test_that("German catalogue is complete and selected by lang", {
  expect_identical(.g3_coach_catalogue_problems("de"), character())
  en <- .g3_coach_catalogue("en")
  de <- .g3_coach_catalogue("de")
  expect_setequal(names(en$templates), names(de$templates))
  expect_setequal(names(en$terms), names(de$terms))
  st <- g3_new_game()$state
  d <- g3_explain_action(st, "m:W:0:0-N:1:0", lang = "de")
  expect_identical(d$lang, "de")
  expect_match(d$text[1], "Springer von W:b1 nach N:c3", fixed = TRUE)
  expect_identical(d$claims$template, g3_explain_action(st, "m:W:0:0-N:1:0")$claims$template)
  expect_error(g3_explain_action(st, "m:W:0:0-N:1:0", lang = "fr"), class = "g3_invalid_state")
  # Missing placeholders are an error, never a silently empty sentence.
  expect_error(.g3_tr("action.move", "en", color = "White"), class = "g3_io_error")
})

test_that("every template used by the code exists in both catalogues", {
  files <- list.files(test_path("..", "..", "R"), pattern = "^(coach.*|exercises|learner)\\.R$", full.names = TRUE)
  skip_if(!length(files), "package sources not available")
  src <- paste(unlist(lapply(files, readLines)), collapse = "\n")
  keys <- unique(regmatches(src, gregexpr('"(action|fact|heur|def|limit|review|suggest|objective|hint|attempt|reason|tutor|plan|score|illegal|item)\\.[a-z0-9_.]+"', src))[[1]])
  keys <- gsub('"', "", keys)
  keys <- keys[!grepl("[._]$", keys)]
  for (lang in c("en", "de")) {
    cat <- .g3_coach_catalogue(lang)
    missing <- setdiff(keys, c(names(cat$templates), names(cat$terms)))
    expect_identical(missing, character(), info = lang)
  }
})
