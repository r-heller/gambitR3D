# Phase 01: the frozen experimental profile.

test_that("profile loads, validates and carries a content hash", {
  prof <- g3_rules_profile()
  expect_s3_class(prof, "g3_rules_profile")
  expect_identical(prof$rules_version, "experimental-v0.1")
  expect_match(prof$rules_hash, "^[0-9a-f]{64}$")
  expect_true(all(prof$decisions$origin %in% c("supplied_concept", "proposed_resolution")))
  expect_true(sum(prof$decisions$origin == "proposed_resolution") >= 10L)
  expect_error(g3_rules_profile("official-v1"), class = "g3_unsupported_rules")
})

test_that("changing a rule changes the hash; rewording a rationale does not", {
  path <- system.file("rules", "experimental-v0.1.json", package = "gambitR3D")
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  base <- .g3_profile_from_list(raw)$rules_hash
  reworded <- raw
  reworded$decisions[[1]]$rationale <- "Different words, same rule."
  expect_identical(.g3_profile_from_list(reworded)$rules_hash, base)
  changed <- raw
  changed$semantics$platform_actions$creates_en_passant <- TRUE
  expect_false(identical(.g3_profile_from_list(changed)$rules_hash, base))
  # An unsupported semantic value is rejected, not reinterpreted.
  bad <- raw
  bad$semantics$movement$ray_intermediate_blockers <- "same_board_only"
  expect_error(.g3_profile_from_list(bad), class = "g3_unsupported_rules")
})

test_that("64 distinct physical squares and 32 starting pieces", {
  tabs <- .g3_tables()
  expect_length(tabs$sq_id, 64L)
  expect_false(anyDuplicated(tabs$sq_id) > 0L)
  st <- g3_new_game()$state
  expect_identical(nrow(st$pieces), 32L)
  expect_identical(sum(st$pieces$color == "white"), 16L)
  expect_false(anyDuplicated(st$pieces$square_id) > 0L)
})

test_that("main boards cover the specified files and ranks", {
  tabs <- .g3_tables()
  co <- .g3_square_coords(tabs$init_dock, integer(4L), tabs)
  for (b in c("W", "N", "B")) {
    idx <- which(tabs$board_ids[tabs$sq_board] == b)
    expect_identical(sort(unique(co$file[idx])), 1:4)
  }
  rk <- function(b) sort(unique(co$rank[tabs$board_ids[tabs$sq_board] == b]))
  expect_identical(rk("W"), 1:4)
  expect_identical(rk("N"), 3:6)
  expect_identical(rk("B"), 5:8)
})

test_that("W/N and N/B share exactly eight projected coordinates each", {
  tabs <- .g3_tables()
  co <- .g3_square_coords(tabs$init_dock, integer(4L), tabs)
  coords <- function(b) co$ci[tabs$board_ids[tabs$sq_board] == b]
  expect_length(intersect(coords("W"), coords("N")), 8L)
  expect_length(intersect(coords("N"), coords("B")), 8L)
  expect_length(intersect(coords("W"), coords("B")), 0L)
  # b1 exists twice at the start: W:0:0 (knight) and WQ:1:1 (pawn); b8 too.
  b1 <- which(co$ci == 1L * 10L + 1L + 1L)
  expect_setequal(tabs$sq_id[b1], c("W:0:0", "WQ:1:1"))
  b8 <- which(co$ci == 1L * 10L + 8L + 1L)
  expect_setequal(tabs$sq_id[b8], c("B:0:3", "BQ:1:0"))
})

test_that("orientation 0 and 180 transforms", {
  tabs <- .g3_tables()
  d <- match("N:R:hi", tabs$dock_ids)
  # N:R:hi: min file e (4), min rank 3 + 3 = 6.
  co0 <- .g3_square_coords(c(1L, d, 11L, 12L), c(0L, 0L, 0L, 0L), tabs)
  co180 <- .g3_square_coords(c(1L, d, 11L, 12L), c(0L, 180L, 0L, 0L), tabs)
  wk <- 53:56 # WK:0:0, WK:1:0, WK:0:1, WK:1:1
  expect_identical(.g3_coord_label(co0$file[wk], co0$rank[wk]), c("e6", "f6", "e7", "f7"))
  expect_identical(.g3_coord_label(co180$file[wk], co180$rank[wk]), c("f7", "e7", "f6", "e6"))
})

test_that("dock graph: 12 nodes, 24 edges, symmetric, degrees 3/4/5/5/4/3, no W-B edge", {
  tabs <- .g3_tables()
  adj <- tabs$dock_adj
  expect_length(adj, 12L)
  edges <- do.call(rbind, lapply(seq_along(adj), function(i) cbind(i, adj[[i]])))
  und <- unique(t(apply(edges, 1L, sort)))
  expect_identical(nrow(und), 24L)
  for (i in seq_along(adj)) for (j in adj[[i]]) expect_true(i %in% adj[[j]])
  deg <- stats::setNames(lengths(adj), tabs$dock_ids)
  for (side in c("L", "R")) {
    order <- paste(c("W", "W", "N", "N", "B", "B"), side, c("lo", "hi"), sep = ":")
    expect_identical(unname(deg[order]), c(3L, 4L, 5L, 5L, 4L, 3L))
  }
  par <- substr(tabs$dock_ids, 1L, 1L)
  expect_false(any(par[und[, 1L]] == "W" & par[und[, 2L]] == "B" | par[und[, 1L]] == "B" & par[und[, 2L]] == "W"))
})

test_that("adjacency table equals the independently generated graph", {
  prof <- g3_rules_profile()
  gen <- .g3_generate_dock_graph(prof$semantics$docks)
  for (d in names(gen)) {
    expect_setequal(unlist(prof$semantics$dock_adjacency[[d]]), gen[[d]])
  }
})

test_that("display preferences are not part of the semantic profile", {
  txt <- jsonlite::toJSON(g3_rules_profile()$semantics, auto_unbox = TRUE)
  expect_false(grepl("neutral|highest|lowest|camera|parent_offset|display", txt))
})
