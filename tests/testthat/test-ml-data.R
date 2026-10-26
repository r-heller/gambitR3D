# Phase 12: tensors, datasets, outcome masking and R-side protocol checks.
# Pure R: no Python process.

test_that("safetensors files round-trip in R with a stable digest", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "t.safetensors")
  tens <- list(
    ints = list(dtype = "I32", shape = c(2L, 3L), data = c(1L, -2L, 3L, 4L, 5L, 2147483L)),
    floats = list(dtype = "F32", shape = 4L, data = c(0.5, -1.25, 3, 1e-3)),
    bytes = list(dtype = "U8", shape = c(1L, 2L), data = c(0L, 255L))
  )
  sha <- .g3_write_safetensors(tens, f, metadata = list(format = "test"))
  expect_identical(sha, .g3_file_sha256(f))
  r <- .g3_read_safetensors(f)
  expect_identical(r$ints$data, tens$ints$data)
  expect_equal(r$floats$data, tens$floats$data, tolerance = 1e-7)
  expect_identical(r$bytes$data, tens$bytes$data)
  expect_identical(r$ints$shape, c(2L, 3L))
  expect_identical(attr(r, "metadata")$format, "test")
  # The header length is 8-byte aligned, as safetensors writers do.
  con <- file(f, "rb")
  hl <- readBin(con, "integer", n = 2L, size = 4L, endian = "little")
  close(con)
  expect_identical(hl[1L] %% 8L, 0L)
  d1 <- .g3_weights_digest(f)
  .g3_write_safetensors(tens[c("floats", "bytes", "ints")], f)
  expect_identical(.g3_weights_digest(f), d1)
  expect_error(.g3_write_safetensors(list(x = list(dtype = "F32", shape = 1L, data = NaN)), f), class = "g3_io_error")
})

test_that("generated datasets are grouped by game, masked for truncation and verifiable", {
  dir <- withr::local_tempdir()
  man <- .g3_ml_build_dataset(list(games = 2L, positions = 8L, max_walk_plies = 6L, platform_positions = 3L,
                                   teacher = list(nodes = 60L, depth = 2L), fixture_nodes = 3000L,
                                   fixture_max_plies = 3L), dir)
  expect_s3_class(man, "g3_dataset")
  expect_identical(man$rules$hash, .g3_tables()$rules_hash)
  expect_identical(man$feature_schema_hash, .g3_feature_schema()$hash)
  expect_false(is.null(man$split_hash))
  expect_true(all(c("removed_from", "within_split_duplicates", "board_only_overlap_keys") %in% names(man$dedup)))
  v <- .g3_ml_verify_dataset(dir)
  expect_true(v$valid, info = paste(v$problems, collapse = "; "))
  expect_identical(v$checked, man$counts$records)
  recs <- unlist(lapply(man$records, function(rf) {
    lapply(readLines(file.path(dir, rf$file)), jsonlite::fromJSON, simplifyVector = FALSE)
  }), recursive = FALSE)
  # A game never spans two splits.
  sp <- tapply(vapply(recs, function(r) r$split, ""), vapply(recs, function(r) r$game_id, ""), function(x) length(unique(x)))
  expect_true(all(sp == 1L))
  # Truncated games carry no outcome; terminal fixture games do, from each
  # position's side to act (perspective flips between the players).
  for (r in recs) {
    if (identical(r$outcome_status, "truncated")) expect_null(r$outcome)
  }
  fl <- Filter(function(r) identical(r$game_id, "fixture-forced_loss"), recs)
  expect_identical(vapply(fl, function(r) r$state$side, ""), c("white", "black"))
  expect_identical(vapply(fl, function(r) r$outcome, ""), c("loss", "win"))
  m1 <- Filter(function(r) identical(r$game_id, "fixture-mate_in_one"), recs)
  expect_identical(m1[[1L]]$outcome, "win")
  # Shard outcome codes: -1 masks everything that is not a verified result.
  for (sh in man$shards) {
    t <- .g3_read_safetensors(file.path(dir, sh$file))
    rs <- Filter(function(r) identical(r$split, sh$split), recs)
    code <- vapply(rs, function(r) if (identical(r$outcome_status, "terminal")) .G3_OUTCOME_CODE[[r$outcome]] else -1L, 0L)
    expect_identical(t$outcome$data, code)
    expect_true(all(abs(rowsum(t$policy_target$data, rep(seq_len(sh$n), each = sh$max_candidates)) - 1) < 1e-6))
  }
  # Tampering is detected.
  sh <- man$shards[[1L]]
  cat(" ", file = file.path(dir, sh$file), append = TRUE)
  expect_false(.g3_ml_verify_dataset(dir)$valid)
})

test_that("WDL perspective flips swap win and loss only", {
  w <- c(win = 0.6, draw = 0.3, loss = 0.1)
  f <- .g3_wdl_flip(w)
  expect_identical(names(f), c("win", "draw", "loss"))
  expect_equal(unname(f), c(0.1, 0.3, 0.6))
  expect_equal(unname(f[["win"]] - f[["loss"]]), -unname(w[["win"]] - w[["loss"]]))
  st <- g3_apply_action(g3_setup_position(pcs("wK K white WK:0:0", "bQ Q black B:0:0", "bRd R black N:2:3",
                                               "bRf R black BK:1:1", "bK K black B:0:3"), side = "black"),
                        "m:B:0:0-B:3:0")$state
  expect_identical(.g3_terminal_value(st), -1)
  expect_identical(unname(.g3_terminal_wdl(st)), c(0, 0, 1))
})

stub_worker <- function() {
  w <- new.env()
  w$proc <- list(is_alive = function() FALSE)
  w$log <- "none"
  w$pid <- NA_integer_
  w
}

test_that("worker replies are validated in R before use", {
  enc <- .g3_encode_state(g3_new_game()$state)
  n <- length(enc$action_ids)
  ok <- list(candidate_digest = enc$candidate_digest, n_candidates = n, logits = as.list(seq_len(n) / 10),
             wdl_logits = list(0.2, 0.1, -0.3), aux_heuristic = 0)
  out <- .g3_ml_check_output(stub_worker(), ok, enc)
  expect_equal(sum(out$probability), 1)
  expect_identical(out$action_ids, enc$action_ids)
  expect_equal(out$value, unname(out$wdl[["win"]] - out$wdl[["loss"]]))
  expect_equal(out$expected_score, unname(out$wdl[["win"]] + 0.5 * out$wdl[["draw"]]))
  expect_identical(which.max(out$probability), n)
  bad <- ok
  bad$candidate_digest <- strrep("0", 64)
  expect_error(.g3_ml_check_output(stub_worker(), bad, enc), "digest", class = "g3_incompatible_model")
  bad <- ok
  bad$logits <- bad$logits[-1L]
  expect_error(.g3_ml_check_output(stub_worker(), bad, enc), class = "g3_io_error")
  bad <- ok
  bad$logits[[2L]] <- NULL
  bad$logits <- c(bad$logits, list(NULL))
  expect_error(.g3_ml_check_output(stub_worker(), bad, enc), class = "g3_io_error")
  bad <- ok
  bad$wdl_logits <- list(1, "x", 2)
  expect_error(.g3_ml_check_output(stub_worker(), bad, enc), class = "g3_io_error")
  # Oversized requests are refused before anything is written.
  w <- stub_worker()
  w$proc <- list(is_alive = function() TRUE, write_input = function(...) stop("must not write"))
  big <- list(protocol_version = "1.0", payload = strrep("x", .G3_MAX_LINE_BYTES + 1L))
  expect_error(.g3_worker_send(w, big), class = "g3_budget_exceeded")
})

test_that("training limits enforce the CPU smoke envelope", {
  expect_identical(.g3_ml_limits()$max_steps, 20L)
  expect_error(.g3_ml_limits(list(max_steps = 1000L)), "authorized", class = "g3_budget_exceeded")
  expect_identical(.g3_ml_limits(list(max_steps = 1000L, authorized = TRUE))$max_steps, 1000L)
  expect_error(.g3_train_config(list(learning_rate = 1)), class = "g3_budget_exceeded")
  expect_error(g3_setup_ml(""), class = "g3_io_error")
})

test_that("the protocol schema documents every worker operation", {
  doc <- jsonlite::fromJSON(system.file("schemas", "worker-protocol-v1.json", package = "gambitR3D"), simplifyVector = FALSE)
  ops <- c("hello", "initialize_model", "infer_batch", "train_chunk", "save_checkpoint", "load_checkpoint", "cancel", "shutdown")
  expect_true(all(ops %in% names(doc$operations)))
  py <- readLines(system.file("python", "gambitr3d_worker", "protocol.py", package = "gambitR3D"))
  line <- paste(py[grep("^OPERATIONS = ", py) + 0:1], collapse = " ")
  expect_true(all(vapply(ops, function(o) grepl(paste0('"', o, '"'), line, fixed = TRUE), TRUE)))
  expect_equal(doc$liveness$heartbeat_seconds, .G3_HEARTBEAT_S)
  expect_equal(doc$liveness$stale_after_seconds, .G3_STALE_S)
  expect_identical(as.integer(doc$operations$hello$result$max_line_bytes), .G3_MAX_LINE_BYTES)
})
