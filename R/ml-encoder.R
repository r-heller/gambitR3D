# Neural-network feature encoder (feature schema g3-features/1).
#
# This is the single authoritative encoder: the Python worker only receives
# its integer output. A position becomes 72 tokens (64 physical squares in
# canonical order W, N, B, WQ, WK, BQ, BK by local v then u; 7 board
# tokens; 1 global token) with integer categorical slots (0 = not
# applicable) and integer numeric numerators. Every legal action from the
# R engine becomes one candidate row, in the engine's canonical order,
# without truncation. Slot meanings, vocabularies and divisors are defined
# in `inst/schemas/features-v1.json`; its hash (SHA-256 of the canonical
# JSON) identifies the schema in manifests and in the worker handshake.
#
# Only logical game state enters: display preferences (neutral height,
# view, camera, colours), piece id strings, ply and wall-clock time are not
# part of the state and cannot be encoded. Repetition is summarised (count
# of the current key, record length): a lossy value feature, while exact
# draw adjudication stays with the engine.

#' @noRd
.g3_feature_schema <- function() {
  if (!is.null(.g3_env$ml_feature_schema)) return(.g3_env$ml_feature_schema)
  f <- system.file("schemas", "features-v1.json", package = "gambitR3D")
  if (!nzchar(f)) .g3_abort("g3_io_error", "The feature schema document is missing.")
  doc <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  nm <- function(x) vapply(x, function(s) s$name, "")
  iv <- function(x, field) vapply(x, function(s) as.integer(s[[field]]), 0L)
  sch <- list(
    name = doc$schema,
    hash = .g3_hash_object(doc),
    path = f,
    n_tokens = as.integer(doc$tokens$count),
    token_cat = nm(doc$token_categorical), token_vocab = iv(doc$token_categorical, "vocab"),
    token_num = nm(doc$token_numeric), token_div = iv(doc$token_numeric, "divisor"),
    cand_cat = nm(doc$candidate_categorical), cand_vocab = iv(doc$candidate_categorical, "vocab"),
    cand_num = nm(doc$candidate_numeric), cand_div = iv(doc$candidate_numeric, "divisor")
  )
  .g3_env$ml_feature_schema <- sch
  sch
}

# Identity of the action schema used by the candidate encoder.
#' @noRd
.g3_action_schema_hash <- function() .g3_sha256(.G3_ACTION_SCHEMA)

#' @noRd
.g3_candidate_digest <- function(ids) .g3_sha256(paste(ids, collapse = "\n"))

# Piece code shared by square tokens and candidates: 1 empty, 2..7 white
# P..K, 8..13 black P..K.
#' @noRd
.g3_enc_piece_code <- function(core, p) (core$pcolor[p] - 1L) * 6L + core$ptype[p] + 1L

# Encode a core position and its legal actions. Returns integer matrices
# `tokens_cat` [72 x 29], `tokens_num` [72 x 5], `cand_cat` [A x 18],
# `cand_num` [A x 2], the canonical `action_ids` and `candidate_digest`.
#' @noRd
.g3_encode_core <- function(core, tabs = .g3_tables(), ctx = .g3_ctx(core, tabs),
                            legal = .g3_legal(core, tabs, ctx)) {
  sch <- .g3_feature_schema()
  tok <- matrix(0L, sch$n_tokens, length(sch$token_cat), dimnames = list(NULL, sch$token_cat))
  num <- matrix(0L, sch$n_tokens, length(sch$token_num), dimnames = list(NULL, sch$token_num))
  s <- 1:64
  occ <- core$occ
  has <- occ > 0L
  pi <- occ[has]
  sqb <- tabs$sq_board
  is_plat <- sqb >= 4L
  plat <- pmax(sqb - 3L, 1L)

  # Square tokens.
  pcode <- rep(1L, 64L)
  pcode[has] <- .g3_enc_piece_code(core, pi)
  promoted <- integer(64L)
  promoted[has] <- ifelse(core$porigin[pi] == 1L & core$ptype[pi] != 1L, 2L, 1L)
  moved <- integer(64L)
  moved[has] <- 1L + as.integer(core$pmoved[pi])
  ep_sq <- rep(1L, 64L)
  if (length(core$ep)) {
    ep_sq[ctx$ci == core$ep[3L] * 10L + core$ep[4L] + 1L] <- 3L
    ep_sq[core$ep[2L]] <- 2L
  }
  tok[s, "token_type"] <- 1L
  tok[s, "square"] <- s
  tok[s, "board"] <- sqb
  tok[s, "piece"] <- pcode
  tok[s, "promoted"] <- promoted
  tok[s, "has_moved"] <- moved
  tok[s, "local_u"] <- tabs$sq_u + 1L
  tok[s, "local_v"] <- tabs$sq_v + 1L
  tok[s, "file"] <- ctx$file + 1L
  tok[s, "rank"] <- ctx$rank + 1L
  tok[s, "parent"] <- ifelse(is_plat, 3L + core$dock[plat], sqb)
  tok[s, "orientation"] <- ifelse(is_plat, 1L + as.integer(core$orient[plat] == 180L), 1L)
  tok[s, "coord_squares"] <- pmin(tabulate(ctx$ci, 60L)[ctx$ci], 5L)
  tok[s, "coord_occupants"] <- pmin(ctx$cnt[ctx$ci], 4L) + 1L
  tok[s, "en_passant"] <- ep_sq
  num[s, "file_scaled"] <- ctx$file
  num[s, "rank_scaled"] <- ctx$rank

  # Board tokens.
  b <- 64L + 1:7
  bocc <- vapply(1:7, function(k) sum(has[sqb == k]), 0L)
  ctrl <- rep(1L, 7L)
  for (k in 4:7) {
    if (bocc[k] == 1L) ctrl[k] <- 1L + core$pcolor[occ[sqb == k & has]]
  }
  tok[b, "token_type"] <- 2L
  tok[b, "board"] <- 1:7
  tok[b, "parent"] <- c(1:3, 3L + core$dock)
  tok[b, "orientation"] <- c(1L, 1L, 1L, 1L + as.integer(core$orient == 180L))
  tok[b, "board_kind"] <- c(1L, 1L, 1L, 2L, 2L, 2L, 2L)
  tok[b, "occupants"] <- bocc + 1L
  tok[b, "controller"] <- ctrl
  num[b, "occupants_scaled"] <- bocc

  # Global token.
  g <- sch$n_tokens
  tok[g, "token_type"] <- 3L
  tok[g, "side"] <- core$side
  tok[g, c("castle_white_kingside", "castle_white_queenside", "castle_black_kingside", "castle_black_queenside")] <-
    1L + as.integer(core$castle)
  tok[g, c("first_turn_white", "first_turn_black")] <- 1L + as.integer(core$first)
  if (length(core$ep)) {
    tok[g, "ep_file"] <- core$ep[3L] + 2L
    tok[g, "ep_rank"] <- core$ep[4L] + 2L
  } else {
    tok[g, c("ep_file", "ep_rank")] <- 1L
  }
  tok[g, "ep_effective"] <- 1L + as.integer(.g3_ep_effective(core, ctx, tabs))
  key <- core$rep[length(core$rep)]
  tok[g, "repetition"] <- if (length(key)) min(max(sum(core$rep == key), 1L), 3L) else 1L
  num[g, "halfmove_scaled"] <- min(core$halfmove, 100L)
  num[g, "repetition_record_scaled"] <- min(length(core$rep), 100L)

  cand <- .g3_encode_candidates(core, legal, tabs, ctx, sch)
  ids <- .g3_action_ids(core, legal, tabs)
  list(
    tokens_cat = tok, tokens_num = num, cand_cat = cand$cat, cand_num = cand$num,
    action_ids = ids, candidate_digest = .g3_candidate_digest(ids)
  )
}

# Candidate rows for an internal legal-action matrix `m`.
#' @noRd
.g3_encode_candidates <- function(core, m, tabs, ctx, sch = .g3_feature_schema()) {
  n <- nrow(m)
  cat <- matrix(0L, n, length(sch$cand_cat), dimnames = list(NULL, sch$cand_cat))
  num <- matrix(0L, n, length(sch$cand_num), dimnames = list(NULL, sch$cand_num))
  if (!n) return(list(cat = cat, num = num))
  type <- m[, "type"]
  from <- m[, "from"]
  piece <- m[, "piece"]
  is_pl <- type == 2L
  is_cs <- type == 3L
  pl <- pmax(m[, "plat"], 1L)
  # A platform action keeps the carried piece on its physical square.
  dest <- ifelse(is_pl, from, m[, "to"])
  cap <- m[, "cap"]
  new_or <- (core$orient[pl] + m[, "rot"]) %% 360L
  rf <- ctx$file[dest]
  rr <- ctx$rank[dest]
  if (any(is_pl)) {
    k <- which(is_pl)
    cell <- from[k] - (48L + (pl[k] - 1L) * 4L)
    o <- ifelse(new_or[k] == 180L, 2L, 1L)
    rf[k] <- tabs$plat_file[cbind(m[k, "dock"], o, cell)]
    rr[k] <- tabs$plat_rank[cbind(m[k, "dock"], o, cell)]
  }
  rook_src <- integer(n)
  rook_dst <- integer(n)
  for (i in which(is_cs)) {
    def <- tabs$castle[[core$side]][[m[i, "cside"]]]
    rook_src[i] <- def$rook_from
    rook_dst[i] <- def$rook_to
  }
  cat[, "action_type"] <- type
  cat[, "source"] <- from
  cat[, "destination"] <- dest
  cat[, "piece"] <- .g3_enc_piece_code(core, piece)
  cat[, "captured"] <- ifelse(cap > 0L, .g3_enc_piece_code(core, pmax(cap, 1L)), 1L)
  cat[, "promotion"] <- ifelse(m[, "promo"] > 0L, m[, "promo"], 1L)
  cat[, "platform"] <- ifelse(is_pl, pl + 1L, 1L)
  cat[, "from_dock"] <- ifelse(is_pl, core$dock[pl] + 1L, 1L)
  cat[, "to_dock"] <- ifelse(is_pl, m[, "dock"] + 1L, 1L)
  cat[, "rotation"] <- ifelse(is_pl, 2L + as.integer(m[, "rot"] == 180L), 1L)
  cat[, "result_orientation"] <- ifelse(is_pl, 2L + as.integer(new_or == 180L), 1L)
  cat[, "castle_side"] <- ifelse(is_cs, m[, "cside"] + 1L, 1L)
  cat[, "special"] <- m[, "flag"] + 1L
  cat[, "result_file"] <- rf + 1L
  cat[, "result_rank"] <- rr + 1L
  cat[, "destination_board"] <- tabs$sq_board[dest]
  cat[, "rook_source"] <- rook_src
  cat[, "rook_destination"] <- rook_dst
  num[, "file_displacement"] <- rf - ctx$file[from]
  num[, "rank_displacement"] <- rr - ctx$rank[from]
  list(cat = cat, num = num)
}

# Encode a public state (convenience for tests and tools).
#' @noRd
.g3_encode_state <- function(state) {
  .g3_check_state(state)
  tabs <- .g3_tables()
  core <- .g3_core(state, tabs)
  ctx <- .g3_ctx(core, tabs)
  legal <- if (identical(state$outcome$status, "terminal")) .g3_legal(core, tabs, ctx)[0L, , drop = FALSE] else .g3_legal(core, tabs, ctx)
  .g3_encode_core(core, tabs, ctx, legal)
}

# Check an encoding against the schema's shapes and vocabularies.
#' @noRd
.g3_check_encoding <- function(enc, sch = .g3_feature_schema()) {
  p <- character()
  if (!identical(dim(enc$tokens_cat), c(sch$n_tokens, length(sch$token_cat)))) p <- c(p, "token shape")
  if (any(enc$tokens_cat < 0L) || any(sweep(enc$tokens_cat, 2L, sch$token_vocab, ">="))) p <- c(p, "token vocabulary")
  if (ncol(enc$cand_cat) != length(sch$cand_cat) || nrow(enc$cand_cat) != length(enc$action_ids)) p <- c(p, "candidate shape")
  if (nrow(enc$cand_cat) && (any(enc$cand_cat < 0L) || any(sweep(enc$cand_cat, 2L, sch$cand_vocab, ">=")))) {
    p <- c(p, "candidate vocabulary")
  }
  if (!identical(enc$candidate_digest, .g3_candidate_digest(enc$action_ids))) p <- c(p, "candidate digest")
  p
}

# One position as the infer_batch payload element (plain lists for JSON).
#' @noRd
.g3_enc_payload <- function(enc) {
  list(
    tokens_cat = unname(enc$tokens_cat), tokens_num = unname(enc$tokens_num),
    cand_cat = unname(enc$cand_cat), cand_num = unname(enc$cand_num),
    candidate_ids = as.list(enc$action_ids), candidate_digest = enc$candidate_digest
  )
}

# Pad encodings into batch arrays in row-major order (for shards). Returns
# a list of arrays as row-major vectors with shapes, ready for safetensors.
#' @noRd
.g3_enc_stack <- function(encs, a_max = NULL) {
  sch <- .g3_feature_schema()
  n <- length(encs)
  counts <- vapply(encs, function(e) length(e$action_ids), 0L)
  a <- max(c(1L, counts, a_max))
  kc <- length(sch$cand_cat)
  kn <- length(sch$cand_num)
  cand_cat <- integer(n * a * kc)
  cand_num <- integer(n * a * kn)
  for (i in seq_len(n)) {
    e <- encs[[i]]
    k <- counts[i]
    if (!k) next
    # Row-major rows for position i: a block of a * kc values.
    cand_cat[(i - 1L) * a * kc + seq_len(k * kc)] <- as.vector(t(e$cand_cat))
    cand_num[(i - 1L) * a * kn + seq_len(k * kn)] <- as.vector(t(e$cand_num))
  }
  list(
    tok_cat = list(dtype = "I32", shape = c(n, sch$n_tokens, length(sch$token_cat)),
                   data = unlist(lapply(encs, function(e) as.vector(t(e$tokens_cat))), use.names = FALSE)),
    tok_num = list(dtype = "I32", shape = c(n, sch$n_tokens, length(sch$token_num)),
                   data = unlist(lapply(encs, function(e) as.vector(t(e$tokens_num))), use.names = FALSE)),
    cand_cat = list(dtype = "I32", shape = c(n, a, kc), data = cand_cat),
    cand_num = list(dtype = "I32", shape = c(n, a, kn), data = cand_num),
    n_cand = list(dtype = "I32", shape = n, data = counts)
  )
}
