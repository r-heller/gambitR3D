# Rules profiles.
#
# A profile is a JSON document under inst/rules/. Its `semantics` section
# holds every resolved rule value; its `decisions` section holds the
# human-readable rationale. The rules hash is computed from the profile id
# and the canonical semantics only, so rewording a rationale never changes
# the hash while changing a rule always does.

.G3_SUPPORTED_PROFILES <- "experimental-v0.1"

# Package-level cache for loaded profiles and derived engine tables.
.g3_env <- new.env(parent = emptyenv())

#' Read the experimental rules profile
#'
#' Loads, validates and hashes a rules profile shipped with the package.
#' The profile records every rule the engine applies, and a decision ledger
#' that marks which rules come from the supplied concept and which are
#' proposed resolutions of open questions.
#'
#' @param profile Profile id. Only `"experimental-v0.1"` is supported.
#' @return A `g3_rules_profile` list with `profile_id`, `rules_version`,
#'   `rules_hash` (SHA-256 of the canonical semantics, hexadecimal),
#'   `semantics`, `decisions` (a data frame) and `status`.
#' @examples
#' # Inspect the rules profile and its decision ledger.
#' prof <- gambitR3D::g3_rules_profile()
#' prof$rules_hash
#' utils::head(prof$decisions[, c("id", "topic", "origin")])
#' @export
g3_rules_profile <- function(profile = "experimental-v0.1") {
  if (!is.character(profile) || length(profile) != 1L || is.na(profile)) {
    .g3_abort("g3_unsupported_rules", "`profile` must be a single profile id.")
  }
  if (!profile %in% .G3_SUPPORTED_PROFILES) {
    .g3_abort(
      "g3_unsupported_rules",
      paste0("Unsupported rules profile '", profile, "'. Supported: ",
             paste(.G3_SUPPORTED_PROFILES, collapse = ", "), "."),
      profile = profile
    )
  }
  key <- paste0("profile:", profile)
  if (!is.null(.g3_env[[key]])) {
    return(.g3_env[[key]])
  }
  path <- system.file("rules", paste0(profile, ".json"), package = "gambitR3D")
  if (!nzchar(path)) {
    .g3_abort("g3_io_error", paste0("Rules file missing for profile ", profile, "."))
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  prof <- .g3_profile_from_list(raw)
  .g3_env[[key]] <- prof
  prof
}

# Build a validated profile object from parsed JSON. Exposed internally so
# tests can check that edited semantics change the hash.
#' @noRd
.g3_profile_from_list <- function(raw) {
  .g3_validate_profile(raw)
  hash <- .g3_hash_object(list(profile_id = raw$profile_id, semantics = raw$semantics))
  dec <- raw$decisions
  decisions <- data.frame(
    id = vapply(dec, function(d) d$id, ""),
    topic = vapply(dec, function(d) d$topic, ""),
    origin = vapply(dec, function(d) d$origin, ""),
    path = vapply(dec, function(d) d$path, ""),
    rationale = vapply(dec, function(d) d$rationale, ""),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      profile_id = raw$profile_id,
      rules_version = raw$profile_id,
      rules_hash = hash,
      status = raw$status,
      format = raw$format,
      semantics = raw$semantics,
      decisions = decisions
    ),
    class = "g3_rules_profile"
  )
}

#' @export
print.g3_rules_profile <- function(x, ...) {
  cat("<g3_rules_profile> ", x$profile_id, "\n", sep = "")
  cat("  status:     ", x$status, "\n", sep = "")
  cat("  rules hash: ", x$rules_hash, "\n", sep = "")
  n_prop <- sum(x$decisions$origin == "proposed_resolution")
  cat("  decisions:  ", nrow(x$decisions), " (", n_prop,
      " proposed resolutions, not official rules)\n", sep = "")
  invisible(x)
}

# Validate every semantic value the engine relies on. Unknown values are
# rejected rather than reinterpreted.
#' @noRd
.g3_validate_profile <- function(raw) {
  fail <- function(msg) .g3_abort("g3_unsupported_rules", paste0("Invalid rules profile: ", msg))
  if (!is.list(raw) || is.null(raw$semantics)) fail("no semantics section.")
  if (!identical(raw$profile_id, "experimental-v0.1")) fail("unknown profile id.")
  if (!identical(raw$format, "gambitR3D-rules/1")) fail("unknown format.")
  s <- raw$semantics

  # Coordinates.
  if (!identical(unlist(s$coordinates$files), c("a", "b", "c", "d", "e", "f"))) fail("files.")
  if (!identical(c(s$coordinates$rank_min, s$coordinates$rank_max), c(0L, 9L))) fail("ranks.")

  # Boards.
  mb <- s$main_boards
  if (length(mb) != 3L) fail("three main boards required.")
  ids <- vapply(mb, function(b) b$id, "")
  if (!identical(ids, c("W", "N", "B"))) fail("main board ids.")
  if (!all(vapply(mb, function(b) identical(b$size, 4L), TRUE))) fail("main board size.")
  pl <- s$platforms
  pids <- vapply(pl, function(p) p$id, "")
  if (!identical(pids, c("WQ", "WK", "BQ", "BK"))) fail("platform ids.")
  if (!identical(unlist(s$square_order), c(ids, pids))) fail("square order.")
  if (!identical(s$square_order_within_board, "v_then_u")) fail("square order within board.")

  # Docks: ids derived from parents x sides x ends; adjacency must equal the
  # independently generated graph.
  dock_ids <- .g3_dock_ids_from(s$docks)
  if (!all(vapply(pl, function(p) p$initial_dock %in% dock_ids, TRUE))) fail("initial docks.")
  if (!all(vapply(pl, function(p) identical(p$initial_orientation, 0L), TRUE))) fail("initial orientation.")
  if (!identical(unlist(s$docks$orientations), c(0L, 180L))) fail("orientations.")
  adj <- s$dock_adjacency
  if (!setequal(names(adj), dock_ids)) fail("adjacency nodes.")
  for (d in names(adj)) {
    nb <- unlist(adj[[d]])
    if (!all(nb %in% dock_ids) || d %in% nb || anyDuplicated(nb)) fail(paste("adjacency of", d))
    for (n in nb) if (!d %in% unlist(adj[[n]])) fail(paste("asymmetric adjacency", d, n))
  }
  generated <- .g3_generate_dock_graph(s$docks)
  for (d in dock_ids) {
    if (!setequal(unlist(adj[[d]]), generated[[d]])) fail(paste("table differs from algorithm at", d))
  }

  # Setup: unique ids and squares, valid types/colours/squares, one king each.
  st <- s$setup
  sq_ids <- .g3_square_ids_from(mb, pl)
  pid <- vapply(st, function(p) p$id, "")
  psq <- vapply(st, function(p) p$square, "")
  ptype <- vapply(st, function(p) p$type, "")
  pcol <- vapply(st, function(p) p$color, "")
  if (anyDuplicated(pid) || anyDuplicated(psq)) fail("duplicate setup ids or squares.")
  if (!all(psq %in% sq_ids)) fail("setup square outside the 64 physical squares.")
  if (!all(ptype %in% c("P", "N", "B", "R", "Q", "K"))) fail("piece type.")
  if (!all(pcol %in% c("white", "black"))) fail("piece colour.")
  if (sum(ptype == "K" & pcol == "white") != 1L || sum(ptype == "K" & pcol == "black") != 1L) fail("kings.")

  # Enumerated rule values the engine implements.
  expect <- function(value, allowed, what) if (!identical(value, allowed)) fail(what)
  expect(s$first_side, "white", "first side")
  expect(s$movement$ray_intermediate_blockers, "any_occupant_on_any_board", "ray rule")
  expect(s$movement$zero_projected_displacement, "forbidden", "zero displacement")
  expect(s$platform_actions$occupants_required, 1L, "platform occupants")
  expect(s$platform_actions$rotation_only_on_same_dock, FALSE, "rotation only")
  expect(s$promotion$mandatory, TRUE, "promotion")
  expect(unlist(s$promotion$choices), c("Q", "R", "B", "N"), "promotion choices")
  expect(s$draws$repetition_count, 3L, "repetition")
  expect(s$draws$halfmove_limit, 100L, "halfmove")
  expect(s$draws$truncation_is_draw, FALSE, "truncation")
  expect(
    unlist(s$terminal_precedence),
    c("checkmate", "stalemate", "repetition", "halfmove", "bare_kings"),
    "terminal precedence"
  )
  for (col in c("white", "black")) {
    cs <- s$castling[[col]]
    for (side in c("kingside", "queenside")) {
      x <- cs[[side]]
      if (!all(c(x$king_from, x$rook_from, x$king_to, x$rook_to) %in% sq_ids)) fail("castling squares.")
      if (!x$rook %in% pid || !cs$king %in% pid) fail("castling identities.")
    }
  }
  if (!is.list(raw$decisions) || !length(raw$decisions)) fail("decision ledger missing.")
  origins <- vapply(raw$decisions, function(d) d$origin, "")
  if (!all(origins %in% c("supplied_concept", "proposed_resolution"))) fail("decision origin.")
  invisible(TRUE)
}

#' @noRd
.g3_dock_ids_from <- function(docks) {
  out <- character()
  for (p in unlist(docks$parents)) {
    for (e in unlist(docks$ends)) {
      for (sd in unlist(docks$sides)) out <- c(out, paste(p, sd, e, sep = ":"))
    }
  }
  out
}

# Generate the dock graph from the rule text, independently of the table:
# same parent -> other corner on the same pin rank (other side, same end) or
# same pin file (same side, other end); adjacent parents W-N and N-B -> same
# side and pin ranks at most two apart.
#' @noRd
.g3_generate_dock_graph <- function(docks) {
  ids <- .g3_dock_ids_from(docks)
  parts <- strsplit(ids, ":", fixed = TRUE)
  par <- vapply(parts, `[`, "", 1L)
  sd <- vapply(parts, `[`, "", 2L)
  en <- vapply(parts, `[`, "", 3L)
  pin_rank <- vapply(seq_along(ids), function(i) {
    pr <- unlist(docks$pin_ranks[[par[i]]])
    if (en[i] == "lo") pr[1L] else pr[2L]
  }, 0L)
  order_par <- match(par, unlist(docks$parents))
  out <- stats::setNames(vector("list", length(ids)), ids)
  for (i in seq_along(ids)) {
    nb <- character()
    for (j in seq_along(ids)) {
      if (i == j) next
      same_par <- par[i] == par[j]
      if (same_par && ((en[i] == en[j] && sd[i] != sd[j]) || (sd[i] == sd[j] && en[i] != en[j]))) {
        nb <- c(nb, ids[j])
      } else if (!same_par && abs(order_par[i] - order_par[j]) == 1L &&
                 sd[i] == sd[j] && abs(pin_rank[i] - pin_rank[j]) <= 2L) {
        nb <- c(nb, ids[j])
      }
    }
    out[[ids[i]]] <- nb
  }
  out
}

#' @noRd
.g3_square_ids_from <- function(main_boards, platforms) {
  out <- character()
  for (b in main_boards) {
    for (v in 0:(b$size - 1L)) for (u in 0:(b$size - 1L)) out <- c(out, paste(b$id, u, v, sep = ":"))
  }
  for (p in platforms) {
    for (v in 0:(p$size - 1L)) for (u in 0:(p$size - 1L)) out <- c(out, paste(p$id, u, v, sep = ":"))
  }
  out
}
