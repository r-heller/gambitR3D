# Optional offline tutor: the contract a paraphraser must pass.
#
# gambitR3D ships no language model. A future offline tutor model may
# only rephrase the deterministic coaching text; it never decides what is
# true. Everything it proposes goes through g3_validate_tutor_output():
#
#   1. structure: one non-empty `text` (at most 1500 characters) and at
#      least one `evidence_refs` entry naming evidence in the context
#   2. moves: every proposed action (in `actions`, or written as an action
#      id in the text) must be legal and replay through g3_apply_action()
#      from the referenced evidence's position, and must follow a line the
#      engine already verified (the tutor cannot invent a best move)
#   3. squares: every board-qualified square named in the text must occur
#      in the context (so a tutor cannot reveal a later hint level)
#   4. vocabulary: no centipawns, Elo, ratings, accuracy percentages or
#      certainty words; "forces mate" only when the evidence holds an
#      exhaustive mate proof
#
# If anything fails, or no tutor runtime is installed, the deterministic
# text is shown and the reason is recorded. No remote endpoint exists:
# nothing here opens a connection, and no game or profile data leaves the
# computer.

.G3_TUTOR_MAX_CHARS <- 1500L
.G3_TUTOR_FORBIDDEN <- c(
  "centipawn", "centi-pawn", "\\belo\\b", "\\brating\\b", "\\baccuracy\\b", "always win", "always lose",
  "guarantee", "certainly win", "zentibauer", "wertungszahl", "garantiert", "immer gewinn", "immer verlier"
)
.G3_TUTOR_MATE_WORDS <- c("forces mate", "forced mate", "forces checkmate", "forced checkmate",
                          "erzwingt matt", "erzwungenes matt", "forciertes matt", "erzwingt schachmatt")

#' Evidence context for an offline tutor
#'
#' Builds the structured evidence that a paraphrasing tutor receives and
#' must cite: the deterministic text, the position(s) involved, the lines
#' the engine has verified, the board-qualified squares that may be named,
#' and whether a checkmate is exhaustively proven.
#'
#' @param x A `g3_hint`, `g3_attempt` or `g3_explanation`.
#' @param exercise The `g3_exercise` for a hint or an attempt.
#' @param state The `g3_state` for an explanation (its own position).
#' @return A `g3_tutor_context` list: `deterministic_text`, `evidence`
#'   (named list; each element has `evidence_id`, `kind`, `state`,
#'   `verified_lines` (list of action-id vectors), `mate_proven`) and
#'   `allowed_squares`.
#' @examples
#' ex <- gambitR3D::g3_exercise_catalogue()$exercises[[1]]
#' h <- gambitR3D::g3_exercise_hint(ex, 2L)
#' ctx <- gambitR3D::g3_tutor_context(h, exercise = ex)
#' ctx$allowed_squares
#' @export
g3_tutor_context <- function(x, exercise = NULL, state = NULL) {
  sq_names <- function(txt) unique(unlist(regmatches(txt, gregexpr(.g3_square_name_regex(), txt, perl = TRUE))))
  if (inherits(x, "g3_hint")) {
    .g3_check_exercise(exercise)
    lines <- list()
    if (x$level >= 4L) {
      lines <- c(list(exercise$continuation$actions), as.list(exercise$solutions$action_id))
    }
    det <- paste(x$deterministic_text, collapse = " ")
    allowed <- unique(c(sq_names(det), sq_names(.g3_objective_text(exercise, x$lang))))
    ev <- list(hint = list(
      evidence_id = "hint", kind = paste0("hint_level_", x$level), state = exercise$start_state,
      verified_lines = lines, mate_proven = x$level >= 4L && exercise$objective$criterion == "checkmate",
      exercise_id = exercise$exercise_id
    ))
  } else if (inherits(x, "g3_attempt")) {
    .g3_check_exercise(exercise)
    lines <- list()
    if (isTRUE(x$evidence$legal)) {
      lines <- list(if (!is.null(x$evidence$reply)) c(x$action_id, x$evidence$reply$action_id) else x$action_id)
    }
    det <- paste(x$feedback, collapse = " ")
    allowed <- sq_names(paste(det, paste(x$claims$text, collapse = " ")))
    ev <- list(attempt = list(evidence_id = "attempt", kind = "attempt", state = exercise$start_state,
                              verified_lines = lines, mate_proven = FALSE, exercise_id = exercise$exercise_id))
  } else if (inherits(x, "g3_explanation")) {
    .g3_check_state(state)
    if (!identical(.g3_state_hash(state), x$state_hash)) {
      .g3_abort("g3_stale_revision", "The explanation belongs to a different position.")
    }
    det <- paste(x$text, collapse = " ")
    allowed <- sq_names(det)
    ev <- list(facts = list(evidence_id = "facts", kind = "legal_facts", state = state,
                            verified_lines = list(x$action_id),
                            mate_proven = identical(x$facts$outcome$reason, "checkmate")))
  } else {
    .g3_abort("g3_invalid_state", "Tutor context needs a g3_hint, g3_attempt or g3_explanation.")
  }
  structure(list(deterministic_text = det, evidence = ev, allowed_squares = allowed),
            class = "g3_tutor_context")
}

#' @noRd
.g3_square_name_regex <- function() "(?<![A-Za-z0-9:])(WQ|WK|BQ|BK|W|N|B):[a-f][0-9](?![0-9])"

#' Validate a tutor proposal
#'
#' The gate every optional offline tutor (paraphraser) output must pass
#' before it is shown. A proposal is a list with `text`, `evidence_refs`
#' (ids of evidence in `context$evidence`) and optionally `actions` (a
#' proposed move or continuation, as action ids starting from the
#' position of the first referenced evidence). See the rules in the
#' source file header; when the proposal fails, the deterministic text is
#' returned instead.
#'
#' @param proposal A list: `text` (one string), `evidence_refs`
#'   (character), optional `actions` (character).
#' @param context A `g3_tutor_context` from [g3_tutor_context()].
#' @return A `g3_tutor_validation` list: `valid`, `problems` (character),
#'   `text` (the proposal text if valid, otherwise the deterministic
#'   text), `source` (`"tutor"` or `"deterministic"`), `evidence_refs`
#'   and `checked_actions`.
#' @examples
#' ex <- gambitR3D::g3_exercise_catalogue()$exercises[[1]]
#' h <- gambitR3D::g3_exercise_hint(ex, 4L)
#' ctx <- gambitR3D::g3_tutor_context(h, exercise = ex)
#' # A legal move the engine did not verify is rejected.
#' bad <- base::list(text = "Play the queen to B:b6.", evidence_refs = "hint",
#'                   actions = "m:B:0:0-B:0:1")
#' gambitR3D::g3_validate_tutor_output(bad, ctx)$problems
#' good <- base::list(text = "The queen mates on B:e5.", evidence_refs = "hint",
#'                    actions = "m:B:0:0-B:3:0")
#' gambitR3D::g3_validate_tutor_output(good, ctx)$valid
#' @export
g3_validate_tutor_output <- function(proposal, context) {
  if (!inherits(context, "g3_tutor_context")) {
    .g3_abort("g3_invalid_state", "`context` must be a g3_tutor_context.")
  }
  p <- character()
  checked <- character()
  text <- proposal$text
  refs <- as.character(unlist(proposal$evidence_refs))
  if (!is.list(proposal)) {
    p <- "the proposal is not a list"
  } else {
    if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(trimws(text))) {
      p <- c(p, "text must be one non-empty string")
      text <- ""
    } else if (nchar(text) > .G3_TUTOR_MAX_CHARS) {
      p <- c(p, "text is too long")
    }
    if (!length(refs)) {
      p <- c(p, "no evidence references (missing evidence)")
    } else if (!all(refs %in% names(context$evidence))) {
      p <- c(p, paste0("unknown evidence reference: ", setdiff(refs, names(context$evidence))[1L]))
    }
  }
  if (!length(p)) {
    ev <- context$evidence[[refs[1L]]]
    lines <- ev$verified_lines
    # Proposed actions: explicit continuation plus action ids in the text.
    acts <- as.character(unlist(proposal$actions))
    in_text <- unique(unlist(regmatches(text, gregexpr(
      "(?<![A-Za-z])(m:[A-Z]{1,2}:[0-9]:[0-9]-[A-Z]{1,2}:[0-9]:[0-9](=[QRBN])?|t:[A-Z]{2}>[WNB]:[LR]:(lo|hi)@(0|180)(=[QRBN])?|c:(white|black):(kingside|queenside))",
      text, perl = TRUE
    ))))
    if (length(acts)) {
      rl <- .g3_replay_line(ev$state, acts)
      checked <- acts
      if (!rl$ok) {
        p <- c(p, paste0("proposed action ", rl$failed_at, " (", acts[rl$failed_at], ") is not legal: ", rl$message))
      } else if (!any(vapply(lines, function(l) length(l) >= length(acts) && identical(l[seq_along(acts)], acts), TRUE))) {
        p <- c(p, "the proposed line is legal but not one the engine verified")
      }
    }
    for (a in in_text) {
      checked <- c(checked, a)
      rl <- .g3_replay_line(ev$state, a)
      if (!rl$ok) {
        p <- c(p, paste0("action in text is not legal: ", a))
      } else if (!any(vapply(lines, function(l) length(l) && identical(l[1L], a), TRUE))) {
        p <- c(p, paste0("action in text is not a verified one: ", a))
      }
    }
    named <- unique(unlist(regmatches(text, gregexpr(.g3_square_name_regex(), text, perl = TRUE))))
    extra <- setdiff(named, context$allowed_squares)
    if (length(extra)) p <- c(p, paste0("names a square not in the evidence: ", extra[1L]))
    low <- tolower(text)
    for (w in .G3_TUTOR_FORBIDDEN) {
      if (grepl(w, low, perl = TRUE)) p <- c(p, paste0("forbidden wording: ", gsub("\\\\b", "", w)))
    }
    proven <- any(vapply(context$evidence[refs], function(e) isTRUE(e$mate_proven), TRUE))
    if (!proven && any(vapply(.G3_TUTOR_MATE_WORDS, function(w) grepl(w, low, fixed = TRUE), TRUE))) {
      p <- c(p, "claims a forced mate without an exhaustive proof in the evidence")
    }
  }
  valid <- !length(p)
  structure(
    list(valid = valid, problems = p, text = if (valid) text else context$deterministic_text,
         source = if (valid) "tutor" else "deterministic", evidence_refs = refs, checked_actions = unique(checked)),
    class = "g3_tutor_validation"
  )
}

# Is an offline tutor model runtime available? None ships with the
# package; the model registry may provide one later. Never contacts a
# remote service.
#' @noRd
.g3_tutor_runtime <- function(model) {
  hook <- get0(".g3_tutor_model_runtime", envir = asNamespace("gambitR3D"), inherits = FALSE)
  if (is.function(hook)) {
    return(hook(model))
  }
  list(available = FALSE, fn = NULL,
       reason = "no offline tutor model runtime is installed in this build")
}

# Apply an optional tutor to deterministic text. Returns the text to show,
# its source and the reason for any fallback.
#' @noRd
.g3_tutor_apply <- function(det_text, context, tutor, lang) {
  fallback <- function(reason) {
    list(text = .g3_split_text(det_text, context), source = "deterministic",
         fallback_reason = .g3_tr("tutor.fallback", lang, reason = reason))
  }
  fn <- NULL
  if (is.function(tutor)) {
    fn <- tutor
  } else if (is.character(tutor) || inherits(tutor, "g3_model")) {
    rt <- .g3_tutor_runtime(tutor)
    if (!isTRUE(rt$available) || !is.function(rt$fn)) return(fallback(rt$reason))
    fn <- rt$fn
  } else {
    return(fallback("unsupported tutor argument"))
  }
  proposal <- tryCatch(fn(context), error = function(e) e)
  if (inherits(proposal, "error")) return(fallback(paste("tutor error:", conditionMessage(proposal))))
  v <- g3_validate_tutor_output(proposal, context)
  if (!v$valid) return(fallback(paste(v$problems, collapse = "; ")))
  list(text = v$text, source = "tutor", fallback_reason = NA_character_)
}

# Deterministic text is stored as one string in the context; hints keep
# their sentence vector, so return the original sentences when possible.
#' @noRd
.g3_split_text <- function(det_text, context) {
  det_text
}
