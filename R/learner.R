# Local learner profile and spaced practice.
#
# The profile is one JSON file chosen by the user (schema
# `g3-learner-profile/1`), written atomically (temporary file + rename),
# never uploaded and never read by any training code unless the separate
# dataset flag is switched on. It holds attempts, per-exercise schedule
# boxes, an inspectable selection log and settings:
#
#   use_for_practice             record attempts and adapt the schedule
#                                (local only; default TRUE once the user
#                                records an attempt)
#   include_in_training_dataset  may a dataset builder read the attempts?
#                                Default FALSE; nothing reads them otherwise
#   upload                       always FALSE: upload is not implemented and
#                                no remote endpoint exists
#   daily_limit                  exercises per day (default 10)
#
# Spaced practice is a transparent Leitner schedule. Boxes 0..5 have
# intervals of 1, 2, 4, 7, 14 and 30 days. Solved with at most one hint:
# box + 1. Solved with two or more hints, or acceptable: the box is kept
# (at least 1). Incorrect or illegal: box 0. The next due date is the
# attempt date plus the box's interval.
#
# Theme states are descriptive counts, not judgements about a person:
#   new              no attempt yet
#   recently_missed  the last attempt was incorrect/illegal, or, with at
#                    least two attempts, at most one of the last three was
#                    solved
#   mastered         the last three attempts were all solved with at most
#                    one hint each
#   practising       anything else
#
# Daily selection (g3_practice_plan()) takes, in order: due reviews
# (recently missed themes first); new exercises from recently missed
# themes (up to a third of the day); at least one exercise from a mastered
# theme when one exists (mixing, about a fifth of the day); new exercises
# from new themes, rotating themes; remaining new exercises; early reviews.
# Every selected row carries its reason. No rating, no strength estimate
# and no psychological label is computed.

.G3_LEARNER_SCHEMA <- "g3-learner-profile/1"
.G3_LEARNER_RULES <- "leitner-6/1"
.G3_LEARNER_INTERVALS <- c(1L, 2L, 4L, 7L, 14L, 30L)
.G3_ATTEMPT_STATUSES <- c("solved", "acceptable", "incorrect", "illegal")

#' @noRd
.g3_learner_defaults <- function() {
  list(
    use_for_practice = TRUE,
    include_in_training_dataset = FALSE,
    upload = FALSE,
    daily_limit = 10L,
    intervals_days = .G3_LEARNER_INTERVALS,
    mastery_window = 3L,
    mastered_share = 0.2,
    lang = "en"
  )
}

#' @noRd
.g3_learner_new <- function() {
  list(
    schema = .G3_LEARNER_SCHEMA,
    profile_id = .g3_new_id("learner"),
    created = .g3_now(),
    updated = .g3_now(),
    rules = .G3_LEARNER_RULES,
    settings = .g3_learner_defaults(),
    exercises = list(),
    attempts = list(),
    schedule = list(),
    selection_log = list()
  )
}

#' @noRd
.g3_check_profile_path <- function(profile_path) {
  if (!is.character(profile_path) || length(profile_path) != 1L || is.na(profile_path) || !nzchar(profile_path)) {
    .g3_abort("g3_io_error", "`profile_path` must be one file path.")
  }
  invisible(profile_path)
}

# Read a profile; a missing file gives a fresh in-memory profile (nothing
# is written by reading).
#' @noRd
.g3_learner_read <- function(profile_path) {
  .g3_check_profile_path(profile_path)
  if (!file.exists(profile_path)) {
    p <- .g3_learner_new()
    attr(p, "exists") <- FALSE
    return(p)
  }
  raw <- .g3_read_json_file(profile_path)
  if (!identical(raw$schema, .G3_LEARNER_SCHEMA)) {
    .g3_abort("g3_io_error", paste0("Not a gambitR3D learner profile: ", profile_path))
  }
  raw$settings <- utils::modifyList(.g3_learner_defaults(), raw$settings %||% list())
  raw$settings$intervals_days <- as.integer(unlist(raw$settings$intervals_days))
  raw$settings$upload <- FALSE
  for (nm in c("exercises", "attempts", "schedule", "selection_log")) raw[[nm]] <- raw[[nm]] %||% list()
  attr(raw, "exists") <- TRUE
  raw
}

# Atomic write of a profile; creates the parent directory if needed.
#' @noRd
.g3_learner_write <- function(profile, profile_path) {
  dir <- dirname(profile_path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  profile$updated <- .g3_now()
  attr(profile, "exists") <- NULL
  txt <- jsonlite::toJSON(profile, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
  .g3_atomic_write(as.character(txt), profile_path)
}

#' Learner settings and privacy controls
#'
#' Reads or changes the local profile's settings. With no changes it only
#' reads (a missing profile is not created). Upload cannot be enabled: it
#' is not implemented and no remote endpoint exists.
#'
#' @param profile_path Path of the local profile JSON file.
#' @param use_for_practice Record attempts locally and adapt the practice
#'   schedule (`TRUE`/`FALSE`); `NULL` keeps the current value.
#' @param include_in_training_dataset Allow a dataset builder to include
#'   the recorded attempts (default `FALSE`); `NULL` keeps it.
#' @param daily_limit Exercises per day (1 to 200); `NULL` keeps it.
#' @param upload Must be `NULL` or `FALSE`; `TRUE` raises `g3_io_error`.
#' @return The settings list (invisibly when changed): `use_for_practice`,
#'   `include_in_training_dataset`, `upload` (always `FALSE`),
#'   `daily_limit`, `intervals_days`, `mastery_window`, `mastered_share`,
#'   `lang`.
#' @examples
#' path <- base::file.path(base::tempdir(), "learner-example.json")
#' gambitR3D::g3_learner_settings(path)$include_in_training_dataset
#' gambitR3D::g3_learner_settings(path, daily_limit = 5L)
#' gambitR3D::g3_learner_delete(path, confirm = TRUE)
#' @export
g3_learner_settings <- function(profile_path, use_for_practice = NULL, include_in_training_dataset = NULL,
                                daily_limit = NULL, upload = NULL) {
  prof <- .g3_learner_read(profile_path)
  if (isTRUE(upload)) {
    .g3_abort("g3_io_error", "Uploading a learner profile is not implemented; the profile stays on this computer.")
  }
  changes <- list()
  flag <- function(x, nm) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) .g3_abort("g3_io_error", paste0("`", nm, "` must be TRUE or FALSE."))
    x
  }
  if (!is.null(use_for_practice)) changes$use_for_practice <- flag(use_for_practice, "use_for_practice")
  if (!is.null(include_in_training_dataset)) {
    changes$include_in_training_dataset <- flag(include_in_training_dataset, "include_in_training_dataset")
  }
  if (!is.null(daily_limit)) {
    dl <- suppressWarnings(as.integer(daily_limit))
    if (length(dl) != 1L || is.na(dl) || dl < 1L || dl > 200L) .g3_abort("g3_io_error", "`daily_limit` must be 1 to 200.")
    changes$daily_limit <- dl
  }
  if (!length(changes)) return(prof$settings)
  prof$settings <- utils::modifyList(prof$settings, changes)
  .g3_learner_write(prof, profile_path)
  invisible(prof$settings)
}

#' Record an exercise attempt in the local profile
#'
#' Adds a scored attempt (from [g3_score_attempt()]) to the local profile
#' and updates the exercise's schedule box. Nothing is written when the
#' profile's `use_for_practice` setting is `FALSE`. Only the exercise id,
#' theme, action ids, status, hint count and date are stored.
#'
#' @param profile_path Path of the local profile JSON file (created on the
#'   first record, with its directory).
#' @param attempt A `g3_attempt`.
#' @param date Practice date (a `Date`, default today); used by the
#'   schedule.
#' @return Invisibly, a list with `recorded` (logical), `reason`,
#'   `attempt_id`, `box` and `due`.
#' @examples
#' ex <- gambitR3D::g3_exercise_catalogue()$exercises[[1]]
#' at <- gambitR3D::g3_score_attempt(ex, ex$primary_action_id)
#' path <- base::file.path(base::tempdir(), "learner-record-example.json")
#' gambitR3D::g3_learner_record(path, at)
#' gambitR3D::g3_learner_progress(path)$themes
#' gambitR3D::g3_learner_delete(path, confirm = TRUE)
#' @export
g3_learner_record <- function(profile_path, attempt, date = Sys.Date()) {
  if (!inherits(attempt, "g3_attempt") || !attempt$status %in% .G3_ATTEMPT_STATUSES) {
    .g3_abort("g3_invalid_state", "`attempt` must be a g3_attempt from g3_score_attempt().")
  }
  date <- as.Date(date)
  prof <- .g3_learner_read(profile_path)
  if (!isTRUE(prof$settings$use_for_practice)) {
    return(invisible(list(recorded = FALSE, reason = "use_for_practice is off", attempt_id = NA_character_,
                          box = NA_integer_, due = NA_character_)))
  }
  id <- attempt$exercise_id
  aid <- substr(.g3_sha256(paste(id, attempt$scored_at, length(prof$attempts), sep = "|")), 1L, 16L)
  prof$attempts[[length(prof$attempts) + 1L]] <- list(
    attempt_id = aid, exercise_id = id, theme = attempt$theme, status = attempt$status,
    hints_used = as.integer(attempt$hints_used), actions = as.list(attempt$actions),
    grade_basis = attempt$grade_basis, date = format(date), scored_at = attempt$scored_at,
    rules_hash = attempt$rules_hash
  )
  if (is.null(prof$exercises[[id]])) {
    prof$exercises[[id]] <- list(theme = attempt$theme, first_seen = format(date))
  }
  sc <- prof$schedule[[id]] %||% list(box = 0L, n_attempts = 0L)
  box <- as.integer(sc$box)
  box <- switch(
    attempt$status,
    solved = if (attempt$hints_used <= 1L) min(box + 1L, length(prof$settings$intervals_days) - 1L) else max(box, 1L),
    acceptable = max(box, 1L),
    0L
  )
  due <- date + prof$settings$intervals_days[box + 1L]
  prof$schedule[[id]] <- list(box = box, due = format(due), last_status = attempt$status,
                              last_date = format(date), n_attempts = as.integer(sc$n_attempts) + 1L,
                              theme = attempt$theme)
  .g3_learner_write(prof, profile_path)
  invisible(list(recorded = TRUE, reason = NA_character_, attempt_id = aid, box = box, due = format(due)))
}

#' Learner progress tables
#'
#' Reads the local learner profile and returns its tables. A missing
#' profile gives typed empty tables; nothing is created by reading.
#'
#' @param profile_path Path of the local profile JSON file.
#' @return A `g3_learner_progress` list: `attempts` (data frame:
#'   `attempt_id`, `exercise_id`, `theme`, `status`, `hints_used`,
#'   `grade_basis`, `date`, `scored_at`), `hints` (per exercise:
#'   `exercise_id`, `theme`, `attempts`, `hints_total`, `hints_last`,
#'   `solved_without_hints`), `themes` (per theme: `theme`, `attempts`,
#'   `solved`, `acceptable`, `incorrect`, `illegal`, `mean_hints`,
#'   `last_status`, `last_date`, `state`, `state_rule`), `schedule` (per
#'   exercise: `exercise_id`, `theme`, `box`, `due`, `last_status`,
#'   `last_date`, `n_attempts`), `selection_log` (`date`, `exercise_id`,
#'   `reason_code`, `reason`), `settings` and `summary` (list: `exists`,
#'   `profile_path`, `schema`, `rules`, `n_attempts`, `n_exercises`,
#'   `first_date`, `last_date`).
#' @examples
#' gambitR3D::g3_learner_progress(base::tempfile(fileext = ".json"))$summary$exists
#' @export
g3_learner_progress <- function(profile_path) {
  prof <- .g3_learner_read(profile_path)
  .g3_learner_tables(prof, profile_path)
}

#' @noRd
.g3_learner_tables <- function(prof, profile_path) {
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  at <- prof$attempts
  attempts <- data.frame(
    attempt_id = vapply(at, function(a) chr(a$attempt_id), ""),
    exercise_id = vapply(at, function(a) chr(a$exercise_id), ""),
    theme = vapply(at, function(a) chr(a$theme), ""),
    status = vapply(at, function(a) chr(a$status), ""),
    hints_used = vapply(at, function(a) as.integer(a$hints_used %||% 0L), 0L),
    grade_basis = vapply(at, function(a) chr(a$grade_basis), ""),
    date = vapply(at, function(a) chr(a$date), ""),
    scored_at = vapply(at, function(a) chr(a$scored_at), ""),
    stringsAsFactors = FALSE
  )
  ex_ids <- unique(attempts$exercise_id)
  hints <- data.frame(
    exercise_id = ex_ids,
    theme = attempts$theme[match(ex_ids, attempts$exercise_id)],
    attempts = vapply(ex_ids, function(e) sum(attempts$exercise_id == e), 0L),
    hints_total = vapply(ex_ids, function(e) sum(attempts$hints_used[attempts$exercise_id == e]), 0L),
    hints_last = vapply(ex_ids, function(e) utils::tail(attempts$hints_used[attempts$exercise_id == e], 1L), 0L),
    solved_without_hints = vapply(ex_ids, function(e) {
      any(attempts$status[attempts$exercise_id == e] == "solved" & attempts$hints_used[attempts$exercise_id == e] == 0L)
    }, TRUE),
    stringsAsFactors = FALSE
  )
  rownames(hints) <- NULL
  themes <- .g3_theme_states(attempts, prof$settings$mastery_window %||% 3L)
  sc <- prof$schedule
  schedule <- data.frame(
    exercise_id = names(sc) %||% character(),
    theme = vapply(sc, function(s) chr(s$theme), ""),
    box = vapply(sc, function(s) as.integer(s$box), 0L),
    due = vapply(sc, function(s) chr(s$due), ""),
    last_status = vapply(sc, function(s) chr(s$last_status), ""),
    last_date = vapply(sc, function(s) chr(s$last_date), ""),
    n_attempts = vapply(sc, function(s) as.integer(s$n_attempts), 0L),
    stringsAsFactors = FALSE
  )
  rownames(schedule) <- NULL
  sl <- unlist(lapply(prof$selection_log, function(s) {
    lapply(s$selected, function(r) list(date = chr(s$date), exercise_id = chr(r$exercise_id),
                                        reason_code = chr(r$reason_code), reason = chr(r$reason)))
  }), recursive = FALSE)
  selection_log <- data.frame(
    date = vapply(sl, function(r) r$date, ""), exercise_id = vapply(sl, function(r) r$exercise_id, ""),
    reason_code = vapply(sl, function(r) r$reason_code, ""), reason = vapply(sl, function(r) r$reason, ""),
    stringsAsFactors = FALSE
  )
  dates <- attempts$date
  structure(
    list(
      attempts = attempts, hints = hints, themes = themes, schedule = schedule, selection_log = selection_log,
      settings = prof$settings,
      summary = list(
        exists = isTRUE(attr(prof, "exists")), profile_path = profile_path, schema = .G3_LEARNER_SCHEMA,
        rules = .G3_LEARNER_RULES, n_attempts = nrow(attempts), n_exercises = length(ex_ids),
        first_date = if (length(dates)) min(dates) else NA_character_,
        last_date = if (length(dates)) max(dates) else NA_character_
      )
    ),
    class = "g3_learner_progress"
  )
}

# Per-theme counts and the descriptive state (rules in the file header).
#' @noRd
.g3_theme_states <- function(attempts, window = 3L) {
  th <- unique(attempts$theme)
  rows <- lapply(th, function(t) {
    a <- attempts[attempts$theme == t, , drop = FALSE]
    last <- utils::tail(a, window)
    ok <- last$status == "solved"
    state <- if (!nrow(a)) "new" else if (nrow(last) >= window && all(ok & last$hints_used <= 1L)) "mastered" else
      if (utils::tail(a$status, 1L) %in% c("incorrect", "illegal") || (nrow(last) >= 2L && sum(ok) <= 1L)) "recently_missed" else
        "practising"
    data.frame(
      theme = t, attempts = nrow(a), solved = sum(a$status == "solved"), acceptable = sum(a$status == "acceptable"),
      incorrect = sum(a$status == "incorrect"), illegal = sum(a$status == "illegal"),
      mean_hints = mean(a$hints_used), last_status = utils::tail(a$status, 1L), last_date = utils::tail(a$date, 1L),
      state = state,
      state_rule = switch(state,
                          mastered = "last attempts all solved with at most one hint",
                          recently_missed = "last attempt missed, or (two or more attempts) at most one of the last three solved",
                          practising = "attempted, neither rule above applies"),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(theme = character(), attempts = integer(), solved = integer(), acceptable = integer(),
                      incorrect = integer(), illegal = integer(), mean_hints = numeric(), last_status = character(),
                      last_date = character(), state = character(), state_rule = character(),
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out$attempts <- as.integer(out$attempts)
  rownames(out) <- NULL
  out
}

#' @export
print.g3_learner_progress <- function(x, ...) {
  s <- x$summary
  cat("<g3_learner_progress> ", s$n_attempts, " attempt(s) on ", s$n_exercises, " exercise(s)",
      if (!s$exists) " (no profile file yet)" else "", "\n", sep = "")
  if (nrow(x$themes)) print(x$themes[, c("theme", "attempts", "solved", "state")], row.names = FALSE)
  invisible(x)
}

#' Plan today's practice
#'
#' Selects exercises for one day with the transparent spaced-practice
#' rules described in the source header: due reviews first, missed themes
#' reintroduced, mastered themes mixed in, then new themes and exercises.
#' Only validated exercises are offered unless `include_candidates` is
#' `TRUE`. Every row says why it was selected; the selection can be
#' appended to the profile's selection log for later inspection.
#'
#' @param profile_path Path of the local profile JSON file.
#' @param exercises A `g3_exercise_set` (for example
#'   [g3_exercise_catalogue()] or exercises from [g3_coach_game()]).
#' @param date Practice date (default today).
#' @param daily_limit Overrides the profile's daily limit; exercises
#'   already attempted on `date` count against it.
#' @param include_candidates Also offer candidate (unvalidated) exercises.
#' @param record Append the selection to the profile's log (only when
#'   `use_for_practice` is on and a profile file exists or attempts are
#'   recorded).
#' @param lang `"en"` (default) or `"de"` for the reason texts.
#' @return A data frame: `order`, `exercise_id`, `theme`, `reason_code`
#'   (`due_review`, `missed_theme`, `mastered_mix`, `new_theme`,
#'   `new_exercise`, `early_review`, `personalisation_off`), `reason`,
#'   `box`, `due`. Attribute `rules` names the schedule rules.
#' @examples
#' set <- gambitR3D::g3_exercise_catalogue()
#' plan <- gambitR3D::g3_practice_plan(base::tempfile(fileext = ".json"), set,
#'                                     daily_limit = 4L, record = FALSE)
#' plan[, c("theme", "reason_code", "reason")]
#' @export
g3_practice_plan <- function(profile_path, exercises, date = Sys.Date(), daily_limit = NULL,
                             include_candidates = FALSE, record = TRUE, lang = "en") {
  if (!inherits(exercises, "g3_exercise_set")) .g3_abort("g3_invalid_state", "`exercises` must be a g3_exercise_set.")
  lang <- .g3_coach_lang(lang)
  date <- as.Date(date)
  prof <- .g3_learner_read(profile_path)
  prog <- .g3_learner_tables(prof, profile_path)
  limit <- as.integer(daily_limit %||% prof$settings$daily_limit)
  done_today <- sum(prog$attempts$date == format(date))
  remaining <- max(0L, limit - done_today)
  s <- exercises$summary
  if (!include_candidates) s <- s[s$status == "validated", , drop = FALSE]
  s <- s[order(match(s$theme, .G3_EXERCISE_THEMES), s$exercise_id), , drop = FALSE]
  sched <- prog$schedule
  sel <- list()
  taken <- character()
  take <- function(ids, code, ...) {
    for (id in ids) {
      if (length(sel) >= remaining) return(invisible())
      if (id %in% taken) next
      th <- s$theme[match(id, s$exercise_id)]
      r <- match(id, sched$exercise_id)
      sel[[length(sel) + 1L]] <<- list(
        exercise_id = id, theme = th, reason_code = code,
        reason = .g3_tr(paste0("plan.", code), lang, theme = .g3_theme_word(th, lang),
                        box = if (is.na(r)) 0L else sched$box[r], due = if (is.na(r)) "" else sched$due[r]),
        box = if (is.na(r)) NA_integer_ else sched$box[r], due = if (is.na(r)) NA_character_ else sched$due[r]
      )
      taken <<- c(taken, id)
    }
  }
  if (!isTRUE(prof$settings$use_for_practice)) {
    take(s$exercise_id, "personalisation_off")
  } else {
    ts <- prog$themes
    missed <- ts$theme[ts$state == "recently_missed"]
    mastered <- ts$theme[ts$state == "mastered"]
    seen <- sched$exercise_id
    avail <- sched[sched$exercise_id %in% s$exercise_id, , drop = FALSE]
    # 1. Due reviews: missed themes first, then lower boxes, earlier dates.
    due <- avail[!is.na(avail$due) & as.Date(avail$due) <= date, , drop = FALSE]
    due <- due[order(!due$theme %in% missed, due$box, due$due, due$exercise_id), , drop = FALSE]
    take(due$exercise_id, "due_review")
    unseen <- s[!s$exercise_id %in% seen, , drop = FALSE]
    # 2. Reintroduce missed themes with new exercises (up to a third).
    cap <- length(sel) + max(1L, ceiling(remaining / 3))
    for (id in unseen$exercise_id[unseen$theme %in% missed]) if (length(sel) < cap) take(id, "missed_theme")
    # 3. Mix in mastered themes (about a fifth, at least one).
    n_mix <- max(1L, round(remaining * (prof$settings$mastered_share %||% 0.2)))
    if (length(mastered)) {
      pool <- c(unseen$exercise_id[unseen$theme %in% mastered],
                avail$exercise_id[avail$theme %in% mastered][order(avail$due[avail$theme %in% mastered])])
      cap <- length(sel) + n_mix
      for (id in pool) if (length(sel) < cap) take(id, "mastered_mix")
    }
    # 4. New themes, rotating: one exercise per theme per round.
    new_th <- unseen[!unseen$theme %in% ts$theme, , drop = FALSE]
    if (nrow(new_th)) {
      rnd <- stats::ave(seq_len(nrow(new_th)), new_th$theme, FUN = seq_along)
      take(new_th$exercise_id[order(rnd, match(new_th$theme, .G3_EXERCISE_THEMES))], "new_theme")
    }
    # 5. Remaining unseen exercises, then early reviews by due date.
    take(unseen$exercise_id, "new_exercise")
    early <- avail[order(avail$due, avail$exercise_id), , drop = FALSE]
    take(early$exercise_id, "early_review")
  }
  out <- data.frame(
    order = seq_along(sel),
    exercise_id = vapply(sel, function(x) x$exercise_id, ""),
    theme = vapply(sel, function(x) x$theme, ""),
    reason_code = vapply(sel, function(x) x$reason_code, ""),
    reason = vapply(sel, function(x) x$reason, ""),
    box = vapply(sel, function(x) as.integer(x$box), 0L),
    due = vapply(sel, function(x) as.character(x$due), ""),
    stringsAsFactors = FALSE
  )
  attr(out, "rules") <- .G3_LEARNER_RULES
  if (record && isTRUE(prof$settings$use_for_practice) && isTRUE(attr(prof, "exists"))) {
    prof$selection_log[[length(prof$selection_log) + 1L]] <- list(
      date = format(date), rules = .G3_LEARNER_RULES, daily_limit = limit, done_today = done_today,
      selected = lapply(sel, function(x) x[c("exercise_id", "reason_code", "reason")])
    )
    .g3_learner_write(prof, profile_path)
  }
  out
}

#' Export, delete or reset a learner profile
#'
#' `g3_learner_export()` copies the profile to `path` as JSON (with an
#' export stamp) or, for `format = "csv"`, writes its tables as CSV files
#' into the directory `path`. `g3_learner_delete()` removes the profile
#' file. `g3_learner_reset()` clears attempts, schedule and selection log
#' but keeps the settings. Deleting and resetting need `confirm = TRUE`.
#'
#' @param profile_path Path of the local profile JSON file.
#' @param path Output file (JSON) or directory (CSV).
#' @param format `"json"` (default) or `"csv"`.
#' @param confirm Must be `TRUE` to delete or reset.
#' @return `g3_learner_export()` returns a manifest data frame (`file`,
#'   `bytes`, `sha256`); `g3_learner_delete()` and `g3_learner_reset()`
#'   return invisibly a list with `path` and `deleted` or `reset`.
#' @examples
#' path <- base::file.path(base::tempdir(), "learner-export-example.json")
#' gambitR3D::g3_learner_settings(path, daily_limit = 3L)
#' gambitR3D::g3_learner_export(path, base::tempfile(fileext = ".json"))
#' gambitR3D::g3_learner_reset(path, confirm = TRUE)
#' gambitR3D::g3_learner_delete(path, confirm = TRUE)
#' @export
g3_learner_export <- function(profile_path, path, format = c("json", "csv")) {
  format <- match.arg(format)
  .g3_check_profile_path(profile_path)
  if (!file.exists(profile_path)) .g3_abort("g3_io_error", paste0("No learner profile at ", profile_path))
  prof <- .g3_learner_read(profile_path)
  if (format == "json") {
    attr(prof, "exists") <- NULL
    prof$exported <- .g3_now()
    txt <- jsonlite::toJSON(prof, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
    .g3_atomic_write(as.character(txt), path)
    files <- path
  } else {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    tb <- .g3_learner_tables(prof, profile_path)
    files <- character()
    for (nm in c("attempts", "hints", "themes", "schedule", "selection_log")) {
      f <- file.path(path, paste0(nm, ".csv"))
      tmp <- tempfile(pattern = ".g3tmp-", tmpdir = path)
      utils::write.csv(tb[[nm]], tmp, row.names = FALSE, fileEncoding = "UTF-8")
      if (!file.rename(tmp, f)) .g3_abort("g3_io_error", paste0("Could not write ", f))
      files <- c(files, f)
    }
  }
  data.frame(file = files, bytes = file.info(files)$size,
             sha256 = vapply(files, function(f) digest::digest(file = f, algo = "sha256"), ""),
             stringsAsFactors = FALSE, row.names = NULL)
}

#' @rdname g3_learner_export
#' @export
g3_learner_delete <- function(profile_path, confirm = FALSE) {
  .g3_check_profile_path(profile_path)
  if (!isTRUE(confirm)) .g3_abort("g3_io_error", "Deleting a learner profile needs confirm = TRUE.")
  existed <- file.exists(profile_path)
  if (existed && !file.remove(profile_path)) .g3_abort("g3_io_error", paste0("Could not delete ", profile_path))
  invisible(list(path = profile_path, deleted = existed))
}

#' @rdname g3_learner_export
#' @export
g3_learner_reset <- function(profile_path, confirm = FALSE) {
  .g3_check_profile_path(profile_path)
  if (!isTRUE(confirm)) .g3_abort("g3_io_error", "Resetting a learner profile needs confirm = TRUE.")
  prof <- .g3_learner_read(profile_path)
  fresh <- .g3_learner_new()
  fresh$settings <- prof$settings
  fresh$profile_id <- prof$profile_id
  fresh$created <- prof$created
  .g3_learner_write(fresh, profile_path)
  invisible(list(path = profile_path, reset = TRUE))
}

# For dataset builders: may this profile's attempts enter a training
# dataset? FALSE unless the user switched the flag on. Never uploads.
#' @noRd
.g3_learner_dataset_allowed <- function(profile_path) {
  if (!file.exists(profile_path)) return(FALSE)
  isTRUE(.g3_learner_read(profile_path)$settings$include_in_training_dataset)
}

# Suggested profile location for the app: under the user's data directory,
# outside the package. Nothing is created until an attempt is recorded.
#' @noRd
.g3_learner_default_path <- function() {
  file.path(tools::R_user_dir("gambitR3D", which = "data"), "learner", "profile.json")
}
