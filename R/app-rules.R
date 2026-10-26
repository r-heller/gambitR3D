# Rules-profile inspector.
#
# Shows the active profile's name, hash and status, and its decision ledger
# with each rule's origin (from the supplied concept, or a proposed
# resolution of an open question). The text is the profile's own; the app
# does not reword or certify it.

#' @noRd
.g3_rules_panel <- function(lang = "en") {
  t <- function(key, ...) .g3_t(key, lang, ...)
  prof <- g3_rules_profile()
  dec <- prof$decisions
  htmltools::tagList(
    htmltools::tags$dl(
      class = "g3-facts",
      htmltools::tags$dt(t("rules.profile")), htmltools::tags$dd(prof$profile_id),
      htmltools::tags$dt(t("rules.hash")),
      htmltools::tags$dd(htmltools::tags$code(class = "g3-hash", title = prof$rules_hash, substr(prof$rules_hash, 1L, 16L))),
      htmltools::tags$dt(t("rules.status")), htmltools::tags$dd(t("rules.status_text"))
    ),
    htmltools::tags$details(
      class = "g3-disclosure",
      htmltools::tags$summary(t("rules.decisions", n = nrow(dec))),
      htmltools::tags$ul(
        class = "g3-decisions",
        lapply(seq_len(nrow(dec)), function(i) {
          origin <- t(paste0("rules.origin.", dec$origin[i]))
          htmltools::tags$li(
            htmltools::tags$p(htmltools::strong(paste0(dec$id[i], " ", dec$topic[i])),
                              htmltools::span(class = "g3-chip", origin)),
            htmltools::tags$p(class = "g3-small", dec$rationale[i])
          )
        })
      )
    )
  )
}
