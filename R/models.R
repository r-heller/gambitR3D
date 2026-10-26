# Model agents. Replaced by the model registry implementation.

#' @noRd
.g3_model_decision <- function(state, agent, budget, seed) {
  .g3_abort("g3_incompatible_model", "No model runtime is available; use agent = 'search'.")
}

#' @noRd
.g3_model_position_analysis <- function(state, evaluator, budget, actions) {
  .g3_abort("g3_incompatible_model", "No model runtime is available; use evaluator = 'search'.")
}
