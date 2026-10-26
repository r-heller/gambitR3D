# Contributing to gambitR3D

Thank you for helping. A few conventions keep the package coherent.

## One rules engine

All legality comes from the R engine (`g3_legal_actions()`, `g3_apply_action()`). The app,
search, analysis, coaching and the Python worker are clients of it. Please do not add move logic
anywhere else. A faster implementation is welcome only with differential tests against the R engine.

## Rules changes

The rules profile in `inst/rules/` is a project convention, not an official rulebook. Changing a
semantic value changes the rules hash; that needs a new profile id, new fixtures and a migration
note. Rewording a rationale does not.

## Tests and checks

```r
# Fast offline suite (no Python, no browser):
devtools::test()
# Full package check:
devtools::check()
```

Optional tests are switched on by environment variables: `G3_ML_ENV` (path to a provisioned
training environment, see `vignette("training")`) and `GAMBITR3D_BROWSER_TESTS=true` (Shiny browser tests with Chrome).

## Style

Explicit namespaces for non-base packages (`ggplot2::ggplot()`), roxygen documentation with
runnable examples, typed conditions (`g3_illegal_action`, `g3_invalid_state`, ...), and honest
states: an unavailable feature says why rather than pretending to work.
