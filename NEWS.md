# gambitR3D 0.1.0

First release: a companion to gambitR for an experimental multilevel chess
variant.

## Rules

* Rules profile `experimental-v0.1`: three fixed 4×4 boards, four movable
  2×2 platforms, 64 physical squares that keep their identity when platforms
  move. The profile is machine-readable and hashed, with a ledger of 18
  decisions marking which rules come from the supplied concept and which are
  proposed resolutions. It is a project convention, not an official rulebook.
* A rules engine in R, the reference implementation, and a compiled port
  (`src/engine.cpp`) that gives identical results and runs the search about 150
  times faster. `options(gambitR3D.engine = "r")` selects the R reference.

## Playing and recording

* `g3_run_app()`: a local Shiny app with synchronised 2D and 3D views (the
  Neutral board shown highest or lowest, exploded, isolation and x-ray views),
  hotseat and computer play, autosave, takeback, resignation and agreed draws.
* Native JSON game and position formats with full replay validation; session
  events (resignation, agreed draw, truncation, takeback) are kept apart from
  board actions.
* An alpha-beta search opponent with greedy and random baselines.

## Data, analysis and coaching

* A local SQLite game database: import with quarantine, parameterised queries,
  export with manifests, backup, restore and integrity checks.
* Game and collection analysis with engine-derived features, explicit score
  perspectives, tactical labels with claim strength, fair agent comparisons
  with truncation bounds and paired-block bootstrap intervals, plots and an
  offline HTML report.
* Engine-grounded coaching: explanations, game reviews, graduated hints,
  exercises with validated solution sets, and a local learner profile with
  spaced practice. Works offline, in English and German.

## Models

* An optional, isolated Python/PyTorch worker for the package's own square
  transformer: bounded CPU training with exact resume, self-play with R-owned
  MCTS, checkpoints in safetensors, and model play through the same agent
  interface as the search. Published ordinary-chess models are listed for
  reference only and cannot play this variant.
