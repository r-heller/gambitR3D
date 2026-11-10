## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Raban Heller <raban.heller@uni-ulm.de>'
  New submission

  This is the first submission of gambitR3D to CRAN.

The check also reports two INFO lines, which are not NOTEs:

* `specified C++17`: the compiled engine (`src/engine.cpp`) uses C++17
  features, so `CXX_STD = CXX17` is set in `src/Makevars` and
  `src/Makevars.win`.
* installed size about 6 MB: the Shiny app ships the unminified three.js
  0.186.1 ES modules (MIT, about 2.1 MB) so that it works offline without a
  CDN. Their provenance, checksums and licence are recorded in
  `inst/app/www/vendor/PROVENANCE.md`, and the three.js authors are listed
  as `ctb`/`cph` in `Authors@R`.

## Test environments

* local: macOS (aarch64), R 4.6.1, `R CMD check --as-cran --no-manual`
  (the PDF manual was built locally with TinyTeX (TeX Live 2026) via
  `R CMD Rd2pdf` without errors; the check run itself used `--no-manual`)
* GitHub Actions (r-lib/actions v2):
  * macos-latest (R release)
  * windows-latest (R release)
  * ubuntu-latest (R devel, R release, R oldrel-1)

## Notes for the reviewer

* The package works offline. Nothing in examples, tests or vignettes uses the
  network.
* The optional model features drive an isolated Python/PyTorch worker that the
  user has to set up explicitly with `g3_setup_ml()`. The examples of those
  functions are in `\dontrun{}` because they need that environment and a
  trained checkpoint; the corresponding tests and the training vignette run
  only when such an environment is configured.
* The examples of `g3_run_app()`, `g3_start_job()` and `g3_job_status()` run
  only in interactive sessions: the app opens a browser, and a job runs in a
  supervised background R process whose 'processx' supervisor connections
  outlive a non-interactive example.
* Background jobs, the app and the database write only to paths the user
  passes, or by default to `tools::R_user_dir("gambitR3D", "data")`.
  Examples, tests and vignettes use temporary directories.
* Browser tests (shinytest2/chromote), background-process tests and other slow
  tests are skipped on CRAN.

## This is a new submission.
