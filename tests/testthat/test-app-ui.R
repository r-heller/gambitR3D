# Phase 05/06: the page shell, bundled assets and UI message catalogues.

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")

www <- function(...) file.path(.g3_www_dir(), ...)

test_that("the page has all sections, both navigations and only local assets", {
  rt <- htmltools::renderTags(.g3_app_ui(.g3_app_config()))
  html <- paste(as.character(rt$head), as.character(rt$html))
  for (s in .G3_SECTIONS) {
    expect_match(html, paste0('data-value="', s, '"'), fixed = TRUE)
    expect_match(html, paste0('data-section="', s, '"'), fixed = TRUE)
  }
  expect_match(html, 'class="g3-rail"', fixed = TRUE)
  expect_match(html, 'class="g3-bottombar"', fixed = TRUE)
  expect_match(html, 'aria-controls="g3-more-menu"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'id="play-board"', fixed = TRUE)
  expect_match(html, 'id="analyse-board"', fixed = TRUE)
  # No remote scripts, styles or fonts.
  expect_false(grepl("(src|href)=\"(https?:)?//", html))
  expect_false(grepl("fonts.googleapis|cdn\\.", html))
  # Import-map addresses are URL-like, or browsers would ignore them.
  map <- regmatches(html, regexpr('<script type="importmap">[^<]+</script>', html))
  json <- jsonlite::fromJSON(sub("</script>$", "", sub('^<script type="importmap">', "", map)))
  expect_true(all(startsWith(unlist(json$imports), "./g3www/")))
})

test_that("bundled assets exist, including the pinned three.js files and licence", {
  for (f in c("g3-app.js", "g3-scene.js", "g3-app.css", "g3-emblem.svg", "vendor/PROVENANCE.md",
              "vendor/three/three.module.js", "vendor/three/three.core.js",
              "vendor/three/addons/controls/OrbitControls.js", "vendor/three/LICENSE")) {
    expect_true(file.exists(www(f)), info = f)
  }
  expect_match(paste(readLines(www("vendor/three/LICENSE")), collapse = " "), "MIT License")
  prov <- paste(readLines(www("vendor/PROVENANCE.md")), collapse = "\n")
  hashes <- c(
    "vendor/three/three.module.js" = "9052042d676cb0fdc1ddfefe193053f34b7ac0513a616fdac4535d49987812ea",
    "vendor/three/three.core.js" = "9edde002b066a9a05676a6127f67735b62baf399bdea529f2f7e31657da769e6",
    "vendor/three/addons/controls/OrbitControls.js" = "3d79d07ecb686b4e5d93232eedab255331c1beef711e13164eaa1f68655a5f2b"
  )
  for (f in names(hashes)) {
    expect_identical(digest::digest(file = www(f), algo = "sha256"), hashes[[f]], info = f)
    expect_match(prov, hashes[[f]], fixed = TRUE)
  }
  expect_match(readLines(www("vendor/three/three.module.js"), n = 5)[3], "2010-2026 Three.js Authors")
})

test_that("every UI message key used in R and JavaScript exists in the catalogues", {
  en <- .g3_i18n_catalogue("en")
  de_raw <- unlist(jsonlite::fromJSON(file.path(.g3_i18n_dir(), "de.json"), simplifyVector = FALSE)$messages)
  # German covers everything except the board identifiers, which are the same.
  missing_de <- setdiff(names(en), names(de_raw))
  expect_true(all(startsWith(missing_de, "boardname.")), info = paste(missing_de, collapse = ", "))
  expect_identical(setdiff(names(de_raw), names(en)), character())
  # Literal keys in the R sources.
  r_files <- list.files(file.path(testthat::test_path("..", ".."), "R"), pattern = "^app.*\\.R$", full.names = TRUE)
  if (length(r_files)) {
    src <- paste(unlist(lapply(r_files, readLines)), collapse = "\n")
    keys <- unique(regmatches(src, gregexpr('(?<=t\\(|_t\\(|_txt\\(|key\\()"[a-z]+\\.[a-zA-Z0-9_.:]+"', src, perl = TRUE))[[1]])
    keys <- gsub('"', "", keys)
    expect_identical(setdiff(keys, names(en)), character())
  }
  js <- paste(readLines(www("g3-app.js")), collapse = "\n")
  jkeys <- unique(gsub("'", "", regmatches(js, gregexpr("(?<=G3\\.t\\()'[a-z]+\\.[a-zA-Z0-9_.]+'", js, perl = TRUE))[[1]]))
  jkeys <- jkeys[!grepl("[._]$", jkeys)] # prefixes of built keys, checked below
  expect_gt(length(jkeys), 30L)
  expect_identical(setdiff(jkeys, names(en)), character())
  # Keys built from parts: every variant must exist.
  built <- c(
    paste0("side.", .G3_COLORS), paste0("nav.", .G3_SECTIONS),
    paste0("boardname.", c("W", "N", "B", .G3_PLATFORMS)), paste0("grid.main_meta.", c("W", "N", "B")),
    paste0("dock.", .g3_tables()$dock_ids),
    paste0("piece.", rep(.G3_COLORS, each = 6), ".", c("pawn", "knight", "bishop", "rook", "queen", "king")),
    paste0("result.reason.", c("checkmate", "stalemate", "repetition", "halfmove", "bare_kings",
                               "resign", "draw_agreed", "truncated")),
    paste0("ack.", c("wrong_game", "stale_revision", "stale_state", "illegal_action", "game_over",
                     "save_failed", "invalid_request", "read_only")),
    paste0("cell.target_", c("move", "capture", "castle")), paste0("chooser.squares_", c("all", "xray", "occluded")),
    paste0("panel.castle_", c("kingside", "queenside")), paste0("budget.", names(.G3_APP_BUDGETS)),
    unname(.G3_APP_AGENTS), paste0("jobs.", c("opponent", "analysis")), paste0("settings.backend_", c("file", "db")),
    paste0("rules.origin.", unique(g3_rules_profile()$decisions$origin)),
    paste0("action.type.", c("move", "platform", "castle")),
    paste0("board.", c("view2d", "view3d", "level_W", "level_N", "level_B"))
  )
  expect_identical(setdiff(built, names(en)), character())
  expect_identical(.g3_t("status.to_act", "de", side = "Weiß"), "Weiß am Zug")
  expect_identical(.g3_t("no.such.key"), "no.such.key")
})

test_that("g3_run_app() refuses bad hosts and builds an app object", {
  expect_error(g3_run_app(host = NA_character_), class = "g3_io_error")
  app <- .g3_app(storage_dir = withr::local_tempdir())
  expect_s3_class(app, "shiny.appobj")
})
