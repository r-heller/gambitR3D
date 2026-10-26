# Phase 05-07: real browser checks with shinytest2 and chromote.
#
# These drive the actual app in headless Chrome with real mouse and keyboard
# events (Chrome DevTools Input domain), including WebGL picking in the 3D
# view. They are slow and need Chrome, so they run only when
#   GAMBITR3D_BROWSER_TESTS=true
# and never on CRAN. Screenshots go to GAMBITR3D_EVIDENCE_DIR (default: a
# temporary folder). With GAMBITR3D_APP_LIB set to a library containing an
# installed gambitR3D, the app is started from that installed copy instead
# of the source tree.

browser_ok <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("GAMBITR3D_BROWSER_TESTS"), "true"),
              "Set GAMBITR3D_BROWSER_TESTS=true to run the real-browser tests.")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "No Chrome or Chromium found.")
}

evidence <- function(name) {
  d <- Sys.getenv("GAMBITR3D_EVIDENCE_DIR")
  if (!nzchar(d)) d <- file.path(tempdir(), "g3-evidence")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, name)
}

app_dir <- function(storage, envir = parent.frame(), app_args = "") {
  d <- withr::local_tempdir(.local_envir = envir)
  lib <- Sys.getenv("GAMBITR3D_APP_LIB")
  src <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  loader <- if (nzchar(lib)) {
    sprintf(".libPaths(c(%s, .libPaths())); library(gambitR3D)", deparse(lib))
  } else if (file.exists(file.path(src, "DESCRIPTION"))) {
    sprintf("pkgload::load_all(%s, quiet = TRUE, export_all = FALSE, helpers = FALSE)", deparse(src))
  } else {
    "library(gambitR3D)"
  }
  writeLines(c(loader, sprintf("gambitR3D:::.g3_app(storage_dir = %s%s)", deparse(storage), app_args)),
             file.path(d, "app.R"))
  d
}

# Each app gets its own fresh headless Chrome, so browser storage and WebGL
# contexts never leak from one test into the next.
start_app <- function(width = 1440, height = 1000, envir = parent.frame(), chrome_args = NULL, app_args = "") {
  old <- if (chromote::has_default_chromote_object()) chromote::default_chromote_object() else NULL
  cm <- chromote::Chromote$new(browser = chromote::Chrome$new(
    args = c(chromote::default_chrome_args(), chrome_args)
  ))
  chromote::set_default_chromote_object(cm)
  withr::defer({
    if (!is.null(old)) chromote::set_default_chromote_object(old)
    try(cm$close(), silent = TRUE)
  }, envir = envir)
  storage <- withr::local_tempdir(.local_envir = envir)
  app <- shinytest2::AppDriver$new(app_dir(storage, envir, app_args), width = width, height = height,
                                   load_timeout = 90000, timeout = 30000, seed = 1, name = "g3")
  withr::defer(app$stop(), envir = envir)
  app$wait_for_js("window.G3 && G3.debug.board('play-board') && G3.debug.board('play-board').revision === 0",
                  timeout = 60000)
  attr(app, "storage") <- storage
  app
}

dbg <- function(app, id = "play-board") {
  jsonlite::fromJSON(app$get_js(sprintf("JSON.stringify(G3.debug.board('%s'))", id)), simplifyVector = TRUE)
}

wait_rev <- function(app, rev, id = "play-board", timeout = 30000) {
  app$wait_for_js(sprintf("(function(){var b=G3.debug.board('%s'); return b && b.revision >= %d && !b.pending;})()",
                          id, rev), timeout = timeout)
}

wait_scene <- function(app, id = "play-board") {
  app$wait_for_js(sprintf(
    "(function(){var b=G3.debug.board('%s'); return b && b.view==='3d' && b.scene && b.scene.ready && b.scene.active && !b.scene.animating;})()",
    id), timeout = 30000)
  Sys.sleep(0.3)
}

# Real input events through the DevTools protocol.
mouse_click <- function(app, x, y, modifiers = 0L) {
  s <- app$get_chromote_session()
  s$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
  s$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left", clickCount = 1L, modifiers = modifiers)
  s$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1L, modifiers = modifiers)
  Sys.sleep(0.15)
}

click_el <- function(app, selector) {
  r <- unlist(app$get_js(sprintf(
    "(function(){var e=document.querySelector(%s); e.scrollIntoView({block:'center', behavior:'instant'}); var r=e.getBoundingClientRect(); return [r.left+r.width/2, r.top+r.height/2];})()",
    jsonlite::toJSON(selector, auto_unbox = TRUE))))
  mouse_click(app, r[1], r[2])
}

click_cell <- function(app, square, id = "play-board") {
  click_el(app, sprintf("#%s .g3-cell[data-square='%s']", id, square))
}

press <- function(app, key, times = 1L) {
  codes <- c(Tab = 9, Enter = 13, Escape = 27, PageUp = 33, PageDown = 34, End = 35, Home = 36,
             ArrowLeft = 37, ArrowUp = 38, ArrowRight = 39, ArrowDown = 40)
  s <- app$get_chromote_session()
  for (i in seq_len(times)) {
    k <- codes[[key]]
    s$Input$dispatchKeyEvent(type = "rawKeyDown", key = key, code = key,
                             windowsVirtualKeyCode = k, nativeVirtualKeyCode = k)
    if (key == "Enter") s$Input$dispatchKeyEvent(type = "char", key = "Enter", text = "\r", windowsVirtualKeyCode = 13)
    s$Input$dispatchKeyEvent(type = "keyUp", key = key, code = key, windowsVirtualKeyCode = k, nativeVirtualKeyCode = k)
    Sys.sleep(0.08)
  }
}

focused_square <- function(app) {
  app$get_js("document.activeElement && document.activeElement.getAttribute('data-square')")
}

pick_3d <- function(app, square, modifiers = 0L) {
  app$run_js("document.querySelector('#play-board .g3-view-3d').scrollIntoView({block:'center', behavior:'instant'})")
  Sys.sleep(0.2)
  p <- app$get_js(sprintf("G3.debug.projectSquare('play-board', '%s')", square))
  mouse_click(app, p$x, p$y, modifiers)
}

shot <- function(app, name) {
  path <- evidence(name)
  unlink(path)
  # After a page reload shinytest2's idle helper is gone; then just pause.
  ok <- tryCatch({
    app$wait_for_idle(duration = 300, timeout = 20000)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) Sys.sleep(1)
  app$get_screenshot(path, screenshot_args = list(selector = "html"))
}

# Wait until the server's preference message has reached the browser.
wait_pref <- function(app, pref, value) {
  app$wait_for_js(sprintf("String(G3.prefs['%s']) === '%s'", pref, tolower(as.character(value))), timeout = 10000)
}

# A viewport capture (no full-page resize), for WebGL views: a full-page
# capture resizes the viewport and can catch the canvas mid-redraw.
shot_viewport <- function(app, name, selector = "#play-board .g3-board-toolbar") {
  path <- evidence(name)
  unlink(path)
  app$run_js(sprintf("document.querySelector(%s).scrollIntoView({block:'start', behavior:'instant'})",
                     jsonlite::toJSON(selector, auto_unbox = TRUE)))
  Sys.sleep(0.5)
  img <- app$get_chromote_session()$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(img$data), path)
  invisible(path)
}

set_pref <- function(app, pref, value) {
  click_el(app, sprintf("#play-board [data-pref='%s'][data-value='%s']", pref, value))
  if (identical(pref, "view") && identical(value, "3d")) {
    app$wait_for_js("G3.prefs.view === '3d' || !!G3.debug.board('play-board').fallback", timeout = 10000)
    expect_null(dbg(app)$fallback)
  }
  wait_pref(app, pref, value)
  Sys.sleep(0.3)
}

toggle_pref <- function(app, pref, value) {
  click_el(app, sprintf("#play-board [data-pref='%s']", pref))
  wait_pref(app, pref, value)
  Sys.sleep(0.3)
}

upload_fixture <- function(app, state, name, envir = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = envir), name)
  g3_write_game(.g3_game_new(state, metadata = list(mode = "hotseat")), path)
  app$upload_file(`play-files-upload` = path)
  path
}

layout_report <- function(app) {
  app$get_js("(function(){
    var W=document.documentElement.clientWidth, small=[];
    document.querySelectorAll('.g3-cell, .g3-nav-btn, .g3-seg-btn, .g3-toggle, .g3-tool, .g3-btn, .g3-action-panel button, .g3-select select').forEach(function(e){
      var r=e.getBoundingClientRect();
      if (r.width>0 && r.height>0 && (r.width<43.5 || r.height<43.5)) small.push((e.getAttribute('data-square')||e.className)+':'+Math.round(r.width)+'x'+Math.round(r.height));
    });
    return {scroll: document.documentElement.scrollWidth, client: W, small: small};
  })()")
}

# ----------------------------------------------------------------- tests

test_that("2D select and move, the same state in 3D, 3D picking and x-ray chooser", {
  browser_ok()
  app <- start_app(1440, 1000)
  set_pref(app, "view", "2d")
  click_cell(app, "W:1:1")
  expect_identical(dbg(app)$selection, "W:1:1")
  expect_match(app$get_js("document.querySelector('#play-board .g3-cell[data-square=\"N:1:1\"]').getAttribute('aria-label')"),
               "legal destination")
  shot(app, "01-2d-selected-1440-light-highest.png")
  # Pawn c2 to c4 on N (a cross-level double step); b4/W and b4/N are separate.
  click_cell(app, "N:1:1")
  wait_rev(app, 1)
  d2 <- dbg(app)
  expect_identical(d2$side, "black")
  app$wait_for_js("document.getElementById('g3-live').textContent.length > 0", timeout = 5000)
  live <- app$get_js("document.getElementById('g3-live').textContent")
  expect_match(live, "W:c2-N:c4", fixed = TRUE)
  set_pref(app, "view", "3d")
  wait_scene(app)
  d3 <- dbg(app)
  expect_identical(d3$revision, d2$revision)
  expect_identical(d3$position_key, d2$position_key)
  expect_equal(d3$scene$boards$N$y, 3.4)
  expect_equal(d3$scene$boards$W$z, 3)
  expect_equal(d3$scene$boards$B$z, -3)
  shot(app, "02-3d-same-state-1440-light-highest.png")
  # Find a black piece with legal actions whose square is hidden, from this
  # camera, behind another square (the nearest hit is not the piece's
  # square). Clicking it opens the occlusion chooser instead of guessing;
  # choosing it there selects the piece.
  app$run_js("document.querySelector('#play-board .g3-view-3d').scrollIntoView({block:'center', behavior:'instant'})")
  Sys.sleep(0.3)
  hidden <- app$get_js("(function(){var b=G3.boards['play-board']; var out=null;
    Object.keys(b.byFrom).concat(Object.keys(b.platformFrom)).forEach(function(sq){
      if (out) return; var p=b.scene.projectSquare(sq); if(!p) return;
      var h=b.scene.hits(p.x, p.y); if (h.length > 1 && h[0] !== sq && h.indexOf(sq) > 0) out = {square: sq, first: h[0]};
    }); return out;})()")
  # From White's side at the default elevation, B's rank 7 lies behind N.
  expect_false(is.null(hidden))
  pick_3d(app, hidden$square)
  occl <- unlist(app$get_js("Array.from(document.querySelectorAll('#play-board .g3-action-panel [data-act=\"square\"]')).map(function(b){return b.getAttribute('data-arg')})"))
  expect_true(hidden$square %in% occl)
  expect_identical(occl[1], hidden$first)
  shot(app, "03a-3d-occlusion-chooser-1440.png")
  click_el(app, sprintf("#play-board .g3-action-panel [data-act='square'][data-arg='%s']", hidden$square))
  expect_identical(dbg(app)$selection, hidden$square)
  app$run_js("G3.boards['play-board'].cancel()")
  # Select the knight on B b8 for the x-ray step.
  pick_3d(app, "B:0:3")
  if (!identical(dbg(app)$selection, "B:0:3")) {
    click_el(app, "#play-board .g3-action-panel [data-act='square'][data-arg='B:0:3']")
  }
  expect_identical(dbg(app)$selection, "B:0:3")
  # Turn on x-ray and click where B c6 lies under N c6: every square under
  # the pointer is offered.
  toggle_pref(app, "xray", TRUE)
  wait_scene(app)
  pick_3d(app, "B:1:1")
  chooser <- app$get_js("Array.from(document.querySelectorAll('#play-board .g3-action-panel [data-act=\"square\"]')).map(function(b){return b.getAttribute('data-arg')})")
  expect_true("B:1:1" %in% unlist(chooser))
  expect_gt(length(chooser), 1L)
  expect_match(app$get_js("document.querySelector('#play-board .g3-action-panel .g3-panel-title').textContent"), "X-ray")
  expect_match(app$get_js("document.querySelector('#play-board .g3-board-notices').textContent"), "X-ray view")
  shot(app, "03-3d-xray-chooser-1440-light-highest.png")
  click_el(app, "#play-board .g3-action-panel [data-act='square'][data-arg='B:1:1']")
  wait_rev(app, 2)
  expect_identical(app$get_js("G3.boards['play-board'].state.last.action_id"), "m:B:0:3-B:1:1")
  toggle_pref(app, "xray", FALSE)
  # Context loss: the board falls back to 2D with selection and history kept.
  wait_scene(app)
  pick_3d(app, "W:2:1")
  expect_identical(dbg(app)$selection, "W:2:1")
  expect_true(app$get_js("G3.debug.loseContext('play-board')"))
  app$wait_for_js("G3.debug.board('play-board').fallback === 'context_lost'", timeout = 10000)
  d4 <- dbg(app)
  expect_identical(d4$view, "2d")
  expect_identical(d4$selection, "W:2:1")
  expect_identical(d4$revision, 2L)
  shot(app, "04-context-lost-fallback-1440.png")
  expect_true(app$get_js("G3.debug.restoreContext('play-board')"))
  app$wait_for_js("!!document.querySelector('#play-board .g3-notice-warn button')", timeout = 10000)
  click_el(app, "#play-board .g3-notice-warn button")
  wait_scene(app)
  d5 <- dbg(app)
  expect_identical(d5$view, "3d")
  expect_identical(d5$selection, "W:2:1")
  expect_identical(d5$revision, 2L)
})

test_that("keyboard-only play, promotion chooser and a game to checkmate", {
  browser_ok()
  app <- start_app(1440, 1000)
  set_pref(app, "view", "2d")
  # Tab from the page into the first board grid, then PageDown to W.
  app$run_js("document.activeElement && document.activeElement.blur(); window.scrollTo(0,0)")
  for (i in 1:40) {
    press(app, "Tab")
    if (!is.null(focused_square(app))) break
  }
  expect_false(is.null(focused_square(app)))
  for (i in 1:7) {
    if (identical(app$get_js("document.activeElement.getAttribute('data-board')"), "W")) break
    press(app, "PageDown")
  }
  expect_identical(app$get_js("document.activeElement.getAttribute('data-board')"), "W")
  # Starting square of the roving tab stop is b1 (bottom left); go to d2.
  press(app, "End")
  press(app, "Home")
  sq <- focused_square(app)
  expect_identical(sq, "W:0:0")
  press(app, "ArrowUp")
  press(app, "ArrowRight", 2L)
  expect_identical(focused_square(app), "W:2:1")
  press(app, "Enter")
  expect_identical(dbg(app)$selection, "W:2:1")
  press(app, "Escape")
  expect_null(dbg(app)$selection)
  press(app, "Enter")
  press(app, "ArrowUp")
  expect_identical(focused_square(app), "W:2:2")
  press(app, "Enter")
  wait_rev(app, 1)
  expect_identical(app$get_js("G3.boards['play-board'].state.last.action_id"), "m:W:2:1-W:2:2")
  # Focus stays on the board after the update.
  expect_identical(focused_square(app), "W:2:2")
  shot(app, "05-keyboard-move-1440.png")

  # Promotion: the chooser offers Q, R, B, N; choose the rook.
  promo <- g3_setup_position(pcs(
    "wP P white B:1:2", "wPb P white B:0:2", "wK K white W:0:0",
    "bK K black BQ:0:1", "bP P black BQ:1:0"
  ))
  upload_fixture(app, promo, "promotion.g3.json")
  wait_rev(app, 2)
  click_cell(app, "B:1:2")
  click_cell(app, "B:1:3")
  labels <- unlist(app$get_js("Array.from(document.querySelectorAll('#play-board .g3-action-panel [data-act=\"action\"]')).map(function(b){return b.getAttribute('data-arg')})"))
  expect_setequal(labels, paste0("m:B:1:2-B:1:3=", c("Q", "R", "B", "N")))
  shot(app, "06-promotion-chooser-1440.png")
  click_el(app, "#play-board .g3-action-panel [data-arg='m:B:1:2-B:1:3=R']")
  wait_rev(app, 3)
  expect_identical(app$get_js("G3.boards['play-board'].state.last.action_id"), "m:B:1:2-B:1:3=R")

  # Castling is offered as its own action, and as a marked destination of
  # the selected king.
  st0 <- g3_new_game()$state
  castle <- g3_setup_position(st0$pieces[, c("id", "type", "color", "square_id")],
                              castling = c(white_kingside = TRUE, white_queenside = TRUE,
                                           black_kingside = TRUE, black_queenside = TRUE))
  upload_fixture(app, castle, "castling.g3.json")
  wait_rev(app, 4)
  expect_identical(app$get_js("document.querySelectorAll('#play-board .g3-action-panel [data-arg=\"c:white:kingside\"]').length"), 1L)
  click_cell(app, "WK:0:0")
  expect_match(app$get_js("document.querySelector('#play-board .g3-cell[data-square=\"WK:1:0\"]').getAttribute('aria-label')"),
               "castling destination")
  expect_true(app$get_js("document.querySelector('#play-board .g3-cell[data-square=\"WK:1:0\"]').classList.contains('is-target-castle')"))
  shot(app, "06b-castling-target-1440.png")
  click_el(app, "#play-board .g3-action-panel [data-arg='c:white:kingside']")
  wait_rev(app, 5)
  expect_identical(app$get_js("G3.boards['play-board'].state.last.action_id"), "c:white:kingside")

  # A complete short hotseat game: the mate-in-one position, played out.
  mate <- g3_setup_position(pcs(
    "wK K white WK:0:0",
    "bQ Q black B:0:0", "bRd R black N:2:3", "bRf R black BK:1:1", "bK K black B:0:3"
  ), side = "black")
  upload_fixture(app, mate, "mate-in-one.g3.json")
  wait_rev(app, 6)
  click_cell(app, "B:0:0")
  click_cell(app, "B:3:0")
  wait_rev(app, 7)
  expect_identical(dbg(app)$side, "white")
  app$wait_for_js("document.querySelector('#play-gamebar').textContent.indexOf('Checkmate') >= 0", timeout = 10000)
  expect_match(app$get_js("document.getElementById('g3-live').textContent"), "Checkmate")
  expect_false(app$get_js("G3.boards['play-board'].state.interactive"))
  expect_match(app$get_js("document.querySelector('#play-moves').textContent"), "Black wins")
  shot(app, "07-hotseat-checkmate-1440.png")
})

test_that("a platform moves with a half-turn and follows N at both heights", {
  browser_ok()
  app <- start_app(768, 1000)
  rook <- g3_setup_position(pcs("wK K white W:1:0", "wR R white WK:0:0", "bK K black B:2:3"))
  upload_fixture(app, rook, "platform.g3.json")
  wait_rev(app, 1)
  set_pref(app, "view", "2d")
  click_cell(app, "WK:0:0")
  click_el(app, "#play-board .g3-action-panel [data-act='platform']")
  opts <- unlist(app$get_js("Array.from(document.querySelectorAll('#play-board .g3-dock-list [data-act=\"action\"]')).map(function(b){return b.getAttribute('data-arg')})"))
  expect_setequal(opts, c("t:WK>N:R:lo@0", "t:WK>N:R:lo@180", "t:WK>W:R:hi@0", "t:WK>W:R:hi@180"))
  shot(app, "08-platform-chooser-768.png")
  # Switch to 3D with the chooser open: hovering an option shows a labelled
  # preview ghost at the destination dock; the game is unchanged.
  set_pref(app, "view", "3d")
  wait_scene(app)
  sel <- "#play-board .g3-dock-list [data-arg='t:WK>N:R:lo@180']"
  r <- unlist(app$get_js(sprintf("(function(){var e=document.querySelector(%s); e.scrollIntoView({block:'center', behavior:'instant'}); var r=e.getBoundingClientRect(); return [r.left+r.width/2, r.top+r.height/2];})()",
                                 jsonlite::toJSON(sel, auto_unbox = TRUE))))
  app$get_chromote_session()$Input$dispatchMouseEvent(type = "mouseMoved", x = r[1], y = r[2])
  Sys.sleep(0.4)
  app$run_js("document.querySelector('#play-board .g3-view-3d').scrollIntoView({block:'start', behavior:'instant'})")
  shot(app, "08b-platform-preview-3d-768.png")
  expect_identical(dbg(app)$revision, 1L)
  click_el(app, sel)
  # The transport is animated from the old parent transform to the new one;
  # only the committed end point is playable.
  app$wait_for_js("G3.debug.board('play-board').revision >= 2", timeout = 10000)
  app$wait_for_js("G3.debug.board('play-board').scene.animating === true", timeout = 3000, interval = 20)
  wait_rev(app, 2)
  wait_scene(app)
  set_pref(app, "view", "2d")
  # 2D: WK is drawn beside N, marked as turned, and the rook kept its square.
  expect_identical(app$get_js("document.querySelector('#play-board .g3-tier[data-parent=\"N\"] [data-board=\"WK\"]') !== null"), TRUE)
  expect_match(app$get_js("document.querySelector('#play-board .g3-grid-block[data-board=\"WK\"] .g3-grid-meta').textContent"), "turned 180")
  expect_match(app$get_js("document.querySelector('#play-board .g3-cell[data-square=\"WK:0:0\"]').getAttribute('aria-label')"), "white rook")
  shot(app, "08c-platform-on-N-2d-768.png")
  set_pref(app, "view", "3d")
  wait_scene(app)
  s <- dbg(app)$scene$boards
  expect_equal(c(s$WK$x, s$WK$y, s$WK$z), c(2, 3.4 + 0.9, 2))
  shot(app, "09-platform-on-N-3d-highest-768.png")
  set_pref(app, "neutral_height", "lowest")
  app$wait_for_js("G3.boards['play-board'].state.geometry.neutral_height === 'lowest'", timeout = 10000)
  wait_scene(app)
  # With reduced motion the next change jumps to its end point.
  app$run_js("Shiny.setInputValue('play-board-pref', {name: 'reduced_motion', value: true, nonce: Date.now()}, {priority: 'event'})")
  wait_pref(app, "reduced_motion", TRUE)
  set_pref(app, "neutral_height", "highest")
  app$wait_for_js("G3.boards['play-board'].state.geometry.neutral_height === 'highest'", timeout = 10000)
  expect_false(dbg(app)$scene$animating)
  expect_equal(dbg(app)$scene$boards$N$y, 3.4)
  app$run_js("Shiny.setInputValue('play-board-pref', {name: 'reduced_motion', value: false, nonce: Date.now()}, {priority: 'event'})")
  wait_pref(app, "reduced_motion", FALSE)
  set_pref(app, "neutral_height", "lowest")
  app$wait_for_js("G3.boards['play-board'].state.geometry.neutral_height === 'lowest'", timeout = 10000)
  wait_scene(app)
  s <- dbg(app)$scene$boards
  expect_equal(c(s$WK$y, s$N$y, s$W$y, s$B$y), c(0.9, 0, 3.4, 3.4))
  expect_equal(c(s$W$z, s$N$z, s$B$z), c(3, 0, -3))
  expect_identical(dbg(app)$revision, 2L)
  shot(app, "10-platform-on-N-3d-lowest-768.png")
  # Exploded inspection view is labelled and resets to the true gap.
  toggle_pref(app, "exploded", TRUE)
  app$wait_for_js("G3.boards['play-board'].state.geometry.exploded === true", timeout = 10000)
  wait_scene(app)
  s <- dbg(app)$scene$boards
  expect_equal(c(s$W$z, s$N$z, s$B$z, s$WK$z), c(5.25, 0, -5.25, 2))
  expect_match(app$get_js("document.querySelector('#play-board .g3-board-notices').textContent"), "Exploded inspection view")
  shot(app, "11-exploded-labelled-768.png")
  toggle_pref(app, "exploded", FALSE)
  app$wait_for_js("G3.boards['play-board'].state.geometry.exploded === false", timeout = 10000)
  wait_scene(app)
  expect_equal(app$get_js("G3.boards['play-board'].state.geometry.edge_gap"), 2)
  expect_identical(dbg(app)$revision, 2L)
})

test_that("with WebGL disabled the board falls back to a working 2D view", {
  browser_ok()
  app <- start_app(1024, 900, chrome_args = c("--disable-webgl", "--disable-3d-apis"))
  app$wait_for_js("G3.debug.board('play-board').fallback === 'webgl'", timeout = 20000)
  d <- dbg(app)
  expect_identical(d$view, "2d")
  app$wait_for_js("document.querySelector('#play-board .g3-board-notices').textContent.indexOf('WebGL') >= 0",
                  timeout = 10000)
  expect_match(app$get_js("document.querySelector('#play-board .g3-board-notices').textContent"), "WebGL")
  click_cell(app, "W:3:1")
  click_cell(app, "W:3:2")
  wait_rev(app, 1)
  expect_identical(dbg(app)$side, "black")
  shot(app, "12-webgl-disabled-fallback-1024.png")
})

test_that("a game against the computer and a background analysis", {
  browser_ok()
  app <- start_app(1440, 1000)
  click_el(app, "#play-new_game")
  app$wait_for_js("!!document.querySelector('#play-start_game')", timeout = 10000)
  app$set_inputs(`play-mode` = "computer", wait_ = FALSE)
  app$set_inputs(`play-agent` = "search", `play-budget` = "quick", `play-human_color` = "white", wait_ = FALSE)
  Sys.sleep(0.5)
  click_el(app, "#play-start_game")
  app$wait_for_js("document.querySelector('#play-gamebar').textContent.indexOf('Search') >= 0", timeout = 15000)
  set_pref(app, "view", "2d")
  click_cell(app, "W:1:1")
  click_cell(app, "W:1:3")
  wait_rev(app, 2)
  wait_rev(app, 3, timeout = 120000)
  d <- dbg(app)
  expect_identical(d$side, "white")
  last <- app$get_js("G3.boards['play-board'].state.last")
  expect_identical(last$actor, "black")
  expect_match(app$get_js("document.querySelector('#play-moves').textContent"), "2.")
  shot(app, "13-cpu-reply-1440.png")
  # Analyse a copy: a separate branch, background search alternatives.
  click_el(app, "#play-analyse_copy")
  app$wait_for_js("G3.debug.board('analyse-board') && G3.debug.board('analyse-board').revision === 0", timeout = 15000)
  click_el(app, "#analyse-prev")
  app$wait_for_js("document.querySelector('#analyse-ply_label').textContent.indexOf('1') >= 0", timeout = 10000)
  click_el(app, "#analyse-run")
  app$wait_for_js("!!document.querySelector('#analyse-engine table')", timeout = 120000)
  expect_match(app$get_js("document.querySelector('#analyse-engine').textContent"), "nodes")
  shot(app, "14-analyse-branch-1440.png")
  expect_identical(dbg(app)$revision, d$revision)
  expect_identical(dbg(app)$position_key, d$position_key)
})

layout_check <- function(w) {
  app <- start_app(w, if (w == 320L) 900L else 1000L)
  for (theme in c("light", "dark")) {
    app$run_js(sprintf("Shiny.setInputValue('play-board-pref', {name: 'theme', value: '%s', nonce: Date.now()}, {priority: 'event'})", theme))
    app$wait_for_js(sprintf("document.documentElement.getAttribute('data-bs-theme') === '%s'", theme), timeout = 10000)
    for (h in c("highest", "lowest")) {
      set_pref(app, "neutral_height", h)
      app$wait_for_js(sprintf("G3.boards['play-board'].state.geometry.neutral_height === '%s'", h), timeout = 10000)
      set_pref(app, "view", "2d")
      click_cell(app, "W:2:1")
      expect_identical(dbg(app)$selection, "W:2:1")
      rep2 <- layout_report(app)
      expect_lte(rep2$scroll, rep2$client)
      expect_identical(length(rep2$small), 0L, info = paste(unlist(rep2$small), collapse = ", "))
      app$run_js("window.scrollTo(0,0)")
      shot(app, sprintf("15-layout-%d-%s-%s-2d.png", w, theme, h))
      set_pref(app, "view", "3d")
      wait_scene(app)
      expect_identical(dbg(app)$selection, "W:2:1")
      expect_equal(dbg(app)$scene$boards$N$y, if (h == "highest") 3.4 else 0)
      # The default camera keeps all seven boards inside the canvas.
      inv <- unlist(dbg(app)$scene$in_view)
      expect_setequal(names(inv), c("W", "N", "B", "WQ", "WK", "BQ", "BK"))
      expect_true(all(inv == 1), info = paste(names(inv), inv, collapse = " "))
      rep3 <- layout_report(app)
      expect_lte(rep3$scroll, rep3$client)
      shot_viewport(app, sprintf("15-layout-%d-%s-%s-3d.png", w, theme, h))
      app$run_js("G3.boards['play-board'].cancel()")
    }
  }
  expect_identical(dbg(app)$revision, 0L)
  # Every resource came from the app itself: nothing from the network.
  res <- unlist(app$get_js("performance.getEntriesByType('resource').map(function(e){return e.name})"))
  expect_true(all(startsWith(res, sub("/$", "", app$get_url()))), info = paste(res, collapse = " "))
}

test_that("layout at 320 px: light and dark, both Neutral heights", {
  browser_ok()
  layout_check(320L)
})

test_that("layout at 768 px: light and dark, both Neutral heights", {
  browser_ok()
  layout_check(768L)
})

test_that("layout at 1440 px: light and dark, both Neutral heights", {
  browser_ok()
  layout_check(1440L)
})

test_that("navigation keeps the game, moves focus, offers More on phones, and switches language", {
  browser_ok()
  app <- start_app(1440, 1000)
  set_pref(app, "view", "2d")
  click_cell(app, "W:1:1")
  click_cell(app, "W:1:3")
  wait_rev(app, 1)
  key <- dbg(app)$position_key
  for (sec in c("analyse", "database", "coach", "models", "training", "settings")) {
    click_el(app, sprintf(".g3-rail [data-section='%s']", sec))
    app$wait_for_js(sprintf("document.activeElement && document.activeElement.closest('.tab-pane[data-value=\"%s\"]') !== null && document.activeElement.classList.contains('g3-section-title')", sec),
                    timeout = 10000)
    expect_identical(app$get_js(sprintf("document.querySelector('.g3-rail [data-section=\"%s\"]').getAttribute('aria-current')", sec)), "page")
    shot(app, sprintf("17-section-%s-1440.png", sec))
  }
  # The sections are connected: the catalogue is open and lists this game,
  # and the reference-only models cannot be chosen as opponents.
  expect_match(app$get_js("document.querySelector('#database-status').textContent"), "games.sqlite", fixed = TRUE)
  expect_match(app$get_js("document.querySelector('#database-table').textContent"),
               dbg(app)$game_id, fixed = TRUE)
  expect_match(app$get_js("document.querySelector('#models-list').textContent"), "reference only")
  app$run_js("Shiny.setInputValue('settings-lang', 'de')")
  app$wait_for_js("document.querySelector('.g3-rail [data-section=\"play\"] .g3-nav-label').textContent === 'Spielen'", timeout = 10000)
  click_el(app, ".g3-rail [data-section='play']")
  app$wait_for_js("document.querySelector('#play-gamebar').textContent.indexOf('Schwarz am Zug') >= 0", timeout = 10000)
  expect_identical(dbg(app)$position_key, key)
  expect_identical(dbg(app)$revision, 1L)
  expect_match(app$get_js("document.querySelector('#play-board .g3-cell[data-square=\"W:1:3\"]').getAttribute('aria-label')"), "weißer Bauer")
  shot(app, "18-play-german-1440.png")
  # Phone: bottom bar with Play, Analyse, Training and a labelled More menu.
  app$set_window_size(width = 320, height = 800)
  Sys.sleep(0.8)
  expect_true(app$get_js("getComputedStyle(document.querySelector('.g3-bottombar')).display !== 'none'"))
  expect_identical(app$get_js("getComputedStyle(document.querySelector('.g3-rail')).display"), "none")
  click_el(app, "#g3-more-btn")
  expect_identical(app$get_js("document.getElementById('g3-more-btn').getAttribute('aria-expanded')"), "true")
  shot(app, "19-more-menu-320.png")
  press(app, "Escape")
  expect_identical(app$get_js("document.getElementById('g3-more-menu').hidden"), TRUE)
  click_el(app, "#g3-more-btn")
  click_el(app, "#g3-more-menu [data-section='coach']")
  app$wait_for_js("document.activeElement && document.activeElement.closest('.tab-pane[data-value=\"coach\"]') !== null", timeout = 10000)
  expect_match(app$get_js("document.querySelector('.tab-pane[data-value=\"coach\"]').textContent"), "W:c2-W:c4", fixed = TRUE)
  expect_true(app$get_js("document.getElementById('g3-more-btn').classList.contains('is-current')"))
  rep <- layout_report(app)
  expect_lte(rep$scroll, rep$client)
  shot(app, "20-coach-facts-320.png")
  click_el(app, ".g3-bottombar > ul > li > [data-section='play']")
  expect_identical(dbg(app)$position_key, key)
  # Appearance and language are remembered by the browser; a new session
  # (page reload) restores them and offers the autosaved game for resuming.
  app$run_js("Shiny.setInputValue('play-board-pref', {name: 'theme', value: 'dark', nonce: Date.now()}, {priority: 'event'})")
  app$wait_for_js("document.documentElement.getAttribute('data-bs-theme') === 'dark' && G3.prefsSynced", timeout = 10000)
  app$run_js("location.reload()")
  Sys.sleep(2)
  app$wait_for_js("window.G3 && G3.prefsSynced && G3.debug.board('play-board') && G3.debug.board('play-board').revision === 0",
                  timeout = 60000)
  expect_identical(app$get_js("document.documentElement.getAttribute('data-bs-theme')"), "dark")
  expect_identical(app$get_js("document.querySelector('.g3-bottombar [data-section=\"play\"] .g3-nav-label').textContent"), "Spielen")
  app$wait_for_js("document.querySelector('#play-resume') && document.querySelector('#play-resume').textContent.indexOf('Halbzug 1') >= 0",
                  timeout = 15000)
  click_el(app, "#play-resume_latest")
  wait_rev(app, 1)
  expect_identical(dbg(app)$position_key, key)
  shot(app, "21-reload-restored-dark-german-320.png")
})

test_that("an answer that arrives after the board stopped waiting is reconciled, never misreported", {
  browser_ok()
  app <- start_app(1440, 1000)
  set_pref(app, "view", "2d")
  # Hold the next board request in the browser (as a slow network would) and
  # let the board give up after 0.5 s instead of 20 s.
  hold <- paste(
    "G3.answerTimeoutMs = 500; window.__held = null;",
    "if (!window.__origSet) window.__origSet = Shiny.setInputValue;",
    "Shiny.setInputValue = function (n, v, o) {",
    "  if (/-request$/.test(n) && !window.__held) { window.__held = [n, v, o]; return; }",
    "  return window.__origSet.call(Shiny, n, v, o); };"
  )
  release <- function(tamper = FALSE) {
    app$run_js(paste(
      "Shiny.setInputValue = window.__origSet; var h = window.__held; window.__held = null;",
      if (tamper) "h[1] = Object.assign({}, h[1], {expected_state_hash: 'not-the-position'});",
      "window.__origSet.call(Shiny, h[0], h[1], h[2]);"
    ))
  }
  no_answer <- "No answer from the server yet. The action is not confirmed"
  # 1. The server answers late with a rejection: the board says so, nothing moved.
  app$run_js(hold)
  click_cell(app, "W:1:1")
  click_cell(app, "W:1:3")
  app$wait_for_js("!!window.__held && !!G3.debug.board('play-board').late", timeout = 10000)
  d <- dbg(app)
  expect_match(d$message, no_answer, fixed = TRUE)
  expect_identical(d$revision, 0L)
  shot_viewport(app, "22-no-answer-yet-1440.png", "#play-board .g3-view-2d")
  release(tamper = TRUE)
  app$wait_for_js("(G3.debug.board('play-board').message || '').indexOf('did not play the action') >= 0",
                  timeout = 15000)
  expect_identical(dbg(app)$revision, 0L)
  # 2. The server answers late with a commit: the board shows the move and says why.
  # (A rejected action keeps its piece selected, as on a prompt rejection.)
  app$run_js(hold)
  if (!identical(dbg(app)$selection, "W:1:1")) click_cell(app, "W:1:1")
  click_cell(app, "W:1:3")
  app$wait_for_js("!!window.__held && !!G3.debug.board('play-board').late", timeout = 10000)
  expect_identical(dbg(app)$revision, 0L)
  release()
  wait_rev(app, 1)
  app$wait_for_js("(G3.debug.board('play-board').message || '').indexOf('confirmed the action late') >= 0",
                  timeout = 15000)
  expect_identical(dbg(app)$revision, 1L)
  app$wait_for_js("document.querySelector('#play-moves').textContent.indexOf('c2') >= 0", timeout = 15000)
  shot(app, "23-late-commit-1440.png")
})

test_that("without a game catalogue the catalogue controls are disabled and say why", {
  browser_ok()
  app <- start_app(1440, 1000, app_args = ", db_path = FALSE")
  click_el(app, ".g3-rail [data-section='database']")
  app$wait_for_js("document.querySelector('#database-import_files') && document.querySelector('#database-import_files').disabled",
                  timeout = 15000)
  for (id in c("database-backup", "database-export_sel", "database-delete_sel", "database-show_quarantine")) {
    expect_true(app$get_js(sprintf("document.getElementById('%s').disabled", id)), info = id)
  }
  expect_true(app$get_js(
    "document.querySelector('#database-import_files').closest('.input-group').querySelector('.btn-file').classList.contains('disabled')"
  ))
  for (out in c("import_report", "export_report", "backup_report")) {
    expect_match(app$get_js(sprintf("document.getElementById('database-%s').textContent", out)),
                 "Not available: no game catalogue is open.", fixed = TRUE)
  }
  expect_match(app$get_js("document.getElementById('database-status').textContent"), "switched off", fixed = TRUE)
  shot(app, "24-database-without-catalogue-1440.png")
})

