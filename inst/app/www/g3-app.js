/*
 * gambitR3D browser controller.
 *
 * Draws what the R server sends and sends back intentions. It never decides
 * legality: the server's "g3-board-state" message carries the committed
 * position and the legal actions from g3_legal_actions(); this file filters
 * them to show destinations, and every chosen action goes back to R as
 * { kind, game_id, revision, request_id, expected_state_hash, action_id }.
 * The server answers with "g3-board-ack" (committed / duplicate /
 * rejected) and, after a commit, a new state.
 *
 * One Board instance per widget element (.g3-board) holds the selection,
 * the 2D grids and, lazily, the 3D scene (g3-scene.js, an ES module loaded
 * through the page's import map). Selection lives here, so it survives
 * switching between 2D and 3D and a WebGL failure.
 *
 * 2D keyboard pattern (each board is an ARIA grid with a roving tab stop):
 *   Tab / Shift+Tab      next / previous board (and other controls)
 *   Arrow keys           move within the board, in screen direction
 *   Home / End           first / last square of the row
 *   PageUp / PageDown    previous / next board in reading order
 *   Enter / Space        select a piece, or play the focused destination
 *   Escape               cancel the selection or close a chooser
 */
(function () {
  'use strict';

  const G3 = (window.G3 = window.G3 || {});
  G3.messages = {};
  G3.lang = 'en';
  G3.prefs = {
    view: '3d', default_view: '3d', neutral_height: 'highest', exploded: false, xray: false,
    level: 'all', camera: 'white', reduced_motion: false, theme: 'light'
  };
  G3.boards = {};
  G3.prefsSynced = false;
  const STORE_KEY = 'gambitR3D.prefs.v1';
  const MAIN = ['W', 'N', 'B'];
  const BOARD_ORDER = ['B', 'BQ', 'BK', 'N', 'W', 'WQ', 'WK'];
  const PIECE_KEYS = { P: 'pawn', N: 'knight', B: 'bishop', R: 'rook', Q: 'queen', K: 'king' };

  // ------------------------------------------------------------- helpers

  G3.t = function (key, vars) {
    let txt = Object.prototype.hasOwnProperty.call(G3.messages, key) ? G3.messages[key] : key;
    if (vars) {
      Object.keys(vars).forEach(function (k) {
        txt = txt.split('{' + k + '}').join(String(vars[k]));
      });
    }
    return txt;
  };

  G3.uid = function (prefix) {
    const r = (window.crypto && window.crypto.getRandomValues)
      ? Array.from(window.crypto.getRandomValues(new Uint32Array(2))).map((x) => x.toString(16)).join('')
      : String(Math.random()).slice(2);
    return prefix + '-' + Date.now().toString(36) + '-' + r;
  };

  function el(tag, attrs, children) {
    const node = document.createElement(tag);
    if (attrs) {
      Object.keys(attrs).forEach(function (k) {
        const v = attrs[k];
        if (v === null || v === undefined || v === false) return;
        if (k === 'text') node.textContent = v;
        else if (k === 'className') node.className = v;
        else if (k.startsWith('on') && typeof v === 'function') node.addEventListener(k.slice(2), v);
        else node.setAttribute(k, v === true ? '' : v);
      });
    }
    (children || []).forEach(function (c) {
      if (c === null || c === undefined) return;
      node.appendChild(typeof c === 'string' ? document.createTextNode(c) : c);
    });
    return node;
  }
  G3.el = el;

  function storeGet() {
    try {
      const raw = window.localStorage.getItem(STORE_KEY);
      return raw ? JSON.parse(raw) : null;
    } catch (e) {
      return null;
    }
  }
  function storeSet(v) {
    try {
      window.localStorage.setItem(STORE_KEY, JSON.stringify(v));
    } catch (e) {
      /* storage unavailable: preferences simply are not remembered */
    }
  }

  function setInput(name, value) {
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue(name, value, { priority: 'event' });
    }
  }

  G3.announce = function (text) {
    const live = document.getElementById('g3-live');
    if (!live || !text) return;
    live.textContent = '';
    window.setTimeout(function () { live.textContent = text; }, 60);
  };

  G3.pieceName = function (piece) {
    if (!piece) return G3.t('cell.empty');
    return G3.t('piece.' + piece.color + '.' + PIECE_KEYS[piece.type]);
  };

  G3.boardName = function (id) {
    return G3.t('boardname.' + id);
  };

  // --------------------------------------------------------------- i18n

  G3.applyI18n = function (root) {
    root = root || document;
    root.querySelectorAll('[data-i18n]').forEach(function (node) {
      node.textContent = G3.t(node.getAttribute('data-i18n'));
    });
    root.querySelectorAll('[data-i18n-attr]').forEach(function (node) {
      node.getAttribute('data-i18n-attr').split(';').forEach(function (pair) {
        const i = pair.indexOf(':');
        if (i > 0) node.setAttribute(pair.slice(0, i), G3.t(pair.slice(i + 1)));
      });
    });
  };

  // --------------------------------------------------------- preferences

  function applyGlobalPrefs(p) {
    const root = document.documentElement;
    root.setAttribute('data-bs-theme', p.theme === 'dark' ? 'dark' : 'light');
    root.classList.toggle('g3-reduced-motion', !!p.reduced_motion);
  }

  function sendClientPrefs() {
    const stored = storeGet() || {};
    const mq = function (q) {
      try { return window.matchMedia(q).matches; } catch (e) { return false; }
    };
    const msg = {
      theme: stored.theme || (mq('(prefers-color-scheme: dark)') ? 'dark' : 'light'),
      reduced_motion: stored.reduced_motion !== undefined ? !!stored.reduced_motion : mq('(prefers-reduced-motion: reduce)'),
      neutral_height: stored.neutral_height || null,
      default_view: stored.default_view || null,
      lang: stored.lang || null,
      nonce: Date.now()
    };
    setInput('client_prefs', msg);
  }

  // ---------------------------------------------------------- navigation

  function closeMore(focusButton) {
    const btn = document.getElementById('g3-more-btn');
    const menu = document.getElementById('g3-more-menu');
    if (!btn || !menu) return;
    btn.setAttribute('aria-expanded', 'false');
    menu.hidden = true;
    if (focusButton) btn.focus();
  }

  function markSection(section) {
    document.querySelectorAll('.g3-nav-btn[data-section]').forEach(function (b) {
      if (b.getAttribute('data-section') === section) b.setAttribute('aria-current', 'page');
      else b.removeAttribute('aria-current');
    });
    const more = document.getElementById('g3-more-btn');
    if (more) {
      const inMore = !!document.querySelector('#g3-more-menu [data-section="' + section + '"]');
      more.classList.toggle('is-current', inMore);
    }
    document.body.setAttribute('data-section', section);
  }

  function bindNavigation() {
    document.addEventListener('click', function (ev) {
      const btn = ev.target.closest('.g3-nav-btn[data-section]');
      if (btn) {
        const section = btn.getAttribute('data-section');
        markSection(section);
        closeMore(false);
        setInput('nav', { section: section, focus: true, nonce: Date.now() });
        return;
      }
      const more = ev.target.closest('#g3-more-btn');
      if (more) {
        const menu = document.getElementById('g3-more-menu');
        const open = more.getAttribute('aria-expanded') === 'true';
        more.setAttribute('aria-expanded', open ? 'false' : 'true');
        menu.hidden = open;
        if (!open) {
          const first = menu.querySelector('button');
          if (first) first.focus();
        }
        return;
      }
      if (!ev.target.closest('#g3-more-menu')) closeMore(false);
    });
    document.addEventListener('keydown', function (ev) {
      const menu = document.getElementById('g3-more-menu');
      if (!menu || menu.hidden) return;
      if (ev.key === 'Escape') {
        ev.preventDefault();
        closeMore(true);
      } else if (ev.key === 'ArrowDown' || ev.key === 'ArrowUp') {
        const items = Array.from(menu.querySelectorAll('button'));
        const i = items.indexOf(document.activeElement);
        if (i < 0) return;
        ev.preventDefault();
        const j = (i + (ev.key === 'ArrowDown' ? 1 : items.length - 1)) % items.length;
        items[j].focus();
      }
    });
  }

  // --------------------------------------------------------------- Board

  class Board {
    constructor(node) {
      this.el = node;
      this.id = node.id;
      this.kind = node.getAttribute('data-kind') || 'live';
      this.toolbar = node.querySelector('.g3-board-toolbar');
      this.notices = node.querySelector('.g3-board-notices');
      this.view2d = node.querySelector('.g3-view-2d');
      this.view3d = node.querySelector('.g3-view-3d');
      this.panel = node.querySelector('.g3-action-panel');
      this.state = null;
      this.selection = null;
      this.pending = null;
      this.awaitRevision = null;
      this.chooser = null;
      this.message = null;
      this.view = '2d';
      this.fallback = null;
      this.restorable = false;
      this.scene = null;
      this.sceneLoading = null;
      this.cells = {};
      this.blocks = {};
      this.tabStop = {};
      this.lastAnnounced = null;
      this.panelKey = '';
      this.build2D();
      this.bindToolbar();
      this.bindPanel();
    }

    // ---------------------------------------------------- state & index

    setState(p) {
      const prev = this.state;
      const same = prev && prev.game_id === p.game_id && prev.revision === p.revision &&
        prev.state_hash === p.state_hash;
      this.state = p;
      this.index();
      if (!same) {
        this.selection = null;
        this.chooser = null;
        this.message = null;
        if (this.lastAnnounced === null) {
          this.lastAnnounced = p.revision;
        } else if (p.announce && p.announce.revision === p.revision && this.lastAnnounced !== p.revision) {
          this.lastAnnounced = p.revision;
          G3.announce(p.announce.text);
        }
      } else if (this.selection && !this.isSelectable(this.selection)) {
        this.selection = null;
      }
      if (this.awaitRevision !== null && p.revision >= this.awaitRevision) this.awaitRevision = null;
      this.render();
      if (this.scene) this.scene.update(p, same ? null : prev, this.sceneOptions());
    }

    index() {
      const p = this.state;
      this.sq = {};
      p.squares.forEach((s) => { this.sq[s.id] = s; });
      this.byFrom = {};
      this.platformFrom = {};
      this.castles = [];
      p.actions.forEach((a) => {
        if (a.type === 'platform') {
          (this.platformFrom[a.from] = this.platformFrom[a.from] || []).push(a);
        } else {
          (this.byFrom[a.from] = this.byFrom[a.from] || []).push(a);
          if (a.type === 'castle') this.castles.push(a);
        }
      });
      this.boardInfo = {};
      p.boards.forEach((b) => { this.boardInfo[b.id] = b; });
    }

    canAct() {
      return !!(this.state && this.state.interactive && !this.pending && this.awaitRevision === null);
    }

    isSelectable(id) {
      return !!((this.byFrom[id] && this.byFrom[id].length) || (this.platformFrom[id] && this.platformFrom[id].length));
    }

    targetsOf(id) {
      const out = {};
      (this.byFrom[id] || []).forEach((a) => {
        const k = a.to;
        out[k] = out[k] || [];
        out[k].push(a);
      });
      return out;
    }

    squareName(id) {
      const s = this.sq[id];
      return s ? G3.boardName(s.board) + ' ' + s.coord : id;
    }

    // ------------------------------------------------------- interaction

    activate(id) {
      if (!this.state) return;
      if (this.pending || this.awaitRevision !== null) {
        this.flash('info', G3.t('board.waiting'));
        return;
      }
      if (!this.state.interactive) {
        this.flash('info', this.state.note || G3.t('board.not_interactive'));
        return;
      }
      const sel = this.selection;
      if (sel) {
        const acts = (this.byFrom[sel] || []).filter((a) => a.to === id);
        if (acts.length) {
          this.choose(acts);
          return;
        }
        if (id === sel) {
          this.cancel();
          return;
        }
      }
      if (this.isSelectable(id)) {
        this.selection = id;
        this.chooser = null;
        this.message = null;
        this.render();
        this.pushSelection();
        return;
      }
      const s = this.sq[id];
      if (sel) {
        this.flash('info', G3.t('board.no_action_to', { square: this.squareName(id), from: this.squareName(sel) }));
      } else if (s && s.piece && s.piece.color !== this.state.side) {
        this.flash('info', G3.t('board.not_your_piece', { side: G3.t('side.' + this.state.side) }));
      } else if (s && s.piece) {
        this.flash('info', G3.t('board.no_actions_piece', { square: this.squareName(id) }));
      } else {
        this.flash('info', G3.t('board.select_hint'));
      }
    }

    choose(acts) {
      if (acts.length === 1) {
        this.send(acts[0]);
      } else {
        this.chooser = { kind: 'promotion', actions: acts };
        this.renderPanel(true);
      }
    }

    cancel() {
      if (this.chooser) {
        this.chooser = null;
      } else {
        this.selection = null;
      }
      this.message = null;
      this.render();
      this.pushSelection();
      if (this.scene) this.scene.setPreview(null);
    }

    pushSelection() {
      setInput(this.id + '-selection', this.selection || '');
    }

    flash(type, text) {
      this.message = { type: type, text: text };
      this.renderPanel(false);
    }

    send(action) {
      const p = this.state;
      const req = {
        kind: 'action', game_id: p.game_id, revision: p.revision, request_id: G3.uid('req'),
        expected_state_hash: p.state_hash, action_id: action.id, sent_at: Date.now()
      };
      this.pending = { req: req, action: action };
      this.chooser = null;
      this.message = { type: 'info', text: G3.t('board.sending', { action: action.label }) };
      if (this.scene) this.scene.setPreview(null);
      this.render();
      setInput(this.id + '-request', req);
      const self = this;
      window.clearTimeout(this.pendingTimer);
      this.pendingTimer = window.setTimeout(function () {
        if (self.pending && self.pending.req.request_id === req.request_id) {
          self.pending = null;
          self.flash('error', G3.t('board.no_answer'));
          self.render();
        }
      }, 20000);
    }

    onAck(ack) {
      if (!this.pending || ack.request_id !== this.pending.req.request_id) return;
      window.clearTimeout(this.pendingTimer);
      const ok = ack.status === 'committed' || (ack.status === 'duplicate' && ack.original_status === 'committed');
      this.pending = null;
      if (ok) {
        this.message = null;
        if (!this.state || this.state.revision < ack.revision) this.awaitRevision = ack.revision;
      } else {
        // Keep the selection; explain beside the attempted action.
        this.message = { type: 'error', text: ack.message_text || G3.t('ack.invalid_request') };
      }
      this.render();
    }

    // Picks from the 3D view: a list of squares under the pointer, nearest
    // first. An occluded relevant square, an x-ray pick or an explicit
    // request opens a chooser instead of guessing.
    onPick(ids, opts) {
      if (!ids.length) return;
      opts = opts || {};
      const relevant = (id) => this.isSelectable(id) ||
        (this.selection && (this.byFrom[this.selection] || []).some((a) => a.to === id));
      if (opts.all || (opts.xray && ids.length > 1)) {
        this.openSquareChooser(ids, opts.all ? 'all' : 'xray');
        return;
      }
      const first = ids[0];
      const hidden = ids.slice(1).filter(relevant);
      if (!relevant(first) && hidden.length) {
        this.openSquareChooser([first].concat(hidden), 'occluded');
        return;
      }
      this.activate(first);
    }

    openSquareChooser(ids, why) {
      this.chooser = { kind: 'squares', squares: ids, why: why };
      this.renderPanel(true);
    }

    // ----------------------------------------------------------- toolbar

    bindToolbar() {
      const self = this;
      this.toolbar.addEventListener('click', function (ev) {
        const b = ev.target.closest('button');
        if (!b) return;
        if (b.hasAttribute('data-cmd')) {
          if (b.getAttribute('data-cmd') === 'reset') {
            if (self.scene) self.scene.resetCamera();
            setInput(self.id + '-pref', { name: 'camera', value: 'white', nonce: Date.now() });
          }
          return;
        }
        const name = b.getAttribute('data-pref');
        if (!name) return;
        let value = b.getAttribute('data-value');
        if (value === null) value = b.getAttribute('aria-pressed') !== 'true';
        if (name === 'view' && value === '3d') {
          self.fallback = null;
        }
        setInput(self.id + '-pref', { name: name, value: value, nonce: Date.now() });
      });
      this.toolbar.addEventListener('change', function (ev) {
        const s = ev.target.closest('select[data-pref]');
        if (!s) return;
        setInput(self.id + '-pref', { name: s.getAttribute('data-pref'), value: s.value, nonce: Date.now() });
      });
    }

    applyPrefs(p) {
      this.toolbar.querySelectorAll('[data-pref]').forEach(function (c) {
        const name = c.getAttribute('data-pref');
        if (c.tagName === 'SELECT') {
          if (c.value !== String(p[name])) c.value = String(p[name]);
        } else if (c.hasAttribute('data-value')) {
          c.setAttribute('aria-pressed', String(p[name]) === c.getAttribute('data-value') ? 'true' : 'false');
        } else {
          c.setAttribute('aria-pressed', p[name] ? 'true' : 'false');
        }
      });
      const wanted = this.fallback ? '2d' : p.view;
      this.setView(wanted);
      if (this.scene) this.scene.setOptions(this.sceneOptions());
      this.render2D();
      this.renderNotices();
    }

    sceneOptions() {
      const p = G3.prefs;
      return {
        xray: !!p.xray, level: p.level, camera: p.camera, reducedMotion: !!p.reduced_motion,
        theme: p.theme, exploded: !!(this.state && this.state.geometry && this.state.geometry.exploded)
      };
    }

    setView(view) {
      this.view = view;
      this.el.setAttribute('data-view', view);
      const show3d = view === '3d';
      this.view3d.hidden = !show3d;
      this.view2d.hidden = show3d;
      if (show3d) {
        this.ensureScene();
      } else if (this.scene) {
        this.scene.setActive(false);
      }
      if (show3d && this.scene) this.scene.setActive(true);
    }

    ensureScene() {
      if (this.scene || this.sceneLoading || this.fallback) return;
      const self = this;
      this.view3d.setAttribute('aria-busy', 'true');
      this.sceneLoading = import('g3/scene').then(function (mod) {
        self.sceneLoading = null;
        self.view3d.removeAttribute('aria-busy');
        try {
          self.scene = new mod.Scene3D(self.view3d, {
            onPick: (ids, opts) => self.onPick(ids, opts),
            onContextLost: () => self.fail3D('context_lost'),
            onContextRestored: () => { self.restorable = true; self.renderNotices(); },
            label: () => G3.t('board.canvas_label'),
            t: G3.t
          });
        } catch (e) {
          self.fail3D('webgl', e);
          return;
        }
        if (self.state) self.scene.update(self.state, null, self.sceneOptions());
        self.scene.setOptions(self.sceneOptions());
        self.scene.setMarks(self.marks());
        self.scene.setActive(self.view === '3d');
      }).catch(function (e) {
        self.sceneLoading = null;
        self.view3d.removeAttribute('aria-busy');
        self.fail3D('module', e);
      });
    }

    fail3D(reason, err) {
      if (err && window.console) window.console.warn('gambitR3D 3D view:', reason, err);
      this.fallback = reason;
      this.restorable = false;
      if (this.scene && reason === 'context_lost') {
        // Keep the paused scene so its canvas can report restoration; it is
        // disposed when the user returns to 3D.
        this.lostScene = this.scene;
        this.lostScene.setActive(false);
        this.scene = null;
      } else if (this.scene) {
        try { this.scene.dispose(); } catch (e) { /* already gone */ }
        this.scene = null;
        this.view3d.textContent = '';
      }
      this.setView('2d');
      this.renderNotices();
      setInput(this.id + '-pref', { name: 'view', value: '2d', reason: reason, nonce: Date.now() });
    }

    retry3D() {
      this.fallback = null;
      this.restorable = false;
      if (this.lostScene) {
        try { this.lostScene.dispose(); } catch (e) { /* context already gone */ }
        this.lostScene = null;
        this.view3d.textContent = '';
      }
      setInput(this.id + '-pref', { name: 'view', value: '3d', nonce: Date.now() });
    }

    renderNotices() {
      const n = this.notices;
      n.textContent = '';
      const self = this;
      if (this.fallback) {
        const key = this.fallback === 'context_lost' ? 'board.fallback_lost' : 'board.fallback_webgl';
        const box = el('div', { className: 'g3-notice g3-notice-warn', role: 'status' }, [
          el('span', { className: 'g3-notice-mark', 'aria-hidden': 'true', text: '!' }),
          el('span', { text: G3.t(key) })
        ]);
        if (this.fallback === 'context_lost' && this.restorable) {
          box.appendChild(el('button', { type: 'button', className: 'g3-btn btn btn-sm btn-outline-secondary',
            text: G3.t('board.retry3d'), onclick: function () { self.retry3D(); } }));
        } else if (this.fallback !== 'context_lost') {
          box.appendChild(el('button', { type: 'button', className: 'g3-btn btn btn-sm btn-outline-secondary',
            text: G3.t('board.retry3d'), onclick: function () { self.retry3D(); } }));
        }
        n.appendChild(box);
      }
      if (this.view === '3d' && this.state && this.state.geometry && this.state.geometry.exploded) {
        n.appendChild(el('div', { className: 'g3-notice g3-notice-inspect', role: 'note' }, [
          el('span', { className: 'g3-notice-mark', 'aria-hidden': 'true', text: '↕' }),
          el('span', { text: G3.t('board.exploded_notice') })
        ]));
      }
      if (this.view === '3d' && G3.prefs.xray) {
        n.appendChild(el('div', { className: 'g3-notice g3-notice-inspect', role: 'note' }, [
          el('span', { className: 'g3-notice-mark', 'aria-hidden': 'true', text: '◌' }),
          el('span', { text: G3.t('board.xray_notice') })
        ]));
      }
      if (this.view === '3d' && G3.prefs.level && G3.prefs.level !== 'all') {
        n.appendChild(el('div', { className: 'g3-notice g3-notice-inspect', role: 'note' }, [
          el('span', { className: 'g3-notice-mark', 'aria-hidden': 'true', text: '▣' }),
          el('span', { text: G3.t('board.level_notice', { board: G3.boardName(G3.prefs.level) }) })
        ]));
      }
    }

    // ------------------------------------------------------------- marks

    marks() {
      const out = { selected: null, targets: [], lastFrom: null, lastTo: null, check: null, pending: null };
      const p = this.state;
      if (!p) return out;
      out.selected = this.selection;
      if (this.selection) {
        const t = this.targetsOf(this.selection);
        Object.keys(t).forEach((to) => {
          const a = t[to][0];
          out.targets.push({ square: to, kind: a.type === 'castle' ? 'castle' : (a.capture ? 'capture' : 'move') });
        });
      }
      if (p.last) {
        out.lastFrom = p.last.type === 'platform' ? null : p.last.from;
        out.lastTo = p.last.to || null;
        out.lastPlatform = p.last.type === 'platform' ? p.last.platform : null;
      }
      out.check = p.check_square || null;
      if (this.pending && this.pending.action) out.pending = this.pending.action.to || null;
      return out;
    }

    render() {
      this.render2D();
      this.renderPanel(false);
      this.renderNotices();
      if (this.scene) this.scene.setMarks(this.marks());
    }

    // ---------------------------------------------------------------- 2D

    build2D() {
      this.view2d.textContent = '';
      this.wrap = el('div', { className: 'g3-2d' });
      this.tiers = {};
      MAIN.slice().reverse().forEach((parent) => {
        const tier = el('div', { className: 'g3-tier', 'data-parent': parent });
        const slots = {
          L: el('div', { className: 'g3-slot g3-slot-L' }),
          M: el('div', { className: 'g3-slot g3-slot-M' }),
          R: el('div', { className: 'g3-slot g3-slot-R' })
        };
        tier.appendChild(slots.L);
        tier.appendChild(slots.M);
        tier.appendChild(slots.R);
        this.tiers[parent] = { el: tier, slots: slots };
        this.wrap.appendChild(tier);
      });
      this.view2d.appendChild(this.wrap);
      const self = this;
      this.wrap.addEventListener('click', function (ev) {
        const b = ev.target.closest('.g3-cell');
        if (b) self.activate(b.getAttribute('data-square'));
      });
      this.wrap.addEventListener('keydown', function (ev) { self.onGridKey(ev); });
      this.wrap.addEventListener('focusin', function (ev) {
        const b = ev.target.closest('.g3-cell');
        if (b) self.setTabStop(b.getAttribute('data-board'), b.getAttribute('data-square'));
      });
    }

    block(boardId) {
      if (this.blocks[boardId]) return this.blocks[boardId];
      const title = el('h4', { className: 'g3-grid-title' }, [
        el('span', { className: 'g3-grid-name' }),
        el('span', { className: 'g3-grid-meta' })
      ]);
      const grid = el('div', { className: 'g3-grid', role: 'grid', 'data-board': boardId });
      const blk = el('section', { className: 'g3-grid-block', 'data-board': boardId }, [title, grid]);
      this.blocks[boardId] = { el: blk, title: title, grid: grid, rows: [] };
      return this.blocks[boardId];
    }

    cell(id) {
      if (this.cells[id]) return this.cells[id];
      const b = el('button', { type: 'button', className: 'g3-cell', 'data-square': id, tabindex: '-1' }, [
        el('span', { className: 'g3-coord', 'aria-hidden': 'true' }),
        el('span', { className: 'g3-piece', 'aria-hidden': 'true' }),
        el('span', { className: 'g3-marker', 'aria-hidden': 'true' })
      ]);
      this.cells[id] = b;
      return b;
    }

    // Visual layout: rows of ranks and columns of files. White's view puts
    // higher ranks at the top and file a on the left; Black's view turns it.
    layoutOf(boardId) {
      const flip = G3.prefs.camera === 'black';
      const sqs = this.state.squares.filter((s) => s.board === boardId);
      const files = Array.from(new Set(sqs.map((s) => s.file))).sort((a, b) => a - b);
      const ranks = Array.from(new Set(sqs.map((s) => s.rank))).sort((a, b) => b - a);
      if (flip) {
        files.reverse();
        ranks.reverse();
      }
      const rows = ranks.map((r) => files.map((f) => sqs.find((s) => s.file === f && s.rank === r)));
      return { rows: rows, files: files, ranks: ranks };
    }

    render2D() {
      const p = this.state;
      if (!p) return;
      const active = document.activeElement;
      const activeSq = active && active.classList && active.classList.contains('g3-cell')
        ? active.getAttribute('data-square') : null;
      const flip = G3.prefs.camera === 'black';
      const tierOrder = flip ? ['W', 'N', 'B'] : ['B', 'N', 'W'];
      tierOrder.forEach((parent) => this.wrap.appendChild(this.tiers[parent].el));
      Object.keys(this.tiers).forEach((parent) => {
        const t = this.tiers[parent];
        t.slots.L.textContent = '';
        t.slots.R.textContent = '';
        t.slots.M.textContent = '';
      });
      const marks = this.marks();
      const targets = {};
      marks.targets.forEach((x) => { targets[x.square] = x.kind; });
      // Main boards in the middle slot; platforms beside their current
      // parent, high-end docks above low-end ones.
      const platforms = p.boards.filter((b) => b.type === 'platform').slice().sort((a, b) => {
        const ea = a.dock.endsWith(':hi') ? 0 : 1;
        const eb = b.dock.endsWith(':hi') ? 0 : 1;
        return flip ? eb - ea : ea - eb;
      });
      p.boards.filter((b) => b.type === 'main').forEach((b) => {
        this.tiers[b.id].slots.M.appendChild(this.renderBlock(b, targets, marks));
      });
      platforms.forEach((b) => {
        let side = b.dock.split(':')[1];
        if (flip) side = side === 'L' ? 'R' : 'L';
        this.tiers[b.parent].slots[side].appendChild(this.renderBlock(b, targets, marks));
      });
      Object.keys(this.tiers).forEach((parent) => {
        const t = this.tiers[parent];
        t.el.classList.toggle('has-left', t.slots.L.childElementCount > 0);
        t.el.classList.toggle('has-right', t.slots.R.childElementCount > 0);
      });
      if (activeSq && this.cells[activeSq] && document.activeElement !== this.cells[activeSq]) {
        this.cells[activeSq].focus({ preventScroll: true });
      }
    }

    renderBlock(b, targets, marks) {
      const blk = this.block(b.id);
      const name = G3.boardName(b.id);
      let meta;
      if (b.type === 'platform') {
        meta = G3.t('grid.platform_meta', {
          dock: b.dock, where: G3.t('dock.' + b.dock), turn: b.orientation === 180 ? G3.t('grid.turned') : G3.t('grid.upright')
        });
      } else {
        meta = G3.t('grid.main_meta.' + b.id);
      }
      blk.title.querySelector('.g3-grid-name').textContent = name;
      blk.title.querySelector('.g3-grid-meta').textContent = meta;
      blk.el.classList.toggle('is-platform', b.type === 'platform');
      blk.el.classList.toggle('is-last-platform', !!(marks.lastPlatform && marks.lastPlatform === b.id));
      blk.grid.setAttribute('aria-label', name + '. ' + meta);
      const lay = this.layoutOf(b.id);
      blk.grid.style.setProperty('--cols', lay.files.length);
      blk.grid.textContent = '';
      const stop = this.tabStop[b.id];
      let stopUsed = false;
      lay.rows.forEach((row, ri) => {
        const r = el('div', { role: 'row', className: 'g3-row' });
        row.forEach((s, ci) => {
          const wrapCell = el('div', { role: 'gridcell', className: 'g3-gridcell' });
          if (s) {
            const c = this.cell(s.id);
            this.paintCell(c, s, targets, marks);
            c.setAttribute('data-board', b.id);
            c.setAttribute('data-row', ri);
            c.setAttribute('data-col', ci);
            const isStop = stop ? stop === s.id : (ri === lay.rows.length - 1 && ci === 0);
            c.setAttribute('tabindex', isStop ? '0' : '-1');
            if (isStop) stopUsed = true;
            wrapCell.appendChild(c);
          }
          r.appendChild(wrapCell);
        });
        blk.grid.appendChild(r);
      });
      if (!stopUsed) {
        const first = blk.grid.querySelector('.g3-cell');
        if (first) first.setAttribute('tabindex', '0');
      }
      return blk.el;
    }

    paintCell(c, s, targets, marks) {
      const piece = s.piece;
      c.className = 'g3-cell g3-' + s.shade;
      c.querySelector('.g3-coord').textContent = s.coord;
      const pe = c.querySelector('.g3-piece');
      pe.textContent = piece ? piece.type : '';
      pe.className = 'g3-piece' + (piece ? ' g3-piece-' + piece.color : '');
      const status = [];
      const selectable = this.state.interactive && this.isSelectable(s.id);
      if (selectable) {
        c.classList.add('is-selectable');
        status.push(G3.t('cell.can_act'));
      }
      if (marks.selected === s.id) {
        c.classList.add('is-selected');
        status.push(G3.t('cell.selected'));
      }
      const tk = targets[s.id];
      if (tk) {
        c.classList.add('is-target', 'is-target-' + tk);
        status.push(G3.t('cell.target_' + tk));
      }
      if (marks.pending === s.id) {
        c.classList.add('is-pending');
        status.push(G3.t('cell.pending'));
      }
      if (marks.lastFrom === s.id) {
        c.classList.add('is-last-from');
        status.push(G3.t('cell.last_from'));
      }
      if (marks.lastTo === s.id) {
        c.classList.add('is-last-to');
        status.push(G3.t('cell.last_to'));
      }
      if (marks.check === s.id) {
        c.classList.add('is-check');
        status.push(G3.t('cell.check'));
      }
      const label = G3.boardName(s.board) + ' ' + s.coord + ', ' + G3.pieceName(piece) +
        (status.length ? '. ' + status.join(', ') : '');
      c.setAttribute('aria-label', label);
      c.setAttribute('aria-pressed', marks.selected === s.id ? 'true' : 'false');
    }

    setTabStop(boardId, sq) {
      this.tabStop[boardId] = sq;
      const blk = this.blocks[boardId];
      if (!blk) return;
      blk.grid.querySelectorAll('.g3-cell').forEach(function (c) {
        c.setAttribute('tabindex', c.getAttribute('data-square') === sq ? '0' : '-1');
      });
    }

    onGridKey(ev) {
      const c = ev.target.closest('.g3-cell');
      if (!c) return;
      const boardId = c.getAttribute('data-board');
      const blk = this.blocks[boardId];
      const row = Number(c.getAttribute('data-row'));
      const col = Number(c.getAttribute('data-col'));
      const grid = Array.from(blk.grid.querySelectorAll('.g3-row')).map((r) =>
        Array.from(r.querySelectorAll('.g3-gridcell')).map((gc) => gc.querySelector('.g3-cell')));
      let target = null;
      const at = (r, k) => (grid[r] && grid[r][k]) || null;
      switch (ev.key) {
        case 'ArrowUp': target = at(row - 1, col); break;
        case 'ArrowDown': target = at(row + 1, col); break;
        case 'ArrowLeft': target = at(row, col - 1); break;
        case 'ArrowRight': target = at(row, col + 1); break;
        case 'Home': target = ev.ctrlKey ? at(0, 0) : at(row, 0); break;
        case 'End': target = ev.ctrlKey ? at(grid.length - 1, grid[grid.length - 1].length - 1) : at(row, grid[row].length - 1); break;
        case 'PageUp':
        case 'PageDown': {
          const order = Array.from(this.wrap.querySelectorAll('.g3-grid')).map((g) => g.getAttribute('data-board'));
          const i = order.indexOf(boardId);
          const j = ev.key === 'PageDown' ? Math.min(order.length - 1, i + 1) : Math.max(0, i - 1);
          const g = this.blocks[order[j]].grid;
          target = g.querySelector('.g3-cell[tabindex="0"]') || g.querySelector('.g3-cell');
          break;
        }
        case 'Escape':
          ev.preventDefault();
          this.cancel();
          return;
        default:
          return;
      }
      ev.preventDefault();
      if (target) {
        this.setTabStop(target.getAttribute('data-board'), target.getAttribute('data-square'));
        target.focus();
      }
    }

    // ------------------------------------------------------------- panel

    bindPanel() {
      const self = this;
      this.panel.addEventListener('click', function (ev) {
        const b = ev.target.closest('button[data-act]');
        if (!b) return;
        const kind = b.getAttribute('data-act');
        const arg = b.getAttribute('data-arg');
        if (kind === 'action') {
          const a = self.state.actions.find((x) => x.id === arg);
          if (a && self.canAct()) self.send(a);
        } else if (kind === 'platform') {
          self.chooser = { kind: 'platform', from: arg };
          self.renderPanel(true);
        } else if (kind === 'square') {
          self.chooser = null;
          self.activate(arg);
        } else if (kind === 'cancel') {
          self.cancel();
        } else if (kind === 'select') {
          self.activate(arg);
        }
      });
      const preview = function (ev) {
        const b = ev.target.closest('button[data-act="action"]');
        if (!self.scene) return;
        if (!b) { self.scene.setPreview(null); return; }
        const a = self.state && self.state.actions.find((x) => x.id === b.getAttribute('data-arg'));
        self.scene.setPreview(a && a.type === 'platform' ? a : null);
      };
      this.panel.addEventListener('focusin', preview);
      this.panel.addEventListener('mouseover', preview);
      this.panel.addEventListener('keydown', function (ev) {
        if (ev.key === 'Escape') {
          ev.preventDefault();
          self.cancel();
        }
      });
    }

    actionButton(a, text, extra) {
      return el('button', {
        type: 'button', className: 'g3-btn btn btn-sm ' + (extra || 'btn-outline-secondary'),
        'data-act': 'action', 'data-arg': a.id, 'data-focus-key': 'a:' + a.id,
        disabled: !this.canAct() ? true : null
      }, [text]);
    }

    renderPanel(focusFirst) {
      const p = this.state;
      const panel = this.panel;
      const active = document.activeElement;
      const keepKey = active && panel.contains(active) ? active.getAttribute('data-focus-key') : null;
      panel.textContent = '';
      if (!p) return;
      const parts = [];
      const ch = this.chooser;
      if (ch && ch.kind === 'promotion') {
        const sq = ch.actions[0].to;
        parts.push(el('p', { className: 'g3-panel-title', id: this.id + '-chooser-title',
          text: G3.t('chooser.promotion', { square: this.squareName(sq) }) }));
        const row = el('div', { className: 'g3-btn-row', role: 'group', 'aria-labelledby': this.id + '-chooser-title' });
        ch.actions.forEach((a) => {
          row.appendChild(this.actionButton(a, G3.t('piece.' + PIECE_KEYS[a.promotion]) + ' (' + a.promotion + ')'));
        });
        row.appendChild(el('button', { type: 'button', className: 'g3-btn btn btn-sm btn-link', 'data-act': 'cancel',
          'data-focus-key': 'cancel', text: G3.t('common.cancel') }));
        parts.push(row);
      } else if (ch && ch.kind === 'platform') {
        parts.push(this.platformChooser(ch.from));
      } else if (ch && ch.kind === 'squares') {
        parts.push(el('p', { className: 'g3-panel-title', id: this.id + '-chooser-title',
          text: G3.t('chooser.squares_' + ch.why) }));
        const row = el('div', { className: 'g3-btn-row', role: 'group', 'aria-labelledby': this.id + '-chooser-title' });
        ch.squares.forEach((id) => {
          const s = this.sq[id];
          row.appendChild(el('button', { type: 'button', className: 'g3-btn btn btn-sm btn-outline-secondary',
            'data-act': 'square', 'data-arg': id, 'data-focus-key': 's:' + id,
            text: this.squareName(id) + ' – ' + G3.pieceName(s && s.piece) }));
        });
        row.appendChild(el('button', { type: 'button', className: 'g3-btn btn btn-sm btn-link', 'data-act': 'cancel',
          'data-focus-key': 'cancel', text: G3.t('common.cancel') }));
        parts.push(row);
      } else if (this.selection) {
        const s = this.sq[this.selection];
        parts.push(el('p', { className: 'g3-panel-title', text: G3.t('panel.selected', {
          piece: G3.pieceName(s.piece), square: this.squareName(this.selection) }) }));
        const t = this.targetsOf(this.selection);
        const keys = Object.keys(t);
        if (keys.length) {
          parts.push(el('p', { className: 'g3-small', text: G3.t('panel.destinations', { n: keys.length }) }));
          const row = el('div', { className: 'g3-btn-row g3-dest-row' });
          keys.forEach((to) => {
            const acts = t[to];
            const a = acts[0];
            let label = this.squareName(to);
            if (a.type === 'castle') label = G3.t('panel.castle_' + a.castle);
            else if (a.capture) label += ' ×';
            if (acts.length > 1) {
              row.appendChild(el('button', { type: 'button', className: 'g3-btn btn btn-sm btn-outline-secondary',
                'data-act': 'square', 'data-arg': to, 'data-focus-key': 's:' + to,
                disabled: !this.canAct() ? true : null, text: label + ' …' }));
            } else {
              row.appendChild(this.actionButton(a, label));
            }
          });
          parts.push(row);
        }
        const plat = this.platformFrom[this.selection];
        if (plat && plat.length) {
          parts.push(el('div', { className: 'g3-btn-row' }, [el('button', {
            type: 'button', className: 'g3-btn btn btn-sm btn-outline-primary', 'data-act': 'platform',
            'data-arg': this.selection, 'data-focus-key': 'platform', disabled: !this.canAct() ? true : null,
            text: G3.t('panel.move_platform', { platform: plat[0].platform, n: plat.length })
          })]));
        }
        parts.push(el('div', { className: 'g3-btn-row' }, [el('button', { type: 'button',
          className: 'g3-btn btn btn-sm btn-link', 'data-act': 'cancel', 'data-focus-key': 'cancel',
          text: G3.t('panel.cancel_selection') })]));
      } else if (!p.interactive) {
        parts.push(el('p', { className: 'g3-small', text: p.note || G3.t('board.not_interactive') }));
      } else if (p.interactive) {
        parts.push(el('p', { className: 'g3-small g3-muted', text: G3.t('board.select_hint') }));
        if (this.castles.length) {
          const row = el('div', { className: 'g3-btn-row', role: 'group', 'aria-label': G3.t('panel.castling') });
          this.castles.forEach((a) => row.appendChild(this.actionButton(a, G3.t('panel.castle_' + a.castle))));
          parts.push(row);
        }
      }
      if (this.message) {
        const isErr = this.message.type === 'error';
        parts.push(el('p', { className: isErr ? 'g3-error' : 'g3-info', role: isErr ? 'alert' : 'status' }, [
          el('span', { className: 'g3-msg-mark', 'aria-hidden': 'true', text: isErr ? '!' : 'i' }),
          el('span', { text: this.message.text })
        ]));
      }
      parts.forEach((x) => panel.appendChild(x));
      let focusEl = null;
      if (keepKey) focusEl = panel.querySelector('[data-focus-key="' + CSS.escape(keepKey) + '"]');
      if (!focusEl && focusFirst) focusEl = panel.querySelector('button:not([disabled])');
      if (focusEl) focusEl.focus({ preventScroll: !focusFirst });
    }

    platformChooser(from) {
      const acts = this.platformFrom[from] || [];
      const s = this.sq[from];
      const box = el('div', { className: 'g3-platform-chooser' });
      const tid = this.id + '-platform-title';
      box.appendChild(el('p', { className: 'g3-panel-title', id: tid, text: G3.t('chooser.platform', {
        platform: acts.length ? acts[0].platform : '', piece: G3.pieceName(s && s.piece), square: this.squareName(from)
      }) }));
      box.appendChild(el('p', { className: 'g3-small g3-muted', text: G3.t('chooser.platform_note') }));
      const byDock = {};
      acts.forEach((a) => { (byDock[a.dock] = byDock[a.dock] || []).push(a); });
      const list = el('ul', { className: 'g3-dock-list', 'aria-labelledby': tid });
      Object.keys(byDock).sort().forEach((dock) => {
        const li = el('li', { className: 'g3-dock' });
        li.appendChild(el('p', { className: 'g3-dock-name' }, [
          el('strong', { text: dock }), ' ', el('span', { className: 'g3-muted', text: G3.t('dock.' + dock) })
        ]));
        const row = el('div', { className: 'g3-btn-row' });
        byDock[dock].sort((a, b) => a.rotation - b.rotation).forEach((a) => {
          const txt = G3.t(a.rotation === 180 ? 'chooser.turned' : 'chooser.kept', { coord: a.result_coord }) +
            (a.promotion ? ' = ' + G3.t('piece.' + PIECE_KEYS[a.promotion]) : '');
          const b = this.actionButton(a, txt);
          b.setAttribute('aria-label', a.label + '. ' + G3.t('chooser.result', {
            piece: G3.pieceName(s && s.piece), coord: a.result_coord }));
          row.appendChild(b);
        });
        li.appendChild(row);
        list.appendChild(li);
      });
      box.appendChild(list);
      box.appendChild(el('div', { className: 'g3-btn-row' }, [el('button', { type: 'button',
        className: 'g3-btn btn btn-sm btn-link', 'data-act': 'cancel', 'data-focus-key': 'cancel',
        text: G3.t('common.cancel') })]));
      return box;
    }
  }

  G3.Board = Board;

  function boardFor(id) {
    if (G3.boards[id]) return G3.boards[id];
    const node = document.getElementById(id);
    if (!node) return null;
    G3.boards[id] = new Board(node);
    G3.boards[id].applyPrefs(G3.prefs);
    return G3.boards[id];
  }

  // ------------------------------------------------------ Shiny messages

  function register() {
    const S = window.Shiny;
    S.addCustomMessageHandler('g3-board-state', function (p) {
      const b = boardFor(p.widget);
      if (b) b.setState(p);
    });
    S.addCustomMessageHandler('g3-board-ack', function (ack) {
      const b = boardFor(ack.widget);
      if (b) b.onAck(ack);
    });
    S.addCustomMessageHandler('g3-prefs', function (p) {
      G3.prefs = Object.assign({}, G3.prefs, p);
      if (p.synced) G3.prefsSynced = true;
      applyGlobalPrefs(G3.prefs);
      Object.keys(G3.boards).forEach(function (id) { G3.boards[id].applyPrefs(G3.prefs); });
      if (G3.prefsSynced) {
        storeSet({
          theme: G3.prefs.theme, reduced_motion: !!G3.prefs.reduced_motion,
          default_view: G3.prefs.default_view, neutral_height: G3.prefs.neutral_height, lang: G3.prefs.lang
        });
      }
    });
    S.addCustomMessageHandler('g3-i18n', function (m) {
      G3.messages = m.messages || {};
      G3.lang = m.lang || 'en';
      document.documentElement.setAttribute('lang', G3.lang);
      G3.applyI18n(document);
      Object.keys(G3.boards).forEach(function (id) { G3.boards[id].render(); });
    });
    S.addCustomMessageHandler('g3-nav', function (m) {
      markSection(m.section);
      if (m.focus) {
        window.setTimeout(function () {
          const pane = document.querySelector('.tab-pane[data-value="' + m.section + '"]');
          const h = pane && pane.querySelector('.g3-section-title');
          if (h) h.focus({ preventScroll: false });
          Object.keys(G3.boards).forEach(function (id) {
            const b = G3.boards[id];
            if (b.scene) b.scene.resize();
          });
        }, 30);
      }
    });
  }

  function boot() {
    bindNavigation();
    markSection('play');
    document.querySelectorAll('.g3-board').forEach(function (n) { boardFor(n.id); });
    if (window.Shiny && window.Shiny.addCustomMessageHandler) register();
    // Shiny triggers 'shiny:connected' through jQuery, which native
    // listeners do not receive.
    let sent = false;
    const onConnected = function () {
      if (sent) return;
      sent = true;
      sendClientPrefs();
    };
    if (window.jQuery) window.jQuery(document).on('shiny:connected', onConnected);
    if (window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected && window.Shiny.shinyapp.isConnected()) {
      onConnected();
    }
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot);
  else boot();

  // Read-only hooks for automated browser checks; they expose state, never
  // a way to change the game.
  G3.debug = {
    board: function (id) {
      const b = G3.boards[id];
      if (!b) return null;
      return {
        revision: b.state && b.state.revision, game_id: b.state && b.state.game_id,
        position_key: b.state && b.state.position_key, side: b.state && b.state.side,
        selection: b.selection, view: b.view, fallback: b.fallback, pending: !!b.pending,
        scene: b.scene ? b.scene.info() : null, n_cells: Object.keys(b.cells).length
      };
    },
    projectSquare: function (id, square) {
      const b = G3.boards[id];
      return b && b.scene ? b.scene.projectSquare(square) : null;
    },
    loseContext: function (id) {
      const b = G3.boards[id];
      return b && b.scene ? b.scene.loseContextForTest() : false;
    },
    restoreContext: function (id) {
      const b = G3.boards[id];
      if (b && b.lostScene) b.lostScene.restoreContextForTest();
      return !!(b && b.lostScene);
    }
  };
})();
