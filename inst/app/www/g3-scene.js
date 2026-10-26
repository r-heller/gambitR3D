/*
 * gambitR3D 3D view (ES module; three.js is vendored, see
 * vendor/PROVENANCE.md).
 *
 * The scene is drawn from exact coordinates computed in R
 * (R/display-geometry.R): every square arrives with its x, y, z tile centre
 * for the current Neutral height and exploded setting. Nothing here derives
 * a coordinate from rules or decides legality. Each board is a group placed
 * at its centre; tiles, pieces and marks are children at local offsets, so a
 * transported platform or a height change moves as one assembly.
 *
 * Lifecycle: one renderer, one animation loop (renderer.setAnimationLoop),
 * rendering only when something changed. Shared geometries, materials and
 * text textures are created once and disposed in dispose(). A lost WebGL
 * context is reported to the board controller, which falls back to 2D.
 */
import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';

const PALETTES = {
  light: {
    bg: '#E9EEF2', floor: '#DCE3E9', sqLight: '#DFE8ED', sqDark: '#879CAA',
    frameMain: '#4E6172', framePlat: '#6B5A49', pieceLight: '#F7F5EF', pieceDark: '#262B31',
    inkOnLight: '#1B2128', inkOnDark: '#F7F5EF', blue: '#0072B2', amber: '#E69F00', ink: '#16202A',
    labelOnLight: '#2C3A46', labelOnDark: '#0E1820', ghost: '#0072B2', hover: '#16202A',
    tagFill: '#FFFFFF', tagInk: '#1B2530'
  },
  dark: {
    bg: '#141A20', floor: '#1A222A', sqLight: '#A9BCC8', sqDark: '#5D7383',
    frameMain: '#33424F', framePlat: '#4F4236', pieceLight: '#F2EFE7', pieceDark: '#1C2026',
    inkOnLight: '#1B2128', inkOnDark: '#F2EFE7', blue: '#56B4E9', amber: '#E69F00', ink: '#0B1015',
    labelOnLight: '#16222C', labelOnDark: '#F2F5F7', ghost: '#56B4E9', hover: '#F2EFE7',
    tagFill: '#222C35', tagInk: '#E6EDF2'
  }
};

// Default viewpoints: a narrow field of view from far away keeps the
// perspective mild. The player views look down at about 60 degrees, so the
// back ranks of the board under a raised N stay visible.
const VIEWS = {
  white: { pos: [0, 20.8, 12.3] },
  black: { pos: [0, 20.8, -12.3] },
  top: { pos: [0, 25, 0.001] },
  side: { pos: [23, 9.5, 0] }
};
const TARGET = [0, 1.0, 0];

function ease(t) {
  return t < 0.5 ? 2 * t * t : 1 - Math.pow(-2 * t + 2, 2) / 2;
}

export class Scene3D {
  constructor(container, cb) {
    this.container = container;
    this.cb = cb || {};
    this.opts = { xray: false, level: 'all', camera: 'white', reducedMotion: false, theme: 'light' };
    this.active = false;
    this.dirty = true;
    this.tweens = [];
    this.groups = {};
    this.pickables = [];
    this.textures = new Map();
    this.state = null;
    this.marks = {};
    this.preview = null;
    this.disposed = false;
    this.lost = false;

    const canvas = document.createElement('canvas');
    canvas.className = 'g3-canvas';
    canvas.setAttribute('role', 'img');
    canvas.setAttribute('aria-label', this.cb.label ? this.cb.label() : '3D view');
    canvas.tabIndex = -1;
    // three.js r163+ needs WebGL 2. A browser with WebGL disabled returns
    // null here; the caller falls back to the 2D view.
    // preserveDrawingBuffer keeps the last frame for page captures; the
    // scene renders only on change, so the cost is small.
    const gl = canvas.getContext('webgl2', {
      antialias: true, alpha: false, powerPreference: 'default', preserveDrawingBuffer: true
    });
    if (!gl) throw new Error('WebGL 2 is not available in this browser.');
    this.canvas = canvas;
    this.renderer = new THREE.WebGLRenderer({ canvas: canvas, context: gl, antialias: true, preserveDrawingBuffer: true });
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
    this.renderer.outputColorSpace = THREE.SRGBColorSpace;
    container.textContent = '';
    container.appendChild(canvas);

    this.scene = new THREE.Scene();
    this.camera = new THREE.PerspectiveCamera(30, 1, 0.1, 300);
    this.controls = new OrbitControls(this.camera, canvas);
    this.controls.target.set(TARGET[0], TARGET[1], TARGET[2]);
    this.controls.minDistance = 5;
    this.controls.maxDistance = 60;
    this.controls.maxPolarAngle = Math.PI * 0.49;
    this.controls.addEventListener('change', () => { this.dirty = true; });
    // Once the user orbits, presets no longer follow height changes.
    this.controls.addEventListener('start', () => { this.userMoved = true; });

    this.hemi = new THREE.HemisphereLight(0xffffff, 0x445566, 1.7);
    this.scene.add(this.hemi);
    this.sun = new THREE.DirectionalLight(0xffffff, 1.6);
    this.sun.position.set(6, 14, 9);
    this.scene.add(this.sun);
    this.root = new THREE.Group();
    this.scene.add(this.root);
    this.overlay = new THREE.Group();
    this.scene.add(this.overlay);

    this.raycaster = new THREE.Raycaster();
    this.raycaster.camera = this.camera;
    this.pointer = new THREE.Vector2();

    this.makeShared();
    this.applyTheme();
    this.setViewpoint('white', true);

    this.onLost = (ev) => {
      ev.preventDefault();
      this.lost = true;
      this.active = false;
      if (this.cb.onContextLost) this.cb.onContextLost();
    };
    this.onRestored = () => {
      this.lost = false;
      if (this.cb.onContextRestored) this.cb.onContextRestored();
    };
    canvas.addEventListener('webglcontextlost', this.onLost, false);
    canvas.addEventListener('webglcontextrestored', this.onRestored, false);
    this.bindPointer();
    this.resizeObserver = new ResizeObserver(() => this.resize());
    this.resizeObserver.observe(container);
    this.resize();
    this.renderer.setAnimationLoop((t) => this.loop(t));
  }

  // ------------------------------------------------------------ resources

  makeShared() {
    this.geo = {
      tile: new THREE.BoxGeometry(0.97, 0.08, 0.97),
      disc: new THREE.CylinderGeometry(0.33, 0.35, 0.16, 40),
      label: new THREE.PlaneGeometry(0.42, 0.21),
      selOuter: new THREE.RingGeometry(0.40, 0.48, 48),
      selInner: new THREE.RingGeometry(0.35, 0.40, 48),
      dot: new THREE.CircleGeometry(0.12, 32),
      dotRing: new THREE.RingGeometry(0.12, 0.17, 32),
      capOuter: new THREE.RingGeometry(0.41, 0.48, 48),
      capInner: new THREE.RingGeometry(0.37, 0.41, 48),
      castle: new THREE.RingGeometry(0.30, 0.40, 4, 1),
      lastFrame: new THREE.RingGeometry(0.44, 0.49, 4, 1),
      hover: new THREE.RingGeometry(0.455, 0.49, 4, 1),
      ghost: new THREE.BoxGeometry(1.96, 0.05, 1.96)
    };
    const std = (c) => new THREE.MeshStandardMaterial({ color: c, roughness: 0.75, metalness: 0.0 });
    const flat = (c, extra) => new THREE.MeshBasicMaterial(Object.assign({ color: c, side: THREE.DoubleSide }, extra || {}));
    this.mat = {
      sqLight: std('#ffffff'), sqDark: std('#ffffff'), frameMain: std('#ffffff'), framePlat: std('#ffffff'),
      pieceLight: std('#ffffff'), pieceDark: std('#ffffff'),
      blue: flat('#ffffff', { depthTest: true }), amber: flat('#ffffff'), ink: flat('#ffffff'),
      lastInk: flat('#ffffff', { transparent: true, opacity: 0.85 }),
      hover: flat('#ffffff', { transparent: true, opacity: 0.9 }),
      ghost: new THREE.MeshBasicMaterial({ color: '#ffffff', transparent: true, opacity: 0.35, depthWrite: false })
    };
  }

  applyTheme() {
    const P = PALETTES[this.opts.theme === 'dark' ? 'dark' : 'light'];
    this.P = P;
    this.scene.background = new THREE.Color(P.bg);
    const set = (m, c) => m.color.set(c);
    set(this.mat.sqLight, P.sqLight);
    set(this.mat.sqDark, P.sqDark);
    set(this.mat.frameMain, P.frameMain);
    set(this.mat.framePlat, P.framePlat);
    set(this.mat.pieceLight, P.pieceLight);
    set(this.mat.pieceDark, P.pieceDark);
    set(this.mat.blue, P.blue);
    set(this.mat.amber, P.amber);
    set(this.mat.ink, P.ink);
    set(this.mat.lastInk, P.ink);
    set(this.mat.hover, P.hover);
    set(this.mat.ghost, P.ghost);
    this.applyXray();
    this.dirty = true;
  }

  applyXray() {
    const x = !!this.opts.xray;
    ['sqLight', 'sqDark', 'frameMain', 'framePlat'].forEach((k) => {
      const m = this.mat[k];
      m.transparent = x;
      m.opacity = x ? (k.startsWith('frame') ? 0.18 : 0.38) : 1;
      m.depthWrite = !x;
      m.needsUpdate = true;
    });
  }

  // Text textures (piece letters, coordinate and board labels), cached.
  textTexture(key, draw) {
    if (this.textures.has(key)) return this.textures.get(key);
    const c = document.createElement('canvas');
    draw(c, c.getContext('2d'));
    const tex = new THREE.CanvasTexture(c);
    tex.colorSpace = THREE.SRGBColorSpace;
    tex.anisotropy = 4;
    this.textures.set(key, tex);
    return tex;
  }

  font(px, weight) {
    return (weight || 600) + ' ' + px + 'px system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif';
  }

  pieceTexture(type, color) {
    const P = this.P;
    return this.textTexture('piece:' + type + ':' + color + ':' + this.opts.theme, (c, g) => {
      c.width = 128;
      c.height = 128;
      g.beginPath();
      g.arc(64, 64, 58, 0, Math.PI * 2);
      g.fillStyle = color === 'white' ? P.pieceLight : P.pieceDark;
      g.fill();
      g.lineWidth = 8;
      g.strokeStyle = color === 'white' ? P.inkOnLight : P.inkOnDark;
      g.stroke();
      g.fillStyle = color === 'white' ? P.inkOnLight : P.inkOnDark;
      g.font = this.font(72, 700);
      g.textAlign = 'center';
      g.textBaseline = 'middle';
      g.fillText(type, 64, 70);
    });
  }

  labelTexture(text, shade) {
    const P = this.P;
    return this.textTexture('label:' + text + ':' + shade + ':' + this.opts.theme, (c, g) => {
      c.width = 128;
      c.height = 64;
      g.clearRect(0, 0, 128, 64);
      g.fillStyle = shade === 'dark' ? P.labelOnDark : P.labelOnLight;
      g.font = this.font(40, 600);
      g.textAlign = 'left';
      g.textBaseline = 'middle';
      g.fillText(text, 6, 34);
    });
  }

  badgeTexture(text, fill, ink) {
    return this.textTexture('badge:' + text + ':' + fill + ':' + ink, (c, g) => {
      g.font = this.font(44, 700);
      const w = Math.max(96, Math.ceil(g.measureText(text).width) + 40);
      c.width = w;
      c.height = 72;
      g.font = this.font(44, 700);
      const r = 30;
      g.beginPath();
      g.moveTo(r + 4, 4);
      g.arcTo(w - 4, 4, w - 4, 68, r);
      g.arcTo(w - 4, 68, 4, 68, r);
      g.arcTo(4, 68, 4, 4, r);
      g.arcTo(4, 4, w - 4, 4, r);
      g.closePath();
      g.fillStyle = fill;
      g.fill();
      g.lineWidth = 5;
      g.strokeStyle = ink;
      g.stroke();
      g.fillStyle = ink;
      g.textAlign = 'center';
      g.textBaseline = 'middle';
      g.fillText(text, w / 2, 38);
    });
  }

  sprite(tex, w, h) {
    const m = new THREE.SpriteMaterial({ map: tex, transparent: true, depthWrite: false });
    const s = new THREE.Sprite(m);
    s.scale.set(w, h, 1);
    s.userData.ownMaterial = true;
    return s;
  }

  // ----------------------------------------------------------- building

  clearRoot() {
    const drop = (obj) => {
      obj.traverse((o) => {
        if (o.userData && o.userData.ownMaterial && o.material) o.material.dispose();
        if (o.userData && o.userData.ownGeometry && o.geometry) o.geometry.dispose();
      });
    };
    this.root.children.slice().forEach((ch) => {
      drop(ch);
      this.root.remove(ch);
    });
    this.groups = {};
    this.pickables = [];
    this.pieceMeshes = {};
  }

  update(p, prev, opts) {
    if (this.disposed) return;
    if (opts) this.opts = Object.assign({}, this.opts, opts);
    this.finishTweens();
    // Remember what is on screen now, to animate towards the new state.
    const oldGroups = {};
    Object.keys(this.groups).forEach((id) => {
      const g = this.groups[id];
      oldGroups[id] = { pos: g.position.clone() };
    });
    const oldPieces = {};
    Object.keys(this.pieceMeshes || {}).forEach((pid) => {
      const v = new THREE.Vector3();
      this.pieceMeshes[pid].getWorldPosition(v);
      oldPieces[pid] = v;
    });
    const heightChanged = this.state && this.state.geometry && p.geometry &&
      this.state.geometry.neutral_height !== p.geometry.neutral_height;
    this.state = p;
    if ((heightChanged || !this.framed) && !this.userMoved) {
      this.setViewpoint(this.viewName || 'white', true);
      this.framed = true;
    }
    this.build(p);
    if (!this.opts.reducedMotion && Object.keys(oldGroups).length) {
      this.animateFrom(p, prev, oldGroups, oldPieces);
    }
    this.applyLevel();
    this.drawMarks();
    this.dirty = true;
  }

  build(p) {
    this.clearRoot();
    const flipLabels = this.opts.camera === 'black';
    const bySquareBoard = {};
    p.squares.forEach((s) => { (bySquareBoard[s.board] = bySquareBoard[s.board] || []).push(s); });
    p.boards.forEach((b) => {
      const g = new THREE.Group();
      g.name = 'board:' + b.id;
      g.position.set(b.x, b.y, b.z);
      g.userData = { board: b.id, parent: b.parent, type: b.type, dock: b.dock || null, orientation: b.orientation };
      // Frame under the tiles; platforms get a warmer frame colour.
      const w = b.x_max - b.x_min;
      const d = b.z_max - b.z_min;
      const frame = new THREE.Mesh(new THREE.BoxGeometry(w + 0.16, 0.14, d + 0.16),
        b.type === 'platform' ? this.mat.framePlat : this.mat.frameMain);
      frame.position.set(0, -0.15, 0);
      frame.userData.ownGeometry = true;
      g.add(frame);
      (bySquareBoard[b.id] || []).forEach((s) => {
        const lx = s.x - b.x;
        const lz = s.z - b.z;
        const tile = new THREE.Mesh(this.geo.tile, s.shade === 'dark' ? this.mat.sqDark : this.mat.sqLight);
        tile.position.set(lx, -0.04, lz);
        tile.userData.square = s.id;
        g.add(tile);
        this.pickables.push(tile);
        const lab = new THREE.Mesh(this.geo.label, new THREE.MeshBasicMaterial({
          map: this.labelTexture(s.coord, s.shade), transparent: true, depthWrite: false
        }));
        lab.userData.ownMaterial = true;
        lab.rotation.set(-Math.PI / 2, 0, flipLabels ? Math.PI : 0);
        const off = flipLabels ? -1 : 1;
        lab.position.set(lx - 0.24 * off, 0.003, lz + 0.36 * off);
        g.add(lab);
        if (s.piece) {
          const pg = new THREE.Group();
          pg.position.set(lx, 0, lz);
          pg.userData.square = s.id;
          const body = new THREE.Mesh(this.geo.disc, s.piece.color === 'white' ? this.mat.pieceLight : this.mat.pieceDark);
          body.position.y = 0.08;
          body.userData.square = s.id;
          pg.add(body);
          const letter = this.sprite(this.pieceTexture(s.piece.type, s.piece.color), 0.4, 0.4);
          letter.position.y = 0.4;
          letter.userData.square = s.id;
          pg.add(letter);
          g.add(pg);
          this.pickables.push(body, letter);
          this.pieceMeshes[s.piece.id] = pg;
        }
      });
      // Board identity label: W in front, B behind, N on its left side
      // (clear of the outer boards in both height modes); platforms on
      // their outer side.
      const name = this.cb.t ? this.cb.t('boardname.' + b.id) : b.id;
      const tag = this.sprite(this.badgeTexture(b.id, this.P.tagFill, this.P.tagInk), 0.62, 0.36);
      if (b.id === 'W') tag.position.set(0, 0.25, d / 2 + 0.5);
      else if (b.id === 'B') tag.position.set(0, 0.25, -d / 2 - 0.5);
      else if (b.id === 'N') tag.position.set(-w / 2 - 0.55, 0.25, 1.4);
      else tag.position.set(b.dock && b.dock.indexOf(':L:') > 0 ? -w / 2 - 0.55 : w / 2 + 0.55, 0.25, 0);
      tag.userData.label = name;
      // Identity labels stay readable through other boards.
      tag.material.depthTest = false;
      tag.renderOrder = 10;
      g.add(tag);
      this.root.add(g);
      this.groups[b.id] = g;
    });
  }

  animateFrom(p, prev, oldGroups, oldPieces) {
    const now = performance.now();
    const prevBoards = {};
    if (prev) prev.boards.forEach((b) => { prevBoards[b.id] = b; });
    Object.keys(this.groups).forEach((id) => {
      const g = this.groups[id];
      const old = oldGroups[id];
      if (!old) return;
      const pb = prevBoards[id];
      const turned = pb && pb.orientation !== g.userData.orientation;
      const moved = old.pos.distanceTo(g.position) > 1e-6;
      if (!moved && !turned) return;
      const transport = pb && pb.dock && pb.dock !== g.userData.dock;
      this.tweens.push({
        obj: g, t0: now, dur: transport ? 900 : 550,
        from: old.pos.clone(), to: g.position.clone(),
        rotFrom: turned ? Math.PI : 0, rotTo: 0, arc: transport ? 1.4 : 0, transport: !!transport
      });
      g.position.copy(old.pos);
      if (turned) g.rotation.y = Math.PI;
    });
    if (prev) {
      // Pieces that changed square (moves, captures' movers, castling).
      const prevSq = {};
      prev.squares.forEach((s) => { if (s.piece) prevSq[s.piece.id] = s.id; });
      p.squares.forEach((s) => {
        if (!s.piece) return;
        const was = prevSq[s.piece.id];
        if (!was || was === s.id || !oldPieces[s.piece.id]) return;
        const pg = this.pieceMeshes[s.piece.id];
        const parent = pg.parent;
        const fromLocal = parent.worldToLocal(oldPieces[s.piece.id].clone());
        this.tweens.push({
          obj: pg, t0: now, dur: 450, from: fromLocal, to: pg.position.clone(),
          rotFrom: 0, rotTo: 0, arc: 0.7, transport: false
        });
        pg.position.copy(fromLocal);
      });
    }
  }

  finishTweens() {
    this.tweens.forEach((tw) => {
      tw.obj.position.copy(tw.to);
      tw.obj.rotation.y = tw.rotTo;
    });
    this.tweens = [];
  }

  stepTweens(now) {
    if (!this.tweens.length) return false;
    this.tweens = this.tweens.filter((tw) => {
      const t = Math.min(1, (now - tw.t0) / tw.dur);
      const e = ease(t);
      tw.obj.position.lerpVectors(tw.from, tw.to, e);
      tw.obj.position.y += Math.sin(Math.PI * e) * tw.arc;
      tw.obj.rotation.y = tw.rotFrom + (tw.rotTo - tw.rotFrom) * e;
      if (t >= 1) {
        tw.obj.position.copy(tw.to);
        tw.obj.rotation.y = tw.rotTo;
        return false;
      }
      return true;
    });
    if (!this.tweens.length) this.drawMarks();
    return true;
  }

  // -------------------------------------------------------------- marks

  setMarks(m) {
    this.marks = m || {};
    this.drawMarks();
  }

  setPreview(action) {
    this.preview = action || null;
    this.drawMarks();
  }

  flatMesh(geo, mat, x, z, y, rotZ) {
    const m = new THREE.Mesh(geo, mat);
    m.rotation.set(-Math.PI / 2, 0, rotZ || 0);
    m.position.set(x, y, z);
    m.renderOrder = 2;
    return m;
  }

  squareLocal(id) {
    if (!this.state) return null;
    const s = this.state.squares.find((q) => q.id === id);
    if (!s) return null;
    const g = this.groups[s.board];
    if (!g) return null;
    const b = this.state.boards.find((x) => x.id === s.board);
    return { group: g, x: s.x - b.x, z: s.z - b.z, square: s };
  }

  drawMarks() {
    this.overlay.children.slice().forEach((ch) => {
      ch.traverse((o) => { if (o.userData && o.userData.ownMaterial && o.material) o.material.dispose(); });
      this.overlay.remove(ch);
    });
    Object.values(this.groups).forEach((g) => {
      g.children.filter((c) => c.userData && c.userData.mark).forEach((c) => {
        if (c.userData.ownMaterial && c.material) c.material.dispose();
        g.remove(c);
      });
    });
    const m = this.marks || {};
    const add = (id, objs) => {
      const loc = this.squareLocal(id);
      if (!loc) return;
      objs.forEach((o) => {
        o.position.x += loc.x;
        o.position.z += loc.z;
        o.userData.mark = true;
        loc.group.add(o);
      });
    };
    const y = 0.006;
    if (m.lastFrom) add(m.lastFrom, [this.flatMesh(this.geo.lastFrame, this.mat.lastInk, 0, 0, y, Math.PI / 4)]);
    if (m.lastTo) add(m.lastTo, [this.flatMesh(this.geo.lastFrame, this.mat.lastInk, 0, 0, y, Math.PI / 4)]);
    if (m.selected) {
      add(m.selected, [
        this.flatMesh(this.geo.selOuter, this.mat.amber, 0, 0, y + 0.002),
        this.flatMesh(this.geo.selInner, this.mat.ink, 0, 0, y + 0.002)
      ]);
    }
    (m.targets || []).forEach((t) => {
      if (t.kind === 'capture') {
        add(t.square, [
          this.flatMesh(this.geo.capOuter, this.mat.blue, 0, 0, y + 0.004),
          this.flatMesh(this.geo.capInner, this.mat.ink, 0, 0, y + 0.004)
        ]);
      } else if (t.kind === 'castle') {
        add(t.square, [this.flatMesh(this.geo.castle, this.mat.blue, 0, 0, y + 0.004, Math.PI / 4)]);
      } else {
        add(t.square, [
          this.flatMesh(this.geo.dot, this.mat.blue, 0, 0, y + 0.004),
          this.flatMesh(this.geo.dotRing, this.mat.ink, 0, 0, y + 0.004)
        ]);
      }
    });
    if (m.pending) add(m.pending, [this.flatMesh(this.geo.selInner, this.mat.amber, 0, 0, y + 0.006)]);
    if (m.check) {
      const badge = this.sprite(this.badgeTexture('+', '#FFFFFF', '#16202A'), 0.42, 0.32);
      badge.position.set(0, 0.95, 0);
      add(m.check, [badge]);
    }
    if (this.hovered) add(this.hovered, [this.flatMesh(this.geo.hover, this.mat.hover, 0, 0, y + 0.001, Math.PI / 4)]);
    // Platform preview: a translucent ghost at the destination dock.
    const a = this.preview;
    if (a && a.type === 'platform' && this.state) {
      const dock = this.state.docks.find((d) => d.id === a.dock);
      if (dock) {
        const ghost = new THREE.Mesh(this.geo.ghost, this.mat.ghost);
        ghost.position.set(dock.x, dock.y - 0.03, dock.z);
        this.overlay.add(ghost);
        const tag = this.sprite(this.badgeTexture((this.cb.t ? this.cb.t('board.preview') : 'Preview') +
          ': ' + a.platform + ' → ' + a.dock + (a.rotation === 180 ? ' ↻' : ''), '#FFFFFF', '#16202A'), 2.4, 0.42);
        tag.position.set(dock.x, dock.y + 0.9, dock.z);
        this.overlay.add(tag);
      }
    }
    this.dirty = true;
  }

  // ------------------------------------------------------------ options

  setOptions(o) {
    const before = this.opts;
    this.opts = Object.assign({}, this.opts, o);
    if (before.reducedMotion !== this.opts.reducedMotion) {
      this.controls.enableDamping = !this.opts.reducedMotion;
      if (this.opts.reducedMotion) this.finishTweens();
    }
    if (before.theme !== this.opts.theme) {
      this.applyTheme();
      if (this.state) this.build(this.state);
      this.applyLevel();
      this.drawMarks();
    }
    if (before.xray !== this.opts.xray) this.applyXray();
    if (before.camera !== this.opts.camera) {
      this.finishTweens();
      this.setViewpoint(this.opts.camera);
      if (this.state) {
        this.build(this.state);
        this.applyLevel();
        this.drawMarks();
      }
    }
    if (before.level !== this.opts.level) this.applyLevel();
    this.dirty = true;
  }

  applyLevel() {
    const lv = this.opts.level || 'all';
    Object.values(this.groups).forEach((g) => {
      g.visible = lv === 'all' || g.userData.parent === lv;
    });
    this.dirty = true;
  }

  setViewpoint(name, instant) {
    const v = VIEWS[name] || VIEWS.white;
    const aspect = this.camera.aspect || 1;
    // Only a narrow portrait canvas needs a longer distance to fit the width.
    const k = Math.max(1, 0.7 / aspect);
    // Aim at the middle of the stack: higher when the outer boards are up.
    const lowest = this.state && this.state.geometry && this.state.geometry.neutral_height === 'lowest';
    const ty = lowest ? 2.3 : TARGET[1];
    const pos = new THREE.Vector3(v.pos[0] * k, ty - TARGET[1] + v.pos[1] * k, v.pos[2] * k);
    this.camera.position.copy(pos);
    this.controls.target.set(TARGET[0], ty, TARGET[2]);
    this.userMoved = false;
    this.camera.lookAt(this.controls.target);
    this.controls.update();
    this.viewName = name;
    void instant;
    this.dirty = true;
  }

  resetCamera() {
    this.opts.camera = 'white';
    this.setViewpoint('white', true);
    if (this.state) {
      this.build(this.state);
      this.applyLevel();
      this.drawMarks();
    }
  }

  setActive(on) {
    this.active = !!on && !this.lost;
    if (this.active) this.resize();
    this.dirty = true;
  }

  resize() {
    if (this.disposed) return;
    const w = this.container.clientWidth;
    const h = this.container.clientHeight;
    if (!w || !h) return;
    this.renderer.setSize(w, h, false);
    const before = this.camera.aspect;
    this.camera.aspect = w / h;
    this.camera.updateProjectionMatrix();
    if (Math.abs(before - this.camera.aspect) > 0.05 && this.viewName) this.setViewpoint(this.viewName, true);
    this.dirty = true;
  }

  // ------------------------------------------------------------ picking

  bindPointer() {
    const c = this.canvas;
    let down = null;
    let pressTimer = null;
    this.onDown = (ev) => {
      down = { x: ev.clientX, y: ev.clientY, t: performance.now(), long: false };
      window.clearTimeout(pressTimer);
      if (ev.pointerType === 'touch') {
        pressTimer = window.setTimeout(() => { if (down) down.long = true; }, 550);
      }
    };
    this.onUp = (ev) => {
      window.clearTimeout(pressTimer);
      if (!down) return;
      const dx = ev.clientX - down.x;
      const dy = ev.clientY - down.y;
      const long = down.long;
      down = null;
      if (dx * dx + dy * dy > 36) return;
      if (ev.button === 2) return;
      this.pickAt(ev.clientX, ev.clientY, { all: ev.shiftKey || ev.altKey || long, xray: !!this.opts.xray });
    };
    this.onContext = (ev) => {
      ev.preventDefault();
      this.pickAt(ev.clientX, ev.clientY, { all: true, xray: !!this.opts.xray });
    };
    let lastMove = 0;
    this.onMove = (ev) => {
      const now = performance.now();
      if (now - lastMove < 50) return;
      lastMove = now;
      const ids = this.hits(ev.clientX, ev.clientY);
      const h = ids.length ? ids[0] : null;
      c.style.cursor = h ? 'pointer' : 'grab';
      if (h !== this.hovered) {
        this.hovered = h;
        this.drawMarks();
      }
    };
    this.onLeave = () => {
      if (this.hovered) {
        this.hovered = null;
        this.drawMarks();
      }
    };
    c.addEventListener('pointerdown', this.onDown);
    c.addEventListener('pointerup', this.onUp);
    c.addEventListener('contextmenu', this.onContext);
    c.addEventListener('pointermove', this.onMove);
    c.addEventListener('pointerleave', this.onLeave);
  }

  visibleChain(o) {
    while (o) {
      if (!o.visible) return false;
      o = o.parent;
    }
    return true;
  }

  // Square ids under a screen point, nearest first, each once.
  hits(clientX, clientY) {
    const r = this.canvas.getBoundingClientRect();
    if (!r.width || !r.height) return [];
    this.pointer.set(((clientX - r.left) / r.width) * 2 - 1, -((clientY - r.top) / r.height) * 2 + 1);
    this.raycaster.setFromCamera(this.pointer, this.camera);
    const found = this.raycaster.intersectObjects(this.pickables, false);
    const seen = new Set();
    const out = [];
    found.forEach((h) => {
      const id = h.object.userData.square;
      if (!id || seen.has(id) || !this.visibleChain(h.object)) return;
      seen.add(id);
      out.push(id);
    });
    return out;
  }

  pickAt(clientX, clientY, opts) {
    // Only the committed end point is playable: no picks mid-animation.
    if (this.tweens.length) this.finishTweens();
    const ids = this.hits(clientX, clientY);
    if (ids.length && this.cb.onPick) this.cb.onPick(ids, opts);
  }

  // Screen position (client pixels) of a square's tile centre, for tests.
  projectSquare(id) {
    const loc = this.squareLocal(id);
    if (!loc) return null;
    const v = new THREE.Vector3(loc.x, 0.0, loc.z);
    loc.group.localToWorld(v);
    v.project(this.camera);
    const r = this.canvas.getBoundingClientRect();
    return { x: r.left + (v.x + 1) / 2 * r.width, y: r.top + (1 - v.y) / 2 * r.height };
  }

  info() {
    const boards = {};
    Object.keys(this.groups).forEach((id) => {
      const g = this.groups[id];
      boards[id] = { x: g.position.x, y: g.position.y, z: g.position.z, visible: g.visible, parent: g.userData.parent };
    });
    return {
      ready: !this.disposed && !this.lost, active: this.active, animating: this.tweens.length > 0,
      boards: boards, pickables: this.pickables.length, textures: this.textures.size,
      webgl2: this.renderer.capabilities.isWebGL2, camera: this.viewName,
      renderer: this.renderer.info ? { geometries: this.renderer.info.memory.geometries, textures: this.renderer.info.memory.textures } : null
    };
  }

  loseContextForTest() {
    const ext = this.renderer.getContext().getExtension('WEBGL_lose_context');
    if (!ext) return false;
    this.loseExt = ext;
    ext.loseContext();
    return true;
  }

  restoreContextForTest() {
    if (this.loseExt) this.loseExt.restoreContext();
  }

  // --------------------------------------------------------------- loop

  loop(now) {
    if (this.disposed || !this.active || this.lost) return;
    const animating = this.stepTweens(now);
    const damping = this.controls.enableDamping && this.controls.update();
    if (this.dirty || animating || damping) {
      this.renderer.render(this.scene, this.camera);
      this.dirty = false;
    }
  }

  dispose() {
    if (this.disposed) return;
    this.disposed = true;
    this.renderer.setAnimationLoop(null);
    this.resizeObserver.disconnect();
    const c = this.canvas;
    c.removeEventListener('webglcontextlost', this.onLost);
    c.removeEventListener('webglcontextrestored', this.onRestored);
    c.removeEventListener('pointerdown', this.onDown);
    c.removeEventListener('pointerup', this.onUp);
    c.removeEventListener('contextmenu', this.onContext);
    c.removeEventListener('pointermove', this.onMove);
    c.removeEventListener('pointerleave', this.onLeave);
    this.controls.dispose();
    this.scene.traverse((o) => {
      if (o.userData && o.userData.ownGeometry && o.geometry) o.geometry.dispose();
      if (o.userData && o.userData.ownMaterial && o.material) o.material.dispose();
    });
    Object.values(this.geo).forEach((g) => g.dispose());
    Object.values(this.mat).forEach((m) => m.dispose());
    this.textures.forEach((t) => t.dispose());
    this.textures.clear();
    this.renderer.dispose();
    if (c.parentNode) c.parentNode.removeChild(c);
  }
}
