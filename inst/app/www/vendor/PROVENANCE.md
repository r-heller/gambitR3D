# Third-party browser assets

Everything the app loads in the browser is shipped in this package. Nothing
is fetched from a CDN, and no web fonts are used (the app uses the system
font stack).

## three.js 0.186.1 (MIT)

| Field | Value |
|---|---|
| Project | three.js, <https://threejs.org>, <https://github.com/mrdoob/three.js> |
| Version | 0.186.1 (npm release of 2026-09-24) |
| Licence | MIT, copyright 2010-2026 three.js authors; full text in `three/LICENSE` |
| Source | npm registry tarball <https://registry.npmjs.org/three/-/three-0.186.1.tgz> |
| Tarball SHA-256 | `8cd068708ea44f2c73c944b1cead2ba2f0d5c15c8fc194e5700f4e4f4a033fe7` |
| Tarball integrity (npm) | `sha512-blFeqb49wRCSGUGj7gtpfnSGHy2lwDk94RhUmS1c/hTby70kvChbWpkJ4Pm1390LqzzvTmzgXKHPEafJwCb8jA==` (verified locally against the downloaded file) |
| Retrieved | 2026-10-26 with `npm pack three@0.186.1 --ignore-scripts` |
| Modifications | none; files copied byte for byte |

Files taken from the tarball (`package/` prefix removed):

| Path in this folder | Path in the tarball | SHA-256 |
|---|---|---|
| `three/three.module.js` | `build/three.module.js` | `9052042d676cb0fdc1ddfefe193053f34b7ac0513a616fdac4535d49987812ea` |
| `three/three.core.js` | `build/three.core.js` | `9edde002b066a9a05676a6127f67735b62baf399bdea529f2f7e31657da769e6` |
| `three/addons/controls/OrbitControls.js` | `examples/jsm/controls/OrbitControls.js` | `3d79d07ecb686b4e5d93232eedab255331c1beef711e13164eaa1f68655a5f2b` |
| `three/LICENSE` | `LICENSE` | `8b378ebe60e2fe500158cb0ac71cb5e8b7d92953c2abcc63a0eb90499653b5bc` |

The npm release ships unminified ES modules only. They are loaded through
the page's import map (`three`, `three/addons/`), see `R/app.R`.

To verify: `sha256sum inst/app/www/vendor/three/*.js
inst/app/www/vendor/three/addons/controls/OrbitControls.js
inst/app/www/vendor/three/LICENSE` and compare with the table above.

## Original project assets

`g3-app.js`, `g3-scene.js`, `g3-app.css`, `g3-emblem.svg` and the inline
navigation icons are original to gambitR3D (MIT, as the package). Pieces are
lettered discs drawn at run time (P, N, B, R, Q, K); no third-party artwork,
fonts, logos or piece designs are included.
