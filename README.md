# aobcore

Lean R core for allonboard: producers to native GeoArrow and grid descriptors, scene building, embed transport, and the bundled renderer. Core Imports stay at nanoarrow, geoarrow, wk and htmltools. The package name is a placeholder.

Read the org agent brief first: [allboa/design AGENTS.md](https://github.com/allboa/design/blob/main/AGENTS.md). It covers the design principles, how work flows, and when to stop and ask.

## Status

Early (phase 1). The package exports `scene_spec_version()`, a stub `scene()` that returns an empty scene in a projected view CRS (EPSG:3031 by default) following [scene spec 0.1](https://github.com/allboa/scenespec), `write_scene_html()` (the embed transport) and `probe_scene()` (the polar probe conformance scene and its data). Producers are not written yet. Nothing here is on CRAN.

## A self-contained page

`write_scene_html(scene, blobs, file)` writes one HTML file: the scene as JSON, each Arrow IPC blob as base64, and the bundled renderer inlined, so the page works offline and loads nothing from a CDN. `scene` is a list following scene spec 0.1; `blobs` is a named list of raw vectors whose names are the scene's `blob` keys.

```r
library(aobcore)
p <- probe_scene()
f <- write_scene_html(p$scene, p$blobs, "polar-probe.html")
browseURL(f)
```

The page follows the browser's light or dark preference; `theme = "light"` or `"dark"` fixes it, and the page has a theme button.

![The polar probe conformance scene, light](tools/screenshots/polar-probe-light.png)

## The renderer

The renderer implements scene spec 0.1 on [deck.gl](https://deck.gl) and [apache-arrow](https://arrow.apache.org/docs/js/). Its source is in `js/src/` and is not part of the R package; the package ships one minified bundle, `inst/renderer/aob-renderer.min.js` (about 1.1 MiB, 310 KiB gzipped), and `inst/COPYRIGHTS` for the bundled code. Everything that maps spec concepts to deck.gl is in `js/src/layers.js`.

What it draws: `polygon` (fill with holes, optional stroke), `path`, `point` (fill and stroke, `radius_px`), and their multi- encodings; colors as constant RGBA or an RGBA column; `raster` as values on a grid descriptor colored by a named palette (`ocean`, `viridis`, `ice`, `gray`), on a pre-projected mesh, or without a mesh when the grid is in the view CRS; `nodata`, NaN and Arrow nulls are transparent. `projected` and `cartesian` views are flat views of view CRS units; `globe` is drawn for vector layers only. Data marked `origin_subtracted` are placed back at `view.local_origin`, and raster meshes are drawn relative to it. Data references with a `url` are fetched.

### Rebuild the bundle

The bundle is reproducible from `js/package.json` and `js/package-lock.json` with Node 22:

```sh
cd js
npm ci
npm run build          # writes inst/renderer/aob-renderer.min.js and inst/COPYRIGHTS
npm run check-bundle   # fails if the committed files differ from a fresh build
```

The build fails if the bundle has non-ASCII characters or a closing script tag. CI runs `check-bundle` on every pull request (`.github/workflows/renderer-bundle.yaml`).

### Screenshots

`tools/write-scenes.R` writes the conformance scene and a scene of every 0.1 layer kind as pages and as scene JSON; `js/screenshots.mjs` takes light and dark screenshots of each page with headless Chromium into `tools/screenshots/`:

```sh
R CMD INSTALL .
Rscript tools/write-scenes.R /tmp/scenes
node scenespec/scripts/validate.js /tmp/scenes/*.json   # from allboa/scenespec
cd js && node screenshots.mjs /tmp/scenes               # CHROMIUM_PATH=... to pick a browser
```

## Install

```r
# install.packages("remotes")
remotes::install_github("allboa/aobcore")
```

Imports: nanoarrow, geoarrow, wk, htmltools. gdalraster is suggested for the GDAL vector producer.
