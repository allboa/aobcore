# aobcore

Lean R core for allonboard: producers to native GeoArrow and grid descriptors, scene building, embed transport, and the bundled renderer. Core Imports stay at nanoarrow, geoarrow, wk and htmltools. The package name is a placeholder.

Read the org agent brief first: [allboa/design AGENTS.md](https://github.com/allboa/design/blob/main/AGENTS.md). It covers the design principles, how work flows, and when to stop and ask.

## Status

Early (phase 1), following [scene spec 0.1](https://github.com/allboa/scenespec). Nothing here is on CRAN.

- `scene()` starts a scene in a projected view CRS (EPSG:3031 by default).
- `vector_stream(x, crs)` turns any wk-handleable input (sf, sfc, wk vectors, data frames with a geometry column, or a WKB Arrow stream) into a nanoarrow stream with a native, interleaved GeoArrow geometry column. The core does not reproject: coordinates must already be in the view CRS.
- `gdal_vector_stream(dsn, crs, ...)` reads, clips, densifies and reprojects any GDAL vector source to native GeoArrow (gdalraster, in Suggests; decision 0002). It uses GDAL's Arrow driver when present (`gdal_has_arrow()`), otherwise converts in R.
- `vector_ipc()` writes Arrow IPC bytes; `scene_add_data()`, `scene_add_layer()` and `scene_add_vector()` add data references and layers; `scene_blobs()` returns the bytes; `scene_json()` writes the scene document.
- `write_scene_html(scene)` writes a self-contained page that draws the scene (the embed transport); `probe_scene()` is the polar probe conformance scene with its data.

```r
library(aobcore)
coast <- system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
s <- scene("EPSG:3031")
s <- scene_add_vector(s, "coast", gdal_vector_stream(coast, s$view$crs, densify = 0.25),
                      stroke = c(60, 66, 72, 255), stroke_width_px = 1)
cat(scene_json(s, pretty = TRUE))
write_scene_html(s, file = "coast.html")
```

## A self-contained page

`write_scene_html(scene, blobs = scene_blobs(scene), file)` writes one HTML file: the scene as JSON (`scene_json()`), each Arrow IPC blob as base64, and the bundled renderer inlined, so the page works offline and loads nothing from a CDN. A scene built with `scene_add_*()` carries its blobs; a plain list following scene spec 0.1 can be given with a named list of raw blobs instead.

```r
library(aobcore)
f <- write_scene_html(probe_scene(), file = "polar-probe.html")
browseURL(f)
```

The page follows the browser's light or dark preference; `theme = "light"` or `"dark"` fixes it, and the page has a theme button.

![The polar probe conformance scene, light](tools/screenshots/polar-probe-light.png)

## The renderer

The renderer implements scene spec 0.1 on [deck.gl](https://deck.gl) and [apache-arrow](https://arrow.apache.org/docs/js/). Its source is in `js/src/` and is not part of the R package; the package ships one minified bundle, `inst/renderer/aob-renderer.min.js` (about 1.1 MiB, 310 KiB gzipped), and `inst/COPYRIGHTS` for the bundled code. Everything that maps spec concepts to deck.gl is in `js/src/layers.js`.

What it draws: `polygon` (fill with holes, optional stroke), `path`, `point` (fill and stroke, `radius_px`), and their multi- encodings; colors as constant RGBA or an RGBA column; `raster` as values on a grid descriptor colored by a named palette (`ocean`, `viridis`, `ice`, `gray`; any other name is an error for that layer, which is then not drawn and is reported in the page's status line), on a pre-projected mesh, or without a mesh when the grid is in the view CRS; `nodata`, NaN and Arrow nulls are transparent. `projected` and `cartesian` views are flat views of view CRS units; `globe` is drawn for vector layers only. Data marked `origin_subtracted` are placed back at `view.local_origin`, and raster meshes are drawn relative to it. Data references with a `url` are fetched. Those fetches are the page's only network requests: the renderer, the scene and blobs are inline, and loaders.gl (bundled as a deck.gl dependency) could only reach the network to load a worker or an image by URL, which this renderer never asks it to do.

### Rebuild the bundle

The bundle is reproducible from `js/package.json` and `js/package-lock.json` with Node 22:

```sh
cd js
npm ci
npm run build          # writes inst/renderer/aob-renderer.min.js and inst/COPYRIGHTS
npm run check-bundle   # fails if the committed files differ from a fresh build
npm test               # renderer tests; the page test needs Chromium (CHROMIUM_PATH=...)
```

The build fails if the bundle has non-ASCII characters or a closing script tag. CI runs `check-bundle` and `npm test` on every pull request (`.github/workflows/renderer-bundle.yaml`).

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

Imports: nanoarrow, geoarrow, wk, htmltools. gdalraster is suggested for the GDAL vector producer. For GDAL to encode GeoArrow itself it needs the Arrow driver; on conda-forge that is the `libgdal-arrow-parquet` package.
