# aobcore

Lean R core for allonboard: producers to native GeoArrow and grid descriptors, scene building, embed transport, and the bundled renderer. Core Imports stay at nanoarrow, geoarrow, wk and htmltools. The package name is a placeholder.

Read the org agent brief first: [allboa/design AGENTS.md](https://github.com/allboa/design/blob/main/AGENTS.md). It covers the design principles, how work flows, and when to stop and ask.

## Status

Early (phase 1), following [scene spec 0.1](https://github.com/allboa/scenespec). Nothing here is on CRAN.

- `scene()` starts a scene in a projected view CRS (EPSG:3031 by default).
- `vector_stream(x, crs)` turns any wk-handleable input (sf, sfc, wk vectors, data frames with a geometry column, or a WKB Arrow stream) into a nanoarrow stream with a native, interleaved GeoArrow geometry column. The core does not reproject: coordinates must already be in the view CRS.
- `gdal_vector_stream(dsn, crs, ...)` reads, clips, densifies and reprojects any GDAL vector source to native GeoArrow (gdalraster, in Suggests; decision 0002). It uses GDAL's Arrow driver when present (`gdal_has_arrow()`), otherwise converts in R.
- `vector_ipc()` writes Arrow IPC bytes; `scene_add_data()`, `scene_add_layer()` and `scene_add_vector()` add data references and layers; `scene_blobs()` returns the bytes; `scene_json()` writes the scene document.

```r
library(aobcore)
coast <- system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
s <- scene("EPSG:3031")
s <- scene_add_vector(s, "coast", gdal_vector_stream(coast, s$view$crs, densify = 0.25),
                      stroke = c(60, 66, 72, 255), stroke_width_px = 1)
cat(scene_json(s, pretty = TRUE))
```

Transport and the renderer are not written yet.

## Install

```r
# install.packages("remotes")
remotes::install_github("allboa/aobcore")
```

Imports: nanoarrow, geoarrow, wk, htmltools. gdalraster is suggested for the GDAL vector producer. For GDAL to encode GeoArrow itself it needs the Arrow driver; on conda-forge that is the `libgdal-arrow-parquet` package.
