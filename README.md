# aobcore

Lean R core for allonboard: producers to native GeoArrow and grid descriptors, scene building, embed transport, and the bundled renderer. Core Imports stay at nanoarrow, geoarrow, wk and htmltools. The package name is a placeholder.

Read the org agent brief first: [allboa/design AGENTS.md](https://github.com/allboa/design/blob/main/AGENTS.md). It covers the design principles, how work flows, and when to stop and ask.

## Status

Early skeleton (phase 1). The package exports `scene_spec_version()` and a stub `scene()` that returns an empty scene in a projected view CRS (EPSG:3031 by default), following [scene spec 0.1](https://github.com/allboa/scenespec). Producers, transport and the renderer are not written yet. Nothing here is on CRAN.

## Install

```r
# install.packages("remotes")
remotes::install_github("allboa/aobcore")
```

Imports: nanoarrow, geoarrow, wk, htmltools. gdalraster is suggested for the GDAL vector producer.
