# aobcore 0.0.0.9000

* `cog_info()` and `view_cog()` accept a COG URL given as GDAL's
  `"/vsicurl/https://..."` path and give the renderer the plain URL; before,
  the page asked the browser for the `/vsicurl/` path and every tile failed.
  GDAL's `/vsicurl?url=...` form works too. `/vsis3/`, `/vsigs/` and
  `/vsiaz/` paths are given to the renderer as their public https URLs
  (public objects only); another `/vsi` path that is not embedded is an
  error, since the browser cannot fetch it.
* Tiled COGs per gate A (design decision 0003, R-planned tiles) and scene
  spec 0.2 (#5): `cog_info()` reads each level's grid and tile byte ranges
  through GDAL, `cog_plan()` plans tiles with meshes projected to the view
  CRS (one Arrow vertex table and one index table), `scene_add_tiled_raster()`
  adds a `tiled_raster` layer and a `cog` data reference, and `view_cog()`
  (with `cog_scene()`) draws a COG from R in one call. Local COGs travel with
  their planned tiles' bytes embedded in the page.
* A scene with a tiled raster is written as scene spec 0.2; other scenes are
  still 0.1 (`scene_spec_version(scene)`).
* The renderer draws 0.2 `tiled_raster` layers: level selection by the
  plan's rule, HTTP range requests or embedded tile bytes, codecs none,
  deflate, lzw, zstd and packbits (others are a layer error), predictors,
  scale/offset, nodata and palettes.
* Raster meshes are now drawn unlit; deck.gl lit them with a camera-dependent
  highlight despite `material: false`.
* Two small polar COG fixtures in `inst/extdata` (`polar_3031.tif`,
  `polar_lonlat.tif`), made by `tools/make-polar-cogs.R`.

* Vector producers to native, interleaved GeoArrow: `vector_stream()` for
  wk-handleable input, `gdal_vector_stream()` for GDAL sources (gdalraster,
  in Suggests), and `vector_ipc()` for Arrow IPC bytes (#3).
* `scene_add_data()`, `scene_add_layer()`, `scene_add_vector()`,
  `scene_blobs()` and `scene_json()` build scene spec 0.1 documents (#3).
* A Natural Earth 50m coastline fixture south of 40S in `inst/extdata`.
* `write_scene_html()` writes a self-contained page: the scene JSON, its
  Arrow IPC blobs as base64 and the bundled scene spec 0.1 renderer
  (deck.gl, built from `js/`), with no CDN (#4).
* `probe_scene()` is the polar probe conformance scene with its data in
  `inst/extdata/probe` (#4).
* `scene_json()` writes numbers with as many digits as they need to read
  back exactly (#4).
