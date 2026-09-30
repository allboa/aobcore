# aobcore 0.0.0.9000

* Scenes have a view domain by default (allboa/design decision 0005):
  `scene()`, `cog_scene()` and `view_cog()` take `domain`, by default the
  new `crs_domain()` of the view CRS, which walks out from the projection
  centre until the stretch along each bearing passes `k = 2`. It is written
  as `view.bounds` (scene spec 0.4): the renderer keeps the camera within it
  plus a quarter of its size on each side, and a scene with no `extent`
  opens on its data clipped to it. It limits the camera only; data outside
  it still load. `domain = FALSE`, or `options(aobcore.domain = FALSE)`,
  turns it off and keeps the 0.1 to 0.3 output as before. The domain needs
  'gdalraster'; without it `scene()` writes no bounds, so the same code can
  write 0.4 with 'gdalraster' installed and 0.1 to 0.3 without.
* A view CRS can be any definition GDAL reads, not only an
  `"authority:code"` string: WKT, a PROJ string, PROJJSON text, an EPSG
  number or an `sf` `crs`. `scene()`, `cog_plan()`, `cog_scene()`,
  `view_cog()`, `vector_stream()` and `gdal_vector_stream()` resolve it with
  the new `scene_crs()`: a definition that is exactly an authority's CRS
  becomes its code, and one with no code is carried in the scene as a
  PROJJSON object, as the scene spec already allows. The renderer labels a
  PROJJSON CRS named "unknown" by its projection method.
* RGB(A) COGs are drawn as colour images (scene spec 0.3 `rgb`), not one
  band through a palette. `scene_add_tiled_raster()`, `cog_scene()` and
  `view_cog()` do so by default when the COG has 3 or 4 Byte bands whose
  colour interpretation is Red, Green, Blue (and Alpha); `band =`,
  `palette =` or `rgb = FALSE` keep the single-band palette path, and
  `rgb = c(r, g, b[, a])` picks the bands. Alpha 0, or all colour bands at
  nodata, is transparent. Non-Byte bands take `range` (by default the
  data's) (#13).
* JPEG-compressed COGs are no longer refused: each level's shared JPEG
  tables are carried in the plan (`encoding$jpeg_tables`, scene spec 0.3)
  and the renderer decodes each tile, joined to its tables, with the
  browser's JPEG decoder. Only YCbCr (3 bands) and greyscale (1 band) JPEG
  are allowed; other photometric interpretations are refused with an error
  naming the layer and level (#13).
* `cog_info()` records each band's colour interpretation (`color_interp`)
  and each level's TIFF photometric interpretation (`photometric`), and
  reads JPEGTables, from the TIFF image directories (classic TIFF and
  BigTIFF, through GDAL's virtual file layer, falling back to GDAL's
  metadata when they cannot be read) (#13).
* Scenes are written at the lowest spec version that expresses them: 0.3
  only when a tiled raster uses `rgb` or JPEG tiles, else 0.2 or 0.1 as
  before (#13).
* New fixtures `polar_rgba.tif` (RGBA, LZW) and `polar_ycbcr.tif` (YCbCr
  JPEG), made by `tools/make-rgb-cogs.R` (#13).
* With `embed = TRUE` (the default for a local COG), the scene's `cog`
  reference is the file's base name, not its absolute `file://` path, so a
  shared page no longer reveals the local directory.
  `scene_add_tiled_raster()`, `cog_scene()` and `view_cog()` take `url =`
  to write another reference (#11).
* `cog_plan()` has a tile budget, `max_tiles` (default 1024): an
  `all_levels` plan adds levels coarse to fine and leaves out, with a
  warning, a level that would pass it and every finer level. Pass `extent`
  to plan part of a large COG at full resolution, or `max_tiles = Inf` (#11).
* `view_cog()` and `cog_scene()` add the south-of-40S coastline by default
  only when the view CRS is centred on the South Pole (`coastline = NULL`);
  `TRUE` and `FALSE` still force it (#11).
* The renderer keeps at most 256 idle decoded tiles, dropping the least
  recently drawn, aborts tile fetches the view no longer needs, chooses the
  level and tiles again when the page is resized, and, when a server
  ignores `Range` and sends the whole COG, downloads it once and cuts every
  tile from it rather than downloading the file per tile (#11). Rendering
  again into the same element stops the previous scene, and the handle
  `aob.render()` returns has `finalize()`.
* CI runs the scenespec validator on the scenes `tools/write-scenes.R`
  writes, including `cog_scene()` output (#11).
* `gdal_vector_stream()`: `-nlt` in `options` counts as a native type only
  for point, line and polygon types (single or multi, with any Z, M or 25D
  suffix); `-nlt GEOMETRY` and other non-native types go through WKB and
  are converted in R instead of failing in GDAL, and `PROMOTE_TO_MULTI` and
  `CONVERT_TO_LINEAR` defer to the layer's type. The Arrow route drops Z
  and M in GDAL (`-dim XY`), as its driver writes 2D only (#9).
* `gdal_vector_stream()`'s Arrow route writes the PROJJSON CRS to the
  geometry field's `ARROW:extension:metadata`, as the R route does, so both
  routes give the same field (#9).
* Zero features (for example a clip that removes everything) give an empty
  stream of the declared geometry type: the layer's type in
  `gdal_vector_stream()`, and the `sfc` class or 'geoarrow' type in
  `vector_stream()`. Empty input with no declared native type is still an
  error, now documented (#9).

* A global lon/lat COG in a polar view no longer asks the browser for every
  full-resolution tile. `cog_plan()` measures each level's pixel size as the
  median over the grid (the far pole had pushed it to around 1e20 m, so the
  renderer always chose full resolution), and leaves out tiles stretched past
  `max_stretch` (default 8) times that size, with a message (the far polar
  cap of global data). The extent passed to `view_cog()` / `cog_scene()`,
  widened by half its size on each side, now also culls the plan's tiles.
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
