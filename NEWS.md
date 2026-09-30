# aobcore 0.0.0.9000

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
