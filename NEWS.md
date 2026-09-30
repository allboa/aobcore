# aobcore 0.0.0.9000

* Vector producers to native, interleaved GeoArrow: `vector_stream()` for
  wk-handleable input, `gdal_vector_stream()` for GDAL sources (gdalraster,
  in Suggests), and `vector_ipc()` for Arrow IPC bytes (#3).
* `scene_add_data()`, `scene_add_layer()`, `scene_add_vector()`,
  `scene_blobs()` and `scene_json()` build scene spec 0.1 documents (#3).
* A Natural Earth 50m coastline fixture south of 40S in `inst/extdata`.
