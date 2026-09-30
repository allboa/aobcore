#' The polar probe conformance scene
#'
#' The polar view probe from the design origin record (2026-09-30), as the
#' scene spec 0.1 conformance scene `conformance/polar-probe.json` in
#' allboa/scenespec, with its Arrow IPC data. The view is EPSG:3031. Layers,
#' bottom to top: a synthetic SST-like raster on a lon/lat grid drawn with a
#' pre-projected mesh, Natural Earth 50m land as polygons, a graticule with an
#' RGBA colour column, and the Natural Earth 50m coastline as lines.
#'
#' The data are in `system.file("extdata", "probe", package = "aobcore")`,
#' one Arrow IPC stream file (`.arrows`) per blob.
#'
#' @return A scene of class `"aob_scene"` carrying its six blobs (see
#'   [scene_blobs()]), ready for [write_scene_html()].
#' @export
#' @examples
#' p <- probe_scene()
#' p
#' lengths(scene_blobs(p))
probe_scene <- function() {
  dir <- system.file("extdata", "probe", package = "aobcore")
  keys <- c("land", "coast", "graticule", "sst_mesh", "sst_index", "sst_values")
  blobs <- lapply(keys, function(k) {
    f <- file.path(dir, paste0(k, ".arrows"))
    readBin(f, what = "raw", n = file.size(f))
  })
  names(blobs) <- keys

  vector_ref <- function(blob, encoding) {
    list(format = "arrow-ipc-stream", blob = blob,
         geometry = list(column = "geometry", encoding = encoding))
  }
  table_ref <- function(blob) list(format = "arrow-ipc-stream", blob = blob)
  half <- 5791903.876384494

  scene <- list(
    version = scene_spec_version(),
    view = list(
      type = "projected",
      crs = "EPSG:3031",
      center = c(0, 0),
      extent = c(-half, half, -half, half)
    ),
    data = list(
      land = vector_ref("land", "geoarrow.polygon"),
      coast = vector_ref("coast", "geoarrow.linestring"),
      graticule = vector_ref("graticule", "geoarrow.linestring"),
      sst_mesh = table_ref("sst_mesh"),
      sst_index = table_ref("sst_index"),
      sst_values = table_ref("sst_values")
    ),
    layers = list(
      list(
        id = "sst", kind = "raster", label = "Synthetic SST-like field",
        grid = list(crs = "EPSG:4326", extent = c(-180, 180, -90, -40), dim = c(360L, 50L)),
        values = "sst_values", values_column = "value",
        palette = list(name = "ocean", range = c(-2, 13)),
        mesh = list(vertices = "sst_mesh", indices = "sst_index",
                    position_column = "position", uv_column = "uv", index_column = "index")
      ),
      list(id = "land", kind = "polygon", label = "Land (50m)", data = "land",
           fill = c(218L, 213L, 202L, 255L)),
      list(id = "graticule", kind = "path", label = "Graticule", data = "graticule",
           stroke = list(column = "color"), stroke_width_px = 1),
      list(id = "coast", kind = "path", label = "Coastline (50m)", data = "coast",
           stroke = c(60L, 66L, 72L, 255L), stroke_width_px = 1)
    )
  )
  structure(scene, class = "aob_scene", blobs = blobs)
}
