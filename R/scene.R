#' Scene spec version
#'
#' The version of the allonboard scene spec that this package writes. A
#' scene is written as 0.1 unless it has a tiled raster layer (added by
#' [scene_add_tiled_raster()]), which needs 0.2; 0.1 output is unchanged
#' by the 0.2 additions.
#'
#' @param scene Optional scene. Without it, the version of a new scene.
#' @return A character string: `"0.1"`, or `"0.2"` for a scene with a
#'   `tiled_raster` layer (or one already marked 0.2).
#' @export
#' @examples
#' scene_spec_version()
#' scene_spec_version(scene())
scene_spec_version <- function(scene = NULL) {
  if (is.null(scene)) return("0.1")
  kinds <- vapply(scene$layers, function(l) as.character(l$kind %||% ""), "")
  if (any(kinds == "tiled_raster") || identical(scene$version, "0.2")) "0.2" else "0.1"
}

scene_spec_versions <- c("0.1", "0.2")

#' Create an empty scene
#'
#' A minimal scene: a projected view in `crs`, with no data and no layers.
#' Its fields follow the scene spec (`version`, `view`, `data`, `layers`).
#' A scene starts as version 0.1 and becomes 0.2 when a tiled raster is
#' added.
#'
#' @param crs The view CRS. Defaults to `"EPSG:3031"` (Antarctic Polar
#'   Stereographic). An `"authority:code"` string, or any definition GDAL
#'   reads (WKT, a PROJ string, PROJJSON), which is carried as PROJJSON
#'   when it has no code; see [scene_crs()].
#' @return A list of class `"aob_scene"`.
#' @export
#' @examples
#' s <- scene()
#' s$view$crs
scene <- function(crs = "EPSG:3031") {
  crs <- scene_crs(crs)
  structure(
    list(
      version = scene_spec_version(),
      view = list(type = "projected", crs = crs),
      data = structure(list(), names = character()),
      layers = list()
    ),
    class = "aob_scene"
  )
}

#' @export
print.aob_scene <- function(x, ...) {
  cat("<scene spec ", x$version, "> view ", x$view$type, " ", crs_label(x$view$crs),
      ", ", length(x$data), " data, ", length(x$layers), " layers\n", sep = "")
  invisible(x)
}
