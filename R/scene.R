#' Scene spec version
#'
#' The version of the allonboard scene spec that this package writes.
#'
#' @return A character string, currently `"0.1"`.
#' @export
#' @examples
#' scene_spec_version()
scene_spec_version <- function() {
  "0.1"
}

#' Create an empty scene
#'
#' A minimal scene: a projected view in `crs`, with no data and no layers.
#' Its fields follow the scene spec (`version`, `view`, `data`, `layers`).
#' This is a stub; producers and layers come later.
#'
#' @param crs The view CRS as an `"authority:code"` string. Defaults to
#'   `"EPSG:3031"` (Antarctic Polar Stereographic).
#' @return A list of class `"aob_scene"`.
#' @export
#' @examples
#' s <- scene()
#' s$view$crs
scene <- function(crs = "EPSG:3031") {
  if (!is.character(crs) || length(crs) != 1L || is.na(crs) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$", crs)) {
    stop("`crs` must be a single \"authority:code\" string, such as \"EPSG:3031\".",
         call. = FALSE)
  }
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
  cat("<scene spec ", x$version, "> view ", x$view$type, " ", x$view$crs,
      ", ", length(x$data), " data, ", length(x$layers), " layers\n", sep = "")
  invisible(x)
}
