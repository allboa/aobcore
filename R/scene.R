#' Scene spec version
#'
#' The version of the allonboard scene spec that this package writes: the
#' lowest version that can express the scene. A scene is written as 0.1
#' unless it has a tiled raster layer (added by [scene_add_tiled_raster()]),
#' which needs 0.2; a tiled raster drawn as a colour image (`rgb`) or with
#' JPEG tiles needs 0.3; a view with `bounds` (see [scene()]'s `domain`)
#' needs 0.4. Each version only adds to the one before, so earlier output is
#' unchanged by the later additions.
#'
#' @param scene Optional scene. Without it, the version of a new scene.
#' @return A character string: `"0.1"`, `"0.2"` for a scene with a
#'   `tiled_raster` layer, or `"0.3"` for one whose tiled raster uses `rgb`
#'   or JPEG tiles, or `"0.4"` for one whose view has `bounds`. A scene
#'   already marked with a version keeps at least that version (removing
#'   its bounds by hand leaves it 0.4).
#' @export
#' @examples
#' scene_spec_version()
#' scene_spec_version(scene())
scene_spec_version <- function(scene = NULL) {
  if (is.null(scene)) return("0.1")
  if (!is.null(scene$view$bounds) || identical(scene$version, "0.4")) return("0.4")
  kinds <- vapply(scene$layers, function(l) as.character(l$kind %||% ""), "")
  v03 <- vapply(scene$layers, uses_spec_03, TRUE)
  if (any(v03) || identical(scene$version, "0.3")) return("0.3")
  if (any(kinds == "tiled_raster") || identical(scene$version, "0.2")) "0.2" else "0.1"
}

scene_spec_versions <- c("0.1", "0.2", "0.3", "0.4")

## Does a layer use a scene spec 0.3 feature (a colour image, or JPEG tiles)?
uses_spec_03 <- function(l) {
  if (!identical(l$kind, "tiled_raster")) return(FALSE)
  if (!is.null(l$rgb)) return(TRUE)
  any(vapply(l$plan$levels, function(lv) identical(lv$encoding$codec, "jpeg"), TRUE))
}

#' Create an empty scene
#'
#' A minimal scene: a projected view in `crs`, with no data and no layers.
#' Its fields follow the scene spec (`version`, `view`, `data`, `layers`).
#' A scene starts as version 0.1 and becomes 0.2 when a tiled raster is
#' added (0.3 for a colour image or JPEG tiles). A view with `bounds` is
#' scene spec 0.4.
#'
#' By default the view gets the CRS's domain (allboa/design decision 0005,
#' [crs_domain()]) as `view$bounds`: the renderer keeps the camera within
#' it, plus a margin, and a scene with no `extent` opens on its data clipped
#' to it, or on the whole domain when it has no data. The domain limits the
#' camera only; data outside it still load and draw.
#'
#' @param crs The view CRS. Defaults to `"EPSG:3031"` (Antarctic Polar
#'   Stereographic). An `"authority:code"` string, or any definition GDAL
#'   reads (WKT, a PROJ string, PROJJSON), which is carried as PROJJSON
#'   when it has no code; see [scene_crs()].
#' @param domain The region the camera is kept within: `TRUE` (the default,
#'   unless the option `aobcore.domain` says otherwise) for [crs_domain()]
#'   of `crs` with its defaults (left out, with no error,
#'   when 'gdalraster' is not installed or cannot measure the CRS), `FALSE`
#'   for none, a [crs_domain()] result, or an extent `c(xmin, xmax, ymin,
#'   ymax)` in view CRS units.
#' @return A list of class `"aob_scene"`.
#' @export
#' @examples
#' s <- scene()
#' s$view$crs
scene <- function(crs = "EPSG:3031", domain = getOption("aobcore.domain", TRUE)) {
  crs <- scene_crs(crs)
  view <- list(type = "projected", crs = crs)
  view$bounds <- view_bounds(crs, domain)
  s <- structure(
    list(
      version = "0.1",
      view = view,
      data = structure(list(), names = character()),
      layers = list()
    ),
    class = "aob_scene"
  )
  s$version <- scene_spec_version(s)
  s
}

## The view bounds for `domain` (see scene()), or NULL for none.
view_bounds <- function(crs, domain) {
  if (is.null(domain) || isFALSE(domain)) return(NULL)
  if (isTRUE(domain)) {
    if (!requireNamespace("gdalraster", quietly = TRUE)) return(NULL)
    d <- tryCatch(gdal_quiet(crs_domain(crs)), error = function(e) NULL)
    return(d$extent)
  }
  if (inherits(domain, "aob_domain")) {
    if (!crs_same(domain$crs, crs)) {
      stop("The domain is in ", crs_label(domain$crs), " but the view is in ",
           crs_label(crs), ".", call. = FALSE)
    }
    return(domain$extent)
  }
  if (!is.numeric(domain) || length(domain) != 4L || anyNA(domain) ||
      !(domain[1] < domain[2] && domain[3] < domain[4])) {
    stop("`domain` must be TRUE, FALSE, a crs_domain() result, or c(xmin, xmax, ymin, ymax) ",
         "with xmin < xmax and ymin < ymax.", call. = FALSE)
  }
  as.numeric(domain)
}

#' @export
print.aob_scene <- function(x, ...) {
  cat("<scene spec ", x$version, "> view ", x$view$type, " ", crs_label(x$view$crs),
      if (!is.null(x$view$bounds)) " (bounded)",
      ", ", length(x$data), " data, ", length(x$layers), " layers\n", sep = "")
  invisible(x)
}
