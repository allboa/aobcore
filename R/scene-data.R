#' Add vector data to a scene
#'
#' Adds a data reference to a scene: Arrow IPC stream bytes, held as a blob
#' keyed by `id`, with the geometry column and its native GeoArrow encoding
#' read from the bytes. The bytes travel beside the scene document (see
#' [scene_blobs()]); the scene itself holds only the reference, as in scene
#' spec 0.1.
#'
#' @param scene A scene from [scene()].
#' @param id The data id: a letter, then letters, digits, `_`, `.` or `-`.
#' @param x The data: Arrow IPC stream bytes (a raw vector, as from
#'   [vector_ipc()]), a 'nanoarrow' array stream, or anything
#'   [vector_stream()] accepts. Anything other than bytes or a native
#'   interleaved stream goes through [vector_stream()] with the scene's view
#'   CRS, so its coordinates must already be in that CRS.
#' @return The scene with `data[[id]]` added and the bytes stored as blob
#'   `id`.
#' @seealso [scene_add_layer()], [scene_add_vector()], [scene_json()].
#' @export
#' @examples
#' s <- scene("EPSG:3031")
#' x <- wk::wkt("LINESTRING (0 0, 1000 1000)", crs = "EPSG:3031")
#' s <- scene_add_data(s, "line", x)
#' s$data$line
scene_add_data <- function(scene, id, x) {
  check_scene(scene)
  check_id(id)
  if (id %in% names(scene$data)) {
    stop("Data id \"", id, "\" is already in the scene.", call. = FALSE)
  }
  bytes <- as_scene_ipc(x, scene$view$crs)
  geom <- geometry_field(nanoarrow::read_nanoarrow(bytes)$get_schema())
  if (is.null(geom) || !geom$interleaved) {
    stop("The data for \"", id, "\" has no native, interleaved GeoArrow geometry column.",
         call. = FALSE)
  }
  scene$data[[id]] <- list(
    format = "arrow-ipc-stream",
    blob = id,
    geometry = list(column = geom$column, encoding = geom$encoding)
  )
  blobs <- attr(scene, "blobs") %||% list()
  blobs[[id]] <- bytes
  attr(scene, "blobs") <- blobs
  scene
}

#' Add a vector layer to a scene
#'
#' Adds a layer that draws a data reference already in the scene. The layer
#' kind follows from the data's geometry encoding unless given: points draw
#' as `"point"`, lines as `"path"` and polygons as `"polygon"`. Styling is
#' data: a color is a constant `c(r, g, b, a)` of integers 0 to 255, or the
#' name of an RGBA column in the data.
#'
#' @param scene A scene from [scene()].
#' @param data The data id the layer draws (see [scene_add_data()]).
#' @param id The layer id. Defaults to `data`.
#' @param kind `"point"`, `"path"` or `"polygon"`, or `NULL` to infer it.
#' @param stroke,fill Optional colors: `c(r, g, b, a)` or a column name.
#'   `fill` does not apply to paths.
#' @param stroke_width_px Optional stroke width in pixels.
#' @param radius_px Optional point radius in pixels (points only).
#' @param label Optional human-readable name.
#' @param visible Optional initial visibility.
#' @param popup Optional attributes to show for one feature at a time
#'   (scene spec 0.5): a character vector of column names in the layer's
#'   data, shown in that order labelled by column name, or
#'   `list(columns = ..., trigger = ...)`. `trigger` is `"select"` (the
#'   spec's default: shown when the viewer selects a feature, by a click,
#'   tap or key press, until another selection or a dismissal) or `"point"`
#'   (shown while the pointer is over a feature; a renderer with no way to
#'   point without selecting treats it as `"select"`). The columns must be in
#'   the data and must not be its geometry column. A popup makes the scene
#'   scene spec 0.5.
#' @return The scene with the layer appended (drawn above earlier layers).
#' @seealso [scene_add_legend()] for a key to the layer's colours.
#' @export
#' @examples
#' s <- scene("EPSG:3031")
#' x <- wk::wkt("LINESTRING (0 0, 1000 1000)", crs = "EPSG:3031")
#' s <- scene_add_data(s, "line", x)
#' s <- scene_add_layer(s, "line", stroke = c(60, 66, 72, 255), stroke_width_px = 1)
#' s$layers[[1]]$kind
#'
#' # Attributes shown when a feature is selected (scene spec 0.5)
#' df <- data.frame(name = c("Davis", "Mawson"), depth_m = c(120, 450))
#' df$geometry <- wk::wkt(c("POINT (2300000 600000)", "POINT (1300000 1600000)"),
#'                        crs = "EPSG:3031")
#' s <- scene_add_data(scene(), "stations", df)
#' s <- scene_add_layer(s, "stations", popup = c("name", "depth_m"))
#' s$layers[[1]]$popup
#' scene_spec_version(s)
scene_add_layer <- function(scene, data, id = data, kind = NULL,
                            stroke = NULL, fill = NULL, stroke_width_px = NULL,
                            radius_px = NULL, label = NULL, visible = NULL, popup = NULL) {
  check_scene(scene)
  check_id(data, "data")
  check_id(id)
  ref <- scene$data[[data]]
  if (is.null(ref)) stop("Data id \"", data, "\" is not in the scene.", call. = FALSE)
  if (id %in% vapply(scene$layers, function(l) l$id, "")) {
    stop("Layer id \"", id, "\" is already in the scene.", call. = FALSE)
  }
  enc <- ref$geometry$encoding
  if (is.null(enc)) stop("Data \"", data, "\" has no geometry column.", call. = FALSE)
  inferred <- kind_for_encoding[[enc]]
  kind <- kind %||% inferred
  if (!identical(kind, inferred)) {
    stop("A ", kind, " layer cannot draw ", enc, "; use kind = \"", inferred, "\".",
         call. = FALSE)
  }
  if (!is.null(fill) && kind == "path") stop("`fill` does not apply to a path layer.", call. = FALSE)
  if (!is.null(radius_px) && kind != "point") {
    stop("`radius_px` applies only to a point layer.", call. = FALSE)
  }
  layer <- list(
    id = id,
    kind = kind,
    data = data,
    label = check_scalar(label, is.character, "label"),
    visible = check_scalar(visible, is.logical, "visible"),
    fill = as_color(fill, "fill"),
    stroke = as_color(stroke, "stroke"),
    stroke_width_px = check_px(stroke_width_px, "stroke_width_px"),
    radius_px = check_px(radius_px, "radius_px"),
    popup = as_popup(popup, scene, ref)
  )
  scene$layers[[length(scene$layers) + 1L]] <- layer[!vapply(layer, is.null, TRUE)]
  if (!is.null(layer$popup)) scene <- as_spec_05(scene)
  scene
}

#' Add vector data and a layer that draws it
#'
#' `scene_add_data()` then `scene_add_layer()`, with one id for both.
#'
#' @inheritParams scene_add_data
#' @param ... Passed to [scene_add_layer()] (`kind`, `stroke`, `fill`, `popup`, ...).
#' @return The scene with the data and the layer added.
#' @export
#' @examples
#' x <- wk::wkt("POLYGON ((0 0, 1000 0, 0 1000, 0 0))", crs = "EPSG:3031")
#' s <- scene_add_vector(scene(), "tri", x, fill = c(200, 80, 40, 128))
#' s
scene_add_vector <- function(scene, id, x, ...) {
  scene <- scene_add_data(scene, id, x)
  scene_add_layer(scene, data = id, id = id, ...)
}

#' Blobs carried beside a scene
#'
#' The Arrow IPC bytes that data references with a `blob` key point to. A
#' transport delivers these alongside the scene document.
#'
#' @param scene A scene from [scene()].
#' @return A named list of raw vectors, one per blob key.
#' @export
#' @examples
#' x <- wk::wkt("POINT (0 0)", crs = "EPSG:3031")
#' s <- scene_add_vector(scene(), "pt", x)
#' lengths(scene_blobs(s))
scene_blobs <- function(scene) {
  check_scene(scene)
  attr(scene, "blobs") %||% structure(list(), names = character())
}

## ---- internals -------------------------------------------------------------

kind_for_encoding <- list(
  "geoarrow.point" = "point", "geoarrow.multipoint" = "point",
  "geoarrow.linestring" = "path", "geoarrow.multilinestring" = "path",
  "geoarrow.polygon" = "polygon", "geoarrow.multipolygon" = "polygon"
)

as_scene_ipc <- function(x, crs) {
  if (is.raw(x)) return(x)
  if (inherits(x, "nanoarrow_array_stream")) {
    geom <- geometry_field(x$get_schema())
    if (!is.null(geom) && geom$interleaved) return(vector_ipc(x))
  }
  vector_ipc(vector_stream(x, crs))
}

check_scene <- function(scene) {
  if (!inherits(scene, "aob_scene")) stop("`scene` must be a scene from scene().", call. = FALSE)
  invisible(scene)
}

check_id <- function(id, arg = "id") {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) > 128L ||
      !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", id)) {
    stop("`", arg, "` must be a letter followed by letters, digits, '_', '.' or '-'.",
         call. = FALSE)
  }
  invisible(id)
}

check_scalar <- function(x, test, arg) {
  if (is.null(x)) return(NULL)
  if (!test(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single non-missing value.", call. = FALSE)
  }
  x
}

check_px <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    stop("`", arg, "` must be a single number, 0 or more.", call. = FALSE)
  }
  as.numeric(x)
}

as_color <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    return(list(column = x))
  }
  if (is.numeric(x) && length(x) == 4L && !anyNA(x) && all(x >= 0 & x <= 255) &&
      all(x == round(x))) {
    return(as.integer(x))
  }
  stop("`", arg, "` must be c(r, g, b, a) with integers 0 to 255, or a column name.",
       call. = FALSE)
}
