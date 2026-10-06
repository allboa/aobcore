#' Add a legend to a scene
#'
#' Adds a key to the colours of one layer (scene spec 0.5): a continuous
#' ramp or discrete classes, with an optional entry for missing values.
#' Legends are data: write the same colours the layer was drawn with, so the
#' key and the drawing agree. The renderer shows legends in the order they
#' were added, while their layer is shown. A layer may have more than one
#' legend. A legend makes the scene scene spec 0.5.
#'
#' **Ramps.** `ramp` is `list(range = c(low, high), palette = name)` or
#' `list(range = c(low, high), stops = ..., at = ...)`. The ends of `range`
#' must differ; a reversed range (`low > high`) is a reversed key. A
#' `palette` ramp keys only a layer that has a palette (a palette raster or
#' tiled raster), and must name the same palette and range as the layer.
#' Vector layers and colour images (`rgb`) are keyed with `stops` or
#' `classes`. `stops` are colours at positions along the ramp: an n x 4
#' matrix of RGBA rows (0 to 255), a list of `c(r, g, b, a)` colours, or a
#' list of `list(at = , color = )` as in the spec. `at` (positions from 0 at
#' `range[1]` to 1 at `range[2]`, strictly increasing) defaults to evenly
#' spaced. Colours are interpolated linearly between stops.
#'
#' **Default.** With neither `ramp` nor `classes`, a layer with a palette
#' gets a palette ramp copied from the layer, so
#' `scene_add_legend(s, "sst", "SST (degrees C)")` keys a palette raster.
#'
#' **Palette layers in a 0.5 scene.** For scenes before 0.5 the bundled
#' renderer draws a ramp for each palette raster on its own. A 0.5 scene's
#' legends are only those in `scene$legends`, so once a scene is 0.5 (any
#' legend or popup) that automatic ramp is no longer drawn: add
#' `scene_add_legend()` for each palette layer that should keep its key.
#'
#' @param scene A scene from [scene()].
#' @param layer The id of the layer the legend keys; it must be in the scene.
#' @param title Optional heading, such as the variable and its units. A
#'   renderer may use the layer's label when it is absent.
#' @param ramp A continuous key (see Ramps), or `NULL`.
#' @param classes Discrete entries, shown in order: a list of
#'   `list(label = , color = )`, or a named list of colours
#'   (`list(Protected = c(27, 158, 119, 160), Open = c(117, 112, 179, 160))`).
#'   Give exactly one of `ramp` and `classes`.
#' @param na Optional entry for missing values, shown apart: a colour
#'   `c(r, g, b, a)` (labelled `"no data"`) or `list(label = , color = )`.
#' @return The scene, now version 0.5, with the legend appended to
#'   `scene$legends`.
#' @seealso [scene_add_layer()] for popups, [scene_spec_version()].
#' @export
#' @examples
#' # A palette ramp for the probe's SST raster, copied from the layer.
#' p <- scene_add_legend(probe_scene(), "sst", "SST (degrees C)",
#'                       na = list(label = "no data", color = c(0, 0, 0, 0)))
#' p$legends[[1]]$ramp
#'
#' # Classes for the land polygons.
#' p <- scene_add_legend(p, "land", classes = list(Land = c(218, 213, 202, 255)))
#'
#' # A ramp given as colour stops, for a layer coloured by a column.
#' x <- wk::wkt("POINT (0 0)", crs = "EPSG:3031")
#' s <- scene_add_vector(scene(), "stations", x)
#' s <- scene_add_legend(s, "stations", "Depth (m)",
#'                       ramp = list(range = c(0, 4000),
#'                                   stops = rbind(c(255, 255, 204, 255), c(8, 29, 88, 255))))
#' scene_spec_version(s)
scene_add_legend <- function(scene, layer, title = NULL, ramp = NULL, classes = NULL, na = NULL) {
  check_scene(scene)
  check_id(layer, "layer")
  ids <- vapply(scene$layers, function(l) l$id, "")
  if (!layer %in% ids) stop("Layer id \"", layer, "\" is not in the scene.", call. = FALSE)
  L <- scene$layers[[match(layer, ids)]]
  if (!is.null(ramp) && !is.null(classes)) {
    stop("Give one of `ramp` and `classes`, not both.", call. = FALSE)
  }
  if (is.null(ramp) && is.null(classes)) {
    if (is.null(L$palette)) {
      stop("Layer \"", layer, "\" has no palette; give `ramp` (with stops) or `classes`.",
           call. = FALSE)
    }
    ramp <- list(range = L$palette$range, palette = L$palette$name)
  }
  if (!is.null(title) && (!is.character(title) || length(title) != 1L || is.na(title) ||
                          !nzchar(title))) {
    stop("`title` must be a single non-empty string.", call. = FALSE)
  }
  legend <- list(
    layer = layer,
    title = title,
    ramp = if (!is.null(ramp)) legend_ramp(ramp, L),
    classes = if (!is.null(classes)) legend_classes(classes),
    na = if (!is.null(na)) legend_na(na)
  )
  scene$legends <- c(scene$legends, list(drop_null(legend)))
  as_spec_05(scene)
}

## ---- internals -------------------------------------------------------------

legend_ramp <- function(ramp, L) {
  if (!is.list(ramp) || is.null(names(ramp)) || any(!nzchar(names(ramp)))) {
    stop("`ramp` must be a named list: list(range = , palette = ) or list(range = , stops = ).",
         call. = FALSE)
  }
  extra <- setdiff(names(ramp), c("range", "palette", "stops", "at"))
  if (length(extra)) {
    stop("`ramp` has unknown entries: ", paste(extra, collapse = ", "), ".", call. = FALSE)
  }
  range <- ramp$range
  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) {
    stop("`ramp$range` must be c(low, high), two finite numbers.", call. = FALSE)
  }
  if (range[1] == range[2]) stop("`ramp$range` ends must differ.", call. = FALSE)
  range <- as.numeric(range)
  if (is.null(ramp$palette) == is.null(ramp$stops)) {
    stop("`ramp` needs exactly one of `palette` and `stops`.", call. = FALSE)
  }
  if (!is.null(ramp$palette)) {
    pal <- ramp$palette
    if (!is.character(pal) || length(pal) != 1L || is.na(pal) || !nzchar(pal)) {
      stop("`ramp$palette` must be a single palette name.", call. = FALSE)
    }
    if (!is.null(ramp$at)) stop("`ramp$at` goes with `stops`, not `palette`.", call. = FALSE)
    if (is.null(L$palette)) {
      stop("Layer \"", L$id, "\" has no palette; key it with `stops` or `classes`.", call. = FALSE)
    }
    if (!identical(pal, L$palette$name) || !isTRUE(all(range == L$palette$range))) {
      stop("A palette ramp must name the palette and range of layer \"", L$id, "\" (",
           L$palette$name, ", ", paste(format(L$palette$range), collapse = " to "), ").",
           call. = FALSE)
    }
    return(list(range = range, palette = pal))
  }
  list(range = range, stops = legend_stops(ramp$stops, ramp$at))
}

legend_stops <- function(stops, at = NULL) {
  if (is.matrix(stops)) {
    if (ncol(stops) != 4L) stop("A `stops` matrix needs 4 columns (r, g, b, a).", call. = FALSE)
    cols <- lapply(seq_len(nrow(stops)), function(i) stops[i, ])
  } else if (is.list(stops) && length(stops) && all(vapply(stops, is.list, TRUE))) {
    if (!is.null(at)) {
      stop("Give stop positions in `stops` (list(at = , color = )) or in `at`, not both.",
           call. = FALSE)
    }
    at <- vapply(stops, function(s) as.numeric(s$at %||% NA_real_)[1], 0)
    cols <- lapply(stops, function(s) s$color)
  } else if (is.list(stops)) {
    cols <- stops
  } else {
    stop("`stops` must be an n x 4 matrix of RGBA rows, a list of colours, or a list of ",
         "list(at = , color = ).", call. = FALSE)
  }
  n <- length(cols)
  if (n < 2L) stop("A ramp needs at least two stops.", call. = FALSE)
  at <- at %||% seq(0, 1, length.out = n)
  if (!is.numeric(at) || length(at) != n || anyNA(at)) {
    stop("`at` must give one position for each of the ", n, " stops.", call. = FALSE)
  }
  if (at[1] != 0 || at[n] != 1) stop("The first stop must be at 0 and the last at 1.", call. = FALSE)
  if (any(diff(at) <= 0)) stop("Stop positions must increase strictly.", call. = FALSE)
  unname(Map(function(a, col) list(at = as.numeric(a), color = as_rgba(col, "a stop colour")),
             at, cols))
}

legend_classes <- function(classes) {
  if (!is.list(classes) || !length(classes)) {
    stop("`classes` must be a non-empty list.", call. = FALSE)
  }
  if (all(vapply(classes, is.list, TRUE))) {
    return(unname(lapply(classes, legend_class, what = "Each class")))
  }
  nms <- names(classes)
  if (is.null(nms) || anyNA(nms)) {
    stop("`classes` must be a list of list(label = , color = ) or a named list of colours.",
         call. = FALSE)
  }
  unname(Map(function(label, col) list(label = label, color = as_rgba(col, "a class colour")),
             nms, classes))
}

legend_class <- function(x, what) {
  if (!is.list(x) || !setequal(names(x), c("label", "color"))) {
    stop(what, " must be list(label = , color = ).", call. = FALSE)
  }
  if (!is.character(x$label) || length(x$label) != 1L || is.na(x$label)) {
    stop(what, " needs a single string `label`.", call. = FALSE)
  }
  list(label = x$label, color = as_rgba(x$color, "a legend colour"))
}

legend_na <- function(na) {
  if (is.numeric(na)) return(list(label = "no data", color = as_rgba(na, "`na`")))
  legend_class(na, "`na`")
}

as_rgba <- function(x, what) {
  if (is.numeric(x) && length(x) == 4L && !anyNA(x) && all(x >= 0 & x <= 255) &&
      all(x == round(x))) {
    return(as.integer(x))
  }
  stop("Expected ", what, " as c(r, g, b, a) with integers 0 to 255.", call. = FALSE)
}

## A layer popup from scene_add_layer()'s `popup` argument, checked against
## the layer's data: the columns exist and are not the geometry column.
as_popup <- function(popup, scene, ref) {
  if (is.null(popup)) return(NULL)
  trigger <- NULL
  if (is.list(popup)) {
    if (is.null(names(popup)) || length(setdiff(names(popup), c("columns", "trigger")))) {
      stop("`popup` must be column names or list(columns = , trigger = ).", call. = FALSE)
    }
    trigger <- popup$trigger
    popup <- popup$columns
  }
  if (!is.character(popup) || !length(popup) || anyNA(popup) || any(!nzchar(popup))) {
    stop("`popup` columns must be a character vector of column names.", call. = FALSE)
  }
  if (anyDuplicated(popup)) stop("`popup` names a column more than once.", call. = FALSE)
  if (!is.null(trigger) && (!is.character(trigger) || length(trigger) != 1L ||
                            !trigger %in% c("select", "point"))) {
    stop("`popup` trigger must be \"select\" or \"point\".", call. = FALSE)
  }
  geom <- ref$geometry$column
  if (geom %in% popup) {
    stop("\"", geom, "\" is the geometry column; a popup shows attribute columns.", call. = FALSE)
  }
  fields <- data_fields(scene, ref)
  if (!is.null(fields)) {
    have <- names(fields)
    miss <- setdiff(popup, have)
    if (length(miss)) {
      stop("Popup column", if (length(miss) > 1L) "s", " not in data \"", ref$blob, "\": ",
           paste(miss, collapse = ", "), ". Columns: ",
           paste(setdiff(have, geom), collapse = ", "), ".", call. = FALSE)
    }
    type <- vapply(fields[popup], attribute_type_problem, "")
    bad <- !is.na(type)
    if (any(bad)) {
      stop("Popup column", if (sum(bad) > 1L) "s", " ",
           paste0("\"", popup[bad], "\" (", type[bad], ")", collapse = ", "),
           " in data \"", ref$blob, "\" ", if (sum(bad) > 1L) "are" else "is",
           " not an attribute type a popup shows: boolean, integer (8 to 64 bits), ",
           "float32 or float64, string, date or timestamp.", call. = FALSE)
    }
  }
  drop_null(list(columns = as.list(popup), trigger = trigger))
}

## The fields (schemas, named by column) of a data reference's Arrow table,
## read from its blob's schema without reading its batches (NULL when the
## scene does not carry the blob).
data_fields <- function(scene, ref) {
  bytes <- if (!is.null(ref$blob)) attr(scene, "blobs")[[ref$blob]]
  if (is.null(bytes)) return(NULL)
  nanoarrow::read_nanoarrow(bytes)$get_schema()$children
}

## NA when a field is an attribute type of the explicit-data contract (what
## a popup shows as text: boolean, 8 to 64 bit integers, float32, float64,
## Utf8, LargeUtf8, Date32, Date64, timestamp of any unit and zone),
## otherwise its type's name. A dictionary is not an attribute.
attribute_type_problem <- function(field) {
  if (!is.null(field$dictionary)) return("dictionary")
  fmt <- field$format
  ok <- fmt %in% c("b", "c", "C", "s", "S", "i", "I", "l", "L", "f", "g", "u", "U", "tdD", "tdm") ||
    grepl("^ts[smun]:", fmt)
  if (ok) return(NA_character_)
  tryCatch(nanoarrow::nanoarrow_schema_parse(field)$type, error = function(e) fmt)
}

## Mark a scene as 0.5 after adding a legend or popup. 0.5 rejects a layer
## palette or rgb range with equal ends (it divides by zero).
as_spec_05 <- function(scene) {
  for (l in scene$layers) {
    for (k in c("palette", "rgb")) {
      r <- l[[k]]$range
      if (length(r) == 2L && isTRUE(r[1] == r[2])) {
        stop("Layer \"", l$id, "\" has a ", k, " range with equal ends, which scene spec 0.5 ",
             "(legends and popups) does not allow.", call. = FALSE)
      }
    }
  }
  scene$version <- scene_spec_version(scene)
  scene
}
