# A structural check of a scene against scene spec 0.1, with no node or
# JSON Schema validator. It covers the rules the producers here can break:
# the top-level shape, the view, data references (format, exactly one of
# blob or url, a native GeoArrow geometry encoding) and vector layers (known
# keys, ids, data that resolves, a kind that can draw the encoding). The
# full check is allboa/scenespec scripts/validate.js; the PR says where it
# was run. Returns a character vector of problems, empty when valid.
scene_spec_problems <- function(x) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  id_ok <- function(id) {
    is.character(id) && length(id) == 1L && grepl("^[A-Za-z][A-Za-z0-9_.-]*$", id) &&
      nchar(id) <= 128L
  }
  crs_ok <- function(crs) {
    (is.character(crs) && length(crs) == 1L &&
       grepl("^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$", crs)) ||
      (is.list(crs) && is.character(crs$type))
  }
  color_ok <- function(col) {
    if (is.list(col)) return(identical(names(col), "column") && is.character(col$column))
    is.numeric(col) && length(col) == 4L && all(col == round(col)) && all(col >= 0 & col <= 255)
  }
  kinds <- list(
    point = c("geoarrow.point", "geoarrow.multipoint"),
    path = c("geoarrow.linestring", "geoarrow.multilinestring"),
    polygon = c("geoarrow.polygon", "geoarrow.multipolygon")
  )
  layer_keys <- list(
    point = c("fill", "stroke", "stroke_width_px", "radius_px"),
    path = c("stroke", "stroke_width_px"),
    polygon = c("fill", "stroke", "stroke_width_px")
  )

  extra <- setdiff(names(x), c("$schema", "version", "view", "data", "layers"))
  if (length(extra)) add("unknown top-level keys: ", paste(extra, collapse = ", "))
  missing <- setdiff(c("version", "view", "data", "layers"), names(x))
  if (length(missing)) return(c(p, paste0("missing: ", paste(missing, collapse = ", "))))
  if (!identical(x$version, "0.1")) add("version must be \"0.1\"")

  v <- x$view
  if (!is.list(v) || !isTRUE(v$type %in% c("projected", "cartesian", "globe"))) {
    add("view.type must be projected, cartesian or globe")
  } else {
    if (length(setdiff(names(v), c("type", "crs", "center", "extent", "local_origin")))) {
      add("unknown view keys")
    }
    if (v$type %in% c("projected", "globe") && is.null(v$crs)) add("view.crs is required")
    if (!is.null(v$crs) && !crs_ok(v$crs)) add("view.crs is malformed")
  }

  if (!is.list(x$data) || (length(x$data) && is.null(names(x$data)))) {
    add("data must be an object keyed by id")
  }
  for (id in names(x$data)) {
    d <- x$data[[id]]
    if (!id_ok(id)) add("data id \"", id, "\" is malformed")
    if (length(setdiff(names(d), c("format", "blob", "url", "geometry", "origin_subtracted")))) {
      add("data ", id, ": unknown keys")
    }
    if (!isTRUE(d$format %in% c("arrow-ipc-stream", "arrow-ipc-file"))) add("data ", id, ": bad format")
    if (is.null(d$blob) == is.null(d$url)) add("data ", id, ": needs exactly one of blob or url")
    if (!is.null(d$blob) && !(is.character(d$blob) && nzchar(d$blob))) add("data ", id, ": bad blob")
    if (!is.null(d$geometry)) {
      g <- d$geometry
      if (length(setdiff(names(g), c("column", "encoding", "crs")))) add("data ", id, ": unknown geometry keys")
      if (!(is.character(g$column) && nzchar(g$column))) add("data ", id, ": geometry.column missing")
      if (!isTRUE(g$encoding %in% unlist(kinds))) add("data ", id, ": geometry.encoding not native GeoArrow")
      if (!is.null(g$crs) && !identical(g$crs, v$crs)) add("data ", id, ": geometry.crs differs from view.crs")
    }
  }

  if (!is.list(x$layers) || !is.null(names(x$layers))) add("layers must be an array")
  seen <- character()
  for (i in seq_along(x$layers)) {
    l <- x$layers[[i]]
    where <- paste0("layer ", i, ": ")
    if (!id_ok(l$id)) add(where, "bad id")
    if (isTRUE(l$id %in% seen)) add(where, "duplicate id")
    seen <- c(seen, l$id)
    if (!isTRUE(l$kind %in% names(kinds))) {
      add(where, "kind must be point, path or polygon")
      next
    }
    allowed <- c("id", "kind", "label", "visible", "data", layer_keys[[l$kind]])
    extra <- setdiff(names(l), allowed)
    if (length(extra)) add(where, "keys not allowed: ", paste(extra, collapse = ", "))
    d <- x$data[[l$data]]
    if (is.null(d)) {
      add(where, "data \"", l$data, "\" is not defined")
    } else if (!isTRUE(d$geometry$encoding %in% kinds[[l$kind]])) {
      add(where, l$kind, " cannot draw ", d$geometry$encoding)
    }
    for (k in intersect(c("fill", "stroke"), names(l))) {
      if (!color_ok(l[[k]])) add(where, k, " is not a color")
    }
    for (k in intersect(c("stroke_width_px", "radius_px"), names(l))) {
      if (!(is.numeric(l[[k]]) && length(l[[k]]) == 1L && l[[k]] >= 0)) add(where, k, " is bad")
    }
    if (!is.null(l$label) && !(is.character(l$label) && length(l$label) == 1L)) add(where, "bad label")
    if (!is.null(l$visible) && !(is.logical(l$visible) && length(l$visible) == 1L)) add(where, "bad visible")
  }
  p
}

# Check the scene both as an R list and, when jsonlite is available, as the
# JSON scene_json() writes, parsed back without simplification.
expect_valid_scene <- function(s) {
  expect_identical(scene_spec_problems(unclass(s)), character())
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    parsed <- jsonlite::fromJSON(scene_json(s), simplifyVector = TRUE,
                                 simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_identical(scene_spec_problems(parsed), character())
  }
}

# The extension name and coordinate layout of a stream's geometry column.
geometry_info <- function(schema) {
  for (child in schema$children) {
    ext <- child$metadata[["ARROW:extension:name"]]
    if (!is.null(ext)) {
      node <- child
      while (length(node$children) && !startsWith(node$format, "+w:")) {
        node <- node$children[[1]]
      }
      return(list(column = child$name, ext = ext, coords = node$format))
    }
  }
  NULL
}
