#' Native GeoArrow stream from any wk-handleable input
#'
#' Converts geometry to a native, interleaved GeoArrow column (never WKB)
#' and returns it as a 'nanoarrow' array stream, with coordinates in the
#' view CRS `crs`. This is the producer every other vector path ends in.
#'
#' `x` can be anything 'wk' can handle (an `sfc`, [wk::wkb()], [wk::wkt()],
#' [wk::xy()], a 'geoarrow' vector, ...), a data frame with a geometry column
#' (including an `sf` object), or a 'nanoarrow' array stream such as the WKB
#' stream from a GDAL layer. For a data frame or stream, the other columns
#' pass through as attributes.
#'
#' The core does not reproject: coordinates must already be in `crs`, as
#' scene spec 0.1 requires. The CRS of `x` (from [wk::wk_crs()]) is compared
#' with `crs` and a mismatch is an error. Input with no CRS is taken to be in
#' `crs`. When 'gdalraster' is installed it is used to recognise equivalent
#' definitions (for example an `sf` CRS and `"EPSG:3031"`). To reproject,
#' use [gdal_vector_stream()], which reprojects, clips and densifies in GDAL,
#' or reproject before calling this (for example with `sf::st_transform()`).
#'
#' Z and M values are dropped. Single and multi geometries of one kind are
#' promoted to the multi type. Mixed kinds (points with lines, for example)
#' and geometry collections cannot be encoded as native GeoArrow and are an
#' error.
#'
#' Input with no geometries (for example, a clip that removed everything)
#' gives an empty stream of the type the input declares: an `sfc` class
#' such as `sfc_MULTIPOLYGON`, or a 'geoarrow' vector's type. Empty input
#' that declares no point, line or polygon type (an empty [wk::wkb()], say)
#' is an error, since no native type can be chosen.
#'
#' @param x Geometry: a wk-handleable object, a data frame with a geometry
#'   column, or a 'nanoarrow' array stream.
#' @param crs The view CRS, such as `"EPSG:3031"`, or any definition
#'   [scene_crs()] accepts. It is written to the column's GeoArrow metadata.
#' @param geometry For a data frame or stream, the name of the geometry
#'   column. By default the `sf` geometry column, or the first column 'wk'
#'   can handle.
#' @return A `nanoarrow_array_stream` whose geometry column has a native
#'   GeoArrow extension type (such as `geoarrow.linestring`) with interleaved
#'   coordinates.
#' @seealso [vector_ipc()] to write the stream as Arrow IPC bytes.
#' @export
#' @examples
#' x <- wk::wkt("LINESTRING (0 0, 1000 1000)", crs = "EPSG:3031")
#' s <- vector_stream(x, "EPSG:3031")
#' s$get_schema()$children$geometry$metadata[["ARROW:extension:name"]]
#'
#' df <- data.frame(name = "a")
#' df$geometry <- x
#' as.data.frame(vector_stream(df, "EPSG:3031"))
vector_stream <- function(x, crs, geometry = NULL) {
  crs <- scene_crs(crs)
  native_stream(x, crs, geometry = geometry, check_crs = TRUE)
}

#' Arrow IPC bytes from a native GeoArrow stream
#'
#' Writes a stream with a native GeoArrow geometry column to Arrow IPC
#' stream bytes, the form a scene carries as a data blob. WKB geometry is
#' refused: convert it first with [vector_stream()].
#'
#' The coordinates may be interleaved (what [vector_stream()] and
#' [gdal_vector_stream()] write) or separated (a struct of `x`, `y` and
#' optionally `z`), as scene spec's explicit-data contract allows; both are
#' doubles under 32-bit list offsets. Other layouts (M coordinates, large
#' lists) are refused: convert them with [vector_stream()].
#'
#' @param x A 'nanoarrow' array stream (or anything
#'   [nanoarrow::as_nanoarrow_array_stream()] accepts) with a native
#'   GeoArrow geometry column, such as the result of [vector_stream()] or
#'   [gdal_vector_stream()].
#' @return A raw vector of Arrow IPC stream bytes.
#' @export
#' @examples
#' x <- wk::wkt("POINT (1 2)", crs = "EPSG:3031")
#' bytes <- vector_ipc(vector_stream(x, "EPSG:3031"))
#' length(bytes)
vector_ipc <- function(x) {
  stream <- nanoarrow::as_nanoarrow_array_stream(x)
  geom <- geometry_field(stream$get_schema())
  if (is.null(geom)) {
    stream$release()
    stop("`x` has no native GeoArrow geometry column; convert it with vector_stream().",
         call. = FALSE)
  }
  if (!geom$native) {
    stream$release()
    stop("Geometry column \"", geom$column, "\" is ", geom$encoding,
         ", not native GeoArrow; convert it with vector_stream().", call. = FALSE)
  }
  if (is.null(geom$layout)) {
    stream$release()
    stop("Geometry column \"", geom$column, "\" does not have interleaved (xy, xyz) or ",
         "separated (x, y, z) double coordinates under 32-bit lists; ",
         "convert it with vector_stream().", call. = FALSE)
  }
  ipc_bytes(undictionary(stream))
}

## ---- internals -------------------------------------------------------------

native_encodings <- c(
  "geoarrow.point", "geoarrow.linestring", "geoarrow.polygon",
  "geoarrow.multipoint", "geoarrow.multilinestring", "geoarrow.multipolygon"
)

crs_pattern <- "^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$"

## The IPC writer has no dictionary types, and a factor column becomes one.
## Factors are written as character: each row keeps its label, and a
## legend that lists the levels in their order is built from the levels,
## not from the data (#25).
unfactor <- function(df) {
  fac <- vapply(df, is.factor, TRUE)
  df[fac] <- lapply(df[fac], as.character)
  df
}

## A stream with a dictionary-encoded column is read into R (where the
## column is a factor) and written back with that column as character.
undictionary <- function(stream) {
  dict <- vapply(stream$get_schema()$children, function(ch) !is.null(ch$dictionary), TRUE)
  if (!any(dict)) return(stream)
  nanoarrow::as_nanoarrow_array_stream(unfactor(stream_data_frame(stream)))
}

ipc_bytes <- function(stream) {
  con <- rawConnection(raw(), open = "wb")
  on.exit(close(con))
  nanoarrow::write_nanoarrow(stream, con)
  rawConnectionValue(con)
}

## Find the geometry field of a schema: the first child with a geoarrow
## extension name. Returns NULL when there is none, otherwise the column
## name, the extension name, whether it is native, and its coordinate
## layout ("interleaved", "separated", or NULL when it is not native or
## not laid out as the explicit-data contract allows).
geometry_field <- function(schema) {
  for (child in schema$children) {
    ext <- child$metadata[["ARROW:extension:name"]]
    if (!is.null(ext) && startsWith(ext, "geoarrow.")) {
      native <- ext %in% native_encodings
      layout <- if (native) coord_layout(child, ext)
      return(list(
        column = child$name,
        encoding = ext,
        native = native,
        layout = layout,
        interleaved = identical(layout, "interleaved")
      ))
    }
  }
  NULL
}

## Native GeoArrow nests 32-bit lists ("+l") down to the coordinates, as
## many levels as the type has: interleaved is a fixed-size list of doubles
## whose child is named xy or xyz, separated is a struct of doubles x, y and
## optionally z (the explicit-data contract's two layouts; no M).
coord_layout <- function(field, ext) {
  levels <- c("geoarrow.point" = 0L, "geoarrow.linestring" = 1L, "geoarrow.multipoint" = 1L,
              "geoarrow.polygon" = 2L, "geoarrow.multilinestring" = 2L,
              "geoarrow.multipolygon" = 3L)[[ext]]
  for (k in seq_len(levels)) {
    if (!identical(field$format, "+l") || length(field$children) != 1L) return(NULL)
    field <- field$children[[1]]
  }
  kids <- field$children
  doubles <- length(kids) && all(vapply(kids, function(k) identical(k$format, "g"), TRUE))
  if (!doubles) return(NULL)
  if (field$format %in% c("+w:2", "+w:3") && length(kids) == 1L &&
      identical(names(kids), c("+w:2" = "xy", "+w:3" = "xyz")[[field$format]])) {
    return("interleaved")
  }
  if (identical(field$format, "+s") &&
      (identical(names(kids), c("x", "y")) || identical(names(kids), c("x", "y", "z")))) {
    return("separated")
  }
  NULL
}

## `type` is the geometry type the source declares (such as
## "MULTILINESTRING"), used only when there are no geometries to infer it.
native_stream <- function(x, crs, geometry = NULL, check_crs = TRUE, type = NULL) {
  if (inherits(x, "nanoarrow_array_stream")) {
    x <- stream_data_frame(x)
  }
  if (is.data.frame(x)) {
    col <- geometry %||% geometry_column(x)
    if (!col %in% names(x)) {
      stop("Geometry column \"", col, "\" is not in `x`.", call. = FALSE)
    }
    geom <- x[[col]]
    attrs <- x
    class(attrs) <- "data.frame"
    attrs[[col]] <- NULL
  } else {
    col <- "geometry"
    geom <- x
    attrs <- NULL
  }
  if (!wk::is_handleable(geom)) {
    stop("The geometry of `x` cannot be read by wk (class ",
         paste(class(geom), collapse = "/"), ").", call. = FALSE)
  }
  if (check_crs) check_same_crs(wk::wk_crs(geom), crs)

  native <- as_native_vctr(geom, crs, type)
  if (is.null(attrs)) {
    out <- data.frame(row.names = seq_along(native))
  } else {
    out <- unfactor(attrs)
    rownames(out) <- NULL
  }
  out[[col]] <- native
  nanoarrow::as_nanoarrow_array_stream(out)
}

## A stream read into R. Geometry arrives as a geoarrow vector when the
## field carries a geoarrow extension (geoarrow is loaded as an import); a
## plain "ogc.wkb" field from older GDAL arrives as a list of raw and is
## marked as WKB here.
stream_data_frame <- function(stream) {
  schema <- stream$get_schema()
  df <- as.data.frame(stream)
  for (child in schema$children) {
    ext <- child$metadata[["ARROW:extension:name"]]
    if (identical(ext, "ogc.wkb") && is.list(df[[child$name]])) {
      df[[child$name]] <- wk::wkb(unclass(df[[child$name]]))
    }
  }
  df
}

geometry_column <- function(x) {
  sf_col <- attr(x, "sf_column")
  if (is.character(sf_col) && length(sf_col) == 1L && sf_col %in% names(x)) {
    return(sf_col)
  }
  for (nm in names(x)) {
    if (wk::is_handleable(x[[nm]])) return(nm)
  }
  stop("`x` has no geometry column that wk can read.", call. = FALSE)
}

as_native_vctr <- function(geom, crs, type = NULL) {
  crs <- as.character(crs)
  declared <- tryCatch(wk::wk_vector_meta(geom)$geometry_type, error = function(e) 0L)
  g <- wk::as_wkb(geom)
  g <- wk::wk_set_crs(g, NULL)
  g <- wk::wk_drop_m(wk::wk_drop_z(g))
  if (length(g) == 0L) {
    ## Nothing to infer from: an empty column of the declared type.
    names <- c("POINT", "LINESTRING", "POLYGON", "MULTIPOINT",
               "MULTILINESTRING", "MULTIPOLYGON")
    if (isTRUE(declared %in% 1:6)) type <- names[declared]
    if (is.null(type) || !type %in% names) {
      stop("`x` has no geometries and declares no point, line or polygon type, ",
           "so a native GeoArrow type cannot be chosen.", call. = FALSE)
    }
    schema <- geoarrow::na_extension_geoarrow(type, crs = crs, coord_type = "INTERLEAVED")
    return(geoarrow::as_geoarrow_vctr(wk::wk_set_crs(g, crs), schema = schema))
  }
  g <- wk::wk_set_crs(g, crs)
  schema <- geoarrow::infer_geoarrow_schema(g, coord_type = "INTERLEAVED")
  ext <- schema$metadata[["ARROW:extension:name"]]
  if (!ext %in% native_encodings) {
    types <- unique(wk::wk_meta(g)$geometry_type)
    names <- c("point", "linestring", "polygon", "multipoint",
               "multilinestring", "multipolygon", "geometrycollection")
    stop("Geometry has no native GeoArrow encoding (types: ",
         paste(names[types], collapse = ", "),
         "). Split mixed types into separate layers and explode collections.",
         call. = FALSE)
  }
  geoarrow::as_geoarrow_vctr(g, schema = schema)
}

check_same_crs <- function(src, crs) {
  if (inherits(src, "aob_json")) src <- as.character(src)
  if (is.null(src) || identical(src, wk::wk_crs_inherit())) return(invisible(TRUE))
  if (isTRUE(wk::wk_crs_equal(src, as.character(crs)))) return(invisible(TRUE))
  def <- tryCatch(wk::wk_crs_proj_definition(src), error = function(e) NULL)
  if (is.character(def) && length(def) == 1L && !is.na(def)) {
    if (identical(toupper(def), toupper(as.character(crs)))) return(invisible(TRUE))
    if (isTRUE(gdal_same_crs(def, crs))) return(invisible(TRUE))
  } else {
    def <- "(unrecognised)"
  }
  stop("`x` has CRS ", crs_short(def), " but the view CRS is ", crs_label(crs), ". ",
       "The core does not reproject: transform `x` first ",
       "(for example sf::st_transform()), or use gdal_vector_stream().",
       call. = FALSE)
}

## TRUE when gdalraster can show the two definitions are the same CRS; NA
## when it cannot tell (not installed, or a definition it cannot parse).
gdal_same_crs <- function(a, b) {
  if (!requireNamespace("gdalraster", quietly = TRUE)) return(NA)
  tryCatch(
    gdalraster::srs_is_same(
      gdalraster::srs_to_wkt(as.character(a)), gdalraster::srs_to_wkt(as.character(b)),
      criterion = "EQUIVALENT_EXCEPT_AXIS_ORDER_GEOGCRS"
    ),
    error = function(e) NA
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' A per-row colour column
#'
#' Builds the Arrow array a layer's colour column holds: one `c(r, g, b, a)`
#' per row as a `FixedSizeList<uint8, 4>`. Add it to a stream beside the
#' geometry column (for example as a column of a 'nanoarrow' struct array)
#' and name that column in [scene_add_layer()]'s `stroke` or `fill`.
#'
#' @param m A matrix (or data frame) of whole numbers 0 to 255 with one row
#'   per feature and 4 columns (red, green, blue, alpha) or 3 (alpha is then
#'   255).
#' @return A `nanoarrow_array` of type `fixed_size_list<uint8, 4>` with
#'   `nrow(m)` elements and no nulls.
#' @export
#' @examples
#' m <- rbind(c(255, 0, 0, 255), c(0, 0, 255, 128))
#' a <- rgba_array(m)
#' a$length
#' nanoarrow::infer_nanoarrow_schema(a)$format
rgba_array <- function(m) {
  if (is.data.frame(m)) m <- as.matrix(m)
  if (!is.matrix(m) || !is.numeric(m) || !ncol(m) %in% 3:4) {
    stop("`m` must be a numeric matrix with 3 or 4 columns (r, g, b and optionally a).",
         call. = FALSE)
  }
  if (anyNA(m) || any(m < 0 | m > 255 | m != round(m))) {
    stop("`m` must hold whole numbers 0 to 255.", call. = FALSE)
  }
  if (ncol(m) == 3L) m <- cbind(m, 255L)
  child <- prim_array(nanoarrow::na_uint8(), as.raw(t(m)), length(m))
  nanoarrow::nanoarrow_array_modify(
    nanoarrow::nanoarrow_array_init(nanoarrow::na_fixed_size_list(nanoarrow::na_uint8(), 4L)),
    list(length = nrow(m), null_count = 0L, children = list(child))
  )
}
