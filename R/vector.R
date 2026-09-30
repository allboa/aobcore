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
#' @param x A 'nanoarrow' array stream (or anything
#'   [nanoarrow::as_nanoarrow_array_stream()] accepts) with a native,
#'   interleaved GeoArrow geometry column, such as the result of
#'   [vector_stream()] or [gdal_vector_stream()].
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
  if (!geom$interleaved) {
    stream$release()
    stop("Geometry column \"", geom$column, "\" does not have interleaved coordinates; ",
         "convert it with vector_stream().", call. = FALSE)
  }
  ipc_bytes(stream)
}

## ---- internals -------------------------------------------------------------

native_encodings <- c(
  "geoarrow.point", "geoarrow.linestring", "geoarrow.polygon",
  "geoarrow.multipoint", "geoarrow.multilinestring", "geoarrow.multipolygon"
)

crs_pattern <- "^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$"

ipc_bytes <- function(stream) {
  con <- rawConnection(raw(), open = "wb")
  on.exit(close(con))
  nanoarrow::write_nanoarrow(stream, con)
  rawConnectionValue(con)
}

## Find the geometry field of a schema: the first child with a geoarrow
## extension name. Returns NULL when there is none, otherwise the column
## name, the extension name and whether it is native and interleaved.
geometry_field <- function(schema) {
  for (child in schema$children) {
    ext <- child$metadata[["ARROW:extension:name"]]
    if (!is.null(ext) && startsWith(ext, "geoarrow.")) {
      native <- ext %in% native_encodings
      return(list(
        column = child$name,
        encoding = ext,
        native = native,
        interleaved = native && is_interleaved(child)
      ))
    }
  }
  NULL
}

## Native GeoArrow nests lists down to the coordinates: a fixed-size list
## ("+w:n") is interleaved, a struct ("+s") is separated.
is_interleaved <- function(schema) {
  while (!is.null(schema)) {
    fmt <- schema$format
    if (startsWith(fmt, "+w:")) return(TRUE)
    if (identical(fmt, "+s")) return(FALSE)
    if (!startsWith(fmt, "+l") && !startsWith(fmt, "+L")) return(FALSE)
    schema <- schema$children[[1]]
  }
  FALSE
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
    out <- attrs
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
