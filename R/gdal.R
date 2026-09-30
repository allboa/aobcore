#' Native GeoArrow stream from a GDAL vector source
#'
#' Reads any vector source GDAL can open and returns a 'nanoarrow' array
#' stream with a native, interleaved GeoArrow geometry column, reprojected
#' to the view CRS `crs`. Clipping and densifying happen in the same GDAL
#' call, before reprojection, so curved edges are sampled in the source CRS.
#' Requires the 'gdalraster' package.
#'
#' This follows allboa/design decision 0002. With `route = "gdal"`,
#' `ogr2ogr()` writes the layer to GDAL's Arrow driver in `/vsimem` with
#' `GEOMETRY_ENCODING=GEOARROW_INTERLEAVED` and `-t_srs crs`, and
#' `GDALVector$getArrowStream()` streams it as native GeoArrow with no
#' conversion in R. With `route = "r"`, GDAL reprojects to an in-memory
#' GeoPackage, streams WKB, and 'geoarrow' converts it to native GeoArrow in
#' R (as in [vector_stream()]). `route = "auto"` uses `"gdal"` when GDAL has
#' the Arrow driver ([gdal_has_arrow()]) and `"r"` otherwise. GDAL can only
#' encode GeoArrow for a layer of one declared geometry type; for a layer
#' of mixed or unknown type (and for `-sql` in `options` without `-nlt`),
#' the Arrow driver writes WKB and it is converted in R, so the output is
#' never WKB. Pass `-nlt` in `options` to declare the type yourself.
#'
#' The stream is read fully into memory (as Arrow IPC bytes) and the
#' temporary `/vsimem` files are removed before this returns.
#'
#' @param dsn A data source GDAL can open: a file path, URL or `/vsi` path.
#' @param crs The view CRS as an `"authority:code"` string, such as
#'   `"EPSG:3031"`. Passed to `ogr2ogr -t_srs`.
#' @param layer Optional name of the source layer. By default the first.
#' @param clip Optional clip box `c(xmin, ymin, xmax, ymax)` in source CRS
#'   units, passed to `-clipsrc`.
#' @param densify Optional maximum segment length in source CRS units,
#'   passed to `-segmentize`, so edges curve correctly in the view CRS.
#' @param explode If `TRUE`, split multi-part geometries into single parts
#'   (`-explodecollections`).
#' @param options Further `ogr2ogr` command-line arguments, as a character
#'   vector (for example `c("-where", "scalerank < 2")`).
#' @param route `"auto"`, `"gdal"` or `"r"`: where the GeoArrow encoding
#'   is done (see Details).
#' @return A `nanoarrow_array_stream` whose geometry column (`geometry`) is
#'   native GeoArrow with interleaved coordinates in `crs`.
#' @seealso [vector_ipc()] to write the stream as Arrow IPC bytes.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' coast <- system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
#' s <- gdal_vector_stream(coast, "EPSG:3031", densify = 0.25)
#' s$get_schema()$children$geometry$metadata[["ARROW:extension:name"]]
gdal_vector_stream <- function(dsn, crs, layer = NULL, clip = NULL,
                               densify = NULL, explode = FALSE,
                               options = character(),
                               route = c("auto", "gdal", "r")) {
  route <- match.arg(route)
  check_crs_string(crs)
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("gdal_vector_stream() needs the 'gdalraster' package.", call. = FALSE)
  }
  if (!is.character(dsn) || length(dsn) != 1L || is.na(dsn)) {
    stop("`dsn` must be a single data source name.", call. = FALSE)
  }
  if (inherits(try(gdalraster::srs_to_wkt(crs), silent = TRUE), "try-error")) {
    stop("GDAL cannot resolve the CRS \"", crs, "\" (is its PROJ database installed?).",
         call. = FALSE)
  }
  has_arrow <- gdal_has_arrow()
  if (route == "auto") route <- if (has_arrow) "gdal" else "r"
  if (route == "gdal" && !has_arrow) {
    stop("This GDAL build has no Arrow driver; use route = \"r\" ",
         "(conda-forge users can install libgdal-arrow-parquet).", call. = FALSE)
  }

  args <- c("-t_srs", crs, "-nln", "data", "-lco", "GEOMETRY_NAME=geometry")
  if (!is.null(clip)) {
    if (!is.numeric(clip) || length(clip) != 4L || anyNA(clip)) {
      stop("`clip` must be c(xmin, ymin, xmax, ymax).", call. = FALSE)
    }
    args <- c(args, "-clipsrc", format(clip, digits = 15, trim = TRUE))
  }
  if (!is.null(densify)) {
    if (!is.numeric(densify) || length(densify) != 1L || !(densify > 0)) {
      stop("`densify` must be a single positive number.", call. = FALSE)
    }
    args <- c(args, "-segmentize", format(densify, digits = 15, trim = TRUE))
  }
  if (isTRUE(explode)) args <- c(args, "-explodecollections")

  dir <- paste0("/vsimem/", basename(tempfile("aobcore-")))
  lyr <- NULL
  on.exit({
    if (!is.null(lyr)) lyr$close()
    gdalraster::vsi_rmdir(dir, recursive = TRUE)
  }, add = TRUE)
  if (route == "gdal") {
    dst <- file.path(dir, "data.arrows")
    args <- c("-f", "Arrow", args,
              "-lco", "FORMAT=STREAM",
              "-lco", paste0("GEOMETRY_ENCODING=",
                             if (gdal_native_type(dsn, layer, options)) "GEOARROW_INTERLEAVED" else "WKB"),
              "-lco", "FID=")
  } else {
    dst <- file.path(dir, "data.gpkg")
    args <- c("-f", "GPKG", args)
  }
  gdalraster::vsi_mkdir(dir)
  ok <- gdalraster::ogr2ogr(dsn, dst, src_layers = layer, cl_arg = c(args, options))
  if (!isTRUE(ok)) stop("ogr2ogr failed for ", dsn, call. = FALSE)

  lyr <- gdalraster::GDALVector$new(dst)
  lyr$arrowStreamOptions <- c("GEOMETRY_METADATA_ENCODING=GEOARROW", "INCLUDE_FID=NO")
  stream <- lyr$getArrowStream()
  if (route == "gdal") {
    ## GDAL has already encoded native GeoArrow: straight to IPC bytes.
    bytes <- ipc_bytes(stream)
    lyr$releaseArrowStream()
    out <- nanoarrow::read_nanoarrow(bytes)
    geom <- geometry_field(out$get_schema())
    if (!is.null(geom) && geom$interleaved) return(out)
    ## The layer's geometry type is mixed or unknown, so GDAL wrote WKB.
    df <- stream_data_frame(out)
  } else {
    df <- stream_data_frame(stream)
    lyr$releaseArrowStream()
  }
  ## Coordinates are already in `crs` (GDAL reprojected them).
  native_stream(df, crs, geometry = "geometry", check_crs = FALSE)
}

#' Does GDAL have the Arrow driver?
#'
#' [gdal_vector_stream()] encodes native GeoArrow in GDAL when the Arrow
#' driver is present. Some GDAL builds leave it out (on conda-forge it is in
#' the separate `libgdal-arrow-parquet` package).
#'
#' @return `TRUE` when 'gdalraster' is installed and its GDAL has the Arrow
#'   vector driver, otherwise `FALSE`.
#' @export
#' @examples
#' gdal_has_arrow()
gdal_has_arrow <- function() {
  if (!requireNamespace("gdalraster", quietly = TRUE)) return(FALSE)
  fmts <- tryCatch(gdalraster::gdal_formats(), error = function(e) NULL)
  !is.null(fmts) && "Arrow" %in% fmts$short_name[fmts$vector]
}

## Can the Arrow driver encode this layer as GeoArrow? Only when the layer
## declares one point, line or polygon type (single or multi). A `-nlt` in
## `options` declares it; `-sql` makes it unknowable here.
gdal_native_type <- function(dsn, layer, options) {
  if ("-nlt" %in% options) return(TRUE)
  if ("-sql" %in% options) return(FALSE)
  lyr <- if (is.null(layer)) {
    gdalraster::GDALVector$new(dsn)
  } else {
    gdalraster::GDALVector$new(dsn, layer)
  }
  on.exit(lyr$close())
  type <- toupper(gsub("[^A-Za-z]", "", lyr$getGeomType()))
  type <- sub("(ZM|Z|M|D)$", "", type)
  type %in% c("POINT", "LINESTRING", "POLYGON", "MULTIPOINT",
              "MULTILINESTRING", "MULTIPOLYGON")
}
