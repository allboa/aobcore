#' A view CRS from any CRS definition
#'
#' Turns a CRS definition into the form a scene carries. An
#' `"authority:code"` string (such as `"EPSG:3031"` or `"OGC:CRS84"`) is kept
#' as it is, unchecked (and needs no 'gdalraster'); an unknown code fails
#' later, where GDAL first needs it. Anything else GDAL can read (WKT 1 or 2, a PROJ string such as
#' `"+proj=laea +lat_0=-90"`, PROJJSON text, a file name, or an `sf` `crs`
#' object) is resolved with 'gdalraster': when it is exactly an
#' authority's CRS the code is used, otherwise the definition is carried as
#' a PROJJSON object, which the scene spec accepts wherever it takes a CRS.
#' [scene()], [cog_plan()], [view_cog()], [vector_stream()] and
#' [gdal_vector_stream()] all call this on their `crs` argument.
#'
#' @param crs A CRS definition: a single string, a number (an EPSG code), or
#'   an `sf` `crs` object.
#' @return An `"authority:code"` string, or PROJJSON text of class
#'   `"aob_json"` (written into the scene as a JSON object).
#' @export
#' @examples
#' scene_crs("EPSG:3031")
#' scene_crs(3031)
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' # Antarctic Lambert azimuthal equal area has no EPSG code.
#' laea <- scene_crs("+proj=laea +lat_0=-90 +lon_0=0 +datum=WGS84")
#' class(laea)
#' scene(laea)
scene_crs <- function(crs) {
  if (inherits(crs, "aob_json")) return(crs)
  if (inherits(crs, "crs") && is.list(crs)) crs <- crs$wkt %||% crs$input
  if (is.numeric(crs) && length(crs) == 1L && !is.na(crs) && crs == round(crs) && crs > 0) {
    crs <- paste0("EPSG:", format(crs, scientific = FALSE))
  }
  if (!is.character(crs) || length(crs) != 1L || is.na(crs) || !nzchar(trimws(crs))) {
    stop("`crs` must be a single CRS definition, such as \"EPSG:3031\", WKT or a PROJ string.",
         call. = FALSE)
  }
  if (grepl(crs_pattern, crs)) return(crs)
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("A CRS that is not an \"authority:code\" string needs the 'gdalraster' package.",
         call. = FALSE)
  }
  wkt <- tryCatch(gdalraster::srs_to_wkt(crs), error = function(e) "")
  if (!nzchar(wkt)) {
    stop("GDAL cannot read the CRS \"", crs_short(crs), "\".",
         if (grepl("^[0-9]+$", crs)) paste0(" For an EPSG code write \"EPSG:", crs, "\" or a number."),
         call. = FALSE)
  }
  crs_ref(wkt)
}

## ---- internals -------------------------------------------------------------

## WKT for GDAL from a scene CRS (a code or PROJJSON text).
crs_wkt <- function(crs) {
  wkt <- tryCatch(gdalraster::srs_to_wkt(as.character(crs)), error = function(e) "")
  if (!nzchar(wkt)) {
    stop("GDAL cannot resolve the CRS ", crs_label(crs), " (is its PROJ database installed?).",
         call. = FALSE)
  }
  wkt
}

## A short name for messages and print(): the code, or the PROJJSON "name".
crs_label <- function(crs) {
  if (is.null(crs)) return("(none)")
  if (!inherits(crs, "aob_json")) return(as.character(crs))
  first <- function(re) {
    m <- regmatches(crs, regexec(re, crs))[[1]]
    if (length(m) == 2L && nzchar(m[2]) && m[2] != "unknown") m[2] else NULL
  }
  nm <- first("\"name\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"") %||%
    first("\"method\"[[:space:]]*:[[:space:]]*[{][[:space:]]*\"name\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"")
  if (is.null(nm)) "(PROJJSON)" else paste0("\"", nm, "\" (PROJJSON)")
}

crs_short <- function(x) if (nchar(x) > 60) paste0(substr(x, 1, 57), "...") else x

## Are two scene CRSs the same? The same text, or the same CRS to GDAL.
crs_same <- function(a, b) {
  identical(as.character(a), as.character(b)) || isTRUE(gdal_same_crs(a, b))
}
