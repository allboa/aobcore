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

## ---- the explicit-data contract's CRS rule ------------------------------
## (scene spec README, "Explicit data"; the renderer applies the same rule
## in js/src/arrow.js.) A GeoArrow field's `ARROW:extension:metadata` gives
## `crs` as a PROJJSON object or an "authority:code" string. Two CRSs match
## when they are equal as JSON values (PROJJSON's "$schema" aside), or when
## one authority code names both (the string, or a PROJJSON object's
## top-level id or ids), compared case-insensitively.

## A scene CRS (a code, or PROJJSON text of class "aob_json") as a JSON value.
crs_value <- function(crs) {
  if (inherits(crs, "aob_json")) json_parse(crs) else as.character(crs)
}

## The upper-case "AUTHORITY:CODE" names of a CRS value.
crs_codes <- function(x) {
  if (is.character(x)) {
    return(if (length(x) == 1L && !is.na(x) && grepl(crs_pattern, x)) toupper(x) else character())
  }
  if (!is.list(x) || is.null(names(x))) return(character())
  ids <- x[["ids"]] %||% (if (!is.null(x[["id"]])) list(x[["id"]]))
  out <- vapply(ids, function(i) {
    if (!is.list(i) || is.null(i[["authority"]]) || is.null(i[["code"]])) return(NA_character_)
    code <- i[["code"]]
    if (is.numeric(code)) code <- format(code, scientific = FALSE, trim = TRUE, digits = 15)
    toupper(paste0(i[["authority"]], ":", code))
  }, "")
  out[!is.na(out)]
}

crs_values_match <- function(a, b) {
  bare <- function(x) if (is.list(x) && !is.null(names(x))) x[names(x) != "$schema"] else x
  if (json_same(bare(a), bare(b))) return(TRUE)
  any(crs_codes(a) %in% crs_codes(b))
}

## A CRS value for messages: its first code, or a PROJJSON object's name.
crs_value_label <- function(x) {
  if (is.null(x)) return("(none)")
  codes <- crs_codes(x)
  if (length(codes)) return(codes[1])
  if (is.character(x)) return(paste0("\"", crs_short(x), "\""))
  nm <- if (is.list(x)) x[["name"]]
  if (is.character(nm) && length(nm) == 1L) paste0("\"", nm, "\" (PROJJSON)") else "(PROJJSON)"
}

## The parsed `ARROW:extension:metadata` of a GeoArrow field: a named list,
## empty when the field has none.
field_extension_metadata <- function(field, what) {
  text <- field$metadata[["ARROW:extension:metadata"]]
  empty <- structure(list(), names = character())
  if (is.null(text) || !nzchar(text)) return(empty)
  m <- tryCatch(json_parse(text), error = function(e) NULL)
  if (!is.list(m) || (length(m) && is.null(names(m)))) {
    stop("The ARROW:extension:metadata of ", what, " is not a JSON object.", call. = FALSE)
  }
  if (!length(m)) empty else m
}

## Check a GeoArrow geometry field against the view CRS `crs` by the
## contract. Returns TRUE when its CRS matches, FALSE when it has none (the
## caller writes the view's, or stops); stops when the CRS is not the
## view's or its metadata breaks the contract. A view with no CRS needs none.
check_field_crs <- function(field, crs, what) {
  m <- field_extension_metadata(field, what)
  edges <- m[["edges"]]
  if (!is.null(edges) && !identical(edges, "planar")) {
    stop("The edges of ", what, " are ", format(edges), "; only planar edges are drawn.",
         call. = FALSE)
  }
  type <- m[["crs_type"]]
  value <- m[["crs"]]
  if (!is.null(type)) {
    if (!(is.character(type) && length(type) == 1L && type %in% c("projjson", "authority_code"))) {
      stop("The crs_type of ", what, " is ", format(type),
           ", not projjson or authority_code.", call. = FALSE)
    }
    if (!is.null(value) && type == "projjson" && !(is.list(value) && !is.null(names(value)))) {
      stop("The crs_type of ", what, " is projjson but its crs is not a JSON object.",
           call. = FALSE)
    }
    if (!is.null(value) && type == "authority_code" && !length(crs_codes(value))) {
      stop("The crs_type of ", what, " is authority_code but its crs is not an ",
           "\"authority:code\" string.", call. = FALSE)
    }
  }
  if (is.null(crs)) return(TRUE)
  if (is.null(value)) return(FALSE)
  if (!crs_values_match(value, crs_value(crs))) {
    stop("The CRS of ", what, " is ", crs_value_label(value), " but the view CRS is ",
         crs_label(crs), ". The core does not reproject: transform the data first ",
         "(for example with gdal_vector_stream() or sf::st_transform()). If they are the ",
         "same CRS written differently, pass the data through vector_stream() with the ",
         "view CRS, which writes the view's.",
         call. = FALSE)
  }
  TRUE
}
