#' Scene as scene spec JSON
#'
#' Writes a scene as a scene spec JSON document, as the lowest version that
#' can express it (0.1 to 0.5; see [scene_spec_version()]). The blobs are not
#' included; a transport delivers them beside the document (see
#' [scene_blobs()]). The output is ASCII: other characters are written as
#' `\uXXXX` escapes.
#'
#' @param scene A scene from [scene()].
#' @param pretty If `TRUE`, indent the output two spaces per level.
#' @return A single character string of JSON.
#' @export
#' @examples
#' x <- wk::wkt("LINESTRING (0 0, 1000 1000)", crs = "EPSG:3031")
#' s <- scene_add_vector(scene(), "line", x, stroke = c(60, 66, 72, 255))
#' cat(scene_json(s, pretty = TRUE))
scene_json <- function(scene, pretty = FALSE) {
  check_scene(scene)
  x <- unclass(scene)
  attr(x, "blobs") <- NULL
  x$version <- scene_spec_version(scene)
  json_value(x, if (isTRUE(pretty)) "" else NULL)
}

## A small JSON writer for the scene's plain-list shape: a named list is an
## object (an empty one when it has a names attribute), an unnamed list is
## an array, an atomic vector of length 1 is a scalar and any other length
## is an array. `indent` is NULL for compact output.
json_value <- function(x, indent = NULL) {
  if (is.null(x)) return("null")
  if (inherits(x, "aob_json")) return(json_verbatim(x))
  if (is.list(x)) {
    is_object <- !is.null(names(x))
    parts <- if (length(x) == 0L) {
      character()
    } else if (is_object) {
      if (any(!nzchar(names(x))) || anyDuplicated(names(x))) {
        stop("JSON object names must be unique and non-empty.", call. = FALSE)
      }
      sep <- if (is.null(indent)) ":" else ": "
      paste0(json_string(names(x)), sep,
             vapply(x, json_value, "", indent = next_indent(indent)))
    } else {
      vapply(x, json_value, "", indent = next_indent(indent))
    }
    return(json_wrap(parts, if (is_object) c("{", "}") else c("[", "]"), indent))
  }
  if (!is.atomic(x)) stop("Cannot write an object of class ", class(x)[1], " as JSON.", call. = FALSE)
  vals <- json_atomic(x)
  if (length(x) == 1L && !inherits(x, "AsIs")) return(vals)
  json_wrap(vals, c("[", "]"), NULL)
}

## JSON text written as-is (PROJJSON from GDAL), on one line, with any
## non-ASCII character escaped so the output stays ASCII.
json_verbatim <- function(x) {
  s <- gsub("[\r\n]+[ \t]*", "", enc2utf8(as.character(x)))
  cp <- utf8ToInt(s)
  if (all(cp < 128L)) return(s)
  out <- vapply(cp, function(ch) {
    if (ch < 128L) return(intToUtf8(ch))
    e <- json_string(intToUtf8(ch))
    substr(e, 2L, nchar(e) - 1L)
  }, "")
  paste(out, collapse = "")
}

next_indent <- function(indent) if (is.null(indent)) NULL else paste0(indent, "  ")

json_wrap <- function(parts, brackets, indent) {
  if (length(parts) == 0L) return(paste0(brackets[1], brackets[2]))
  if (is.null(indent)) {
    return(paste0(brackets[1], paste(parts, collapse = ","), brackets[2]))
  }
  inner <- paste0(indent, "  ")
  paste0(brackets[1], "\n", inner, paste(parts, collapse = paste0(",\n", inner)),
         "\n", indent, brackets[2])
}

json_atomic <- function(x) {
  if (is.character(x)) {
    out <- json_string(x)
  } else if (is.logical(x)) {
    out <- ifelse(x, "true", "false")
  } else if (is.integer(x)) {
    out <- as.character(x)
  } else if (is.double(x)) {
    if (any(is.infinite(x) | is.nan(x))) stop("JSON has no Inf or NaN.", call. = FALSE)
    ## Shortest of 15, 16 or 17 significant digits that reads back exactly.
    ## NA is written as null below, so only finite values are checked.
    out <- sprintf("%.15g", x)
    ok <- which(!is.na(x))
    for (digits in c("%.16g", "%.17g")) {
      lossy <- ok[as.numeric(out[ok]) != x[ok]]
      out[lossy] <- sprintf(digits, x[lossy])
    }
  } else {
    stop("Cannot write a ", typeof(x), " vector as JSON.", call. = FALSE)
  }
  out[is.na(x)] <- "null"
  unname(out)
}

json_string <- function(x) {
  vapply(enc2utf8(as.character(x)), function(s) {
    if (is.na(s)) return("null")
    cp <- utf8ToInt(s)
    out <- character(length(cp))
    for (i in seq_along(cp)) {
      ch <- cp[i]
      out[i] <- if (ch == 34L) {
        "\\\""
      } else if (ch == 92L) {
        "\\\\"
      } else if (ch == 10L) {
        "\\n"
      } else if (ch == 13L) {
        "\\r"
      } else if (ch == 9L) {
        "\\t"
      } else if (ch < 32L || (ch > 126L && ch <= 0xFFFF)) {
        sprintf("\\u%04x", ch)
      } else if (ch > 0xFFFF) {
        v <- ch - 0x10000
        sprintf("\\u%04x\\u%04x", 0xD800 + v %/% 1024, 0xDC00 + v %% 1024)
      } else {
        intToUtf8(ch)
      }
    }
    paste0("\"", paste(out, collapse = ""), "\"")
  }, "", USE.NAMES = FALSE)
}
