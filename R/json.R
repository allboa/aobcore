## Minimal JSON writer for scene documents, so the core needs no JSON
## package. Named lists become objects, unnamed lists arrays; an atomic
## vector of length one is a scalar (wrap it in I() to force an array) and
## any other length is an array. Strings are escaped to ASCII, and "<" is
## written as < so the text can sit inside a <script> element.

to_json <- function(x) {
  if (is.null(x)) {
    return("null")
  }
  if (is.list(x)) {
    x <- unclass(x)
    nms <- names(x)
    if (!is.null(nms) || (length(x) == 0L && !is.null(attr(x, "names")))) {
      if (length(x) == 0L) {
        return("{}")
      }
      if (any(is.na(nms) | nms == "")) {
        stop("Every element of a named list must have a name.", call. = FALSE)
      }
      body <- vapply(seq_along(x), function(i) {
        paste0(json_string(nms[[i]]), ":", to_json(x[[i]]))
      }, character(1))
      return(paste0("{", paste(body, collapse = ","), "}"))
    }
    body <- vapply(x, to_json, character(1))
    return(paste0("[", paste(body, collapse = ","), "]"))
  }
  scalar <- length(x) == 1L && !inherits(x, "AsIs")
  vals <- if (is.character(x)) {
    json_string(x)
  } else if (is.logical(x)) {
    if (anyNA(x)) stop("JSON has no NA; use NULL.", call. = FALSE)
    ifelse(x, "true", "false")
  } else if (is.numeric(x)) {
    json_number(x)
  } else if (is.factor(x)) {
    json_string(as.character(x))
  } else {
    stop("Cannot write an object of class ", class(x)[1], " as JSON.", call. = FALSE)
  }
  if (scalar) vals else paste0("[", paste(vals, collapse = ","), "]")
}

json_number <- function(x) {
  x <- unclass(x)
  if (any(!is.finite(x))) {
    stop("JSON numbers must be finite (no NA, NaN or Inf).", call. = FALSE)
  }
  if (is.integer(x)) {
    return(as.character(x))
  }
  ## Shortest of 15, 16 or 17 significant digits that reads back exactly.
  out <- sprintf("%.15g", x)
  for (digits in c("%.16g", "%.17g")) {
    lossy <- as.numeric(out) != x
    out[lossy] <- sprintf(digits, x[lossy])
  }
  out
}

json_string <- function(x) {
  if (anyNA(x)) stop("JSON strings cannot be NA.", call. = FALSE)
  x <- enc2utf8(as.character(x))
  vapply(x, json_escape, character(1), USE.NAMES = FALSE)
}

json_escape <- function(s) {
  cp <- utf8ToInt(s)
  if (length(cp) == 0L) {
    return("\"\"")
  }
  plain <- cp >= 0x20 & cp < 0x7f & !(cp %in% c(0x22, 0x5c, 0x3c))
  out <- character(length(cp))
  out[plain] <- intToUtf8(cp[plain], multiple = TRUE)
  esc <- which(!plain)
  short <- c("34" = "\\\"", "92" = "\\\\", "10" = "\\n", "13" = "\\r", "9" = "\\t")
  for (i in esc) {
    c <- cp[[i]]
    key <- as.character(c)
    out[[i]] <- if (key %in% names(short)) {
      short[[key]]
    } else if (c < 0x10000) {
      sprintf("\\u%04x", c)
    } else {
      c <- c - 0x10000
      sprintf("\\u%04x\\u%04x", 0xd800 + c %/% 0x400, 0xdc00 + c %% 0x400)
    }
  }
  paste0("\"", paste(out, collapse = ""), "\"")
}
