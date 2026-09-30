## A small reader for the TIFF tags GDAL does not report: the photometric
## interpretation (262) and the JPEGTables (347) of every image (IFD). It
## reads the IFD bytes directly through GDAL's VSI layer (so local files,
## /vsicurl/ and cloud paths all work, with GDAL's caching), handles classic
## TIFF and BigTIFF in either byte order, and reads only the tags it needs.
## Returns a list with one entry per IFD in file order: `subfile`
## (NewSubfileType, 254), `width`, `height`, `compression`, `photometric`,
## `samples_per_pixel` and `jpeg_tables` (raw, or NULL). Any failure gives
## NULL: callers fall back to what GDAL reports.
tiff_ifds <- function(dsn, max_ifds = 64L) {
  tryCatch(read_tiff_ifds(dsn, max_ifds), error = function(e) NULL)
}

read_tiff_ifds <- function(dsn, max_ifds) {
  f <- gdalraster::VSIFile$new(dsn)
  on.exit(f$close())
  at <- function(offset, n) {
    f$seek(offset, "SEEK_SET")
    b <- f$read(n)
    if (length(b) != n) stop("short read")
    b
  }
  head <- at(0, 16)
  endian <- if (identical(head[1:2], charToRaw("II"))) "little" else
    if (identical(head[1:2], charToRaw("MM"))) "big" else stop("not a TIFF")
  uint <- function(b, size) {
    if (size == 1L) return(as.numeric(as.integer(b[1])))
    if (size == 2L) return(as.numeric(readBin(b, "integer", 1L, size = 2L, signed = FALSE, endian = endian)))
    if (size == 4L) {
      v <- readBin(b, "integer", 1L, size = 4L, endian = endian)
      return(if (v < 0) v + 2^32 else as.numeric(v))
    }
    lo <- uint(if (endian == "little") b[1:4] else b[5:8], 4L)
    hi <- uint(if (endian == "little") b[5:8] else b[1:4], 4L)
    hi * 2^32 + lo
  }
  magic <- uint(head[3:4], 2L)
  big <- magic == 43
  if (!big && magic != 42) stop("not a TIFF")
  off_size <- if (big) 8L else 4L
  count_size <- if (big) 8L else 2L
  entry_size <- if (big) 20L else 12L
  next_ifd <- if (big) uint(head[9:16], 8L) else uint(head[5:8], 4L)
  type_size <- c(1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8, 0, 0, 0, 8, 8, 8)

  out <- list()
  seen <- numeric()
  while (next_ifd > 0 && length(out) < max_ifds && !next_ifd %in% seen) {
    seen <- c(seen, next_ifd)
    n <- uint(at(next_ifd, count_size), count_size)
    body <- at(next_ifd + count_size, n * entry_size + off_size)
    ifd <- list(subfile = 0, width = NA_real_, height = NA_real_, compression = 1,
                photometric = NA_real_, samples_per_pixel = 1, jpeg_tables = NULL)
    for (i in seq_len(n)) {
      e <- body[(i - 1) * entry_size + seq_len(entry_size)]
      tag <- uint(e[1:2], 2L)
      if (!tag %in% c(254, 256, 257, 259, 262, 277, 347)) next
      type <- uint(e[3:4], 2L)
      count <- uint(e[5:(4 + off_size)], off_size)
      vbytes <- e[(5 + off_size):entry_size]
      size <- if (type >= 1 && type <= length(type_size)) type_size[type] else 0
      if (size == 0) next
      total <- size * count
      data <- if (total <= off_size) vbytes[seq_len(total)] else
        if (tag == 347) at(uint(vbytes, off_size), total) else NULL
      if (tag == 347) {
        ifd$jpeg_tables <- data
      } else if (!is.null(data) && count >= 1) {
        v <- uint(data[seq_len(size)], as.integer(size))
        nm <- c("254" = "subfile", "256" = "width", "257" = "height", "259" = "compression",
                "262" = "photometric", "277" = "samples_per_pixel")[[as.character(tag)]]
        ifd[[nm]] <- v
      }
    }
    out[[length(out) + 1L]] <- ifd
    next_ifd <- uint(body[n * entry_size + seq_len(off_size)], off_size)
  }
  out
}

## TIFF photometric interpretation codes to names.
photometric_name <- function(code) {
  names <- c("0" = "MinIsWhite", "1" = "MinIsBlack", "2" = "RGB", "3" = "Palette",
             "4" = "Mask", "5" = "Separated", "6" = "YCbCr", "8" = "CIELab")
  if (is.null(code) || is.na(code)) return(NA_character_)
  nm <- unname(names[as.character(code)])
  if (is.na(nm)) paste0("code ", code) else nm
}

## The IFD of an image of the given size that is not a mask (NewSubfileType
## bit 4), in file order; `used` are IFD positions already matched.
match_ifd <- function(ifds, dim, used = integer()) {
  for (i in seq_along(ifds)) {
    d <- ifds[[i]]
    if (i %in% used || bitwAnd(as.integer(d$subfile %% 2^31), 4L) != 0L) next
    if (isTRUE(d$width == dim[1] && d$height == dim[2])) return(i)
  }
  NA_integer_
}
