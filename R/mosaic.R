#' Members of a VRT or GTI mosaic
#'
#' Reads the members of a GDAL virtual mosaic without reading any member
#' (allboa/design decision 0011, aobview#41): a VRT (a `.vrt` file or a
#' `vrt://` connection string) from the XML GDAL serialises for it (its
#' `xml:VRT` metadata domain), or a GDAL Tile Index (GTI, GDAL >= 3.9: a
#' `.gti` file, a `.gti.gpkg` or `.gti.fgb` index, or a `GTI:` prefixed
#' path) from its index layer. Each member is named as GDAL opens it (a
#' member relative to the VRT or index is resolved against its directory or
#' URL) with its placement in the mosaic, so [mosaic_plan()] can tell which
#' members a view touches before it probes any. Requires 'gdalraster'.
#'
#' A VRT member is one `<SourceFilename>` of one `<VRTRasterBand>`, with
#' its `<SourceBand>`, `<SrcRect>` and `<DstRect>`; the placement is the
#' `DstRect` in the mosaic's geotransform (the whole member over the whole
#' mosaic when they are absent). A member that the VRT does not copy as it
#' is (a `ComplexSource` with a scale, offset, exponent, lookup table or
#' colour table component, a `KernelFilteredSource`, a mask band as the
#' source band, a source of a derived band) is kept with a `reason`, so
#' [mosaic_plan()] can say why it is not drawn in place. A warped or
#' pansharpened VRT has no members. A source that composites by the
#' member's mask (`UseMaskBand`, as `gdalbuildvrt` writes for a member with
#' an alpha or mask band) is a copy: the member's own alpha is drawn. A
#' GTI member is one feature of the index
#' layer: its `location` field (or the field the layer's `LOCATION_FIELD`
#' metadata or the `.gti` file's `<LocationField>` names) and the bounds of
#' its geometry; every band comes from the same member, so `band` is `NA`.
#'
#' @param dsn A VRT or GTI: a local path, an `http(s)` URL or a GDAL `/vsi`
#'   path, as for [cog_info()].
#' @return `NULL` when GDAL opens `dsn` with a driver other than VRT or GTI
#'   (a plain file: the caller takes its usual route), else a list of class
#'   `"aob_mosaic"`: `dsn`, `kind` (`"vrt"` or `"gti"`), `crs` (as
#'   [cog_info()] gives it), `wkt`, `dim`, `geotransform`, `extent`
#'   (`c(xmin, xmax, ymin, ymax)`), `note` (why a VRT has no members, or
#'   `NULL`) and `members`, a data frame with one row per member and band:
#'   `dsn` (what [cog_info()] opens), `band` (the mosaic band it feeds; `NA`
#'   for every band), `source_band`, `xmin`, `xmax`, `ymin`, `ymax` (its
#'   placement in the mosaic CRS), `src_xoff`, `src_yoff`, `src_xsize`,
#'   `src_ysize` (the window of the member's cells a VRT draws; `NA` for
#'   the whole member) and `reason` (`NA` when the member is drawn as it
#'   is).
#' @seealso [mosaic_plan()] to plan the members a view touches.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
#' vrt <- tempfile(fileext = ".vrt")
#' gdalraster::buildVRT(vrt, f, quiet = TRUE)
#' m <- mosaic_members(vrt)
#' m
#' m$members$dsn
#' # A plain COG is not a mosaic.
#' mosaic_members(f)
mosaic_members <- function(dsn) {
  need_gdalraster("mosaic_members()")
  if (!is.character(dsn) || length(dsn) != 1L || is.na(dsn) || !nzchar(dsn)) {
    stop("`dsn` must be a single path or URL.", call. = FALSE)
  }
  is_url <- grepl("^https?://", dsn)
  ## A "vrt://" or "GTI:" connection string is not a file.
  local <- !is_url && !startsWith(dsn, "/vsi") &&
    (grepl("^[A-Za-z]:[/\\\\]", dsn) || !grepl("^[A-Za-z][A-Za-z0-9+.-]*:", dsn))
  if (local) {
    if (!file.exists(dsn)) stop("No file at \"", dsn, "\".", call. = FALSE)
    dsn <- normalizePath(dsn, winslash = "/")
  }
  gdal_dsn <- dsn_ref(dsn)$gdal
  ds <- open_raster(gdal_dsn)
  on.exit(ds$close(), add = TRUE)
  driver <- toupper(ds$getDriverShortName())
  if (!driver %in% c("VRT", "GTI")) return(NULL)
  wkt <- ds$getProjection()
  if (!nzchar(wkt)) stop("\"", dsn, "\" has no CRS.", call. = FALSE)
  gt <- ds$getGeoTransform()
  if (gt[3] != 0 || gt[5] != 0) {
    stop("\"", dsn, "\" has a rotated geotransform; only north-up grids are supported.",
         call. = FALSE)
  }
  dim <- as.integer(ds$dim()[1:2])
  if (driver == "VRT") {
    xml <- paste(ds$getMetadata(0L, "xml:VRT"), collapse = "\n")
    if (!nzchar(xml)) stop("GDAL gives no XML for the VRT \"", dsn, "\".", call. = FALSE)
    parsed <- vrt_members(xml, dsn, gt, dim)
  } else {
    parsed <- list(members = gti_members(dsn, gdal_dsn), note = NULL)
  }
  structure(list(
    dsn = dsn,
    kind = tolower(driver),
    crs = crs_ref(wkt),
    wkt = wkt,
    dim = dim,
    geotransform = gt,
    extent = c(gt[1], gt[1] + dim[1] * gt[2], gt[4] + dim[2] * gt[6], gt[4]),
    note = parsed$note,
    members = parsed$members
  ), class = "aob_mosaic")
}

#' @export
print.aob_mosaic <- function(x, ...) {
  m <- x$members
  cat("<mosaic> ", x$dsn, "\n", sep = "")
  cat("  ", toupper(x$kind), " in ", crs_label(x$crs), ", ", x$dim[1], " x ", x$dim[2],
      " cells, ", length(unique(m$dsn)), " member", if (length(unique(m$dsn)) != 1L) "s",
      if (!is.null(x$note)) paste0(" (", x$note, ")"), "\n", sep = "")
  for (b in unique(m$band)) {
    rows <- if (is.na(b)) m else m[!is.na(m$band) & m$band == b, ]
    cat("  band ", if (is.na(b)) "any" else b, ": ",
        paste(utils::head(basename(rows$dsn), 6L), collapse = ", "),
        if (nrow(rows) > 6L) paste0(", ... (", nrow(rows), ")"), "\n", sep = "")
  }
  invisible(x)
}

#' Plan the members of a mosaic for a view
#'
#' Plans a VRT or GTI mosaic of COGs across its members, one [cog_plan()]
#' per member, so a scene draws each member from its own file or URL (one
#' `tiled_raster` layer per member, through [scene_add_tiled_raster()]) and
#' nothing is copied. Members are probed lazily: only those whose placement
#' meets `extent` (transformed to the mosaic's CRS) are opened with
#' [cog_info()], so a remote member costs one range request for its header
#' and a member the view does not touch costs nothing. A member that cannot
#' be drawn in place is reported, not planned: one that is not a tiled
#' GeoTIFF with overviews (or small enough for one tile), one in another
#' CRS, one the mosaic places by a window of its cells or somewhere other
#' than its own georeferencing (a VRT that stretches a small image over a
#' huge grid), or one the VRT rescales (see [mosaic_members()]). The caller
#' decides what to do then (aobview falls back to reading the mosaic
#' through GDAL into a temporary COG, with a message naming the member).
#'
#' @param mosaic A mosaic from [mosaic_members()], or a path or URL passed
#'   to it (an error when that is not a VRT or GTI).
#' @param band The mosaic band to draw, 1-based: a VRT band's sources are
#'   planned, each at its source band; a GTI member is planned at this band.
#' @inheritParams cog_plan
#' @param ... Passed to [cog_plan()] for each member (`levels`, `selection`,
#'   `max_segments`, `tolerance`, `max_stretch`, `max_tiles`; the tile
#'   budget is per member).
#' @return A list of class `"aob_mosaic_plan"`: `mosaic`, `crs`, `band`, `plans`
#'   (one [cog_plan()] result per planned member, named by the member's
#'   `dsn`, in the mosaic's order) and `members`, a data frame of every
#'   member of the band with `dsn`, `source_band`, `status` (`"planned"`,
#'   `"skipped"` when its placement misses `extent`, or `"unplanned"` when
#'   it cannot be drawn in place) and `reason` (`NA` unless unplanned); the
#'   unplanned rows are what a caller falls back for.
#' @seealso [mosaic_members()].
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
#' vrt <- tempfile(fileext = ".vrt")
#' gdalraster::buildVRT(vrt, f, quiet = TRUE)
#' p <- mosaic_plan(vrt, "EPSG:3031", extent = c(-2e6, 2e6, -2e6, 2e6), units_per_pixel = 20000)
#' p
#' p$members
mosaic_plan <- function(mosaic, crs = "EPSG:3031", extent = NULL, units_per_pixel = NULL,
                        band = 1L, ...) {
  need_gdalraster("mosaic_plan()")
  if (!inherits(mosaic, "aob_mosaic")) {
    dsn <- mosaic
    mosaic <- mosaic_members(dsn)
    if (is.null(mosaic)) stop("\"", dsn, "\" is not a VRT or GTI mosaic.", call. = FALSE)
  }
  crs <- scene_crs(crs)
  view_wkt <- crs_wkt(crs)
  if (!is.null(extent)) check_extent(extent)
  if (!is.numeric(band) || length(band) != 1L || is.na(band) || band != round(band) || band < 1) {
    stop("`band` must be a single band number, 1 or more.", call. = FALSE)
  }
  band <- as.integer(band)
  m <- mosaic$members
  m <- m[is.na(m$band) | m$band == band, , drop = FALSE]
  if (!nrow(m)) {
    stop("Band ", band, " of \"", mosaic$dsn, "\" has no members",
         if (!is.null(mosaic$note)) paste0(": ", mosaic$note), ".", call. = FALSE)
  }
  m$source_band[is.na(m$source_band)] <- band
  status <- rep("planned", nrow(m))
  reason <- as.character(m$reason)
  status[!is.na(reason)] <- "unplanned"
  ## Only members whose placement meets the view's extent are probed.
  if (!is.null(extent)) {
    bb <- extent_in_crs(extent, view_wkt, mosaic$wkt)
    if (!is.null(bb)) {
      hit <- m$xmin < bb[2] & m$xmax > bb[1] & m$ymin < bb[4] & m$ymax > bb[3]
      status[!hit] <- "skipped"
      reason[!hit] <- NA_character_
    }
  }
  plans <- list()
  for (i in which(status == "planned")) {
    cog <- tryCatch(cog_info(m$dsn[i], band = m$source_band[i]), error = function(e) e)
    why <- if (inherits(cog, "error")) {
      paste0("cannot be read as a COG: ", conditionMessage(cog))
    } else {
      member_in_place(cog, m[i, ], mosaic)
    }
    if (!is.null(why)) {
      status[i] <- "unplanned"
      reason[i] <- why
      next
    }
    plans[[m$dsn[i]]] <- cog_plan(cog, crs, extent = extent, units_per_pixel = units_per_pixel, ...)
  }
  structure(list(
    mosaic = mosaic,
    crs = crs,
    band = band,
    plans = plans,
    members = data.frame(dsn = m$dsn, source_band = as.integer(m$source_band), status = status,
                         reason = reason, stringsAsFactors = FALSE)
  ), class = "aob_mosaic_plan")
}

#' @export
print.aob_mosaic_plan <- function(x, ...) {
  m <- x$members
  n <- vapply(x$plans, function(p) sum(vapply(p$plan$levels, function(l) length(l$tiles), 0L)), 0L)
  cat("<mosaic plan> ", x$mosaic$dsn, ": ", sum(m$status == "planned"), " of ", nrow(m),
      " member", if (nrow(m) != 1L) "s", " planned in ", crs_label(x$crs), ", ", sum(n),
      " tiles\n", sep = "")
  for (i in seq_len(nrow(m))) {
    cat("  ", basename(m$dsn[i]), ": ", m$status[i],
        if (m$status[i] == "planned") paste0(", ", n[[m$dsn[i]]], " tiles"),
        if (!is.na(m$reason[i])) paste0(" (", m$reason[i], ")"), "\n", sep = "")
  }
  invisible(x)
}

## ---- internals -------------------------------------------------------------

## Why a member COG cannot be drawn where the mosaic places it, or NULL: it
## must be in the mosaic's CRS, drawn whole (its SrcRect, when a VRT gives
## one, is all its cells) and placed by its own georeferencing (its
## full-resolution extent is the placement, within a thousandth of a mosaic
## cell), and be tiled with overviews or small enough for one tile.
member_in_place <- function(cog, m, mosaic) {
  if (!isTRUE(gdalraster::srs_is_same(cog$wkt, mosaic$wkt))) {
    return("is not in the mosaic's CRS")
  }
  l0 <- cog$levels[[1]]
  if (!is.na(m$src_xsize) && !(m$src_xoff == 0 && m$src_yoff == 0 &&
                               m$src_xsize == l0$dim[1] && m$src_ysize == l0$dim[2])) {
    return("is drawn from a window of its cells, not whole")
  }
  tol <- 1e-3 * max(abs(mosaic$geotransform[c(2, 6)]))
  if (any(abs(l0$extent - c(m$xmin, m$xmax, m$ymin, m$ymax)) > tol)) {
    return("is not placed in the mosaic by its own georeferencing")
  }
  if (!all(l0$tile_size >= l0$dim) && (l0$tile_size[1] >= l0$dim[1] || length(cog$levels) < 2L)) {
    return("is not a tiled GeoTIFF with overviews")
  }
  NULL
}

## `extent` (c(xmin, xmax, ymin, ymax) in the CRS `from`) as the same in
## `to`, or NULL when it cannot be transformed.
extent_in_crs <- function(extent, from, to) {
  if (isTRUE(gdalraster::srs_is_same(from, to))) return(as.numeric(extent))
  bb <- tryCatch(suppressWarnings(gdalraster::transform_bounds(extent[c(1, 3, 2, 4)], from, to)),
                 error = function(e) NULL)
  if (length(bb) != 4L || any(!is.finite(bb)) || bb[3] <= bb[1] || bb[4] <= bb[2]) return(NULL)
  bb[c(1, 3, 2, 4)]
}

## The members of a VRT from its XML: one row per source of each
## VRTRasterBand (mask bands left out), with the source resolved against the
## VRT's directory when relativeToVRT is set. A small regex parser: GDAL
## wrote the XML, so elements are well formed and attributes double quoted.
vrt_members <- function(xml, dsn, gt, dim) {
  empty <- member_rows()
  root <- regmatches(xml, regexpr("<VRTDataset\\b[^>]*>", xml))
  sub_class <- xml_attr(root, "subClass")
  if (!is.na(sub_class)) {
    return(list(members = empty, note = paste0("a ", sub_class, " VRT has no members drawn as they are")))
  }
  xml <- gsub("(?s)<MaskBand>.*?</MaskBand>", "", xml, perl = TRUE)
  bands <- regmatches(xml, gregexpr("(?s)<VRTRasterBand\\b.*?</VRTRasterBand>", xml, perl = TRUE))[[1]]
  rows <- list()
  for (k in seq_along(bands)) {
    b <- bands[[k]]
    open <- regmatches(b, regexpr("^<VRTRasterBand\\b[^>]*>", b))
    band <- suppressWarnings(as.integer(xml_attr(open, "band")))
    if (is.na(band)) band <- k
    derived <- xml_attr(open, "subClass")
    band_reason <- if (!is.na(derived)) paste0("feeds a ", derived, " band, not a copy")
    for (s in vrt_sources(b)) {
      r <- vrt_source(s, dsn, gt, dim)
      if (is.null(r)) next
      r$band <- band
      if (is.na(r$reason) && !is.null(band_reason)) r$reason <- band_reason
      rows[[length(rows) + 1L]] <- r
    }
  }
  if (!length(rows)) return(list(members = empty, note = "it has no sources"))
  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  rownames(out) <- NULL
  list(members = out[names(empty)], note = NULL)
}

member_rows <- function() {
  data.frame(dsn = character(), band = integer(), source_band = integer(),
             xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric(),
             src_xoff = numeric(), src_yoff = numeric(), src_xsize = numeric(),
             src_ysize = numeric(), reason = character(), stringsAsFactors = FALSE)
}

vrt_source_kinds <- c("SimpleSource", "AveragedSource", "ComplexSource",
                      "KernelFilteredSource", "NoDataFromMaskSource")

vrt_sources <- function(band_xml) {
  kinds <- paste(vrt_source_kinds, collapse = "|")
  pat <- paste0("(?s)<(", kinds, ")\\b[^>]*>.*?</\\1>")
  regmatches(band_xml, gregexpr(pat, band_xml, perl = TRUE))[[1]]
}

## One source element as a member row (a list), or NULL when it names no
## file.
vrt_source <- function(s, dsn, gt, dim) {
  kind <- sub("^<([A-Za-z]+).*$", "\\1", s)
  fn <- regmatches(s, regexpr("(?s)<SourceFilename\\b[^>]*>.*?</SourceFilename>", s, perl = TRUE))
  if (!length(fn)) return(NULL)
  open <- regmatches(fn, regexpr("^<SourceFilename\\b[^>]*>", fn))
  name <- xml_text(sub("^<SourceFilename\\b[^>]*>", "", sub("</SourceFilename>$", "", fn)))
  if (!nzchar(name)) return(NULL)
  relative <- identical(xml_attr(open, "relativeToVRT"), "1")
  path <- if (relative) paste0(dsn_dir(dsn), "/", name) else name
  sb <- xml_text(xml_element(s, "SourceBand"))
  reason <- NA_character_
  source_band <- NA_integer_
  if (nzchar(sb) && grepl("mask", sb, ignore.case = TRUE)) {
    reason <- "is drawn from its mask band"
  } else {
    source_band <- if (nzchar(sb)) suppressWarnings(as.integer(sb)) else 1L
    if (is.na(source_band)) reason <- paste0("has an unknown source band \"", sb, "\"")
  }
  if (is.na(reason)) {
    if (kind %in% c("KernelFilteredSource", "NoDataFromMaskSource")) {
      reason <- paste0("is a ", kind, ", not a copy of its cells")
    } else if (kind == "ComplexSource" && any(vapply(
      c("ScaleOffset", "ScaleRatio", "Exponent", "LUT", "ColorTableComponent"),
      function(e) nzchar(xml_element(s, e)), TRUE))) {
      reason <- "is rescaled or remapped by the VRT"
    }
  }
  src <- xml_rect(s, "SrcRect")
  dst <- xml_rect(s, "DstRect")
  if (anyNA(dst)) dst <- c(0, 0, dim)
  x <- gt[1] + c(dst[1], dst[1] + dst[3]) * gt[2]
  y <- gt[4] + c(dst[2], dst[2] + dst[4]) * gt[6]
  list(dsn = path, band = NA_integer_, source_band = source_band,
       xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y),
       src_xoff = src[1], src_yoff = src[2], src_xsize = src[3], src_ysize = src[4],
       reason = reason)
}

## The xOff, yOff, xSize, ySize of a <SrcRect/> or <DstRect/>, NA when the
## element is absent.
xml_rect <- function(s, name) {
  el <- regmatches(s, regexpr(paste0("<", name, "\\b[^>]*>"), s))
  if (!length(el)) return(rep(NA_real_, 4L))
  v <- vapply(c("xOff", "yOff", "xSize", "ySize"), function(a) xml_attr(el, a), "")
  suppressWarnings(as.numeric(v))
}

## The text of the first <name>...</name> element, "" when absent.
xml_element <- function(s, name) {
  el <- regmatches(s, regexpr(paste0("(?s)<", name, "\\b[^>]*>.*?</", name, ">"), s, perl = TRUE))
  if (!length(el)) return("")
  sub(paste0("</", name, ">$"), "", sub(paste0("^<", name, "\\b[^>]*>"), "", el))
}

## A double-quoted attribute of an element's open tag, NA when absent.
xml_attr <- function(tag, name) {
  if (!length(tag)) return(NA_character_)
  m <- regmatches(tag, regexpr(paste0("\\b", name, "\\s*=\\s*\"[^\"]*\""), tag))
  if (!length(m)) return(NA_character_)
  xml_text(sub("\"$", "", sub("^[^\"]*\"", "", m)))
}

## Element text with the XML entities GDAL writes decoded.
xml_text <- function(x) {
  x <- trimws(x)
  ents <- c(lt = "<", gt = ">", quot = "\"", apos = "'", amp = "&")
  for (e in names(ents)) x <- gsub(paste0("&", e, ";"), ents[[e]], x, fixed = TRUE)
  x
}

## The directory a VRT's or index's relative members resolve against: the
## path or URL with its last segment removed.
dsn_dir <- function(dsn) {
  d <- sub("/[^/]*$", "", gsub("\\\\", "/", dsn))
  if (identical(d, dsn)) "." else d
}

## The members of a GTI from its index layer: the location field and the
## bounds of each feature's geometry. A `.gti` XML file names the index
## dataset (and may name the layer and the location field); any other GTI
## is the index dataset itself, with or without a "GTI:" prefix.
gti_members <- function(dsn, gdal_dsn) {
  index <- sub("^GTI:", "", gdal_dsn)
  layer <- NULL
  field <- NULL
  if (grepl("[.]gti$", index, ignore.case = TRUE)) {
    xml <- vsi_text(index)
    ds <- xml_text(xml_element(xml, "IndexDataset"))
    if (!nzchar(ds)) stop("\"", dsn, "\" names no <IndexDataset>.", call. = FALSE)
    if (!grepl("^(/|[A-Za-z]:[/\\\\]|[A-Za-z][A-Za-z0-9+.-]*:)", ds)) {
      ds <- paste0(dsn_dir(index), "/", ds)
    }
    lyr <- xml_text(xml_element(xml, "IndexLayer"))
    if (nzchar(lyr)) layer <- lyr
    fld <- xml_text(xml_element(xml, "LocationField"))
    if (nzchar(fld)) field <- fld
    index <- ds
  }
  lyr <- tryCatch(
    if (is.null(layer)) gdalraster::GDALVector$new(index) else gdalraster::GDALVector$new(index, layer),
    error = function(e) NULL
  )
  if (is.null(lyr)) stop("GDAL cannot open the tile index \"", index, "\".", call. = FALSE)
  on.exit(lyr$close(), add = TRUE)
  try(lyr$quiet <- TRUE, silent = TRUE)
  if (is.null(field)) {
    md <- lyr$getMetadata()
    md <- md[startsWith(md, "LOCATION_FIELD=")]
    field <- if (length(md)) sub("^LOCATION_FIELD=", "", md[1]) else "location"
  }
  if (!field %in% lyr$getFieldNames()) {
    stop("The tile index \"", index, "\" has no \"", field, "\" field.", call. = FALSE)
  }
  fs <- lyr$fetch(-1)
  geom <- attr(fs, "gis")$geom_column
  n <- nrow(fs)
  out <- member_rows()
  if (!n) return(out)
  if (is.null(geom) || !geom %in% names(fs)) {
    stop("The tile index \"", index, "\" has no geometry column.", call. = FALSE)
  }
  env <- unclass(wk::wk_envelope(wk::wkb(fs[[geom]])))
  loc <- as.character(fs[[field]])
  relative <- !grepl("^(/|[A-Za-z]:[/\\\\]|[A-Za-z][A-Za-z0-9+.-]*:)", loc)
  loc[relative] <- paste0(dsn_dir(index), "/", loc[relative])
  data.frame(dsn = loc, band = NA_integer_, source_band = NA_integer_,
             xmin = env$xmin, xmax = env$xmax, ymin = env$ymin, ymax = env$ymax,
             src_xoff = NA_real_, src_yoff = NA_real_, src_xsize = NA_real_, src_ysize = NA_real_,
             reason = NA_character_, stringsAsFactors = FALSE)
}

## A small text file read through GDAL's virtual file layer.
vsi_text <- function(path) {
  n <- tryCatch(gdalraster::vsi_stat_size(path), error = function(e) -1)
  if (!is.numeric(n) || n < 0) stop("Cannot read \"", path, "\".", call. = FALSE)
  if (n > 2^20) stop("\"", path, "\" is too large for a tile index description.", call. = FALSE)
  f <- gdalraster::VSIFile$new(path)
  on.exit(f$close())
  rawToChar(f$read(n))
}
