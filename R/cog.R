#' Structure of a Cloud Optimized GeoTIFF
#'
#' Reads what a tile plan needs from a COG through GDAL: the source CRS, and
#' for the full-resolution image and each overview (a "level") its grid (own
#' geotransform, extent and size), tile size, encoding, and every tile's
#' byte offset and length in the file. Nothing is decoded. Requires the
#' 'gdalraster' package.
#'
#' Each level is opened as its own GDAL dataset (the `OVERVIEW_LEVEL` open
#' option for overviews), so its geotransform and extent are GDAL's, not
#' derived from the full-resolution grid by a decimation factor (an overview
#' of 12 rows over 50 degrees does not have a pixel 8 times the base). Tile
#' byte ranges are the `BLOCK_OFFSET_<col>_<row>` and `BLOCK_SIZE_<col>_<row>`
#' metadata items in GDAL's `TIFF` domain for the chosen band. Compression
#' and predictor come from the `IMAGE_STRUCTURE` domain, the byte order from
#' the file's first two bytes, and each band's colour interpretation
#' (`"Red"`, `"Green"`, `"Blue"`, `"Alpha"`, `"Gray"`, ...) from GDAL.
#'
#' Two TIFF tags GDAL does not report are read from the file's image file
#' directories (IFDs) directly, through GDAL's virtual file layer: each
#' level's photometric interpretation (tag 262) and, for JPEG, its
#' JPEGTables (tag 347), the quantization and Huffman tables its tiles
#' share. The tables go into the level's `encoding$jpeg_tables` (base64), as
#' scene spec 0.3 carries them. If the IFDs cannot be read, the photometric
#' interpretation falls back to what GDAL's `IMAGE_STRUCTURE` says (`"YCbCr
#' JPEG"` compression is YCbCr) and JPEG levels have no tables.
#'
#' @param dsn A local path to a COG, an `http(s)` URL (read with GDAL's
#'   `/vsicurl/`; a `"/vsicurl/https://..."` path is the same URL), or any
#'   GDAL `/vsi` path. The renderer fetches tiles itself, so `/vsis3/`,
#'   `/vsigs/` and `/vsiaz/` paths are given to it as their public https
#'   URLs (honouring `AWS_S3_ENDPOINT`, always path style, `AWS_HTTPS` and
#'   `AZURE_STORAGE_ACCOUNT`); that works for public objects only.
#' @param band The band to draw, 1-based.
#' @return A list of class `"aob_cog"`: `dsn`, `url` (the reference a scene
#'   gives the renderer), `local` (whether the bytes can be read here for
#'   embedding), `crs` (an `"authority:code"` string when GDAL finds one,
#'   else PROJJSON text), `wkt`, `band`, `samples_per_pixel`, `planar`,
#'   `color_interp` (one per band), `photometric` (the full-resolution
#'   image's TIFF photometric interpretation, such as `"RGB"`, `"YCbCr"` or
#'   `"MinIsBlack"`; `NA` when unknown), `byte_order`, `scale`, `offset`,
#'   `nodata`, and `levels`: one list per level with `level` (0 is full
#'   resolution), `dim`, `geotransform`, `extent` (`c(xmin, xmax, ymin,
#'   ymax)`), `tile_size`, `photometric`, `encoding` and `tiles` (a data
#'   frame of `col`, `row`, `byte_offset`, `byte_length`; sparse tiles with
#'   no bytes are left out).
#' @seealso [cog_plan()] to plan tiles for a view, [view_cog()] for the one
#'   call.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE)
#' cog <- cog_info(system.file("extdata", "polar_3031.tif", package = "aobcore"))
#' cog
#' cog$levels[[2]]$tiles
cog_info <- function(dsn, band = 1L) {
  need_gdalraster("cog_info()")
  if (!is.character(dsn) || length(dsn) != 1L || is.na(dsn) || !nzchar(dsn)) {
    stop("`dsn` must be a single path or URL.", call. = FALSE)
  }
  ref <- dsn_ref(dsn)
  is_url <- grepl("^https?://", dsn)
  local <- !is_url && !startsWith(dsn, "/vsi")
  if (local) {
    if (!file.exists(dsn)) stop("No file at \"", dsn, "\".", call. = FALSE)
    dsn <- normalizePath(dsn, winslash = "/")
  }
  gdal_dsn <- ref$gdal

  ds <- open_raster(gdal_dsn)
  on.exit(ds$close(), add = TRUE)
  nb <- ds$getRasterCount()
  band <- check_band(band, nb)
  color_interp <- vapply(seq_len(nb), function(b) {
    v <- tryCatch(ds$getRasterColorInterp(b), error = function(e) NA_character_)
    if (is.character(v) && length(v) == 1L) v else NA_character_
  }, "")
  wkt <- ds$getProjection()
  if (!nzchar(wkt)) stop("\"", dsn, "\" has no CRS.", call. = FALSE)
  struct0 <- image_structure(ds)
  interleave <- toupper(md_item(struct0, "INTERLEAVE") %||% "BAND")
  planar <- if (nb > 1L && interleave == "BAND") "separate" else "interleaved"
  if (!identical(toupper(md_item(struct0, "LAYOUT") %||% ""), "COG")) {
    ## Any tiled GeoTIFF works; a COG keeps the header and overviews first.
    if (is.null(ds$getMetadataItem(band, "BLOCK_OFFSET_0_0", "TIFF")) ||
        !nzchar(ds$getMetadataItem(band, "BLOCK_OFFSET_0_0", "TIFF"))) {
      stop("\"", dsn, "\" is not a tiled GeoTIFF GDAL can give tile offsets for.",
           call. = FALSE)
    }
  }
  scale <- ds$getScale(band)
  offset <- ds$getOffset(band)
  nodata <- ds$getNoDataValue(band)
  dtype <- gdal_dtype(ds$getDataTypeName(band))

  n_ov <- ds$getOverviewCount(band)
  levels <- vector("list", n_ov + 1L)
  ifds <- tiff_ifds(gdal_dsn)
  used <- integer()
  for (k in 0:n_ov) {
    lv <- if (k == 0L) ds else open_raster(gdal_dsn, paste0("OVERVIEW_LEVEL=", k - 1L))
    L <- cog_level(lv, k, band, dtype, planar, nb, struct0)
    if (k > 0L) lv$close()
    i <- if (length(ifds)) match_ifd(ifds, L$dim, used) else NA_integer_
    ifd <- if (is.na(i)) NULL else ifds[[i]]
    used <- c(used, i)
    ycbcr <- identical(toupper(L$compression), "YCBCR JPEG")
    L$photometric <- if (!is.null(ifd) && !is.na(ifd$photometric)) {
      photometric_name(ifd$photometric)
    } else if (ycbcr) "YCbCr" else NA_character_
    if (L$encoding$codec == "jpeg" && length(ifd$jpeg_tables)) {
      L$encoding$jpeg_tables <- b64_encode(ifd$jpeg_tables)
    }
    L$compression <- NULL
    levels[[k + 1L]] <- L
  }
  byte_order <- tiff_byte_order(gdal_dsn)
  scale <- if (is.na(scale)) 1 else scale
  offset <- if (is.na(offset)) 0 else offset
  for (k in seq_along(levels)) {
    enc <- levels[[k]]$encoding
    enc$byte_order <- byte_order
    if (scale != 1) enc$scale <- scale
    if (offset != 0) enc$offset <- offset
    levels[[k]]$encoding <- enc
  }

  structure(list(
    dsn = dsn,
    url = if (local) file_url(dsn) else ref$http %||% dsn,
    local = local || startsWith(dsn, "/vsimem/"),
    crs = crs_ref(wkt),
    wkt = wkt,
    band = band,
    samples_per_pixel = nb,
    planar = planar,
    color_interp = color_interp,
    photometric = levels[[1]]$photometric,
    byte_order = byte_order,
    scale = scale,
    offset = offset,
    nodata = if (length(nodata) && !is.na(nodata)) nodata else if (is.nan(nodata)) NaN else NULL,
    levels = levels
  ), class = "aob_cog")
}

#' @export
print.aob_cog <- function(x, ...) {
  l0 <- x$levels[[1]]
  cat("<COG> ", x$dsn, "\n", sep = "")
  cat("  crs ", crs_label(x$crs), ", band ", x$band, " of ",
      x$samples_per_pixel, ", ", l0$encoding$dtype, " ", l0$encoding$codec,
      " (predictor ", l0$encoding$predictor, ")\n", sep = "")
  ci <- x$color_interp
  if (length(ci)) {
    cat("  colour ", paste(ifelse(is.na(ci), "?", ci), collapse = ", "),
        if (!is.na(x$photometric %||% NA)) paste0("; photometric ", x$photometric),
        if (!is.null(rgb_default(x))) "; drawn as a colour image", "\n", sep = "")
  }
  for (l in x$levels) {
    cat(sprintf("  level %d: %d x %d cells, %d x %d tiles, %d with bytes\n", l$level,
                l$dim[1], l$dim[2], l$tile_size[1], l$tile_size[2], nrow(l$tiles)))
  }
  invisible(x)
}

#' Plan the tiles of a COG for a view
#'
#' Builds a tile plan (scene spec 0.2): for each level, the tiles to draw,
#' where their bytes are, and a triangle mesh per tile already projected to
#' the view CRS with UVs in tile space. The renderer only fetches bytes,
#' decodes and draws.
#'
#' Each tile's mesh is a regular lattice over the tile's valid pixels,
#' projected with GDAL/PROJ. The lattice is refined until the triangles
#' stay within `tolerance` source pixels of the exact projection (measured
#' forward, at a `max_segments` lattice), so curvature is honoured where it
#' exists and a tile in the view CRS is two triangles. Every tile of a level
#' uses the same lattice (the densest any of its tiles needs), so
#' neighbouring tiles share their edge vertices. Tiles whose lattice
#' does not project (outside the view CRS's domain) are left out with a
#' warning.
#'
#' With `units_per_pixel`, the plan covers one view (`coverage = "view"`):
#' one level, chosen by `selection` from the levels' pixel sizes, and the
#' tiles whose footprint meets `extent`. Without it, the plan holds every
#' level (`coverage = "all_levels"`), up to `max_tiles` tiles in all, and the
#' renderer chooses a level as the view changes, using the `selection` rule.
#'
#' @param cog A COG from [cog_info()], or a path or URL passed to it.
#' @param crs The view CRS: an `"authority:code"` string or any definition
#'   [scene_crs()] accepts (WKT, a PROJ string, PROJJSON).
#' @param extent Optional `c(xmin, xmax, ymin, ymax)` in view CRS units.
#'   Tiles whose footprint misses it are left out. Required with
#'   `units_per_pixel`.
#' @param units_per_pixel Optional size of one device pixel in view CRS
#'   units, for a plan made for one view.
#' @param levels Optional level numbers to include in an `all_levels` plan
#'   (0 is full resolution).
#' @param selection `"coarsest_sufficient"` (the coarsest level whose pixel
#'   is no larger than a screen pixel) or `"nearest_pixel_size"`.
#' @param max_segments Densest mesh lattice per tile edge.
#' @param tolerance Allowed mesh error, in source pixels of the level.
#' @param max_stretch Tiles whose least stretched pixel is larger than this
#'   many times the level's typical (median) pixel in the view are left out,
#'   with a message. This drops the far polar cap from a polar view of
#'   global data (north of about 74N in EPSG:3031 at the default), where
#'   pixels grow without bound. `Inf` keeps every tile
#'   that projects.
#' @param max_tiles Tile budget for an `all_levels` plan. Levels are added
#'   coarse to fine, and a level that would take the plan past this many
#'   tiles is left out with its finer levels, with a warning; the coarsest
#'   level is always kept. The page carries a mesh for every planned tile
#'   (and, for a local COG, its bytes), so this bounds its size. Pass
#'   `extent` to plan part of a large COG at full resolution, or `Inf` for
#'   no budget.
#' @return A list of class `"aob_tile_plan"`: `plan` (the scene spec
#'   `tilePlan` object, with `mesh` data ids still to be named), `vertices`
#'   and `indices` (Arrow IPC stream bytes), `cog`, and `levels` (the
#'   planned levels, each with its tiles' mesh sizes).
#' @seealso [scene_add_tiled_raster()] to add a plan to a scene.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- system.file("extdata", "polar_lonlat.tif", package = "aobcore")
#' p <- cog_plan(f, "EPSG:3031")
#' p
#' # One view: 20 km per device pixel around the pole.
#' pv <- cog_plan(f, "EPSG:3031", extent = c(-2e6, 2e6, -2e6, 2e6), units_per_pixel = 20000)
#' pv$plan$levels[[1]]$level
cog_plan <- function(cog, crs = "EPSG:3031", extent = NULL, units_per_pixel = NULL,
                     levels = NULL, selection = c("coarsest_sufficient", "nearest_pixel_size"),
                     max_segments = 32L, tolerance = 0.25, max_stretch = 8,
                     max_tiles = 1024) {
  need_gdalraster("cog_plan()")
  selection <- match.arg(selection)
  if (!inherits(cog, "aob_cog")) cog <- cog_info(cog)
  crs <- scene_crs(crs)
  view_wkt <- crs_wkt(crs)
  if (!is.null(extent)) check_extent(extent)
  if (!is.null(units_per_pixel)) {
    if (!is.numeric(units_per_pixel) || length(units_per_pixel) != 1L || !(units_per_pixel > 0)) {
      stop("`units_per_pixel` must be a single positive number.", call. = FALSE)
    }
    if (is.null(extent)) stop("A plan for one view needs `extent`.", call. = FALSE)
  }
  if (!is.numeric(max_stretch) || length(max_stretch) != 1L || is.na(max_stretch) ||
      !(max_stretch > 1)) {
    stop("`max_stretch` must be a single number above 1 (or Inf).", call. = FALSE)
  }
  if (!is.numeric(max_tiles) || length(max_tiles) != 1L || is.na(max_tiles) ||
      !(max_tiles >= 1)) {
    stop("`max_tiles` must be a single number, 1 or more (or Inf).", call. = FALSE)
  }
  max_segments <- as.integer(max_segments)
  if (length(max_segments) != 1L || is.na(max_segments) || max_segments < 1L ||
      bitwAnd(max_segments, max_segments - 1L) != 0L) {
    stop("`max_segments` must be a power of two, 1 or more.", call. = FALSE)
  }
  same <- isTRUE(gdalraster::srs_is_same(cog$wkt, view_wkt))
  proj <- function(xy) {
    if (same) return(xy)
    suppressWarnings(gdalraster::transform_xy(xy, cog$wkt, view_wkt))
  }

  sizes <- vapply(cog$levels, level_pixel_size, 0, proj = proj)
  if (any(!is.finite(sizes))) {
    stop("Could not measure the pixel size of every level in ", crs_label(crs), "; ",
         "does the COG lie in the view CRS's domain?", call. = FALSE)
  }
  all_ids <- vapply(cog$levels, function(l) l$level, 0L)
  if (!is.null(units_per_pixel)) {
    pick <- select_level(sizes, units_per_pixel, selection)
    keep <- pick
  } else if (!is.null(levels)) {
    if (!is.numeric(levels) || anyNA(levels) || !all(levels %in% all_ids)) {
      stop("`levels` must be level numbers from 0 to ", max(all_ids), ".", call. = FALSE)
    }
    keep <- match(sort(unique(levels)), all_ids)
  } else {
    keep <- seq_along(cog$levels)
  }

  verts <- list()
  idx <- list()
  nv <- 0
  ni <- 0
  dropped <- 0L
  stretched <- 0L
  out_levels <- list()
  budget <- if (is.null(units_per_pixel)) max_tiles else Inf
  capped <- integer()
  for (k in rev(keep)) {     # coarse to fine, as a renderer loads them
    lv <- cog$levels[[k]]
    n_before <- sum(vapply(out_levels, function(l) length(l$tiles), 0L))
    ## Culling only removes tiles, so without an extent a level with too
    ## many tiles is left out before it is meshed.
    if (length(capped) || (length(out_levels) && is.null(extent) &&
                           n_before + nrow(lv$tiles) > budget)) {
      capped <- c(capped, lv$level)
      next
    }
    meshes <- level_meshes(lv, proj, same, max_segments, tolerance * sizes[k],
                           max_pixel = max_stretch * sizes[k])
    tiles <- list()
    lverts <- list()
    lidx <- list()
    lnv <- nv
    lni <- ni
    for (i in seq_along(meshes)) {
      m <- meshes[[i]]
      if (is.null(m)) next
      fp <- m$footprint
      if (!is.null(extent) && (fp[2] < extent[1] || fp[1] > extent[2] ||
                               fp[4] < extent[3] || fp[3] > extent[4])) next
      t <- lv$tiles[i, ]
      nvert <- nrow(m$position)
      lverts[[length(lverts) + 1L]] <- m
      lidx[[length(lidx) + 1L]] <- m$index
      tile <- list(
        col = as.integer(t$col), row = as.integer(t$row),
        byte_offset = t$byte_offset, byte_length = t$byte_length,
        size = as.integer(lv$tile_size),
        window = m$window,
        footprint = fp,
        mesh = list(first_vertex = lnv, vertex_count = nvert,
                    first_index = lni, index_count = length(m$index))
      )
      tiles[[length(tiles) + 1L]] <- tile[!vapply(tile, is.null, TRUE)]
      lnv <- lnv + nvert
      lni <- lni + length(m$index)
    }
    if (length(out_levels) && n_before + length(tiles) > budget) {
      capped <- c(capped, lv$level)
      next
    }
    stretched <- stretched + attr(meshes, "stretched")
    dropped <- dropped + sum(vapply(meshes, is.null, TRUE)) - attr(meshes, "stretched")
    verts <- c(verts, lverts)
    idx <- c(idx, lidx)
    nv <- lnv
    ni <- lni
    out_levels[[length(out_levels) + 1L]] <- list(
      level = lv$level,
      grid = drop_null(list(crs = cog$crs, extent = lv$extent,
                            dim = as.integer(lv$dim), nodata = nodata_json(cog$nodata))),
      pixel_size = sizes[k],
      encoding = lv$encoding,
      tiles = tiles
    )
  }
  if (length(capped)) {
    warning("Level", if (length(capped) > 1L) "s", " ", paste(sort(capped), collapse = ", "),
            " left out: the plan would pass `max_tiles` (", max_tiles, ") tiles. ",
            "Pass `extent` to plan part of the COG, or raise `max_tiles`.", call. = FALSE)
  }
  if (stretched > 0L) {
    message(stretched, " tile(s) stretched past ", max_stretch, " times their level's pixel ",
            "size in ", crs_label(crs), " were left out (see `max_stretch`).")
  }
  if (dropped > 0L) {
    warning(dropped, " tile(s) could not be projected to ", crs_label(crs), " and were left out.",
            call. = FALSE)
  }
  if (!is.null(units_per_pixel) && length(out_levels[[1]]$tiles) == 0L) {
    warning("No tile of level ", out_levels[[1]]$level, " meets the view extent.", call. = FALSE)
  }

  plan <- list(crs = crs)
  if (!is.null(units_per_pixel)) {
    plan$coverage <- "view"
    plan$planned_for <- list(extent = as.numeric(extent), units_per_pixel = units_per_pixel)
  } else {
    plan$coverage <- "all_levels"
    plan$selection <- list(rule = selection)
  }
  plan$mesh <- list(vertices = "vertices", indices = "indices")
  plan$levels <- out_levels

  structure(list(
    plan = plan,
    vertices = mesh_vertex_ipc(verts),
    indices = mesh_index_ipc(idx),
    cog = cog,
    n_vertices = nv,
    n_indices = ni
  ), class = "aob_tile_plan")
}

#' @export
print.aob_tile_plan <- function(x, ...) {
  p <- x$plan
  cat("<tile plan> ", p$coverage, " in ", crs_label(p$crs), ": ", sep = "")
  n <- vapply(p$levels, function(l) length(l$tiles), 0L)
  cat(sum(n), " tiles, ", x$n_vertices, " vertices, ", x$n_indices / 3, " triangles\n", sep = "")
  for (i in seq_along(p$levels)) {
    l <- p$levels[[i]]
    cat(sprintf("  level %d: %d tiles, pixel %.6g view units\n", l$level, n[i], l$pixel_size))
  }
  invisible(x)
}

#' Add a tiled COG layer to a scene
#'
#' Adds a scene spec `tiled_raster` layer: a `cog` data reference, the
#' plan's mesh tables as Arrow blobs, and the plan. The layer draws one band
#' through a palette, or three or four bands as a colour image (`rgb`).
#'
#' **Colour images.** By default (`rgb = NULL`) a COG with 3 or 4 Byte
#' bands whose colour interpretation is Red, Green, Blue (and Alpha) is
#' drawn as a colour image, unless `palette` is given. `rgb = FALSE` draws
#' one band through a palette; `rgb = TRUE` draws bands 1, 2 and 3 (and 4
#' as alpha when it is the alpha band) in colour; `rgb = c(r, g, b)` or
#' `c(r, g, b, a)` names the bands. A pixel is transparent where the alpha
#' band is 0 or all three colour bands equal the nodata value. Byte bands
#' are drawn as they are (0 to 255); other types need `range` (by default
#' the range of the coarsest level's values over the colour bands).
#'
#' **JPEG.** JPEG-compressed COGs (the usual case for imagery) are carried
#' with each level's shared JPEG tables. Only YCbCr (3 bands, GDAL's
#' default) and greyscale (`MinIsBlack`, 1 band) JPEG are allowed, since a
#' browser's JPEG decoder assumes one of those; others are refused.
#' A JPEG COG's internal mask (its no-data area) is not carried.
#'
#' **Version.** The scene is written as scene spec 0.2, or as 0.3 when the
#' layer uses a 0.3 feature (`rgb`, or JPEG tiles); see
#' [scene_spec_version()].
#'
#' **Transport.** The renderer reads tile bytes by HTTP range requests from
#' the `cog` URL. A page opened from `file://` cannot range-request a local
#' file, so for a local COG (`embed = TRUE`, the default when the COG is not
#' a URL) the bytes of every planned tile are also carried as blobs beside
#' the scene, one per tile, keyed `"<source id>@<byte_offset>+<byte_length>"`.
#' The renderer uses such a blob when it has one and fetches the range
#' otherwise. With `embed = TRUE` the `cog` reference is the file's base
#' name, a URL relative to the page, so a shared page does not reveal the
#' local directory (and works when the COG is served beside it). Give `url`
#' to write another reference. Embedding copies the planned tiles'
#' compressed bytes, so a page for a large local COG is large; plan fewer
#' levels or serve the COG over HTTP instead.
#'
#' With `embed = FALSE`, a local COG's tile bytes are not read. The file is
#' registered in the scene's `"files"` attribute instead, keyed by the
#' layer's source data id, with its normalized path, size, modification time
#' and whether `url` was given (`url_explicit`), for a server to deliver
#' (the scene JSON never sees the path). A page written to disk cannot read
#' it, so [write_scene_html()] warns. A `/vsimem/` COG cannot be served and
#' must be embedded.
#'
#' @param scene A scene from [scene()].
#' @param id The layer id; the data ids are `id`, `<id>_vertices` and
#'   `<id>_indices`.
#' @param plan A plan from [cog_plan()] in the scene's view CRS, or a COG
#'   (path, URL or [cog_info()] result) to plan with default settings.
#' @param palette A palette name the renderer knows (`"ocean"`, `"viridis"`,
#'   `"ice"`, `"gray"`). Giving it chooses the palette path when `rgb` is
#'   `NULL`.
#' @param range `c(low, high)` in scaled values (raw * scale + offset). With
#'   a palette, by default the range of the coarsest level's values. With
#'   `rgb`, the values drawn as zero and full intensity: not needed for Byte
#'   bands (where it stretches the image).
#' @param rgb `NULL` (colour when the COG's bands say Red, Green, Blue and
#'   no palette is given), `TRUE`, `FALSE`, or 3 or 4 band numbers (red,
#'   green, blue and optionally alpha). The `NULL` default looks at whether
#'   `palette` was given at all (with [missing()]), so a function that wraps
#'   this one and always passes `palette` on should pass `rgb` explicitly
#'   too. A band-interleaved RGB COG is drawn through a palette by default
#'   (with a message), since colour images need pixel-interleaved tiles.
#' @param embed Carry the planned tiles' bytes as blobs. Defaults to `TRUE`
#'   for a local COG and `FALSE` for a URL.
#' @param url Optional URL (absolute, or relative to the page) the scene gives
#'   the renderer for the COG. By default the COG's http(s) URL, its
#'   `file://` URL when neither embedded nor a URL, or its base name when
#'   embedded.
#' @param label Optional human-readable name.
#' @param visible Optional initial visibility.
#' @return The scene, now version 0.2 or 0.3, with the layer appended.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
#' s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", f, palette = "ocean")
#' s
#' names(scene_blobs(s))[1:4]
scene_add_tiled_raster <- function(scene, id, plan, palette = "viridis", range = NULL,
                                   rgb = NULL, embed = NULL, url = NULL, label = NULL,
                                   visible = NULL) {
  check_scene(scene)
  check_id(id)
  if (is.null(rgb) && !missing(palette)) rgb <- FALSE
  if (!inherits(plan, "aob_tile_plan")) plan <- cog_plan(plan, scene$view$crs)
  if (!crs_same(plan$plan$crs, scene$view$crs)) {
    stop("The plan is in ", crs_label(plan$plan$crs), " but the view is in ",
         crs_label(scene$view$crs), ".",
         call. = FALSE)
  }
  cog <- plan$cog
  ids <- c(id, paste0(id, c("_vertices", "_indices")))
  clash <- intersect(ids, names(scene$data))
  if (length(clash)) stop("Data id \"", clash[1], "\" is already in the scene.", call. = FALSE)
  if (id %in% vapply(scene$layers, function(l) l$id, "")) {
    stop("Layer id \"", id, "\" is already in the scene.", call. = FALSE)
  }
  if (!is.character(palette) || length(palette) != 1L || is.na(palette) || !nzchar(palette)) {
    stop("`palette` must be a single palette name.", call. = FALSE)
  }
  embed <- embed %||% cog$local
  if (isTRUE(embed) && !cog$local) {
    stop("Only a local COG can be embedded; this one is a URL.", call. = FALSE)
  }
  url_explicit <- !is.null(url)
  if (url_explicit) {
    if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
      stop("`url` must be a single URL.", call. = FALSE)
    }
  } else if (isTRUE(embed)) {
    url <- utils::URLencode(basename(cog$dsn), reserved = TRUE)
  } else if (startsWith(cog$dsn, "/vsimem/")) {
    stop("A /vsimem/ COG can only be embedded: the renderer cannot fetch \"", cog$dsn,
         "\" and a served file must be on disk. Use `embed = TRUE`, or write the COG to ",
         "a file.", call. = FALSE)
  } else if (grepl("^(https?|file)://", cog$url)) {
    url <- cog$url
  } else {
    stop("The renderer cannot fetch \"", cog$url, "\"; give the COG as an http(s) URL",
         if (cog$local) " or embed it", ".", call. = FALSE)
  }
  bands <- resolve_rgb(cog, rgb)
  p <- plan$plan
  check_layer_levels(p$levels, cog, id, bands)
  if (is.null(bands)) {
    range <- range %||% cog_value_range(cog)
  } else if (is.null(range) && !all(vapply(p$levels, function(l) l$encoding$dtype, "") == "uint8")) {
    range <- cog_value_range(cog, bands = bands[1:3])
  }
  if (!is.null(range) && (!is.numeric(range) || length(range) != 2L || anyNA(range) ||
                          range[1] == range[2])) {
    stop("`range` must be c(low, high) with low and high different.", call. = FALSE)
  }
  if (!is.null(bands)) {
    ## An rgb layer names its bands itself; encoding.band is the palette's.
    p$levels <- lapply(p$levels, function(l) {
      l$encoding$band <- NULL
      l
    })
  }

  scene$data[[ids[1]]] <- list(format = "cog", url = url)
  scene$data[[ids[2]]] <- list(format = "arrow-ipc-stream", blob = ids[2])
  scene$data[[ids[3]]] <- list(format = "arrow-ipc-stream", blob = ids[3])
  p$crs <- scene$view$crs
  p$mesh <- list(vertices = ids[2], indices = ids[3])
  layer <- list(
    id = id, kind = "tiled_raster",
    label = check_scalar(label, is.character, "label"),
    visible = check_scalar(visible, is.logical, "visible"),
    source = ids[1],
    plan = p
  )
  if (is.null(bands)) {
    layer$palette <- list(name = palette, range = as.numeric(range))
  } else {
    layer$rgb <- drop_null(list(
      bands = as.integer(bands[1:3]),
      alpha = if (length(bands) == 4L) as.integer(bands[4]),
      range = if (!is.null(range)) as.numeric(range)
    ))
  }
  scene$layers[[length(scene$layers) + 1L]] <- drop_null(layer)
  scene$version <- scene_spec_version(scene)

  blobs <- attr(scene, "blobs") %||% list()
  blobs[[ids[2]]] <- plan$vertices
  blobs[[ids[3]]] <- plan$indices
  if (isTRUE(embed)) blobs <- c(blobs, tile_blobs(cog, p, ids[1]))
  attr(scene, "blobs") <- blobs
  ## A local COG that is not embedded is registered for a server to deliver
  ## (decision 0006): the scene JSON keeps its url and never sees the path.
  if (!isTRUE(embed) && cog$local && !startsWith(cog$dsn, "/vsi")) {
    files <- attr(scene, "files") %||% list()
    files[[ids[1]]] <- file_record(cog$dsn, url_explicit)
    attr(scene, "files") <- files
  }
  scene
}

## A local file registered for serving: the normalized path, its size and
## modification time when registered (a tile plan holds byte offsets, so a
## server must refuse a file that changed), and whether the scene's `url`
## for it was given by the caller (left alone by a server) or defaulted to
## the file:// URL (replaced by the server's own route).
file_record <- function(path, url_explicit) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(path, extra_cols = FALSE)
  list(path = path, size = info$size, mtime = info$mtime, url_explicit = isTRUE(url_explicit))
}

#' Draw a COG in its own view, in one call
#'
#' `view_cog()` plans every level of a COG for a view CRS ([cog_plan()]),
#' builds a scene with the tiled raster ([scene_add_tiled_raster()]) and,
#' optionally, the bundled Natural Earth coastline south of 40S, and writes
#' a self-contained page ([write_scene_html()]). The renderer picks the level
#' as you zoom. `cog_scene()` is the same without writing the page. Both
#' require the 'gdalraster' package.
#'
#' A local COG's planned tile bytes are embedded in the page, so it opens
#' from disk with no server. A COG given by URL is read by the browser with
#' HTTP range requests (the server must allow them, and CORS).
#'
#' A COG with 3 or 4 Byte bands whose colour interpretation is Red, Green,
#' Blue (and Alpha), such as a rendered chart or aerial imagery, is drawn as
#' a colour image; giving `band` or `palette` (or `rgb = FALSE`) draws one
#' band through a palette instead.
#'
#' @param dsn A COG: local path or `http(s)` URL (also as `"/vsicurl/<url>"`).
#' @param crs The view CRS: an `"authority:code"` string or any definition
#'   [scene_crs()] accepts (WKT, a PROJ string, PROJJSON).
#' @param palette,range,rgb Passed to [scene_add_tiled_raster()]. Giving
#'   `palette` or `band` chooses the palette path when `rgb` is `NULL`;
#'   this is detected with [missing()], so a function that wraps these and
#'   always passes `palette` or `band` on should pass `rgb` explicitly too.
#' @param band The band to draw through the palette.
#' @param coastline Add the bundled coastline south of 40S (densified and
#'   projected with [gdal_vector_stream()]). By default only when `crs` is a
#'   south polar view (one centred on the South Pole, such as EPSG:3031 or
#'   EPSG:3976), since the coastline is of the far south.
#' @param extent Optional view `c(xmin, xmax, ymin, ymax)` in view CRS
#'   units: the initial view. Tiles whose footprint misses this extent,
#'   widened by half its size on each side, are left out of the plan, so
#'   the page shows nothing far beyond it. By default every tile is planned
#'   and the view is the full-resolution tiles' footprint, clipped to the
#'   view's domain. An `extent` that misses the domain widens it to cover
#'   the extent.
#' @param domain Passed to [scene()]: by default the CRS's [crs_domain()],
#'   which keeps the camera near the sensible part of the view CRS. Tiles
#'   outside it are still planned and drawn.
#' @param url Passed to [scene_add_tiled_raster()].
#' @param ... Passed to [cog_plan()] (`levels`, `selection`,
#'   `max_segments`, `tolerance`, `max_stretch`, `max_tiles`).
#' @return `cog_scene()`: a scene (spec 0.4 with a domain; without one 0.2,
#'   or 0.3 for a colour image or JPEG tiles) carrying its blobs.
#'   `view_cog()`: the path of the written page, invisibly.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- view_cog(system.file("extdata", "polar_3031.tif", package = "aobcore"),
#'               palette = "ocean", file = tempfile(fileext = ".html"))
#' file.size(f)
#' \dontrun{
#' utils::browseURL(f)
#' }
view_cog <- function(dsn, crs = "EPSG:3031", palette = "viridis", range = NULL, band = 1L,
                     coastline = NULL, extent = NULL, file = tempfile(fileext = ".html"),
                     title = NULL, theme = c("auto", "light", "dark"), url = NULL,
                     rgb = NULL, domain = getOption("aobcore.domain", TRUE), ...) {
  if (is.null(rgb) && (!missing(palette) || !missing(band))) rgb <- FALSE
  need_gdalraster("view_cog()")
  s <- cog_scene_(dsn, crs = crs, palette = palette, range = range, band = band,
                  coastline = coastline, extent = extent, url = url, rgb = rgb,
                  domain = domain, ...)
  write_scene_html(s, file = file, title = title %||% s$layers[[1]]$label, theme = theme)
}

#' @rdname view_cog
#' @param file Path of the HTML file to write.
#' @param title,theme Passed to [write_scene_html()].
#' @export
cog_scene <- function(dsn, crs = "EPSG:3031", palette = "viridis", range = NULL, band = 1L,
                      coastline = NULL, extent = NULL, url = NULL, rgb = NULL,
                      domain = getOption("aobcore.domain", TRUE), ...) {
  need_gdalraster("cog_scene()")
  if (is.null(rgb) && (!missing(palette) || !missing(band))) rgb <- FALSE
  cog_scene_(dsn, crs, palette, range, band, coastline, extent, url, rgb, domain = domain, ...)
}

## cog_scene() once `rgb` says whether a palette or band was given.
cog_scene_ <- function(dsn, crs, palette, range, band, coastline, extent, url, rgb,
                       domain = TRUE, ...) {
  crs <- scene_crs(crs)
  cog <- if (inherits(dsn, "aob_cog")) dsn else cog_info(dsn, band = band)
  if (is.null(rgb)) rgb <- rgb_default(cog, quiet = FALSE) %||% FALSE
  cull <- NULL
  if (!is.null(extent)) {
    check_extent(extent)
    pad <- c(-1, 1, -1, 1) * rep(c(diff(extent[1:2]), diff(extent[3:4])), each = 2) / 2
    cull <- extent + pad
  }
  plan <- cog_plan(cog, crs, extent = cull, ...)
  s <- scene(crs, domain = domain)
  s <- scene_add_tiled_raster(s, "cog", plan, palette = palette, range = range, rgb = rgb,
                              url = url, label = basename(cog$dsn))
  if (isTRUE(coastline %||% south_polar_view(crs))) {
    coast <- system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
    s <- scene_add_vector(s, "coast", gdal_vector_stream(coast, crs, densify = 0.25),
                          stroke = c(60, 66, 72, 255), stroke_width_px = 1,
                          label = "Coastline (50m)")
  }
  if (!is.null(extent)) {
    check_extent(extent)
    ## An explicit view outside the domain is what was asked for: widen the
    ## bounds to take it in rather than let the camera clamp away from it.
    b <- s$view$bounds
    if (!is.null(b) && !extents_overlap(extent, b)) {
      s$view$bounds <- c(min(b[1], extent[1]), max(b[2], extent[2]),
                         min(b[3], extent[3]), max(b[4], extent[4]))
    }
  } else {
    extent <- plan_extent(s$layers[[1]]$plan)
    b <- s$view$bounds
    if (!is.null(extent) && !is.null(b)) {
      clip <- c(max(extent[1], b[1]), min(extent[2], b[2]), max(extent[3], b[3]), min(extent[4], b[4]))
      extent <- if (clip[1] < clip[2] && clip[3] < clip[4]) clip else b
    }
  }
  if (!is.null(extent)) s$view$extent <- as.numeric(extent)
  s
}

## ---- internals -------------------------------------------------------------

## Is the view CRS centred on the South Pole? True when the pole projects
## to a finite point and points at 80S on four meridians lie at one distance
## from it (polar stereographic, azimuthal equal area and equidistant).
south_polar_view <- function(crs) {
  wkt <- tryCatch(gdalraster::srs_to_wkt(as.character(crs)), error = function(e) "")
  if (!nzchar(wkt)) return(FALSE)
  ll <- cbind(c(0, 0, 90, 180, -90), c(-90, -80, -80, -80, -80))
  xy <- tryCatch(suppressWarnings(gdalraster::transform_xy(ll, gdalraster::srs_to_wkt("EPSG:4326"), wkt)),
                 error = function(e) NULL)
  if (is.null(xy) || !all(is.finite(xy)) || max(abs(xy)) > 1e9) return(FALSE)
  d <- sqrt((xy[-1, 1] - xy[1, 1])^2 + (xy[-1, 2] - xy[1, 2])^2)
  max(d) > 0 && (max(d) - min(d)) < 1e-6 * max(d)
}

need_gdalraster <- function(what) {
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop(what, " needs the 'gdalraster' package.", call. = FALSE)
  }
}

drop_null <- function(x) x[!vapply(x, is.null, TRUE)]

open_raster <- function(dsn, open_options = NULL) {
  ds <- tryCatch(
    if (is.null(open_options)) {
      gdalraster::GDALRaster$new(dsn, TRUE)
    } else {
      gdalraster::GDALRaster$new(dsn, TRUE, open_options)
    },
    error = function(e) NULL
  )
  if (is.null(ds)) stop("GDAL cannot open \"", dsn, "\" as a raster.", call. = FALSE)
  try(ds$quiet <- TRUE, silent = TRUE)
  ds
}

check_band <- function(band, nb) {
  if (!is.numeric(band) || length(band) != 1L || is.na(band) || band != round(band) ||
      band < 1 || band > nb) {
    stop("`band` must be a band number from 1 to ", nb, ".", call. = FALSE)
  }
  as.integer(band)
}

extents_overlap <- function(a, b) a[1] < b[2] && a[2] > b[1] && a[3] < b[4] && a[4] > b[3]

check_extent <- function(extent) {
  if (!is.numeric(extent) || length(extent) != 4L || anyNA(extent) ||
      !(extent[1] < extent[2] && extent[3] < extent[4])) {
    stop("`extent` must be c(xmin, xmax, ymin, ymax) with xmin < xmax and ymin < ymax.",
         call. = FALSE)
  }
  invisible(extent)
}

## IMAGE_STRUCTURE items as a named character vector.
image_structure <- function(ds) {
  md <- ds$getMetadata(0L, "IMAGE_STRUCTURE")
  md <- md[nzchar(md) & grepl("=", md, fixed = TRUE)]
  stats::setNames(sub("^[^=]*=", "", md), toupper(sub("=.*$", "", md)))
}

md_item <- function(md, key) if (key %in% names(md)) md[[key]] else NULL

tiff_codecs <- c(NONE = "none", DEFLATE = "deflate", ADOBE_DEFLATE = "deflate", LZW = "lzw",
                 ZSTD = "zstd", LERC = "lerc", LERC_DEFLATE = "lerc_deflate",
                 LERC_ZSTD = "lerc_zstd", WEBP = "webp", PACKBITS = "packbits",
                 JPEG = "jpeg", "YCBCR JPEG" = "jpeg")

gdal_dtype <- function(name) {
  map <- c(Byte = "uint8", Int8 = "int8", UInt16 = "uint16", Int16 = "int16",
           UInt32 = "uint32", Int32 = "int32", Float32 = "float32", Float64 = "float64")
  if (!name %in% names(map)) {
    stop("GDAL data type ", name, " is not in scene spec 0.2 (", paste(names(map), collapse = ", "),
         ").", call. = FALSE)
  }
  map[[name]]
}

cog_level <- function(ds, k, band, dtype, planar, nb, struct0) {
  st <- image_structure(ds)
  comp <- toupper(md_item(st, "COMPRESSION") %||% md_item(struct0, "COMPRESSION") %||% "NONE")
  codec <- tiff_codecs[comp]
  if (is.na(codec)) {
    stop("Level ", k, " is compressed with ", comp, ", which the scene spec does not carry.",
         call. = FALSE)
  }
  pred <- md_item(st, "PREDICTOR") %||% md_item(struct0, "PREDICTOR") %||% "1"
  predictor <- c("1" = "none", "2" = "horizontal", "3" = "floating_point")[pred]
  if (is.na(predictor)) stop("Unknown TIFF predictor ", pred, ".", call. = FALSE)
  gt <- ds$getGeoTransform()
  if (gt[3] != 0 || gt[5] != 0) {
    stop("Level ", k, " has a rotated geotransform; only north-up grids are supported.",
         call. = FALSE)
  }
  dim <- ds$dim()[1:2]
  ts <- ds$getBlockSize(band)
  ncol <- ceiling(dim[1] / ts[1])
  nrow <- ceiling(dim[2] / ts[2])
  grid <- expand.grid(col = seq_len(ncol) - 1L, row = seq_len(nrow) - 1L)
  item <- function(what, c, r) {
    v <- ds$getMetadataItem(band, sprintf("%s_%d_%d", what, c, r), "TIFF")
    if (is.null(v) || !nzchar(v)) NA_real_ else as.numeric(v)
  }
  off <- mapply(item, "BLOCK_OFFSET", grid$col, grid$row)
  len <- mapply(item, "BLOCK_SIZE", grid$col, grid$row)
  if (all(is.na(off))) {
    stop("GDAL gives no tile offsets for level ", k, "; is this a tiled GeoTIFF?", call. = FALSE)
  }
  ok <- !is.na(off) & !is.na(len) & len > 0
  tiles <- data.frame(col = grid$col[ok], row = grid$row[ok],
                      byte_offset = unname(off[ok]), byte_length = unname(len[ok]))
  enc <- list(codec = unname(codec), predictor = unname(predictor), dtype = dtype,
              byte_order = "little", samples_per_pixel = as.integer(nb),
              planar = planar, band = as.integer(band))
  list(
    level = as.integer(k),
    dim = as.integer(dim),
    geotransform = gt,
    extent = c(gt[1], gt[1] + dim[1] * gt[2], gt[4] + dim[2] * gt[6], gt[4]),
    tile_size = as.integer(ts),
    compression = comp,
    encoding = enc,
    tiles = tiles
  )
}

tiff_byte_order <- function(dsn) {
  f <- gdalraster::VSIFile$new(dsn)
  on.exit(f$close())
  b <- f$read(2)
  if (identical(b, charToRaw("II"))) "little" else if (identical(b, charToRaw("MM"))) "big" else
    stop("\"", dsn, "\" does not start with a TIFF byte order mark.", call. = FALSE)
}

## An authority:code string when GDAL finds one, otherwise PROJJSON text
## marked to be written verbatim.
crs_ref <- function(wkt) {
  code <- tryCatch(suppressMessages(gdalraster::srs_find_epsg(wkt)), error = function(e) NULL)
  if (is.character(code) && length(code) == 1L && !is.na(code) &&
      grepl("^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$", code) &&
      isTRUE(gdalraster::srs_is_same(wkt, gdalraster::srs_to_wkt(code)))) {
    return(code)
  }
  structure(gdalraster::srs_to_projjson(wkt), class = "aob_json")
}

## How GDAL and the renderer each reach a dsn: `gdal`, the path GDAL
## opens, and `http`, the plain URL the browser fetches (NULL when there is
## none). A "/vsicurl/" prefix means nothing to a browser. /vsis3/,
## /vsigs/ and /vsiaz/ paths map to their public https URLs, which work in
## the browser only for public (unsigned) objects; GDAL keeps the /vsi path
## and its credentials.
dsn_ref <- function(dsn) {
  if (grepl("^https?://", dsn)) return(list(gdal = paste0("/vsicurl/", dsn), http = dsn))
  http <- NULL
  if (grepl("^/vsicurl/https?://", dsn)) {
    http <- sub("^/vsicurl/", "", dsn)
  } else if (startsWith(dsn, "/vsicurl?")) {
    for (p in strsplit(sub("^/vsicurl\\?", "", dsn), "&", fixed = TRUE)[[1]]) {
      if (startsWith(p, "url=")) http <- utils::URLdecode(sub("^url=", "", p))
    }
    if (!is.null(http) && !grepl("^https?://", http)) http <- NULL
  } else if (grepl("^/vsis3/[^/]+/.", dsn)) {
    key <- url_path(sub("^/vsis3/", "", dsn))
    endpoint <- vsi_config("AWS_S3_ENDPOINT")
    if (nzchar(endpoint)) {
      scheme <- if (toupper(vsi_config("AWS_HTTPS")) %in% c("NO", "FALSE", "OFF")) "http" else "https"
      endpoint <- sub("^https?://", "", sub("/+$", "", endpoint))
      http <- paste0(scheme, "://", endpoint, "/", key)
    } else {
      bucket <- sub("/.*$", "", key)
      ## Path style for dotted bucket names: the *.s3.amazonaws.com
      ## certificate covers one subdomain level only.
      http <- if (grepl(".", bucket, fixed = TRUE)) paste0("https://s3.amazonaws.com/", key) else
        paste0("https://", bucket, ".s3.amazonaws.com/", sub("^[^/]+/", "", key))
    }
  } else if (grepl("^/vsigs/[^/]+/.", dsn)) {
    http <- paste0("https://storage.googleapis.com/", url_path(sub("^/vsigs/", "", dsn)))
  } else if (grepl("^/vsiaz/[^/]+/.", dsn)) {
    account <- vsi_config("AZURE_STORAGE_ACCOUNT")
    if (nzchar(account)) {
      http <- paste0("https://", account, ".blob.core.windows.net/", url_path(sub("^/vsiaz/", "", dsn)))
    }
  }
  list(gdal = dsn, http = http)
}

## A GDAL configuration option (GDAL also reads it from the environment).
vsi_config <- function(key) {
  v <- tryCatch(gdalraster::get_config_option(key), error = function(e) "")
  if (!is.character(v) || length(v) != 1L || is.na(v)) "" else v
}

## A bucket/key path with each segment percent-encoded for a URL.
url_path <- function(path) {
  seg <- strsplit(path, "/", fixed = TRUE)[[1]]
  paste(vapply(seg, utils::URLencode, "", reserved = TRUE), collapse = "/")
}

file_url <- function(path) {
  path <- gsub("\\\\", "/", path)
  if (!startsWith(path, "/")) path <- paste0("/", path)
  enc <- vapply(strsplit(path, "/", fixed = TRUE)[[1]], utils::URLencode, "", reserved = TRUE)
  paste0("file://", paste(enc, collapse = "/"))
}

nodata_json <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.nan(x)) return("NaN")
  x
}

## Size of one source pixel of a level in view units: the median over the
## cells of a 17 x 17 lattice of the square root of each cell's projected
## area per pixel (cells that do not project are skipped). The median keeps
## a few hugely stretched cells (a global grid near the far pole of a polar
## view) from swamping the level's size.
level_pixel_size <- function(lv, proj) {
  n <- 16L
  f <- seq(0, 1, length.out = n + 1L)
  u <- rep(f * lv$dim[1], times = n + 1L)
  v <- rep(f * lv$dim[2], each = n + 1L)
  gt <- lv$geotransform
  xy <- proj(cbind(gt[1] + u * gt[2], gt[4] + v * gt[6]))
  x <- matrix(xy[, 1], n + 1L)
  y <- matrix(xy[, 2], n + 1L)
  i <- seq_len(n)
  area <- cell_area(x, y, i)
  ok <- is.finite(area)
  if (!any(ok)) return(NA_real_)
  cell_px <- prod(lv$dim) / n^2
  stats::median(sqrt(area[ok] / cell_px))
}

## Areas of the quads of a lattice (matrices indexed [u, v]).
cell_area <- function(x, y, i) {
  x00 <- x[i, i]; x10 <- x[i + 1L, i]; x01 <- x[i, i + 1L]; x11 <- x[i + 1L, i + 1L]
  y00 <- y[i, i]; y10 <- y[i + 1L, i]; y01 <- y[i, i + 1L]; y11 <- y[i + 1L, i + 1L]
  abs((x11 - x00) * (y01 - y10) - (x01 - x10) * (y11 - y00)) / 2
}

select_level <- function(sizes, upp, rule) {
  if (rule == "coarsest_sufficient") {
    ok <- which(sizes <= upp)
    if (length(ok)) ok[which.max(sizes[ok])] else which.min(sizes)
  } else {
    d <- abs(sizes - upp)
    best <- which(d == min(d))
    best[which.min(sizes[best])]
  }
}

## Meshes for every tile of a level. Returns a list parallel to lv$tiles,
## NULL where the tile does not project.
level_meshes <- function(lv, proj, same, n_max, tol, max_pixel = Inf) {
  gt <- lv$geotransform
  tw <- lv$tile_size[1]
  th <- lv$tile_size[2]
  nt <- nrow(lv$tiles)
  if (nt == 0L) return(list())
  vw <- pmin(tw, lv$dim[1] - lv$tiles$col * tw)
  vh <- pmin(th, lv$dim[2] - lv$tiles$row * th)
  n <- if (same) 1L else n_max
  f <- seq(0, 1, length.out = n + 1L)
  np <- (n + 1L)^2
  fu <- rep(f, times = n + 1L)
  fv <- rep(f, each = n + 1L)
  ## Pixel coordinates of every lattice point of every tile, then one
  ## projection call for the level.
  pu <- rep(vw, each = np) * fu
  pv <- rep(vh, each = np) * fv
  gu <- rep(lv$tiles$col * tw, each = np) + pu
  gv <- rep(lv$tiles$row * th, each = np) + pv
  xy <- proj(cbind(gt[1] + gu * gt[2], gt[4] + gv * gt[6]))
  lattice <- function(t, k) matrix(xy[(t - 1L) * np + seq_len(np), k], n + 1L)
  ok <- vapply(seq_len(nt), function(t) all(is.finite(xy[(t - 1L) * np + seq_len(np), ])), TRUE)
  ## Tiles whose least stretched pixel is still far larger than the level's
  ## typical pixel (a global grid's far polar cap in a polar view) are
  ## left out; a tile with any part near the view's domain is kept.
  stretched <- rep(FALSE, nt)
  if (!same && is.finite(max_pixel)) {
    i <- seq_len(n)
    for (t in which(ok)) {
      a <- cell_area(lattice(t, 1), lattice(t, 2), i)
      px <- sqrt(min(a) / (vw[t] * vh[t] / n^2))
      stretched[t] <- !is.finite(px) || px > max_pixel
    }
    ok <- ok & !stretched
  }
  ## One segment count for the whole level (the most any tile needs), so
  ## neighbouring tiles share edge vertices and no T-junction cracks open.
  s <- if (same || !any(ok)) 1L else {
    max(vapply(which(ok), function(t) lattice_segments(lattice(t, 1), lattice(t, 2), tol), 1L))
  }
  keep <- seq(1L, n + 1L, by = n %/% s)
  index <- lattice_index(s)
  out <- lapply(seq_len(nt), function(t) {
    if (!ok[t]) return(NULL)
    r <- (t - 1L) * np + seq_len(np)
    x <- lattice(t, 1)
    y <- lattice(t, 2)
    us <- matrix(pu[r], n + 1L)[keep, keep] / tw
    vs <- matrix(pv[r], n + 1L)[keep, keep] / th
    win <- if (vw[t] < tw || vh[t] < th) {
      list(x = 0L, y = 0L, width = as.integer(vw[t]), height = as.integer(vh[t]))
    }
    list(
      position = cbind(as.vector(x[keep, keep]), as.vector(y[keep, keep])),
      uv = cbind(as.vector(us), as.vector(vs)),
      index = index,
      footprint = c(min(x), max(x), min(y), max(y)),
      window = win
    )
  })
  attr(out, "stretched") <- sum(stretched)
  out
}

## Fewest segments per edge (a power of two up to the lattice's) whose
## triangles stay within tol of every lattice point. x, y are
## (n + 1) x (n + 1) matrices indexed [u, v].
lattice_segments <- function(x, y, tol) {
  n <- nrow(x) - 1L
  s <- 1L
  while (s < n) {
    step <- n %/% s
    i <- seq_len(n + 1L) - 1L
    i0 <- pmin(i %/% step, s - 1L) * step
    fx <- (i - i0) / step
    I0 <- i0 + 1L
    I1 <- i0 + step + 1L
    FU <- matrix(fx, n + 1L, n + 1L)
    FV <- matrix(fx, n + 1L, n + 1L, byrow = TRUE)
    interp <- function(m) {
      a <- m[I0, I0]; b <- m[I1, I0]; c <- m[I0, I1]; d <- m[I1, I1]
      lower <- FU + FV <= 1
      ifelse(lower, a + FU * (b - a) + FV * (c - a),
             d + (1 - FU) * (c - d) + (1 - FV) * (b - d))
    }
    err <- sqrt((interp(x) - x)^2 + (interp(y) - y)^2)
    if (max(err) <= tol) break
    s <- s * 2L
  }
  s
}

## Triangle indices of an s x s lattice, vertices ordered u fastest: each
## cell is a, c, b and b, c, d (split along the b-c diagonal).
lattice_index <- function(s) {
  cell <- expand.grid(i = seq_len(s) - 1L, j = seq_len(s) - 1L)
  a <- cell$j * (s + 1L) + cell$i
  b <- a + 1L
  c <- a + s + 1L
  d <- c + 1L
  as.integer(rbind(a, c, b, b, c, d))
}

le_bytes <- function(x, type) {
  if (type == "float") writeBin(as.double(x), raw(), size = 4L, endian = "little")
  else writeBin(as.integer(x), raw(), size = 4L, endian = "little")
}

prim_array <- function(schema, bytes, n) {
  nanoarrow::nanoarrow_array_modify(
    nanoarrow::nanoarrow_array_init(schema),
    list(length = n, null_count = 0L, buffers = list(NULL, nanoarrow::as_nanoarrow_buffer(bytes)))
  )
}

xy_array <- function(m) {
  child <- prim_array(nanoarrow::na_float(), le_bytes(t(m), "float"), length(m))
  nanoarrow::nanoarrow_array_modify(
    nanoarrow::nanoarrow_array_init(nanoarrow::na_fixed_size_list(nanoarrow::na_float(), 2L)),
    list(length = nrow(m), null_count = 0L, children = list(child))
  )
}

table_ipc <- function(cols, n) {
  schema <- nanoarrow::na_struct(lapply(cols, nanoarrow::infer_nanoarrow_schema))
  arr <- nanoarrow::nanoarrow_array_modify(nanoarrow::nanoarrow_array_init(schema),
                                           list(length = n, null_count = 0L, children = cols))
  ipc_bytes(nanoarrow::basic_array_stream(list(arr), schema = schema))
}

## One vertex table (position and uv, FixedSizeList<float32, 2>) and one
## index table (uint32, counted from each tile's first vertex).
mesh_vertex_ipc <- function(meshes) {
  pos <- do.call(rbind, c(list(matrix(numeric(), 0, 2)), lapply(meshes, `[[`, "position")))
  uv <- do.call(rbind, c(list(matrix(numeric(), 0, 2)), lapply(meshes, `[[`, "uv")))
  table_ipc(list(position = xy_array(pos), uv = xy_array(uv)), nrow(pos))
}

mesh_index_ipc <- function(idx) {
  i <- unlist(idx, use.names = FALSE) %||% integer()
  table_ipc(list(index = prim_array(nanoarrow::na_uint32(), le_bytes(i, "int"), length(i))),
            length(i))
}

## The bytes of every planned tile, keyed "<source>@<offset>+<length>".
tile_blobs <- function(cog, plan, source) {
  ranges <- do.call(rbind, lapply(plan$levels, function(l) {
    if (!length(l$tiles)) return(NULL)
    data.frame(o = vapply(l$tiles, function(t) t$byte_offset, 0),
               n = vapply(l$tiles, function(t) t$byte_length, 0))
  }))
  if (is.null(ranges) || !nrow(ranges)) return(list())
  ranges <- unique(ranges)
  f <- gdalraster::VSIFile$new(cog$dsn)
  on.exit(f$close())
  out <- lapply(seq_len(nrow(ranges)), function(i) {
    f$seek(ranges$o[i], "SEEK_SET")
    b <- f$read(ranges$n[i])
    if (length(b) != ranges$n[i]) stop("Could not read tile bytes from ", cog$dsn, call. = FALSE)
    b
  })
  names(out) <- tile_blob_key(source, ranges$o, ranges$n)
  out
}

tile_blob_key <- function(source, offset, length) {
  paste0(source, "@", format(offset, scientific = FALSE, trim = TRUE), "+",
         format(length, scientific = FALSE, trim = TRUE))
}

## Range of scaled values in the coarsest level, read through GDAL, over
## `bands` (by default the COG's chosen band).
cog_value_range <- function(cog, bands = cog$band) {
  k <- length(cog$levels) - 1L
  gdal_dsn <- dsn_ref(cog$dsn)$gdal
  ds <- if (k == 0L) open_raster(gdal_dsn) else open_raster(gdal_dsn, paste0("OVERVIEW_LEVEL=", k - 1L))
  on.exit(ds$close())
  d <- ds$dim()
  v <- unlist(lapply(unique(bands), function(b) ds$read(b, 0L, 0L, d[1], d[2], d[1], d[2])))
  if (!is.null(cog$nodata) && !is.nan(cog$nodata)) v[v == cog$nodata] <- NA
  v <- v[is.finite(v)]
  if (!length(v)) return(c(0, 1))
  r <- signif(range(v) * cog$scale + cog$offset, 7)
  if (r[1] == r[2]) r + c(-0.5, 0.5) else r
}

#' The extent of a tile plan
#'
#' The union of the footprints of the finest level's tiles, in view CRS
#' units: a default initial view (`view$extent`) for a scene built from the
#' plan. [cog_scene()] uses it, clipped to the view's domain.
#'
#' @param plan A tile plan from [cog_plan()].
#' @return `c(xmin, xmax, ymin, ymax)`, or `NULL` when the plan has no tiles.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- system.file("extdata", "polar_lonlat.tif", package = "aobcore")
#' plan_extent(cog_plan(f, "EPSG:3031", levels = 1L))
plan_extent <- function(plan) {
  if (inherits(plan, "aob_tile_plan")) plan <- plan$plan
  if (!is.list(plan) || !is.list(plan$levels) || !length(plan$levels)) {
    stop("`plan` must be a tile plan from cog_plan().", call. = FALSE)
  }
  lv <- plan$levels[[length(plan$levels)]]
  fp <- do.call(rbind, lapply(lv$tiles, `[[`, "footprint"))
  if (is.null(fp)) return(NULL)
  c(min(fp[, 1]), max(fp[, 2]), min(fp[, 3]), max(fp[, 4]))
}

## The colour bands a COG is drawn with by default: c(r, g, b) or
## c(r, g, b, a) when it has 3 or 4 pixel-interleaved Byte bands whose
## colour interpretation is Red, Green, Blue (and Alpha) in that order; else
## NULL. A band-interleaved RGB COG falls back to the palette path (with a
## message when `quiet` is FALSE), since scene spec 0.3 draws colour images
## from interleaved tiles only.
rgb_default <- function(cog, quiet = TRUE) {
  ci <- cog$color_interp
  nb <- cog$samples_per_pixel
  dtype <- cog$levels[[1]]$encoding$dtype
  if (is.null(ci) || !nb %in% 3:4 || !identical(dtype, "uint8")) return(NULL)
  want <- c("Red", "Green", "Blue", "Alpha")[seq_len(nb)]
  if (!identical(unname(ci), want)) return(NULL)
  if (!identical(cog$planar, "interleaved")) {
    if (!quiet) {
      message("This RGB COG stores its bands separately (INTERLEAVE=BAND); drawing band ",
              cog$band, " through a palette. Rewrite it with INTERLEAVE=PIXEL to draw it ",
              "in colour.")
    }
    return(NULL)
  }
  seq_len(nb)
}

## The rgb argument of scene_add_tiled_raster() as band numbers (3 or 4),
## or NULL for the palette path.
resolve_rgb <- function(cog, rgb) {
  nb <- cog$samples_per_pixel
  if (is.null(rgb)) return(rgb_default(cog, quiet = FALSE))
  if (isFALSE(rgb)) return(NULL)
  if (isTRUE(rgb)) {
    if (nb < 3L) {
      stop("`rgb = TRUE` needs 3 or 4 bands; this COG has ", nb, ".", call. = FALSE)
    }
    b <- rgb_default(cog)
    if (!is.null(b)) return(b)
    ## A band-interleaved RGB COG gets here and is refused, with advice, by
    ## check_layer_levels().
    alpha <- nb >= 4L && identical(cog$color_interp[4], "Alpha")
    return(if (alpha) 1:4 else 1:3)
  }
  if (!is.numeric(rgb) || !length(rgb) %in% 3:4 || anyNA(rgb) || any(rgb != round(rgb)) ||
      any(rgb < 1 | rgb > nb)) {
    stop("`rgb` must be NULL, TRUE, FALSE, or 3 or 4 band numbers from 1 to ", nb, ".",
         call. = FALSE)
  }
  if (length(rgb) == 4L && rgb[4] %in% rgb[1:3]) {
    stop("The alpha band in `rgb` must not also be a colour band.", call. = FALSE)
  }
  as.integer(rgb)
}

## What a layer's levels must satisfy beyond the codec: colour images need
## interleaved samples, and JPEG tiles need YCbCr (3 bands) or MinIsBlack
## (1 band), interleaved, since a browser's JPEG decoder assumes one of
## those. The photometric interpretation of each level is in the COG
## (cog_info()), matched by level number.
check_layer_levels <- function(levels, cog, id, bands) {
  fail <- function(...) stop("Layer \"", id, "\": ", ..., call. = FALSE)
  photometric <- function(k) {
    for (l in cog$levels) if (identical(l$level, k)) return(l$photometric %||% NA_character_)
    NA_character_
  }
  for (l in levels) {
    enc <- l$encoding
    spp <- enc$samples_per_pixel %||% 1L
    interleaved <- identical(enc$planar %||% "interleaved", "interleaved")
    if (!is.null(bands) && !interleaved) {
      fail("level ", l$level, " stores its bands separately; a colour image needs pixel ",
           "interleaved bands (write the COG with INTERLEAVE=PIXEL).")
    }
    if (!identical(enc$codec, "jpeg")) next
    ph <- photometric(l$level)
    ok <- (spp == 3L && identical(ph, "YCbCr")) || (spp == 1L && identical(ph, "MinIsBlack"))
    if (!ok) {
      fail("level ", l$level, " is JPEG with ", spp, " band", if (spp != 1L) "s",
           " and photometric ", if (is.na(ph)) "unknown" else ph, "; only YCbCr JPEG ",
           "(3 bands, GDAL's default) and greyscale MinIsBlack JPEG (1 band) can be drawn. ",
           "Rewrite the COG, for example with GDAL's COG driver and COMPRESS=JPEG.")
    }
    if (!interleaved) {
      fail("level ", l$level, " is JPEG with separate bands; only interleaved JPEG can be drawn.")
    }
  }
  invisible()
}
