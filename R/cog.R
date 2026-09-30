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
#' the file's first two bytes.
#'
#' @param dsn A local path to a COG, an `http(s)` URL (read with GDAL's
#'   `/vsicurl/`; a `"/vsicurl/https://..."` path is the same URL), or any
#'   GDAL `/vsi` path. The renderer fetches tiles itself, so `/vsis3/`,
#'   `/vsigs/` and `/vsiaz/` paths are given to it as their public https
#'   URLs (honouring `AWS_S3_ENDPOINT`, `AWS_HTTPS` and
#'   `AZURE_STORAGE_ACCOUNT`); that works for public objects only.
#' @param band The band to draw, 1-based.
#' @return A list of class `"aob_cog"`: `dsn`, `url` (the reference a scene
#'   gives the renderer), `local` (whether the bytes can be read here for
#'   embedding), `crs` (an `"authority:code"` string when GDAL finds one,
#'   else PROJJSON text), `wkt`, `band`, `samples_per_pixel`, `planar`,
#'   `byte_order`, `scale`, `offset`, `nodata`, and `levels`: one list per
#'   level with `level` (0 is full resolution), `dim`, `geotransform`,
#'   `extent` (`c(xmin, xmax, ymin, ymax)`), `tile_size`, `encoding` and
#'   `tiles` (a data frame of `col`, `row`, `byte_offset`, `byte_length`;
#'   sparse tiles with no bytes are left out).
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
  for (k in 0:n_ov) {
    lv <- if (k == 0L) ds else open_raster(gdal_dsn, paste0("OVERVIEW_LEVEL=", k - 1L))
    levels[[k + 1L]] <- cog_level(lv, k, band, dtype, planar, nb, struct0)
    if (k > 0L) lv$close()
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
  cat("  crs ", if (nchar(x$crs) > 40) "PROJJSON" else x$crs, ", band ", x$band, " of ",
      x$samples_per_pixel, ", ", l0$encoding$dtype, " ", l0$encoding$codec,
      " (predictor ", l0$encoding$predictor, ")\n", sep = "")
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
#' level (`coverage = "all_levels"`) and the renderer chooses a level as the
#' view changes, using the `selection` rule.
#'
#' @param cog A COG from [cog_info()], or a path or URL passed to it.
#' @param crs The view CRS, an `"authority:code"` string.
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
                     max_segments = 32L, tolerance = 0.25) {
  need_gdalraster("cog_plan()")
  selection <- match.arg(selection)
  if (!inherits(cog, "aob_cog")) cog <- cog_info(cog)
  check_crs_string(crs)
  view_wkt <- tryCatch(gdalraster::srs_to_wkt(crs), error = function(e) "")
  if (!nzchar(view_wkt)) {
    stop("GDAL cannot resolve the CRS \"", crs, "\" (is its PROJ database installed?).",
         call. = FALSE)
  }
  if (!is.null(extent)) check_extent(extent)
  if (!is.null(units_per_pixel)) {
    if (!is.numeric(units_per_pixel) || length(units_per_pixel) != 1L || !(units_per_pixel > 0)) {
      stop("`units_per_pixel` must be a single positive number.", call. = FALSE)
    }
    if (is.null(extent)) stop("A plan for one view needs `extent`.", call. = FALSE)
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
    stop("Could not measure the pixel size of every level in ", crs, "; ",
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
  out_levels <- list()
  for (k in rev(keep)) {     # coarse to fine, as a renderer loads them
    lv <- cog$levels[[k]]
    meshes <- level_meshes(lv, proj, same, max_segments, tolerance * sizes[k])
    dropped <- dropped + sum(vapply(meshes, is.null, TRUE))
    tiles <- list()
    for (i in seq_along(meshes)) {
      m <- meshes[[i]]
      if (is.null(m)) next
      fp <- m$footprint
      if (!is.null(extent) && (fp[2] < extent[1] || fp[1] > extent[2] ||
                               fp[4] < extent[3] || fp[3] > extent[4])) next
      t <- lv$tiles[i, ]
      nvert <- nrow(m$position)
      verts[[length(verts) + 1L]] <- m
      idx[[length(idx) + 1L]] <- m$index
      tile <- list(
        col = as.integer(t$col), row = as.integer(t$row),
        byte_offset = t$byte_offset, byte_length = t$byte_length,
        size = as.integer(lv$tile_size),
        window = m$window,
        footprint = fp,
        mesh = list(first_vertex = nv, vertex_count = nvert,
                    first_index = ni, index_count = length(m$index))
      )
      tiles[[length(tiles) + 1L]] <- tile[!vapply(tile, is.null, TRUE)]
      nv <- nv + nvert
      ni <- ni + length(m$index)
    }
    out_levels[[length(out_levels) + 1L]] <- list(
      level = lv$level,
      grid = drop_null(list(crs = cog$crs, extent = lv$extent,
                            dim = as.integer(lv$dim), nodata = nodata_json(cog$nodata))),
      pixel_size = sizes[k],
      encoding = lv$encoding,
      tiles = tiles
    )
  }
  if (dropped > 0L) {
    warning(dropped, " tile(s) could not be projected to ", crs, " and were left out.",
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
  cat("<tile plan> ", p$coverage, " in ", p$crs, ": ", sep = "")
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
#' Adds a scene spec 0.2 `tiled_raster` layer: a `cog` data reference, the
#' plan's mesh tables as Arrow blobs, and the plan. The scene becomes a 0.2
#' scene.
#'
#' **Transport.** The renderer reads tile bytes by HTTP range requests from
#' the `cog` URL. A page opened from `file://` cannot range-request a local
#' file, so for a local COG (`embed = TRUE`, the default when the COG is not
#' a URL) the bytes of every planned tile are also carried as blobs beside
#' the scene, one per tile, keyed `"<source id>@<byte_offset>+<byte_length>"`.
#' The renderer uses such a blob when it has one and fetches the range
#' otherwise. The `cog` reference still names the source file (as a
#' `file://` URL). Embedding copies the planned tiles' compressed bytes, so a
#' page for a large local COG is large; plan fewer levels or serve the COG
#' over HTTP instead.
#'
#' @param scene A scene from [scene()].
#' @param id The layer id; the data ids are `id`, `<id>_vertices` and
#'   `<id>_indices`.
#' @param plan A plan from [cog_plan()] in the scene's view CRS, or a COG
#'   (path, URL or [cog_info()] result) to plan with default settings.
#' @param palette A palette name the renderer knows (`"ocean"`, `"viridis"`,
#'   `"ice"`, `"gray"`).
#' @param range `c(low, high)` in scaled values (raw * scale + offset). By
#'   default the range of the coarsest level's values.
#' @param embed Carry the planned tiles' bytes as blobs. Defaults to `TRUE`
#'   for a local COG and `FALSE` for a URL.
#' @param label Optional human-readable name.
#' @param visible Optional initial visibility.
#' @return The scene, now version 0.2, with the layer appended.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
#' s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", f, palette = "ocean")
#' s
#' names(scene_blobs(s))[1:4]
scene_add_tiled_raster <- function(scene, id, plan, palette = "viridis", range = NULL,
                                   embed = NULL, label = NULL, visible = NULL) {
  check_scene(scene)
  check_id(id)
  if (!inherits(plan, "aob_tile_plan")) plan <- cog_plan(plan, scene$view$crs)
  if (!identical(plan$plan$crs, scene$view$crs)) {
    stop("The plan is in ", plan$plan$crs, " but the view is in ", scene$view$crs, ".",
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
  if (!isTRUE(embed) && !grepl("^(https?|file)://", cog$url)) {
    stop("The renderer cannot fetch \"", cog$url, "\"; give the COG as an http(s) URL",
         if (cog$local) " or embed it", ".", call. = FALSE)
  }
  range <- range %||% cog_value_range(cog)
  if (!is.numeric(range) || length(range) != 2L || anyNA(range)) {
    stop("`range` must be c(low, high).", call. = FALSE)
  }

  scene$version <- "0.2"
  scene$data[[ids[1]]] <- list(format = "cog", url = cog$url)
  scene$data[[ids[2]]] <- list(format = "arrow-ipc-stream", blob = ids[2])
  scene$data[[ids[3]]] <- list(format = "arrow-ipc-stream", blob = ids[3])
  p <- plan$plan
  p$mesh <- list(vertices = ids[2], indices = ids[3])
  layer <- list(
    id = id, kind = "tiled_raster",
    label = check_scalar(label, is.character, "label"),
    visible = check_scalar(visible, is.logical, "visible"),
    source = ids[1],
    plan = p,
    palette = list(name = palette, range = as.numeric(range))
  )
  scene$layers[[length(scene$layers) + 1L]] <- drop_null(layer)

  blobs <- attr(scene, "blobs") %||% list()
  blobs[[ids[2]]] <- plan$vertices
  blobs[[ids[3]]] <- plan$indices
  if (isTRUE(embed)) blobs <- c(blobs, tile_blobs(cog, p, ids[1]))
  attr(scene, "blobs") <- blobs
  scene
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
#' @param dsn A COG: local path or `http(s)` URL (also as `"/vsicurl/<url>"`).
#' @param crs The view CRS.
#' @param palette,range Passed to [scene_add_tiled_raster()].
#' @param band The band to draw.
#' @param coastline Add the bundled coastline south of 40S (densified and
#'   projected with [gdal_vector_stream()]).
#' @param extent Optional initial view `c(xmin, xmax, ymin, ymax)` in view
#'   CRS units. By default the full-resolution tiles' footprint.
#' @param ... Passed to [cog_plan()] (`levels`, `selection`,
#'   `max_segments`, `tolerance`).
#' @return `cog_scene()`: a scene (spec 0.2) carrying its blobs.
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
                     coastline = TRUE, extent = NULL, file = tempfile(fileext = ".html"),
                     title = NULL, theme = c("auto", "light", "dark"), ...) {
  s <- cog_scene(dsn, crs = crs, palette = palette, range = range, band = band,
                 coastline = coastline, extent = extent, ...)
  write_scene_html(s, file = file, title = title %||% s$layers[[1]]$label, theme = theme)
}

#' @rdname view_cog
#' @param file Path of the HTML file to write.
#' @param title,theme Passed to [write_scene_html()].
#' @export
cog_scene <- function(dsn, crs = "EPSG:3031", palette = "viridis", range = NULL, band = 1L,
                      coastline = TRUE, extent = NULL, ...) {
  need_gdalraster("cog_scene()")
  cog <- if (inherits(dsn, "aob_cog")) dsn else cog_info(dsn, band = band)
  plan <- cog_plan(cog, crs, ...)
  s <- scene(crs)
  s <- scene_add_tiled_raster(s, "cog", plan, palette = palette, range = range,
                              label = basename(cog$dsn))
  if (isTRUE(coastline)) {
    coast <- system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
    s <- scene_add_vector(s, "coast", gdal_vector_stream(coast, crs, densify = 0.25),
                          stroke = c(60, 66, 72, 255), stroke_width_px = 1,
                          label = "Coastline (50m)")
  }
  if (!is.null(extent)) {
    check_extent(extent)
  } else {
    extent <- plan_extent(s$layers[[1]]$plan)
  }
  if (!is.null(extent)) s$view$extent <- as.numeric(extent)
  s
}

## ---- internals -------------------------------------------------------------

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
                 LERC_ZSTD = "lerc_zstd", WEBP = "webp", PACKBITS = "packbits")

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
    stop("Level ", k, " is compressed with ", comp, ", which scene spec 0.2 does not carry",
         if (comp %in% c("JPEG", "YCBCR")) " (JPEG keeps shared tables in the TIFF header)",
         ".", call. = FALSE)
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
  code <- tryCatch(gdalraster::srs_find_epsg(wkt), error = function(e) NULL)
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
    kv <- strsplit(strsplit(sub("^/vsicurl\\?", "", dsn), "&", fixed = TRUE)[[1]], "=")
    for (p in kv) {
      if (length(p) == 2L && p[1] == "url") http <- utils::URLdecode(p[2])
    }
    if (!is.null(http) && !grepl("^https?://", http)) http <- NULL
  } else if (grepl("^/vsis3/[^/]+/.", dsn)) {
    key <- sub("^/vsis3/", "", dsn)
    endpoint <- vsi_config("AWS_S3_ENDPOINT")
    if (nzchar(endpoint)) {
      scheme <- if (toupper(vsi_config("AWS_HTTPS")) %in% c("NO", "FALSE", "OFF")) "http" else "https"
      endpoint <- sub("^https?://", "", sub("/+$", "", endpoint))
      http <- paste0(scheme, "://", endpoint, "/", key)
    } else {
      bucket <- sub("/.*$", "", key)
      http <- paste0("https://", bucket, ".s3.amazonaws.com/", sub("^[^/]+/", "", key))
    }
  } else if (grepl("^/vsigs/[^/]+/.", dsn)) {
    http <- paste0("https://storage.googleapis.com/", sub("^/vsigs/", "", dsn))
  } else if (grepl("^/vsiaz/[^/]+/.", dsn)) {
    account <- vsi_config("AZURE_STORAGE_ACCOUNT")
    if (nzchar(account)) {
      http <- paste0("https://", account, ".blob.core.windows.net/", sub("^/vsiaz/", "", dsn))
    }
  }
  list(gdal = dsn, http = http)
}

## A GDAL configuration option, falling back to the environment.
vsi_config <- function(key) {
  v <- tryCatch(gdalraster::get_config_option(key), error = function(e) "")
  if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) v <- Sys.getenv(key)
  v
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

## Size of one source pixel of a level in view units: the square root of
## the projected area of the level's grid per pixel, measured on a 17 x 17
## lattice (cells that do not project are skipped).
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
  sqrt(sum(area[ok]) / (sum(ok) * cell_px))
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
level_meshes <- function(lv, proj, same, n_max, tol) {
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
  ## One segment count for the whole level (the most any tile needs), so
  ## neighbouring tiles share edge vertices and no T-junction cracks open.
  s <- if (same || !any(ok)) 1L else {
    max(vapply(which(ok), function(t) lattice_segments(lattice(t, 1), lattice(t, 2), tol), 1L))
  }
  keep <- seq(1L, n + 1L, by = n %/% s)
  index <- lattice_index(s)
  lapply(seq_len(nt), function(t) {
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

## Range of scaled values in the coarsest level, read through GDAL.
cog_value_range <- function(cog) {
  k <- length(cog$levels) - 1L
  gdal_dsn <- dsn_ref(cog$dsn)$gdal
  ds <- if (k == 0L) open_raster(gdal_dsn) else open_raster(gdal_dsn, paste0("OVERVIEW_LEVEL=", k - 1L))
  on.exit(ds$close())
  d <- ds$dim()
  v <- ds$read(cog$band, 0L, 0L, d[1], d[2], d[1], d[2])
  if (!is.null(cog$nodata) && !is.nan(cog$nodata)) v[v == cog$nodata] <- NA
  v <- v[is.finite(v)]
  if (!length(v)) return(c(0, 1))
  r <- signif(range(v) * cog$scale + cog$offset, 7)
  if (r[1] == r[2]) r + c(-0.5, 0.5) else r
}

## Union of the finest level's tile footprints.
plan_extent <- function(plan) {
  lv <- plan$levels[[length(plan$levels)]]
  fp <- do.call(rbind, lapply(lv$tiles, `[[`, "footprint"))
  if (is.null(fp)) return(NULL)
  c(min(fp[, 1]), max(fp[, 2]), min(fp[, 3]), max(fp[, 4]))
}
