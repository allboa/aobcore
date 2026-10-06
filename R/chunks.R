#' Chunk references for a COG
#'
#' Describes a tiled GeoTIFF (a COG) as a scene spec 0.6 `chunks` data
#' reference: its grid at every level, the codec chain that decodes one
#' tile, and a ref (`level`, `col`, `row`, `offset`, `length`) for every
#' tile GDAL says is stored. A renderer then reads and decodes each tile
#' without reading the TIFF itself. Requires the 'gdalraster' package.
#'
#' The structure comes from [cog_info()]: each level's grid and tile size,
#' and each tile's byte range as GDAL reads it from the TIFF's TileOffsets
#' and TileByteCounts (the `BLOCK_OFFSET_<col>_<row>` and
#' `BLOCK_SIZE_<col>_<row>` items of its `TIFF` metadata domain). A tile
#' that is not stored (a sparse COG) has no ref, which in scene spec 0.6
#' means every cell in it is no data. Nothing is decoded.
#'
#' **Codecs.** The chain is `bytes` (with the file's byte order), then the
#' TIFF predictor as `predictor` (`horizontal` for predictor 2,
#' `floating_point` for 3), then the compression: DEFLATE (and
#' ADOBE_DEFLATE) is `deflate`, ZSTD `zstd`, LZW `lzw`, and no compression
#' adds nothing. JPEG is the whole chain, `jpeg` with the file's shared
#' JPEGTables as `tables`, for YCbCr (3 bands) or greyscale (`MinIsBlack`,
#' 1 band) JPEG only. Other compressions (PACKBITS, LERC, WEBP) have no
#' scene spec 0.6 codec and are an error: draw such a COG as a `cog`
#' source instead. Every level must have the same compression, predictor
#' and (JPEG) tables, since one chain decodes every chunk.
#'
#' **Bands.** A COG with pixel-interleaved bands (`INTERLEAVE=PIXEL`) is
#' `interleave = "pixel"`: each chunk holds every band. One with bands
#' stored separately (`INTERLEAVE=BAND`) is `interleave = "separate"`, with a
#' ref per band for every stored tile, so its tile ranges are read for
#' every band.
#'
#' **Grid.** Level 0 is the full-resolution image (its geotransform, size
#' and tile size); each overview is a coarser level with GDAL's own
#' geotransform and size for it (and its tile size when that differs). The
#' CRS is the COG's (`cog$crs`). Rotated grids are refused by
#' [cog_info()].
#'
#' @param cog A COG from [cog_info()], or a path or URL passed to it.
#' @param url The URL of the COG's bytes that the reference gives (the
#'   default url of every ref): absolute, or relative to the page. By
#'   default the COG's `url` from [cog_info()], which for a local file is
#'   its `file://` URL; a page opened from disk cannot range-request that,
#'   so give a URL the page can reach, or add the layer with
#'   [scene_add_tiled_raster()] `(format = "chunks")`, which embeds or
#'   serves the bytes.
#' @return A list of class `"aob_chunks"`: the scene spec 0.6 data
#'   reference (`format`, `url`, `grid`, `dtype`, `bands` and `interleave`
#'   when there is more than one band, `codecs`, `nodata`, `scale` and
#'   `offset` when they are not 1 and 0, and `refs` with one inline row per
#'   stored tile), ready for a scene's `data`.
#' @seealso [scene_add_tiled_raster()] with `format = "chunks"` to draw a
#'   COG through its chunk references.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE)
#' f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
#' ch <- cog_chunks(f, url = "polar_3031.tif")
#' ch
#' ch$codecs
#' ch$refs$rows[[1]]
cog_chunks <- function(cog, url = NULL) {
  need_gdalraster("cog_chunks()")
  if (!inherits(cog, "aob_cog")) cog <- cog_info(cog)
  url <- url %||% cog$url
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    stop("`url` must be a single URL.", call. = FALSE)
  }
  l0 <- cog$levels[[1]]
  enc <- l0$encoding
  nb <- as.integer(cog$samples_per_pixel)
  separate <- nb > 1L && identical(cog$planar, "separate")
  if (!(l0$geotransform[2] > 0)) {
    stop("The COG's cells run right to left (a negative x cell size); a chunks grid ",
         "needs them left to right.", call. = FALSE)
  }

  levels <- lapply(cog$levels[-1], function(lv) {
    out <- list(level = as.integer(lv$level), dim = as.integer(lv$dim),
                geotransform = as.numeric(lv$geotransform))
    if (!identical(as.integer(lv$tile_size), as.integer(l0$tile_size))) {
      out$chunk_size <- as.integer(lv$tile_size)
    }
    out
  })
  grid <- list(crs = cog$crs, geotransform = as.numeric(l0$geotransform),
               dim = as.integer(l0$dim), chunk_size = as.integer(l0$tile_size))
  if (length(levels)) grid$levels <- levels

  ranges <- chunk_ranges(cog, separate)
  rows <- lapply(seq_len(nrow(ranges)), function(i) {
    r <- list(level = ranges$level[i], col = ranges$col[i], row = ranges$row[i],
              band = if (separate) ranges$band[i], offset = ranges$byte_offset[i],
              length = ranges$byte_length[i])
    r[!vapply(r, is.null, TRUE)]
  })

  ref <- list(format = "chunks", url = url, grid = grid, dtype = enc$dtype,
              bands = if (nb > 1L) nb,
              interleave = if (nb > 1L) if (separate) "separate" else "pixel",
              codecs = chunk_codecs(cog),
              nodata = nodata_json(cog$nodata),
              scale = if (!isTRUE(cog$scale == 1)) cog$scale,
              offset = if (!isTRUE(cog$offset == 0)) cog$offset,
              refs = list(rows = rows))
  structure(drop_null(ref), class = "aob_chunks")
}

#' @export
print.aob_chunks <- function(x, ...) {
  cat("<chunks> ", x$url, "\n", sep = "")
  g <- x$grid
  nb <- x$bands %||% 1L
  cat("  crs ", crs_label(g$crs), ", ", x$dtype, ", ", nb, " band", if (nb != 1L) "s",
      if (nb > 1L) paste0(" (", x$interleave, ")"), "\n", sep = "")
  cat("  codecs ", paste(vapply(x$codecs, codec_label, ""), collapse = ", "), "\n", sep = "")
  lv <- vapply(x$refs$rows, function(r) as.integer(r$level %||% 0L), 0L)
  dims <- c(list(list(level = 0L, dim = g$dim, chunk_size = g$chunk_size)), g$levels %||% list())
  for (d in dims) {
    cs <- d$chunk_size %||% g$chunk_size
    n <- ceiling(d$dim[1] / cs[1]) * ceiling(d$dim[2] / cs[2]) * if (identical(x$interleave, "separate")) nb else 1L
    cat(sprintf("  level %d: %d x %d cells, %d x %d chunks, %d of %d stored\n", d$level, d$dim[1],
                d$dim[2], cs[1], cs[2], sum(lv == d$level), as.integer(n)))
  }
  invisible(x)
}

codec_label <- function(c) {
  cf <- c$configuration
  if (identical(c$name, "bytes")) return(paste0("bytes (", cf$endian %||% "little", " endian)"))
  if (identical(c$name, "predictor")) return(paste0("predictor ", cf$type))
  if (identical(c$name, "jpeg")) return(if (is.null(cf$tables)) "jpeg" else "jpeg (shared tables)")
  c$name
}

## The codec chain of a COG's tiles. Every level must share one encoding,
## since one chain decodes every chunk.
chunk_codecs <- function(cog) {
  fail <- function(...) stop(..., call. = FALSE)
  encs <- lapply(cog$levels, function(l) l$encoding)
  e0 <- encs[[1]]
  for (k in seq_along(encs)[-1]) {
    e <- encs[[k]]
    if (!identical(e$codec, e0$codec) || !identical(e$predictor, e0$predictor) ||
        !identical(e$jpeg_tables, e0$jpeg_tables)) {
      fail("Level ", cog$levels[[k]]$level, " is encoded as ", e$codec, " (predictor ", e$predictor,
           ") but level 0 as ", e0$codec, " (predictor ", e0$predictor, ")",
           if (identical(e$codec, "jpeg") && identical(e0$codec, "jpeg")) " or with other JPEG tables",
           "; a chunks reference has one codec chain for every level. Draw it as a cog, or ",
           "rewrite it with one compression for every level.")
    }
  }
  if (identical(e0$codec, "jpeg")) {
    nb <- cog$samples_per_pixel
    ph <- cog$photometric %||% NA_character_
    ok <- (nb == 3L && identical(ph, "YCbCr")) || (nb == 1L && identical(ph, "MinIsBlack"))
    if (!ok) {
      fail("The COG is JPEG with ", nb, " band", if (nb != 1L) "s", " and photometric ",
           if (is.na(ph)) "unknown" else ph, "; the jpeg codec is YCbCr (3 bands) or greyscale ",
           "MinIsBlack (1 band) only.")
    }
    if (!identical(cog$planar, "interleaved")) {
      fail("The COG is JPEG with separate bands; the jpeg codec needs them interleaved.")
    }
    return(list(drop_null(list(name = "jpeg",
                               configuration = if (!is.null(e0$jpeg_tables)) list(tables = e0$jpeg_tables)))))
  }
  compressor <- c(none = NA, deflate = "deflate", zstd = "zstd", lzw = "lzw")[e0$codec]
  if (is.na(compressor) && !identical(e0$codec, "none")) {
    fail("The COG is compressed with ", e0$codec, ", which has no scene spec 0.6 codec ",
         "(deflate, zstd, lzw, jpeg or none). Draw it as a cog (`format = \"cog\"`), or ",
         "rewrite it with one of those.")
  }
  out <- list(list(name = "bytes", configuration = list(endian = cog$byte_order %||% "little")))
  if (!identical(e0$predictor, "none")) {
    out[[length(out) + 1L]] <- list(name = "predictor", configuration = list(type = e0$predictor))
  }
  if (!is.na(compressor)) out[[length(out) + 1L]] <- list(name = unname(compressor))
  out
}

## Every stored tile's byte range, with its level (and band): the chosen
## band's from cog_info(), and for a COG whose bands are stored separately,
## every other band's read from GDAL the same way.
chunk_ranges <- function(cog, separate) {
  nb <- cog$samples_per_pixel
  bands <- if (separate) seq_len(nb) else cog$band
  gdal_dsn <- dsn_ref(cog$dsn)$gdal
  ## The other bands' ranges of one level, from the level opened once.
  other_bands <- function(lv) {
    k <- lv$level
    ds <- if (k == 0L) open_raster(gdal_dsn) else open_raster(gdal_dsn, paste0("OVERVIEW_LEVEL=", k - 1L))
    on.exit(ds$close())
    lapply(stats::setNames(setdiff(bands, cog$band), setdiff(bands, cog$band)),
           function(b) block_ranges(ds, b, lv$dim, lv$tile_size, k))
  }
  out <- list()
  for (lv in cog$levels) {
    per_band <- if (length(bands) > 1L) other_bands(lv) else list()
    per_band[[as.character(cog$band)]] <- lv$tiles
    for (b in bands) {
      t <- per_band[[as.character(b)]]
      if (nrow(t)) {
        out[[length(out) + 1L]] <- data.frame(level = as.integer(lv$level), band = as.integer(b),
                                              col = as.integer(t$col), row = as.integer(t$row),
                                              byte_offset = t$byte_offset,
                                              byte_length = t$byte_length)
      }
    }
  }
  if (!length(out)) {
    return(data.frame(level = integer(), band = integer(), col = integer(), row = integer(),
                      byte_offset = numeric(), byte_length = numeric()))
  }
  do.call(rbind, out)
}

## A plan level over chunks: only what the producer measured and chose
## (scene spec 0.6 chunkPlanLevel); the grid, codecs, byte ranges, tile size
## and edge windows are the source's.
chunk_plan_level <- function(lv) {
  list(level = lv$level, pixel_size = lv$pixel_size,
       tiles = lapply(lv$tiles, function(t) t[c("col", "row", "footprint", "mesh")]))
}
