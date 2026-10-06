# cog_chunks() and scene_add_tiled_raster(format = "chunks"): a COG as a
# scene spec 0.6 chunks data reference (aobcore #63).
fixture <- function(name) system.file("extdata", name, package = "aobcore")

## A small GeoTIFF in EPSG:3031 written by GDAL (by default a COG with one
## overview), or NULL when this GDAL cannot write it. `values(b)` gives
## band b's cells, row 0 at the top.
make_chunk_tif <- function(options, dtype = "Int16", nb = 1L, nx = 300L, ny = 200L,
                           driver = "COG", values = NULL, nodata = NULL) {
  mem <- gdalraster::create("MEM", "", nx, ny, nb, dtype, return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(c(-5e5, 1e4, 0, 4e5, 0, -1e4))
  mem$setProjection(gdalraster::srs_to_wkt("EPSG:3031"))
  x <- rep(seq_len(nx) - 1, times = ny)
  y <- rep(seq_len(ny) - 1, each = nx)
  for (b in seq_len(nb)) {
    if (!is.null(nodata)) mem$setNoDataValue(b, nodata)
    v <- if (is.null(values)) (x * 3 + y * 7 * b) %% 200 else values(b, x, y)
    mem$write(b, 0L, 0L, nx, ny, v)
  }
  f <- tempfile(fileext = ".tif")
  ok <- tryCatch({
    gdalraster::createCopy(driver, f, mem, quiet = TRUE, options = options)
    TRUE
  }, error = function(e) FALSE)
  if (ok && file.exists(f)) normalizePath(f, winslash = "/") else NULL
}

## Every stored tile's byte range as GDAL reports it, read here directly
## (not through cog_info()): level, band, col, row, offset, length.
gdal_tile_ranges <- function(f, bands = 1L) {
  ds <- gdalraster::GDALRaster$new(f, TRUE)
  n_ov <- ds$getOverviewCount(1L)
  ds$close()
  out <- list()
  for (k in 0:n_ov) {
    d <- if (k == 0L) gdalraster::GDALRaster$new(f, TRUE) else
      gdalraster::GDALRaster$new(f, TRUE, paste0("OVERVIEW_LEVEL=", k - 1L))
    dim <- d$dim()
    ts <- d$getBlockSize(1L)
    for (b in bands) for (r in seq_len(ceiling(dim[2] / ts[2])) - 1L) for (c in seq_len(ceiling(dim[1] / ts[1])) - 1L) {
      o <- d$getMetadataItem(b, sprintf("BLOCK_OFFSET_%d_%d", c, r), "TIFF")
      n <- d$getMetadataItem(b, sprintf("BLOCK_SIZE_%d_%d", c, r), "TIFF")
      if (length(o) && nzchar(o) && length(n) && nzchar(n) && as.numeric(n) > 0) {
        out[[length(out) + 1L]] <- data.frame(level = k, band = b, col = c, row = r,
                                              offset = as.numeric(o), length = as.numeric(n))
      }
    }
    d$close()
  }
  df <- do.call(rbind, out)
  df[order(df$level, df$band, df$row, df$col), , drop = FALSE]
}

## The refs of a chunks reference as a data frame, in the same order.
refs_frame <- function(ch) {
  df <- do.call(rbind, lapply(ch$refs$rows, function(r) {
    data.frame(level = r$level %||% 0L, band = r$band %||% 1L, col = r$col, row = r$row,
               offset = r$offset, length = r$length)
  }))
  df[order(df$level, df$band, df$row, df$col), , drop = FALSE]
}

expect_refs_match_gdal <- function(ch, f, bands = 1L) {
  got <- refs_frame(ch)
  want <- gdal_tile_ranges(f, bands)
  rownames(got) <- rownames(want) <- NULL
  expect_equal(got, want, ignore_attr = TRUE)
}

test_that("cog_chunks() maps each TIFF compression and predictor to a codec chain", {
  skip_if_no_gdal()
  chain <- function(ch) vapply(ch$codecs, function(c) {
    cf <- c$configuration
    paste0(c$name, if (!is.null(cf$type)) paste0(":", cf$type), if (!is.null(cf$endian)) paste0(":", cf$endian))
  }, "")
  cases <- list(
    list(opt = "COMPRESS=NONE", dtype = "Int16", want = "bytes:little"),
    list(opt = c("COMPRESS=DEFLATE", "PREDICTOR=YES"), dtype = "Int16",
         want = c("bytes:little", "predictor:horizontal", "deflate")),
    list(opt = c("COMPRESS=ZSTD", "PREDICTOR=YES"), dtype = "Float32",
         want = c("bytes:little", "predictor:floating_point", "zstd")),
    list(opt = "COMPRESS=LZW", dtype = "Byte", want = c("bytes:little", "lzw")),
    list(opt = c("TILED=YES", "BLOCKXSIZE=128", "BLOCKYSIZE=128", "COMPRESS=DEFLATE", "ENDIANNESS=BIG"),
         dtype = "UInt16", driver = "GTiff", want = c("bytes:big", "deflate"))
  )
  made <- 0L
  for (cs in cases) {
    f <- make_chunk_tif(c(cs$opt, if (is.null(cs$driver)) "BLOCKSIZE=128"), dtype = cs$dtype,
                        driver = cs$driver %||% "COG")
    if (is.null(f)) next
    made <- made + 1L
    ch <- cog_chunks(f, url = "x.tif")
    expect_s3_class(ch, "aob_chunks")
    expect_identical(chain(ch), cs$want, label = paste(cs$opt, collapse = " "))
    expect_identical(ch$dtype, c(Int16 = "int16", Float32 = "float32", Byte = "uint8", UInt16 = "uint16")[[cs$dtype]])
    expect_identical(ch$url, "x.tif")
    expect_identical(ch$grid$crs, "EPSG:3031")
    expect_identical(ch$grid$chunk_size, c(128L, 128L))
    expect_identical(ch$grid$dim, c(300L, 200L))
    expect_equal(ch$grid$geotransform, c(-5e5, 1e4, 0, 4e5, 0, -1e4))
    expect_null(ch$bands)
    expect_refs_match_gdal(ch, f)
    unlink(f)
  }
  expect_gt(made, 2L)

  ## The overview's own grid, and a COG's levels as grid levels.
  f <- make_chunk_tif(c("COMPRESS=DEFLATE", "BLOCKSIZE=128", "OVERVIEW_COUNT=1"))
  on.exit(unlink(f), add = TRUE)
  ch <- cog_chunks(f)
  expect_length(ch$grid$levels, 1L)
  expect_identical(ch$grid$levels[[1]]$level, 1L)
  expect_identical(ch$grid$levels[[1]]$dim, c(150L, 100L))
  expect_equal(ch$grid$levels[[1]]$geotransform, c(-5e5, 2e4, 0, 4e5, 0, -2e4))
  ## An overview's tile size, when GDAL wrote it larger than level 0's.
  ts1 <- cog_info(f)$levels[[2]]$tile_size
  if (identical(ts1, c(128L, 128L))) {
    expect_null(ch$grid$levels[[1]]$chunk_size)
  } else {
    expect_identical(ch$grid$levels[[1]]$chunk_size, ts1)
  }
  expect_true(any(vapply(ch$refs$rows, function(r) identical(r$level, 1L), TRUE)))
  expect_identical(ch$url, cog_info(f)$url)
  expect_refs_match_gdal(ch, f)
  expect_output(print(ch), "level 0: 300 x 200 cells, 128 x 128 chunks, 6 of 6 stored")
  expect_output(print(ch), "level 1: 150 x 100 cells, ")
})

test_that("cog_chunks() writes JPEG as the whole chain with its tables, and refuses what 0.6 cannot carry", {
  skip_if_no_gdal()
  jpg <- make_chunk_tif(c("COMPRESS=JPEG", "BLOCKSIZE=128"), dtype = "Byte", nb = 3L)
  if (!is.null(jpg)) {
    on.exit(unlink(jpg), add = TRUE)
    ch <- cog_chunks(jpg)
    expect_length(ch$codecs, 1L)
    expect_identical(ch$codecs[[1]]$name, "jpeg")
    expect_identical(ch$codecs[[1]]$configuration$tables, cog_info(jpg)$levels[[1]]$encoding$jpeg_tables)
    expect_identical(ch$bands, 3L)
    expect_identical(ch$interleave, "pixel")
    expect_identical(ch$dtype, "uint8")
  }
  pb <- make_chunk_tif(c("TILED=YES", "COMPRESS=PACKBITS"), driver = "GTiff")
  if (!is.null(pb)) {
    on.exit(unlink(pb), add = TRUE)
    expect_error(cog_chunks(pb), "compressed with packbits, which has no scene spec 0.6 codec")
    s <- scene("EPSG:3031")
    expect_error(scene_add_tiled_raster(s, "x", pb, palette = "ocean", format = "chunks"),
                 "packbits")
    ## As a cog source it still draws.
    expect_identical(scene_add_tiled_raster(s, "x", pb, palette = "ocean")$version, "0.2")
  }
  mixed <- make_chunk_tif(c("COMPRESS=DEFLATE", "OVERVIEW_COMPRESS=LZW", "BLOCKSIZE=128", "OVERVIEW_COUNT=1"))
  if (!is.null(mixed) && !identical(cog_info(mixed)$levels[[2]]$encoding$codec, "deflate")) {
    on.exit(unlink(mixed), add = TRUE)
    expect_error(cog_chunks(mixed), "Level 1 is encoded as lzw .* but level 0 as deflate .*one codec chain")
  }
  expect_error(cog_chunks(fixture("polar_3031.tif"), url = ""), "`url` must be a single URL")
})

test_that("cog_chunks() gives a ref per band for bands stored separately, equal to GDAL's", {
  skip_if_no_gdal()
  f <- make_chunk_tif(c("COMPRESS=DEFLATE", "BLOCKSIZE=128", "INTERLEAVE=BAND", "OVERVIEW_COUNT=1"),
                      dtype = "UInt16", nb = 2L)
  skip_if(is.null(f), "this GDAL cannot write a band-interleaved COG")
  on.exit(unlink(f))
  ch <- cog_chunks(f)
  expect_identical(ch$bands, 2L)
  expect_identical(ch$interleave, "separate")
  expect_true(all(vapply(ch$refs$rows, function(r) r$band %in% 1:2, TRUE)))
  expect_refs_match_gdal(ch, f, bands = 1:2)
  ## The same refs whichever band cog_info() was asked for.
  expect_identical(refs_frame(cog_chunks(cog_info(f, band = 2L))), refs_frame(ch))
  ## A palette layer over band 2 names it, and embeds band 2's chunks.
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "b", cog_plan(cog_info(f, band = 2L), "EPSG:3031"),
                              palette = "ocean", format = "chunks")
  expect_identical(s$layers[[1]]$band, 2L)
  b2 <- refs_frame(ch)
  b2 <- b2[b2$band == 2L, ]
  keys <- grep("@", names(scene_blobs(s)), value = TRUE, fixed = TRUE)
  expect_setequal(keys, tile_blob_key("b", b2$offset, b2$length))
  expect_validator_ok(scene_json(s))
})

test_that("a sparse COG's unwritten tiles have no ref and no plan tile", {
  skip_if_no_gdal()
  nd <- -32768
  f <- make_chunk_tif(c("COMPRESS=DEFLATE", "BLOCKSIZE=128", "SPARSE_OK=TRUE", "OVERVIEWS=NONE"),
                      nodata = nd, values = function(b, x, y) ifelse(x >= 128 & x < 256 & y < 128, nd, x + y))
  skip_if(is.null(f), "this GDAL cannot write a sparse COG")
  on.exit(unlink(f))
  ch <- cog_chunks(f)
  pos <- vapply(ch$refs$rows, function(r) paste(r$level, r$col, r$row), "")
  expect_false("0 1 0" %in% pos)
  expect_length(pos, 3L * 2L - 1L)
  expect_identical(ch$nodata, nd)
  expect_refs_match_gdal(ch, f)
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "sp", f, palette = "ocean", format = "chunks")
  tiles <- s$layers[[1]]$plan$levels[[1]]$tiles
  expect_false(any(vapply(tiles, function(t) t$col == 1L && t$row == 0L, TRUE)))
  expect_validator_ok(scene_json(s))
})

test_that("scene_add_tiled_raster(format = \"chunks\") draws a COG over its chunks, embedding only the planned ones", {
  skip_if_no_gdal()
  f <- fixture("polar_3031.tif")
  cog <- cog_info(f)
  plan <- cog_plan(cog, "EPSG:3031", levels = 1:2)
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", plan, palette = "ocean", format = "chunks")
  expect_identical(s$version, "0.6")
  src <- s$data$sst
  expect_identical(src$format, "chunks")
  expect_identical(src$url, "polar_3031.tif")
  expect_false(inherits(src, "aob_chunks"))
  ## The refs are every stored tile of the COG; the plan is levels 1 and 2.
  expect_length(src$refs$rows, sum(vapply(cog$levels, function(l) nrow(l$tiles), 0L)))
  L <- s$layers[[1]]
  expect_null(L$band)
  expect_identical(L$plan$mesh, list(vertices = "sst_vertices", indices = "sst_indices"))
  for (lv in L$plan$levels) {
    expect_named(lv, c("level", "pixel_size", "tiles"))
    for (t in lv$tiles) expect_named(t, c("col", "row", "footprint", "mesh"))
  }
  ## The same levels, tiles, footprints and meshes as the cog plan.
  cg <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", plan, palette = "ocean")
  expect_identical(lapply(L$plan$levels, chunk_plan_level), L$plan$levels)
  expect_identical(lapply(cg$layers[[1]]$plan$levels, chunk_plan_level), L$plan$levels)
  expect_identical(cg$layers[[1]]$palette, L$palette)
  ## Embedded: exactly the planned chunks' bytes, keyed as a cog's tiles.
  planned <- do.call(rbind, lapply(plan$plan$levels, function(l) do.call(rbind, lapply(l$tiles, function(t) {
    data.frame(o = t$byte_offset, n = t$byte_length)
  }))))
  keys <- grep("@", names(scene_blobs(s)), value = TRUE, fixed = TRUE)
  expect_setequal(keys, tile_blob_key("sst", planned$o, planned$n))
  expect_lt(length(keys), length(src$refs$rows))
  expect_identical(scene_blobs(s)[keys], scene_blobs(cg)[keys])
  bytes <- readBin(f, "raw", file.size(f))
  expect_identical(scene_blobs(s)[[keys[1]]], {
    p <- regmatches(keys[1], regexec("@([0-9]+)\\+([0-9]+)$", keys[1]))[[1]]
    bytes[as.numeric(p[2]) + seq_len(as.numeric(p[3]))]
  })
  ## The page keeps every chunk blob (they are used) and carries the scene.
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf), add = TRUE)
  expect_no_warning(write_scene_html(s, file = tf))
  html <- paste(readLines(tf, warn = FALSE), collapse = "\n")
  for (k in keys) expect_match(html, paste0("data-aob-blob=\"", k, "\""), fixed = TRUE)
  expect_match(html, "\"format\":\"chunks\"", fixed = TRUE)
  expect_validator_ok(scene_json(s))

  ## A colour image and a JPEG one, over chunks.
  rgba <- scene_add_tiled_raster(scene("EPSG:3031"), "rgba", fixture("polar_rgba.tif"), format = "chunks")
  expect_identical(rgba$layers[[1]]$rgb$bands, 1:3)
  expect_identical(rgba$data$rgba$bands, 4L)
  expect_validator_ok(scene_json(rgba))
  ycbcr <- scene_add_tiled_raster(scene("EPSG:3031"), "j", fixture("polar_ycbcr.tif"), format = "chunks")
  expect_identical(ycbcr$data$j$codecs[[1]]$name, "jpeg")
  expect_validator_ok(scene_json(ycbcr))
})

test_that("a local COG over chunks that is not embedded is registered and served", {
  skip_if_no_gdal()
  f <- fixture("polar_3031.tif")
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", cog_plan(f, "EPSG:3031", levels = 3L),
                              palette = "ocean", embed = FALSE, format = "chunks")
  expect_false(any(grepl("@", names(scene_blobs(s)), fixed = TRUE)))
  rec <- attr(s, "files")$sst
  expect_identical(rec$path, normalizePath(f, winslash = "/"))
  expect_match(s$data$sst$url, "^file://")
  content <- serve_content(s, scene_blobs(s), attr(s, "files"), "t", "auto")
  expect_identical(names(content$files), "sst")
  page <- rawToChar(content$page)
  expect_match(page, "\"url\":\"files/sst/polar_3031.tif\"", fixed = TRUE)
  ## A relative url with neither a file nor chunk blobs cannot be served.
  s2 <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", cog_plan(f, "EPSG:3031", levels = 3L),
                               palette = "ocean", embed = FALSE, url = "sst.tif", format = "chunks")
  expect_error(serve_content(s2, scene_blobs(s2), list(), "t", "auto"),
               "The chunks data reference `sst` has the relative url \"sst.tif\".*cannot answer")
})

test_that("write_scene_html() keeps blobs named by a refs table's url column and per-ref keys", {
  skip_if_no_gdal()
  f <- fixture("polar_3031.tif")
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", cog_plan(f, "EPSG:3031", levels = 3L),
                              palette = "ocean", embed = FALSE, url = "polar_3031.tif", format = "chunks")
  rows <- s$data$sst$refs$rows
  r <- rows[vapply(rows, function(x) identical(x$level, 3L), TRUE)][[1]]
  bytes <- readBin(f, "raw", file.size(f))
  chunk <- bytes[r$offset + seq_len(r$length)]
  key <- tile_blob_key("sst", r$offset, r$length)
  ## Inline rows: the per-ref key of a ref at the reference's url is used.
  b <- c(scene_blobs(s), stats::setNames(list(chunk, as.raw(1:3)), c(key, "unused")))
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf))
  expect_warning(write_scene_html(s, b, tf), "left out of the page: unused.")
  expect_match(paste(readLines(tf, warn = FALSE), collapse = "\n"), paste0("data-aob-blob=\"", key, "\""),
               fixed = TRUE)

  ## A refs table (an Arrow IPC stream blob). A ref with no url (the
  ## reference's own) has its per-ref key; a url column names a blob
  ## holding the bytes at that url.
  ref_table <- function(...) {
    ipc_bytes(nanoarrow::as_nanoarrow_array_stream(data.frame(level = 3L, col = 0L, row = 0L, ...,
                                                              offset = r$offset, length = r$length)))
  }
  p <- unclass(s)
  attr(p, "blobs") <- NULL
  attr(p, "files") <- NULL
  p$data$sst$refs <- list(table = "sst_refs")
  p$data$sst_refs <- list(format = "arrow-ipc-stream", blob = "sst_refs")
  b <- c(scene_blobs(s), stats::setNames(list(ref_table(), chunk, as.raw(1:3)), c("sst_refs", key, "unused")))
  expect_warning(used <- check_scene_shape(p, b), "left out of the page: unused.")
  expect_true(all(c("sst_refs", key) %in% used))
  b2 <- c(scene_blobs(s), stats::setNames(list(ref_table(url = "copy.tif"), chunk, bytes, as.raw(1:3)),
                                          c("sst_refs", key, "copy.tif", "unused")))
  expect_warning(used <- check_scene_shape(p, b2), paste0("left out of the page: ", key, ", unused."),
                 fixed = TRUE)
  expect_true("copy.tif" %in% used)
})
