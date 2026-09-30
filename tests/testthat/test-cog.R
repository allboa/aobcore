fixture <- function(name) system.file("extdata", name, package = "aobcore")

## Undo TIFF predictor 2 on little-endian int16 tile bytes.
int16_tile <- function(bytes, w, h) {
  v <- readBin(bytes, "integer", n = w * h, size = 2L, signed = TRUE, endian = "little")
  m <- matrix(v, nrow = h, byrow = TRUE)
  for (j in seq_len(w)[-1]) m[, j] <- m[, j] + m[, j - 1L]
  ((m + 32768L) %% 65536L) - 32768L
}

read_range <- function(f, offset, length) {
  con <- file(f, "rb")
  on.exit(close(con))
  seek(con, offset)
  readBin(con, "raw", length)
}

test_that("cog_info() reads every level's own grid from GDAL", {
  skip_if_no_gdal()
  cog <- cog_info(fixture("polar_3031.tif"))
  expect_s3_class(cog, "aob_cog")
  expect_identical(cog$crs, "EPSG:3031")
  expect_identical(vapply(cog$levels, function(l) l$level, 0L), 0:3)
  expect_identical(lapply(cog$levels, `[[`, "dim"),
                   list(c(400L, 400L), c(200L, 200L), c(100L, 100L), c(50L, 50L)))
  for (l in cog$levels) expect_equal(l$extent, c(-6400000, 6400000, -6400000, 6400000))
  enc <- cog$levels[[1]]$encoding
  expect_identical(enc[c("codec", "predictor", "dtype", "byte_order")],
                   list(codec = "deflate", predictor = "horizontal", dtype = "int16",
                        byte_order = "little"))
  expect_identical(enc$scale, 0.001)
  expect_identical(cog$nodata, -32768)
  expect_output(print(cog), "level 3: 50 x 50 cells")

  ## 12 rows over 50 degrees: the overview's pixel is 50 / 12 degrees tall,
  ## not 8 times the base's 0.5 (the spike planner's bug).
  ll <- cog_info(fixture("polar_lonlat.tif"))
  top <- ll$levels[[4]]
  expect_identical(top$dim, c(90L, 12L))
  expect_equal(top$geotransform[6], -50 / 12)
  expect_equal(top$extent, c(-180, 180, -90, -40))
  expect_identical(ll$levels[[1]]$encoding$codec, "lzw")
})

test_that("tile byte ranges are the tiles GDAL reads", {
  skip_if_no_gdal()
  f <- fixture("polar_3031.tif")
  cog <- cog_info(f)
  all <- do.call(rbind, lapply(cog$levels, `[[`, "tiles"))
  expect_true(all(all$byte_offset > 0 & all$byte_offset + all$byte_length <= file.size(f)))
  o <- order(all$byte_offset)
  expect_true(all(utils::head(all$byte_offset[o] + all$byte_length[o], -1) <= all$byte_offset[o][-1]))

  ## Decode each full-resolution tile in R (zlib, then the predictor) and
  ## compare with GDAL's read of the same window.
  lv <- cog$levels[[1]]
  expect_identical(nrow(lv$tiles), 16L)
  ds <- gdalraster::GDALRaster$new(f)
  on.exit(ds$close())
  for (i in seq_len(nrow(lv$tiles))) {
    t <- lv$tiles[i, ]
    raw <- memDecompress(read_range(f, t$byte_offset, t$byte_length), "gzip")
    m <- int16_tile(raw, 128L, 128L)
    vw <- min(128L, 400L - t$col * 128L)
    vh <- min(128L, 400L - t$row * 128L)
    g <- ds$read(1L, t$col * 128L, t$row * 128L, vw, vh, vw, vh)
    g[is.na(g)] <- -32768
    expect_identical(as.numeric(t(m[seq_len(vh), seq_len(vw)])), as.numeric(g))
  }
})

test_that("an all-levels plan covers every tile with packed meshes", {
  skip_if_no_gdal()
  p <- cog_plan(fixture("polar_lonlat.tif"), "EPSG:3031")
  expect_s3_class(p, "aob_tile_plan")
  expect_output(print(p), "all_levels in EPSG:3031")
  plan <- p$plan
  expect_identical(plan$coverage, "all_levels")
  expect_identical(plan$selection$rule, "coarsest_sufficient")
  expect_identical(vapply(plan$levels, function(l) l$level, 0L), 3:0)
  expect_identical(vapply(plan$levels, function(l) length(l$tiles), 0L), c(1L, 2L, 3L, 6L))
  sizes <- vapply(plan$levels, function(l) l$pixel_size, 0)
  expect_true(all(diff(sizes) < 0))

  tiles <- unlist(lapply(plan$levels, `[[`, "tiles"), recursive = FALSE)
  runs <- t(vapply(tiles, function(t) unlist(t$mesh), numeric(4)))
  ## Row runs are contiguous, in order, and cover both tables.
  expect_equal(runs[, "first_vertex"], cumsum(c(0, utils::head(runs[, "vertex_count"], -1))))
  expect_equal(runs[, "first_index"], cumsum(c(0, utils::head(runs[, "index_count"], -1))))
  expect_equal(sum(runs[, "vertex_count"]), p$n_vertices)

  ## Read the float32 lists from their buffers (converting FixedSizeList
  ## columns to R needs vctrs, which is not a dependency).
  va <- nanoarrow::read_nanoarrow(p$vertices)$get_next()
  xy <- function(col) {
    b <- nanoarrow::convert_buffer(va$children[[col]]$children[[1]]$buffers[[2]])
    matrix(as.numeric(b)[seq_len(2 * va$length)], ncol = 2, byrow = TRUE)
  }
  ia <- nanoarrow::read_nanoarrow(p$indices)$get_next()
  idx <- nanoarrow::convert_array(ia$children$index, double())
  expect_identical(nanoarrow::infer_nanoarrow_schema(nanoarrow::read_nanoarrow(p$vertices))$children$position$format, "+w:2")
  expect_identical(nanoarrow::infer_nanoarrow_schema(nanoarrow::read_nanoarrow(p$indices))$children$index$format, "I")
  expect_identical(va$length, as.integer(p$n_vertices))
  pos <- xy("position")
  uv <- xy("uv")
  for (k in seq_along(tiles)) {
    t <- tiles[[k]]
    r <- t$mesh$first_vertex + seq_len(t$mesh$vertex_count)
    i <- idx[t$mesh$first_index + seq_len(t$mesh$index_count)]
    expect_true(all(i < t$mesh$vertex_count))
    ## UVs in tile space stop at the window.
    w <- t$window %||% list(width = 128, height = 128)
    expect_equal(max(uv[r, 1]), w$width / 128, tolerance = 1e-6)
    expect_equal(max(uv[r, 2]), w$height / 128, tolerance = 1e-6)
    ## The footprint bounds the mesh (float32 positions).
    fp <- t$footprint
    expect_true(all(pos[r, 1] >= fp[1] - 1 & pos[r, 1] <= fp[2] + 1))
    expect_true(all(pos[r, 2] >= fp[3] - 1 & pos[r, 2] <= fp[4] + 1))
  }
  ## A lon/lat source is curved in 3031: more than two triangles per tile,
  ## and the pole row meets at (0, 0).
  expect_gt(p$n_indices / 3, 2 * length(tiles))
  expect_lt(min(sqrt(rowSums(pos^2))), 1)
})

test_that("a plan in the source CRS is one quad per tile", {
  skip_if_no_gdal()
  p <- cog_plan(fixture("polar_3031.tif"), "EPSG:3031")
  n <- sum(vapply(p$plan$levels, function(l) length(l$tiles), 0L))
  expect_identical(n, 22L)
  expect_identical(p$n_vertices, 4 * n)
  expect_identical(vapply(p$plan$levels, function(l) l$pixel_size, 0),
                   c(256000, 128000, 64000, 32000))
  edge <- p$plan$levels[[4]]$tiles[[16]]
  expect_identical(c(edge$col, edge$row), c(3L, 3L))
  expect_identical(edge$window, list(x = 0L, y = 0L, width = 16L, height = 16L))
  expect_equal(edge$footprint, c(6400000 - 16 * 32000, 6400000, -6400000, -6400000 + 16 * 32000))
})

test_that("a plan for one view picks one level and culls tiles", {
  skip_if_no_gdal()
  f <- fixture("polar_3031.tif")
  ext <- c(-2e6, 2e6, -2e6, 2e6)
  p <- cog_plan(f, "EPSG:3031", extent = ext, units_per_pixel = 70000)
  expect_identical(p$plan$coverage, "view")
  expect_identical(p$plan$planned_for, list(extent = ext, units_per_pixel = 70000))
  expect_length(p$plan$levels, 1L)
  expect_identical(p$plan$levels[[1]]$level, 1L)
  expect_length(p$plan$levels[[1]]$tiles, 4L)
  p0 <- cog_plan(f, "EPSG:3031", extent = ext, units_per_pixel = 1000)
  expect_identical(p0$plan$levels[[1]]$level, 0L)
  expect_identical(vapply(p0$plan$levels[[1]]$tiles, function(t) c(t$col, t$row), c(0L, 0L)),
                   matrix(c(1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L), 2))
  pn <- cog_plan(f, "EPSG:3031", extent = ext, units_per_pixel = 100000,
                 selection = "nearest_pixel_size")
  expect_identical(pn$plan$levels[[1]]$level, 2L)
  expect_error(cog_plan(f, units_per_pixel = 1000), "needs `extent`")
  expect_error(cog_plan(f, max_segments = 3), "power of two")
})

test_that("a tiled raster makes a valid 0.2 scene; other scenes stay 0.1", {
  skip_if_no_gdal()
  f <- fixture("polar_3031.tif")
  s0 <- scene("EPSG:3031")
  expect_identical(scene_spec_version(s0), "0.1")
  s <- scene_add_tiled_raster(s0, "sst", f, palette = "ocean", label = "SST")
  expect_identical(s$version, "0.2")
  expect_identical(scene_spec_version(s), "0.2")
  expect_output(print(s), "scene spec 0.2")
  expect_identical(s$data$sst$format, "cog")
  expect_match(s$data$sst$url, "^file:///.*polar_3031[.]tif$")
  layer <- s$layers[[1]]
  expect_identical(layer$kind, "tiled_raster")
  expect_equal(layer$palette$range, c(-1.839, 14.607))
  expect_valid_tiled_scene(s)

  ## Embedded tile bytes are the file's bytes at each range.
  blobs <- scene_blobs(s)
  tiles <- unlist(lapply(layer$plan$levels, `[[`, "tiles"), recursive = FALSE)
  keys <- sprintf("sst@%.0f+%.0f", vapply(tiles, `[[`, 0, "byte_offset"),
                  vapply(tiles, `[[`, 0, "byte_length"))
  expect_setequal(setdiff(names(blobs), c("sst_vertices", "sst_indices")), keys)
  t1 <- tiles[[1]]
  expect_identical(blobs[[keys[1]]], read_range(f, t1$byte_offset, t1$byte_length))

  ## Without embedding, only the mesh blobs travel.
  s2 <- scene_add_tiled_raster(scene(), "sst", f, embed = FALSE, range = c(0, 1))
  expect_named(scene_blobs(s2), c("sst_vertices", "sst_indices"))
  expect_error(scene_add_tiled_raster(s, "sst", f), "already in the scene")
  expect_error(scene_add_tiled_raster(scene("EPSG:3413"), "x", cog_plan(f, "EPSG:3031")),
               "view is in EPSG:3413")
})

test_that("a lon/lat COG over the pole plans to a valid scene", {
  skip_if_no_gdal()
  s <- scene_add_tiled_raster(scene(), "sst", fixture("polar_lonlat.tif"), palette = "ocean")
  expect_identical(s$layers[[1]]$plan$levels[[1]]$grid$crs, "EPSG:4326")
  expect_valid_tiled_scene(s)
})

test_that("a CRS without an EPSG code is written as PROJJSON", {
  skip_if_no_gdal()
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f))
  srs <- "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=15 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  mem <- gdalraster::create("MEM", "", 64L, 64L, 1L, "Float32", return_obj = TRUE)
  mem$setGeoTransform(c(-3.2e6, 1e5, 0, 3.2e6, 0, -1e5))
  mem$setProjection(gdalraster::srs_to_wkt(srs))
  mem$write(1L, 0L, 0L, 64L, 64L, as.numeric(seq_len(64 * 64)))
  gdalraster::createCopy("COG", f, mem, quiet = TRUE, options = "COMPRESS=DEFLATE")
  mem$close()
  cog <- cog_info(f)
  expect_s3_class(cog$crs, "aob_json")
  s <- scene_add_tiled_raster(scene(), "x", cog)
  back <- jsonlite::fromJSON(scene_json(s), simplifyVector = FALSE)
  grid_crs <- back$layers[[1]]$plan$levels[[1]]$grid$crs
  expect_type(grid_crs, "list")
  expect_true(grepl("CRS$", grid_crs$type))
  expect_valid_tiled_scene(s)
})

test_that("cog_info() refuses what the spec cannot carry", {
  skip_if_no_gdal()
  mem <- gdalraster::create("MEM", "", 64L, 64L, 1L, "Byte", return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(c(0, 1, 0, 64, 0, -1))
  mem$setProjection(gdalraster::srs_to_wkt("EPSG:3031"))
  mem$write(1L, 0L, 0L, 64L, 64L, rep(1, 64 * 64))
  png <- tempfile(fileext = ".png")
  jpeg <- tempfile(fileext = ".tif")
  on.exit(unlink(c(png, jpeg, paste0(png, ".aux.xml"))), add = TRUE)
  made <- function(...) isTRUE(tryCatch(gdalraster::createCopy(...), error = function(e) FALSE))
  if (made("PNG", png, mem, quiet = TRUE)) expect_error(cog_info(png), "tiled GeoTIFF")
  if (made("GTiff", jpeg, mem, quiet = TRUE, options = c("TILED=YES", "COMPRESS=JPEG"))) {
    expect_error(cog_info(jpeg), "JPEG")
  }
  expect_error(cog_info(tempfile()), "No file")
  expect_error(cog_info(fixture("polar_3031.tif"), band = 2), "band")
})

test_that("a /vsicurl/ path gives the renderer its plain URL", {
  u <- "https://example.org/data/chart.tif"
  expect_identical(dsn_ref(u)$http, u)
  expect_identical(dsn_ref(paste0("/vsicurl/", u))$http, u)
  expect_identical(dsn_ref("http://127.0.0.1:8000/a.tif")$http, "http://127.0.0.1:8000/a.tif")
  expect_null(dsn_ref("/vsis3/bucket/chart.tif")$http)
  expect_null(dsn_ref("/vsimem/chart.tif")$http)
  expect_null(dsn_ref("chart.tif")$http)
})

test_that("a COG the browser cannot fetch is refused unless embedded", {
  skip_if_no_gdal()
  cog <- cog_info(fixture("polar_3031.tif"))
  plan <- cog_plan(cog, "EPSG:3031", levels = 3)
  plan$cog$url <- "/vsis3/bucket/polar_3031.tif"
  plan$cog$local <- FALSE
  expect_error(scene_add_tiled_raster(scene("EPSG:3031"), "sst", plan), "cannot fetch")
})

test_that("view_cog() writes the polar COG page in one call", {
  skip_if_no_gdal()
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  expect_invisible(out <- view_cog(fixture("polar_3031.tif"), palette = "ocean", file = f))
  expect_identical(out, f)
  page <- readChar(f, file.size(f), useBytes = TRUE)
  expect_match(page, "\"kind\":\"tiled_raster\"", fixed = TRUE)
  expect_match(page, "data-aob-blob=\"cog@", fixed = TRUE)
  expect_match(page, "\"version\":\"0.2\"", fixed = TRUE)
  expect_false(grepl("[^\\x01-\\x7f]", page, perl = TRUE))
  s <- cog_scene(fixture("polar_3031.tif"), coastline = FALSE, extent = c(-1e6, 1e6, -1e6, 1e6))
  expect_identical(s$view$extent, c(-1e6, 1e6, -1e6, 1e6))
  expect_length(s$layers, 1L)
})
