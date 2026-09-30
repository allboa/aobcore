fixture <- function(name) system.file("extdata", name, package = "aobcore")

read_range <- function(f, offset, length) {
  con <- file(f, "rb")
  on.exit(close(con))
  seek(con, offset)
  readBin(con, "raw", length)
}

## A small 3-band Byte GeoTIFF in EPSG:3031 written by GDAL, or NULL when
## this GDAL cannot write it.
make_rgb_tif <- function(driver, options, nb = 3L, n = 64L, interp = c("Red", "Green", "Blue", "Alpha")) {
  mem <- gdalraster::create("MEM", "", n, n, nb, "Byte", return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(c(-3.2e6, 1e5, 0, 3.2e6, 0, -1e5))
  mem$setProjection(gdalraster::srs_to_wkt("EPSG:3031"))
  x <- rep(seq_len(n), times = n)
  y <- rep(seq_len(n), each = n)
  for (b in seq_len(nb)) {
    mem$setRasterColorInterp(b, interp[b])
    mem$write(b, 0L, 0L, n, n, as.integer(if (b == 4L) ifelse(x > n / 2, 255, 0) else (x * b * 3 + y * 2) %% 256))
  }
  f <- tempfile(fileext = ".tif")
  ok <- tryCatch({
    gdalraster::createCopy(driver, f, mem, quiet = TRUE, options = options)
    TRUE
  }, error = function(e) FALSE)
  if (ok && file.exists(f)) f else NULL
}

test_that("cog_info() records colour interpretation, photometric and JPEG tables", {
  skip_if_no_gdal()
  rgba <- cog_info(fixture("polar_rgba.tif"))
  expect_identical(rgba$color_interp, c("Red", "Green", "Blue", "Alpha"))
  expect_identical(rgba$photometric, "RGB")
  expect_identical(rgba$planar, "interleaved")
  expect_identical(rgba$levels[[1]]$encoding$codec, "lzw")
  expect_null(rgba$levels[[1]]$encoding$jpeg_tables)
  expect_identical(rgb_default(rgba), 1:4)
  expect_output(print(rgba), "Red, Green, Blue, Alpha; photometric RGB; drawn as a colour image")

  f <- fixture("polar_ycbcr.tif")
  jpg <- cog_info(f)
  expect_identical(jpg$color_interp, c("Red", "Green", "Blue"))
  expect_identical(jpg$photometric, "YCbCr")
  expect_identical(rgb_default(jpg), 1:3)
  for (l in jpg$levels) {
    expect_identical(l$photometric, "YCbCr")
    expect_identical(l$encoding[c("codec", "predictor", "dtype", "samples_per_pixel", "planar")],
                     list(codec = "jpeg", predictor = "none", dtype = "uint8",
                          samples_per_pixel = 3L, planar = "interleaved"))
    tables <- b64_decode(l$encoding$jpeg_tables)
    ## An abbreviated JPEG stream: SOI ... EOI.
    expect_identical(tables[1:2], as.raw(c(0xff, 0xd8)))
    expect_identical(tables[length(tables) - 1:0], as.raw(c(0xff, 0xd9)))
  }
  ## Single-band COGs are unchanged apart from the new fields.
  sst <- cog_info(fixture("polar_3031.tif"))
  expect_identical(sst$color_interp, "Gray")
  expect_identical(sst$photometric, "MinIsBlack")
  expect_null(rgb_default(sst))
})

test_that("a JPEG tile joined to its level's tables is the image GDAL reads", {
  skip_if_no_gdal()
  f <- fixture("polar_ycbcr.tif")
  cog <- cog_info(f)
  lv <- cog$levels[[1]]
  t <- lv$tiles[1, ]
  tile <- read_range(f, t$byte_offset, t$byte_length)
  tables <- b64_decode(lv$encoding$jpeg_tables)
  expect_identical(tile[1:2], as.raw(c(0xff, 0xd8)))
  ## The scene spec's rule: tables without EOI, then the tile without SOI.
  jpg <- tempfile(fileext = ".jpg")
  on.exit(unlink(c(jpg, paste0(jpg, ".aux.xml"))))
  writeBin(c(tables[seq_len(length(tables) - 2L)], tile[-(1:2)]), jpg)
  ts <- lv$tile_size
  a <- gdalraster::GDALRaster$new(jpg)
  on.exit(a$close(), add = TRUE)
  expect_identical(a$getRasterCount(), 3L)
  expect_equal(a$dim()[1:2], ts)
  b <- gdalraster::GDALRaster$new(f)
  on.exit(b$close(), add = TRUE)
  for (band in 1:3) {
    got <- a$read(band, 0L, 0L, ts[1], ts[2], ts[1], ts[2])
    want <- b$read(band, 0L, 0L, ts[1], ts[2], ts[1], ts[2])
    expect_lt(mean(abs(got - want)), 1)
  }
})

test_that("RGB(A) COGs default to a colour image in scene spec 0.3", {
  skip_if_no_gdal()
  s <- cog_scene(fixture("polar_rgba.tif"), coastline = FALSE)
  expect_identical(s$version, "0.3")
  expect_identical(scene_spec_version(s), "0.3")
  L <- s$layers[[1]]
  expect_null(L$palette)
  expect_identical(L$rgb, list(bands = 1:3, alpha = 4L))
  for (l in L$plan$levels) expect_null(l$encoding$band)
  expect_valid_tiled_scene(s)
  expect_match(scene_json(s), "\"version\":\"0.3\"", fixed = TRUE)

  j <- cog_scene(fixture("polar_ycbcr.tif"), coastline = FALSE)
  expect_identical(j$version, "0.3")
  expect_identical(j$layers[[1]]$rgb, list(bands = 1:3))
  expect_true(all(vapply(j$layers[[1]]$plan$levels, function(l) nzchar(l$encoding$jpeg_tables), TRUE)))
  expect_valid_tiled_scene(j)

  ## view_cog() writes the same layer.
  page <- view_cog(fixture("polar_rgba.tif"), coastline = FALSE, file = tempfile(fileext = ".html"))
  on.exit(unlink(page))
  html <- paste(readLines(page, warn = FALSE), collapse = "\n")
  expect_match(html, "\"rgb\":{\"bands\":[1,2,3],\"alpha\":4}", fixed = TRUE)
})

test_that("band =, palette = or rgb = FALSE keep the single-band palette path", {
  skip_if_no_gdal()
  f <- fixture("polar_rgba.tif")
  for (s in list(cog_scene(f, band = 2L, coastline = FALSE),
                 cog_scene(f, palette = "gray", coastline = FALSE),
                 cog_scene(f, rgb = FALSE, coastline = FALSE))) {
    expect_identical(s$version, "0.2")
    expect_null(s$layers[[1]]$rgb)
    expect_false(is.null(s$layers[[1]]$palette))
    expect_valid_tiled_scene(s)
  }
  expect_identical(cog_scene(f, band = 2L, coastline = FALSE)$layers[[1]]$plan$levels[[1]]$encoding$band, 2L)
  ## scene_add_tiled_raster(): a palette given means a palette.
  p <- cog_plan(f, "EPSG:3031")
  expect_null(scene_add_tiled_raster(scene(), "x", p, palette = "gray")$layers[[1]]$rgb)
  expect_identical(scene_add_tiled_raster(scene(), "x", p)$layers[[1]]$rgb$alpha, 4L)
  ## A palette over a JPEG COG is still 0.3: the tiles are JPEG.
  j <- cog_scene(fixture("polar_ycbcr.tif"), band = 3L, coastline = FALSE)
  expect_identical(j$version, "0.3")
  expect_identical(j$layers[[1]]$palette$name, "viridis")
  expect_valid_tiled_scene(j)
  ## Single-band COGs stay 0.2 with a palette whatever rgb says by default.
  expect_identical(cog_scene(fixture("polar_3031.tif"), coastline = FALSE)$version, "0.2")
})

test_that("rgb = names bands, and bad band choices are refused", {
  skip_if_no_gdal()
  f <- fixture("polar_rgba.tif")
  s <- cog_scene(f, rgb = c(3, 2, 1), coastline = FALSE)
  expect_identical(s$layers[[1]]$rgb, list(bands = c(3L, 2L, 1L)))
  expect_valid_tiled_scene(s)
  s <- cog_scene(f, rgb = c(1, 1, 1, 4), range = c(0, 200), coastline = FALSE)
  expect_identical(s$layers[[1]]$rgb, list(bands = c(1L, 1L, 1L), alpha = 4L, range = c(0, 200)))
  expect_valid_tiled_scene(s)
  expect_error(cog_scene(f, rgb = c(1, 2, 3, 3)), "alpha band")
  expect_error(cog_scene(f, rgb = c(1, 2, 5)), "band numbers from 1 to 4")
  expect_error(cog_scene(f, rgb = c(1, 2)), "3 or 4 band numbers")
  expect_error(cog_scene(fixture("polar_3031.tif"), rgb = TRUE), "needs 3 or 4 bands")
})

test_that("rgb of non-Byte bands gets a range from the data", {
  skip_if_no_gdal()
  mem <- gdalraster::create("MEM", "", 64L, 64L, 3L, "UInt16", return_obj = TRUE)
  mem$setGeoTransform(c(-3.2e6, 1e5, 0, 3.2e6, 0, -1e5))
  mem$setProjection(gdalraster::srs_to_wkt("EPSG:3031"))
  for (b in 1:3) mem$write(b, 0L, 0L, 64L, 64L, rep(c(100, 4000) * b, length.out = 64 * 64))
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f))
  gdalraster::createCopy("COG", f, mem, quiet = TRUE, options = "COMPRESS=DEFLATE")
  mem$close()
  expect_null(rgb_default(cog_info(f)))
  s <- cog_scene(f, rgb = TRUE, coastline = FALSE)
  expect_identical(s$layers[[1]]$rgb$range, c(100, 12000))
  expect_valid_tiled_scene(s)
})

test_that("JPEG that is not YCbCr or greyscale is refused per layer", {
  skip_if_no_gdal()
  rgb_jpeg <- make_rgb_tif("GTiff", c("TILED=YES", "COMPRESS=JPEG", "PHOTOMETRIC=RGB",
                                       "INTERLEAVE=PIXEL"))
  skip_if(is.null(rgb_jpeg), "this GDAL cannot write RGB JPEG GeoTIFF")
  on.exit(unlink(rgb_jpeg))
  cog <- cog_info(rgb_jpeg)
  expect_identical(cog$photometric, "RGB")
  expect_identical(cog$levels[[1]]$encoding$codec, "jpeg")
  expect_error(scene_add_tiled_raster(scene(), "chart", cog),
               "Layer \"chart\": level 0 is JPEG with 3 bands and photometric RGB; only YCbCr",
               fixed = TRUE)
  expect_error(scene_add_tiled_raster(scene(), "chart", cog, palette = "gray"), "photometric RGB")

  ## Greyscale (MinIsBlack) JPEG is fine, with a palette.
  grey <- make_rgb_tif("COG", c("COMPRESS=JPEG"), nb = 1L, interp = "Gray")
  skip_if(is.null(grey), "this GDAL cannot write a greyscale JPEG COG")
  on.exit(unlink(grey), add = TRUE)
  g <- cog_info(grey)
  expect_identical(g$photometric, "MinIsBlack")
  s <- scene_add_tiled_raster(scene(), "g", g)
  expect_identical(s$version, "0.3")
  expect_valid_tiled_scene(s)
})

test_that("JPEG COGs with an internal mask and BigTIFF are read", {
  skip_if_no_gdal()
  ## An RGBA source written as JPEG: GDAL keeps the alpha as a mask IFD per
  ## level, which is not one of the levels.
  f <- make_rgb_tif("COG", c("COMPRESS=JPEG", "OVERVIEW_COUNT=1"), nb = 4L)
  skip_if(is.null(f), "this GDAL cannot write a JPEG COG from RGBA")
  on.exit(unlink(f))
  ifds <- tiff_ifds(f)
  expect_true(any(vapply(ifds, function(d) bitwAnd(as.integer(d$subfile), 4L) != 0L, TRUE)))
  cog <- cog_info(f)
  expect_identical(cog$samples_per_pixel, 3L)
  expect_identical(vapply(cog$levels, `[[`, "", "photometric"), c("YCbCr", "YCbCr"))
  expect_true(all(vapply(cog$levels, function(l) !is.null(l$encoding$jpeg_tables), TRUE)))

  big <- make_rgb_tif("COG", c("COMPRESS=JPEG", "BIGTIFF=YES"))
  skip_if(is.null(big), "this GDAL cannot write BigTIFF")
  on.exit(unlink(big), add = TRUE)
  expect_identical(readBin(big, "raw", 4)[3:4], as.raw(c(0x2b, 0x00)))
  b <- cog_info(big)
  expect_identical(b$photometric, "YCbCr")
  expect_identical(b64_decode(b$levels[[1]]$encoding$jpeg_tables)[1:2], as.raw(c(0xff, 0xd8)))
})

test_that("the TIFF reader fails soft", {
  skip_if_no_gdal()
  expect_null(tiff_ifds(tempfile()))
  junk <- tempfile()
  on.exit(unlink(junk))
  writeBin(charToRaw("not a tiff at all"), junk)
  expect_null(tiff_ifds(junk))
  expect_identical(photometric_name(6), "YCbCr")
  expect_identical(photometric_name(NA), NA_character_)
  expect_identical(photometric_name(32844), "code 32844")
})

test_that("scene_spec_version() is the lowest version that fits", {
  tr <- list(kind = "tiled_raster", palette = list(name = "gray", range = c(0, 1)),
             plan = list(levels = list(list(encoding = list(codec = "deflate")))))
  with_layer <- function(l) list(version = "0.1", layers = list(l))
  expect_identical(scene_spec_version(with_layer(tr)), "0.2")
  tr_rgb <- tr
  tr_rgb$palette <- NULL
  tr_rgb$rgb <- list(bands = 1:3)
  expect_identical(scene_spec_version(with_layer(tr_rgb)), "0.3")
  tr_jpeg <- tr
  tr_jpeg$plan$levels[[1]]$encoding$codec <- "jpeg"
  expect_identical(scene_spec_version(with_layer(tr_jpeg)), "0.3")
})
