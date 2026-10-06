# A VRT or GTI mosaic's members, and a plan per member (aobview#41,
# allboa/design decision 0011).

fixture <- function(name) system.file("extdata", name, package = "aobcore")

## polar_3031.tif as two COG halves (west and east, 200 x 400 cells each,
## 128 x 128 tiles) in a new directory, with a VRT gdalraster builds over
## them: list(dir, a, b, vrt).
cog_halves <- function() {
  dir <- tempfile("mosaic-")
  dir.create(dir)
  ## Members come back as normalizePath() gives them (on Windows, the long
  ## form with forward slashes, not tempdir()'s short 8.3 form).
  dir <- normalizePath(dir, winslash = "/")
  f <- fixture("polar_3031.tif")
  a <- file.path(dir, "west.tif")
  b <- file.path(dir, "east.tif")
  opts <- c("-of", "COG", "-co", "BLOCKSIZE=128")
  gdalraster::translate(f, a, c(opts, "-srcwin", "0", "0", "200", "400"), quiet = TRUE)
  gdalraster::translate(f, b, c(opts, "-srcwin", "200", "0", "200", "400"), quiet = TRUE)
  vrt <- file.path(dir, "halves.vrt")
  gdalraster::buildVRT(vrt, c(a, b), quiet = TRUE)
  list(dir = dir, a = a, b = b, vrt = vrt)
}

## A VRT over polar_3031.tif's grid written by hand, with the given source
## elements in band 1.
hand_vrt <- function(dir, sources, srs = "EPSG:3031", band_attr = "") {
  f <- file.path(dir, "hand.vrt")
  writeLines(c(
    '<VRTDataset rasterXSize="400" rasterYSize="400">',
    paste0("<SRS>", srs, "</SRS>"),
    "<GeoTransform>-6400000, 32000, 0, 6400000, 0, -32000</GeoTransform>",
    paste0('<VRTRasterBand dataType="Int16" band="1"', band_attr, ">"),
    sources,
    "</VRTRasterBand>",
    "</VRTDataset>"), f)
  f
}

simple_source <- function(file, dst = c(0, 0, 200, 400), src = c(0, 0, 200, 400), band = 1L,
                          kind = "SimpleSource", extra = "", relative = "1") {
  rect <- function(nm, r) sprintf('<%s xOff="%s" yOff="%s" xSize="%s" ySize="%s"/>', nm,
                                  r[1], r[2], r[3], r[4])
  paste0("<", kind, '><SourceFilename relativeToVRT="', relative, '">', file,
         "</SourceFilename><SourceBand>", band, "</SourceBand>", rect("SrcRect", src),
         rect("DstRect", dst), extra, "</", kind, ">")
}

test_that("mosaic_members() reads a VRT gdalraster built, resolving relative members", {
  skip_if_no_gdal()
  h <- cog_halves()
  m <- mosaic_members(h$vrt)
  expect_s3_class(m, "aob_mosaic")
  expect_identical(m$kind, "vrt")
  expect_identical(m$crs, "EPSG:3031")
  expect_identical(m$dim, c(400L, 400L))
  expect_equal(m$extent, c(-6400000, 6400000, -6400000, 6400000))
  expect_null(m$note)
  mem <- m$members
  ## buildVRT writes relativeToVRT="1" names: resolved against the VRT's directory.
  expect_match(readLines(h$vrt), 'relativeToVRT="1">west.tif', all = FALSE)
  expect_identical(mem$dsn, c(h$a, h$b))
  expect_identical(mem$band, c(1L, 1L))
  expect_identical(mem$source_band, c(1L, 1L))
  expect_equal(mem$xmin, c(-6400000, 0))
  expect_equal(mem$xmax, c(0, 6400000))
  expect_equal(mem$ymin, c(-6400000, -6400000))
  expect_equal(mem$ymax, c(6400000, 6400000))
  expect_equal(mem$src_xsize, c(200, 200))
  expect_true(all(is.na(mem$reason)))
  expect_output(print(m), "VRT in EPSG:3031, 400 x 400 cells, 2 members")
  expect_output(print(m), "band 1: west.tif, east.tif")
  ## The same VRT by a relative path from its directory.
  old <- setwd(h$dir)
  on.exit(setwd(old), add = TRUE)
  expect_identical(mosaic_members("halves.vrt")$members$dsn, c(h$a, h$b))
  setwd(old)
  ## A vrt:// connection string is a VRT too (GDAL writes absolute names).
  v <- mosaic_members(paste0("vrt://", h$a, "?bands=1"))
  expect_identical(v$kind, "vrt")
  expect_identical(v$members$dsn, h$a)
  ## A multi-band VRT: one row per member and band, each at its source band.
  rgba <- file.path(h$dir, "rgba.vrt")
  gdalraster::buildVRT(rgba, fixture("polar_rgba.tif"), quiet = TRUE)
  r <- mosaic_members(rgba)$members
  expect_identical(r$band, 1:4)
  expect_identical(r$source_band, 1:4)
  expect_identical(unique(r$dsn), fixture("polar_rgba.tif"))
})

test_that("a plain file is not a mosaic, and bad input is an error", {
  skip_if_no_gdal()
  expect_null(mosaic_members(fixture("polar_3031.tif")))
  expect_error(mosaic_members(c("a", "b")), "single path")
  expect_error(mosaic_members(tempfile(fileext = ".vrt")), "No file at")
  expect_error(mosaic_members(fixture("coastline_south_40s.geojson")), "cannot open")
  expect_error(mosaic_plan(fixture("polar_3031.tif"), "EPSG:3031"), "not a VRT or GTI mosaic")
})

test_that("the VRT parser keeps sources it cannot draw in place, with a reason", {
  skip_if_no_gdal()
  h <- cog_halves()
  ## Entities in a name, an absolute name, a ComplexSource that rescales, a
  ## kernel filter, a mask band and a source band of another number.
  dir.create(file.path(h$dir, "a&b"))
  file.copy(h$a, file.path(h$dir, "a&b", "west.tif"))
  srcs <- c(
    simple_source("a&amp;b/west.tif"),
    simple_source(h$b, dst = c(200, 0, 200, 400), relative = "0", kind = "ComplexSource",
                  extra = "<ScaleRatio>2</ScaleRatio>"),
    simple_source("west.tif", kind = "ComplexSource",
                  extra = "<NODATA>-32768</NODATA><UseMaskBand>true</UseMaskBand>"),
    simple_source("west.tif", kind = "KernelFilteredSource",
                  extra = "<Kernel><Size>3</Size><Coefs>0 0 0 0 1 0 0 0 0</Coefs></Kernel>"),
    simple_source("west.tif", band = "mask,1"),
    simple_source("west.tif", band = 1L, src = c(0, 0, 100, 400), dst = c(0, 0, 100, 400))
  )
  f <- hand_vrt(h$dir, srcs)
  m <- mosaic_members(f)$members
  expect_identical(nrow(m), 6L)
  expect_identical(m$dsn[1], file.path(h$dir, "a&b", "west.tif"))
  expect_identical(m$dsn[2], h$b)
  expect_identical(m$reason[1], NA_character_)
  expect_match(m$reason[2], "rescaled")
  expect_identical(m$reason[3], NA_character_)
  expect_match(m$reason[4], "KernelFilteredSource")
  expect_match(m$reason[5], "mask band")
  expect_true(is.na(m$source_band[5]))
  expect_equal(m[6, c("src_xsize", "src_ysize")], data.frame(src_xsize = 100, src_ysize = 400),
               ignore_attr = TRUE)
  ## A derived band's sources feed a pixel function.
  d <- hand_vrt(h$dir, c("<PixelFunctionType>sum</PixelFunctionType>", simple_source("west.tif")),
                band_attr = ' subClass="VRTDerivedRasterBand"')
  expect_match(mosaic_members(d)$members$reason, "VRTDerivedRasterBand")
  ## A warped VRT has no members.
  w <- file.path(h$dir, "warp.vrt")
  gdalraster::warp(h$a, w, "EPSG:3976", cl_arg = c("-of", "VRT"), quiet = TRUE)
  wm <- mosaic_members(w)
  expect_identical(nrow(wm$members), 0L)
  expect_match(wm$note, "VRTWarpedDataset")
  expect_output(print(wm), "0 members \\(a VRTWarpedDataset VRT")
  expect_error(mosaic_plan(wm, "EPSG:3031"), "has no members: a VRTWarpedDataset")
  ## The parser alone, on XML with nothing to read.
  expect_identical(nrow(vrt_members("<VRTDataset></VRTDataset>", "x.vrt", 1:6, c(1L, 1L))$members), 0L)
})

test_that("mosaic_plan() plans each member in place and probes only the members a view touches", {
  skip_if_no_gdal()
  h <- cog_halves()
  probed <- character()
  real <- cog_info
  local_mocked_bindings(cog_info = function(dsn, band = 1L) {
    probed <<- c(probed, dsn)
    real(dsn, band = band)
  })
  p <- mosaic_plan(h$vrt, "EPSG:3031")
  expect_s3_class(p, "aob_mosaic_plan")
  expect_identical(names(p$plans), c(h$a, h$b))
  expect_identical(p$members$status, c("planned", "planned"))
  expect_identical(probed, c(h$a, h$b))
  expect_output(print(p), "2 of 2 members planned in EPSG:3031, 22 tiles")
  ## Each member's plan is the member's own COG, referenced by its file.
  expect_s3_class(p$plans[[1]], "aob_tile_plan")
  expect_identical(p$plans[[1]]$cog$dsn, h$a)
  expect_identical(p$plans[[2]]$cog$dsn, h$b)
  ## The two plans' full-resolution tiles cover the mosaic.
  fp <- do.call(rbind, lapply(p$plans, function(q) {
    do.call(rbind, lapply(q$plan$levels[[length(q$plan$levels)]]$tiles, `[[`, "footprint"))
  }))
  expect_equal(c(min(fp[, 1]), max(fp[, 2]), min(fp[, 3]), max(fp[, 4])),
               c(-6400000, 6400000, -6400000, 6400000))
  expect_equal(plan_extent(p$plans[[1]]), c(-6400000, 0, -6400000, 6400000))
  expect_equal(plan_extent(p$plans[[2]]), c(0, 6400000, -6400000, 6400000))

  ## A view over the east half only: the west member is skipped, not probed.
  probed <- character()
  e <- mosaic_plan(h$vrt, "EPSG:3031", extent = c(1e6, 5e6, -3e6, 3e6), units_per_pixel = 70000)
  expect_identical(e$members$status, c("skipped", "planned"))
  expect_identical(probed, h$b)
  expect_identical(e$plans[[h$b]]$plan$coverage, "view")
  expect_identical(e$plans[[h$b]]$plan$levels[[1]]$level, 1L)
  ## The extent is transformed to the mosaic's CRS: the eastern hemisphere
  ## in lon/lat is the east half.
  probed <- character()
  ll <- mosaic_plan(h$vrt, "EPSG:4326", extent = c(10, 170, -89, -50))
  expect_identical(ll$members$status, c("skipped", "planned"))
  expect_identical(probed, h$b)
  ## The plan's own arguments pass through.
  one <- mosaic_plan(h$vrt, "EPSG:3031", levels = 0L)
  expect_identical(vapply(one$plans[[1]]$plan$levels, function(l) l$level, 0L), 0L)
  expect_error(mosaic_plan(h$vrt, "EPSG:3031", band = 2), "Band 2 .* has no members")
  expect_error(mosaic_plan(h$vrt, "EPSG:3031", band = 0), "`band`")
})

test_that("members that cannot be drawn in place are reported, not planned", {
  skip_if_no_gdal()
  h <- cog_halves()
  ## A striped GeoTIFF (no tiles, no overviews) beside a COG.
  striped <- file.path(h$dir, "striped.tif")
  gdalraster::translate(fixture("polar_3031.tif"), striped, c("-of", "GTiff"), quiet = TRUE)
  v <- file.path(h$dir, "mixed.vrt")
  gdalraster::buildVRT(v, c(h$a, striped), quiet = TRUE)
  p <- mosaic_plan(v, "EPSG:3031")
  expect_identical(p$members$status, c("planned", "unplanned"))
  expect_match(p$members$reason[2], "not a tiled GeoTIFF with overviews")
  expect_identical(names(p$plans), h$a)
  expect_output(print(p), "striped.tif: unplanned \\(is not a tiled")
  ## A member stretched over the whole grid (its DstRect is not its own
  ## extent), one drawn from a window of its cells, one in another CRS,
  ## and one that does not exist.
  f <- hand_vrt(h$dir, c(
    simple_source("west.tif", dst = c(0, 0, 400, 400)),
    simple_source("west.tif", src = c(0, 0, 100, 400), dst = c(0, 0, 100, 400)),
    simple_source("gone.tif")
  ))
  p2 <- mosaic_plan(f, "EPSG:3031")
  expect_identical(p2$members$status, rep("unplanned", 3L))
  expect_match(p2$members$reason[1], "own georeferencing")
  expect_match(p2$members$reason[2], "window of its cells")
  expect_match(p2$members$reason[3], "cannot be read as a COG: No file at")
  expect_length(p2$plans, 0L)
  other <- hand_vrt(h$dir, simple_source("west.tif"), srs = "EPSG:3976")
  expect_match(mosaic_plan(other, "EPSG:3031")$members$reason, "not in the mosaic's CRS")
  ## A rescaled source is unplanned without being probed.
  probed <- 0L
  local_mocked_bindings(cog_info = function(dsn, band = 1L) {
    probed <<- probed + 1L
    stop("probed")
  })
  sc <- hand_vrt(h$dir, simple_source("west.tif", kind = "ComplexSource",
                                      extra = "<ScaleOffset>1</ScaleOffset>"))
  p3 <- mosaic_plan(sc, "EPSG:3031")
  expect_identical(p3$members$status, "unplanned")
  expect_match(p3$members$reason, "rescaled")
  expect_identical(probed, 0L)
})

## A GTI index of the two halves, written with GDAL's `raster index`
## (gdalraster::gdal_run(), GDAL >= 3.11), or NULL when that is not here.
gti_index <- function(h, file = "halves.gti.gpkg") {
  if (!"gdal_run" %in% getNamespaceExports("gdalraster")) return(NULL)
  if (!nrow(gdalraster::gdal_formats("GTI"))) return(NULL)
  idx <- file.path(h$dir, file)
  ok <- tryCatch({
    gdalraster::gdal_run("raster index", c("--input", h$a, "--input", h$b, "--output", idx),
                         close = TRUE, quiet = TRUE)
    file.exists(idx)
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) return(NULL)
  idx
}

test_that("mosaic_members() reads a GTI's index layer, and mosaic_plan() plans its members", {
  skip_if_no_gdal()
  h <- cog_halves()
  idx <- gti_index(h)
  skip_if(is.null(idx), "GDAL cannot write a GTI index here (gdal raster index)")
  g <- mosaic_members(idx)
  expect_identical(g$kind, "gti")
  expect_identical(g$crs, "EPSG:3031")
  expect_identical(g$dim, c(400L, 400L))
  mem <- g$members
  expect_setequal(mem$dsn, c(h$a, h$b))
  expect_true(all(is.na(mem$band)))
  expect_true(all(is.na(mem$source_band)))
  i <- match(c(h$a, h$b), mem$dsn)
  expect_equal(mem$xmin[i], c(-6400000, 0))
  expect_equal(mem$xmax[i], c(0, 6400000))
  expect_output(print(g), "GTI in EPSG:3031, 400 x 400 cells, 2 members")
  ## The index with a "GTI:" prefix, and named by a .gti file (an index
  ## dataset relative to the .gti file).
  expect_setequal(mosaic_members(paste0("GTI:", idx))$members$dsn, c(h$a, h$b))
  gti <- file.path(h$dir, "halves.gti")
  writeLines(c("<GDALTileIndexDataset>",
               paste0("<IndexDataset>", basename(idx), "</IndexDataset>"),
               "<LocationField>location</LocationField>",
               "</GDALTileIndexDataset>"), gti)
  expect_setequal(mosaic_members(gti)$members$dsn, c(h$a, h$b))
  ## Locations relative to the index resolve against its directory.
  rel <- file.path(h$dir, "rel.gti.gpkg")
  file.copy(idx, rel)
  lyr <- gdalraster::ogr_ds_layer_names(rel)[1]
  gdalraster::ogr_execute_sql(rel, paste0("UPDATE \"", lyr, "\" SET location = ",
                                          "replace(location, '", h$dir, "/', '')"))
  r <- suppressMessages(gdalraster::GDALVector$new(rel))
  locs <- r$fetch(-1)$location
  r$close()
  expect_setequal(locs, c("west.tif", "east.tif"))
  expect_setequal(mosaic_members(rel)$members$dsn, c(h$a, h$b))
  ## One plan per member, at the band asked for.
  probed <- character()
  real <- cog_info
  local_mocked_bindings(cog_info = function(dsn, band = 1L) {
    probed <<- c(probed, dsn)
    real(dsn, band = band)
  })
  p <- mosaic_plan(g, "EPSG:3031", extent = c(-5e6, -1e6, -3e6, 3e6))
  expect_identical(sort(probed), h$a)
  expect_identical(p$members$status[match(c(h$a, h$b), p$members$dsn)], c("planned", "skipped"))
  expect_identical(p$members$source_band, c(1L, 1L))
  expect_identical(p$plans[[h$a]]$cog$band, 1L)
})
