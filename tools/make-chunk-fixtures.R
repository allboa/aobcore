# Write the renderer's scene spec 0.6 (chunk references) test fixtures.
#
#   Rscript tools/make-chunk-fixtures.R
#
# Needs aobcore installed (for cog_info(), cog_plan() and
# scene_add_tiled_raster()) and gdalraster. Writes:
#
# - js/test/fixtures/chunks/int16-pixel.tif: a 300 x 200 COG in EPSG:3031,
#   2 int16 bands interleaved by pixel, deflate with the horizontal
#   predictor (so the predictor's stride is 2 samples), 128 x 128 tiles (the
#   right and bottom tiles padded), one overview, and one tile left
#   unwritten (SPARSE_OK, every cell no data).
# - js/test/fixtures/chunks/float32-big.tif: a 100 x 70 GeoTIFF, 2 float32
#   bands interleaved by pixel, big endian, deflate with the floating point
#   predictor, 64 x 64 tiles.
# - js/test/chunks-gdal.json: a 0.6 scene drawing int16-pixel.tif as
#   chunks (its real TileOffsets and TileByteCounts as refs, levels 0 and
#   1, band 2 through a palette) with its mesh blobs (base64), and cases:
#   a chunks source each (the two TIFFs, and scenespec's tiny.zarr as
#   vendored in js/test/fixtures/scenespec) with cells whose values GDAL
#   reads, so the renderer's chunk decoding is checked against GDAL.
library(gdalraster)
library(aobcore)

b64 <- utils::getFromNamespace("b64_encode", "aobcore")
dir.create(file.path("js", "test", "fixtures", "chunks"), showWarnings = FALSE)

wave <- function(nx, ny, b) {
  x <- rep(seq_len(nx) - 1, times = ny)
  y <- rep(seq_len(ny) - 1, each = nx)
  sin(x / 23 + b) * 9000 + cos(y / 17 * b) * 7000 + (x - y) * 20
}

write_tif <- function(f, driver, dtype, nx, ny, gt, values, options, nodata = NULL) {
  mem <- create("MEM", "", nx, ny, 2L, dtype, return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(gt)
  mem$setProjection(srs_to_wkt("EPSG:3031"))
  for (b in 1:2) {
    if (!is.null(nodata)) mem$setNoDataValue(b, nodata)
    mem$write(b, 0L, 0L, nx, ny, values(b))
  }
  unlink(f)
  createCopy(driver, f, mem, quiet = TRUE, options = options)
  f
}

# The int16 COG. Tile column 2, row 0 (cells 256 to 299, rows 0 to 127) is
# all no data, so GDAL does not write it.
nodata <- -32768
fint <- write_tif(file.path("js", "test", "fixtures", "chunks", "int16-pixel.tif"), "COG", "Int16",
                  300L, 200L, c(-3000000, 20000, 0, 2000000, 0, -20000),
                  function(b) {
                    v <- round(wave(300L, 200L, b))
                    x <- rep(0:299, times = 200L)
                    y <- rep(0:199, each = 300L)
                    v[x >= 256 & y < 128] <- nodata
                    v
                  },
                  c("COMPRESS=DEFLATE", "PREDICTOR=YES", "BLOCKSIZE=128", "INTERLEAVE=PIXEL",
                    "OVERVIEW_COMPRESS=DEFLATE", "OVERVIEW_PREDICTOR=YES", "OVERVIEW_COUNT=1",
                    "RESAMPLING=NEAREST", "SPARSE_OK=TRUE"),
                  nodata = nodata)
fflt <- write_tif(file.path("js", "test", "fixtures", "chunks", "float32-big.tif"), "GTiff", "Float32",
                  100L, 70L, c(-1000000, 10000, 0, 700000, 0, -10000),
                  function(b) wave(100L, 70L, b) / 1000,
                  c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64",
                    "INTERLEAVE=PIXEL", "ENDIANNESS=BIG"))

# A TIFF's levels as a chunks data reference: the grid of level 0 and its
# overviews, the codec chain of its encoding, and a ref for every tile
# GDAL wrote (TileOffsets and TileByteCounts; an unwritten tile has none).
chunks_ref <- function(f, url) {
  cog <- cog_info(f)
  l0 <- cog$levels[[1]]
  enc <- l0$encoding
  stopifnot(enc$codec == "deflate", enc$planar == "interleaved", cog$samples_per_pixel == 2L)
  codecs <- list(list(name = "bytes", configuration = list(endian = enc$byte_order)))
  if (enc$predictor != "none") {
    codecs[[2]] <- list(name = "predictor", configuration = list(type = enc$predictor))
  }
  codecs[[length(codecs) + 1L]] <- list(name = "deflate")
  levels <- lapply(cog$levels[-1], function(lv) {
    stopifnot(identical(lv$tile_size, l0$tile_size))
    list(level = lv$level, dim = lv$dim, geotransform = lv$geotransform)
  })
  rows <- list()
  for (lv in cog$levels) for (i in seq_len(nrow(lv$tiles))) {
    t <- lv$tiles[i, ]
    rows[[length(rows) + 1L]] <- list(level = lv$level, col = t$col, row = t$row, url = url,
                                      offset = t$byte_offset, length = t$byte_length)
  }
  grid <- list(crs = "EPSG:3031", geotransform = l0$geotransform, dim = l0$dim, chunk_size = l0$tile_size)
  if (length(levels)) grid$levels <- levels
  ref <- list(format = "chunks", grid = grid, dtype = enc$dtype, bands = 2L, interleave = "pixel",
              codecs = codecs)
  if (!is.null(cog$nodata)) ref$nodata <- cog$nodata
  ref$refs <- list(rows = rows)
  ref
}

# Values GDAL reads at cells (x, y) of a level, for both bands. Level k > 0
# is the (k - 1)-th overview.
gdal_cells <- function(f, cells, level = 0L, open_options = character()) {
  oo <- if (level > 0L) c(open_options, paste0("OVERVIEW_LEVEL=", level - 1L)) else open_options
  ds <- if (length(oo)) new(GDALRaster, f, TRUE, oo) else new(GDALRaster, f)
  on.exit(ds$close())
  nd <- ds$getNoDataValue(1L)
  out <- list()
  for (k in seq_len(nrow(cells))) for (b in 1:ds$getRasterCount()) {
    v <- ds$read(b, cells$x[k], cells$y[k], 1L, 1L, 1L, 1L)
    if (is.na(v)) v <- nd
    out[[length(out) + 1L]] <- list(level = level, band = b, x = cells$x[k], y = cells$y[k], value = v)
  }
  out
}

src_int <- chunks_ref(fint, "fixtures/chunks/int16-pixel.tif")
src_flt <- chunks_ref(fflt, "fixtures/chunks/float32-big.tif")
cells_int <- c(
  gdal_cells(fint, data.frame(x = c(0, 1, 2, 127, 128, 255, 200, 299, 256, 299, 0),
                              y = c(0, 0, 0, 127, 0, 5, 150, 199, 128, 130, 199)), 0L),
  gdal_cells(fint, data.frame(x = c(0, 1, 149, 70), y = c(0, 0, 99, 40)), 1L)
)
cells_flt <- gdal_cells(fflt, data.frame(x = c(0, 1, 63, 64, 99, 10), y = c(0, 0, 63, 0, 69, 66)), 0L)

# The vendored Zarr v2 array (GDAL's Zarr driver: x, y are array indices).
zdir <- file.path("js", "test", "fixtures", "scenespec", "conformance")
zscene <- jsonlite::fromJSON(file.path(zdir, "zarr-v2-chunks.json"), simplifyVector = FALSE)
cells_zarr <- gdal_cells(file.path(zdir, "tiny.zarr"),
                         data.frame(x = c(0, 1, 9, 10, 23, 5, 19, 23, 20),
                                    y = c(0, 0, 9, 0, 0, 12, 19, 10, 15)), 0L)

# The scene: int16-pixel.tif planned at levels 0 and 1 as a 0.2 tiled
# raster, then its cog source replaced by the chunks reference and its plan
# levels cut down to what a plan over chunks gives.
s <- scene_add_tiled_raster(scene("EPSG:3031"), "cog", cog_plan(fint, "EPSG:3031", levels = 0:1),
                            palette = "ocean", range = c(-20000, 20000), embed = FALSE,
                            url = "int16-pixel.tif")
blobs <- lapply(scene_blobs(s), b64)
s <- unclass(s)
attr(s, "blobs") <- NULL
s$version <- "0.6"
src_scene <- src_int
src_scene$url <- "int16-pixel.tif"
src_scene$refs$rows <- lapply(src_scene$refs$rows, function(r) {
  r$url <- NULL
  r
})
s$data$cog <- src_scene
s$layers[[1]]$band <- 2L
s$layers[[1]]$plan$levels <- lapply(s$layers[[1]]$plan$levels, function(lv) {
  list(level = lv$level, pixel_size = lv$pixel_size,
       tiles = lapply(lv$tiles, function(t) t[c("col", "row", "footprint", "mesh")]))
})
out <- list(
  scene = s,
  blobs = blobs,
  cases = list(
    list(name = "zarr v2 float32 deflate (scenespec tiny.zarr)", base = "fixtures/scenespec/conformance",
         source = zscene$data$tiny, cells = cells_zarr),
    list(name = "COG int16, 2 bands pixel, deflate horizontal (int16-pixel.tif)", base = ".",
         source = src_int, cells = cells_int),
    list(name = "GeoTIFF float32 big endian, 2 bands pixel, deflate floating_point (float32-big.tif)",
         base = ".", source = src_flt, cells = cells_flt)
  )
)
f <- file.path("js", "test", "chunks-gdal.json")
writeLines(aobcore:::json_value(out, ""), f)
cat("wrote", f, file.size(f), "bytes;", fint, file.size(fint), "bytes;", fflt, file.size(fflt), "bytes\n")
