# Write the renderer test fixtures. js/test/codec-tiles.json: one tile per encoding case, with the tile's
# bytes as GDAL wrote them and the values GDAL reads back, so the renderer's
# decoders are checked against GDAL.
#
#   Rscript tools/make-codec-tiles.R
#
# Needs aobcore installed (for cog_info()) and gdalraster. Each case is a
# tile of a GeoTIFF: its encoding (as cog_info() reads it), tile size,
# window, base64 bytes, and from GDAL the window's first and last rows and
# the sum of its raw values.
library(gdalraster)
library(aobcore)

b64 <- utils::getFromNamespace("b64_encode", "aobcore")

make_tif <- function(dtype, nb, nx, ny, values, options, driver = "GTiff") {
  mem <- create("MEM", "", nx, ny, nb, dtype, return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(c(0, 1, 0, ny, 0, -1))
  mem$setProjection(srs_to_wkt("EPSG:3031"))
  for (b in seq_len(nb)) mem$write(b, 0L, 0L, nx, ny, values(b))
  f <- tempfile(fileext = ".tif")
  createCopy(driver, f, mem, quiet = TRUE, options = options)
  f
}

tile_case <- function(name, f, col, row, band = 1L) {
  cog <- cog_info(f, band = band)
  lv <- cog$levels[[1]]
  t <- lv$tiles[lv$tiles$col == col & lv$tiles$row == row, ]
  stopifnot(nrow(t) == 1L)
  con <- file(f, "rb")
  seek(con, t$byte_offset)
  bytes <- readBin(con, "raw", t$byte_length)
  close(con)
  ts <- lv$tile_size
  vw <- min(ts[1], lv$dim[1] - col * ts[1])
  vh <- min(ts[2], lv$dim[2] - row * ts[2])
  ds <- new(GDALRaster, f)
  v <- ds$read(band, col * ts[1], row * ts[2], vw, vh, vw, vh)
  ds$close()
  # GDAL (via gdalraster) reads nodata cells as NA; the tile holds the raw value.
  if (!is.null(cog$nodata)) v[is.na(v)] <- cog$nodata
  m <- matrix(v, nrow = vh, byrow = TRUE)
  list(
    name = name,
    encoding = lv$encoding,
    size = as.integer(ts),
    window = list(x = 0L, y = 0L, width = as.integer(vw), height = as.integer(vh)),
    bytes = b64(bytes),
    first_row = as.numeric(m[1, ]),
    last_row = as.numeric(m[vh, ]),
    sum = sum(as.numeric(v))
  )
}

f3031 <- system.file("extdata", "polar_3031.tif", package = "aobcore")
flonlat <- system.file("extdata", "polar_lonlat.tif", package = "aobcore")
wave <- function(nx, ny, k = 1) function(b) {
  x <- rep(seq_len(nx), times = ny)
  y <- rep(seq_len(ny), each = nx)
  sin(x / 7 * k + b) * 1000 + cos(y / 5) * 700 + x * y / 3
}
nx <- 100L
ny <- 70L

cases <- list(
  tile_case("deflate horizontal int16 (polar_3031.tif)", f3031, 1L, 1L),
  tile_case("deflate horizontal int16, edge tile (polar_3031.tif)", f3031, 3L, 3L),
  tile_case("lzw horizontal int16, edge tile (polar_lonlat.tif)", flonlat, 5L, 0L),
  tile_case("zstd floating_point float32",
            make_tif("Float32", 1L, nx, ny, wave(nx, ny),
                     c("COMPRESS=ZSTD", "PREDICTOR=3", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64")), 1L, 0L),
  tile_case("deflate floating_point float64",
            make_tif("Float64", 1L, nx, ny, wave(nx, ny, 2),
                     c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64")), 0L, 1L),
  tile_case("lzw none uint16",
            make_tif("UInt16", 1L, nx, ny, function(b) abs(wave(nx, ny)(b)),
                     c("COMPRESS=LZW", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64")), 1L, 1L),
  tile_case("packbits none uint8",
            make_tif("Byte", 1L, nx, ny, function(b) abs(wave(nx, ny)(b)) %% 256,
                     c("COMPRESS=PACKBITS", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64")), 0L, 0L),
  tile_case("none none int32 big endian",
            make_tif("Int32", 1L, nx, ny, wave(nx, ny),
                     c("TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64", "ENDIANNESS=BIG")), 1L, 0L),
  tile_case("deflate horizontal uint16 big endian",
            make_tif("UInt16", 1L, nx, ny, function(b) abs(wave(nx, ny)(b)),
                     c("TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64", "ENDIANNESS=BIG",
                       "COMPRESS=DEFLATE", "PREDICTOR=2")), 0L, 1L),
  tile_case("deflate horizontal uint8, 3 bands interleaved, band 2",
            make_tif("Byte", 3L, nx, ny, function(b) abs(wave(nx, ny)(b)) %% 256,
                     c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64",
                       "INTERLEAVE=PIXEL")), 1L, 1L, band = 2L),
  tile_case("zstd none int16, 2 bands separate, band 2",
            make_tif("Int16", 2L, nx, ny, wave(nx, ny),
                     c("COMPRESS=ZSTD", "TILED=YES", "BLOCKXSIZE=64", "BLOCKYSIZE=64", "INTERLEAVE=BAND")), 0L, 0L, band = 2L)
)

out <- file.path("js", "test", "codec-tiles.json")
writeLines(aobcore:::json_value(cases, ""), out)
cat("wrote", out, file.size(out), "bytes\n")

# js/test/tiled-scene.json: a scene spec 0.2 scene with one tiled raster
# (polar_3031.tif, levels 3 and 2) whose cog URL is relative, for the
# renderer test that serves inst/extdata over HTTP and reads the tiles with
# range requests. Its mesh blobs are base64.
s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", cog_plan(f3031, "EPSG:3031", levels = 2:3),
                            palette = "ocean", range = c(-2, 15), embed = FALSE)
s$data$sst$url <- "polar_3031.tif"
s$view$extent <- c(-6.4e6, 6.4e6, -6.4e6, 6.4e6)
blobs <- lapply(scene_blobs(s), b64)
out <- file.path("js", "test", "tiled-scene.json")
writeLines(paste0('{"scene":', scene_json(s), ',"blobs":', aobcore:::json_value(blobs), "}"), out)
cat("wrote", out, file.size(out), "bytes\n")

# js/test/tiled-scene-fine.json: the same COG planned at full resolution
# only (level 0, 16 tiles, all in view), for the renderer test of a server
# that ignores Range: many tiles, one whole-file download.
s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", cog_plan(f3031, "EPSG:3031", levels = 0),
                            palette = "ocean", range = c(-2, 15), embed = FALSE,
                            url = "polar_3031.tif")
s$view$extent <- c(-6.4e6, 6.4e6, -6.4e6, 6.4e6)
blobs <- lapply(scene_blobs(s), b64)
out <- file.path("js", "test", "tiled-scene-fine.json")
writeLines(paste0('{"scene":', scene_json(s), ',"blobs":', aobcore:::json_value(blobs), "}"), out)
cat("wrote", out, file.size(out), "bytes\n")
