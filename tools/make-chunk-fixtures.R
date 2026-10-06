# Write the renderer's scene spec 0.6 (chunk references) test fixtures.
#
#   Rscript tools/make-chunk-fixtures.R
#
# Needs aobcore installed (for cog_chunks(), cog_plan() and
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
# - js/test/fixtures/chunks/uint16-zstd-band.tif: a 90 x 70 GeoTIFF, 2
#   uint16 bands stored separately (INTERLEAVE=BAND, so a ref per band),
#   zstd with the horizontal predictor, 32 x 32 tiles.
# - js/test/fixtures/chunks/float32-lzw.tif: a 90 x 70 GeoTIFF, 1 float32
#   band, lzw with the floating point predictor, 32 x 32 tiles.
# - js/test/chunks-gdal.json: cases, each a chunks source with cells whose
#   values GDAL reads (the four TIFFs as cog_chunks() describes them, and
#   scenespec's tiny.zarr as vendored in js/test/fixtures/scenespec), so the
#   renderer's chunk decoding is checked against GDAL; a 0.6 scene drawing
#   int16-pixel.tif over its chunks (scene_add_tiled_raster(format =
#   "chunks"), levels 0 and 1, band 2 through a palette, read by range) with
#   its mesh blobs (base64); and the same layer planned at level 1 only and
#   embedded, with its blobs (meshes and the planned chunks' bytes, keyed
#   "<source>@<offset>+<length>").
library(gdalraster)
library(aobcore)

b64 <- utils::getFromNamespace("b64_encode", "aobcore")
dir.create(file.path("js", "test", "fixtures", "chunks"), showWarnings = FALSE)

wave <- function(nx, ny, b) {
  x <- rep(seq_len(nx) - 1, times = ny)
  y <- rep(seq_len(ny) - 1, each = nx)
  sin(x / 23 + b) * 9000 + cos(y / 17 * b) * 7000 + (x - y) * 20
}

write_tif <- function(f, driver, dtype, nx, ny, gt, values, options, nodata = NULL, nb = 2L) {
  mem <- create("MEM", "", nx, ny, nb, dtype, return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(gt)
  mem$setProjection(srs_to_wkt("EPSG:3031"))
  for (b in seq_len(nb)) {
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

fzstd <- write_tif(file.path("js", "test", "fixtures", "chunks", "uint16-zstd-band.tif"), "GTiff", "UInt16",
                   90L, 70L, c(-500000, 10000, 0, 400000, 0, -10000),
                   function(b) round(wave(90L, 70L, b) + 20000),
                   c("COMPRESS=ZSTD", "PREDICTOR=2", "TILED=YES", "BLOCKXSIZE=32", "BLOCKYSIZE=32",
                     "INTERLEAVE=BAND"))
flzw <- write_tif(file.path("js", "test", "fixtures", "chunks", "float32-lzw.tif"), "GTiff", "Float32",
                  90L, 70L, c(-500000, 10000, 0, 400000, 0, -10000),
                  function(b) wave(90L, 70L, b) / 1000,
                  c("COMPRESS=LZW", "PREDICTOR=3", "TILED=YES", "BLOCKXSIZE=32", "BLOCKYSIZE=32"),
                  nb = 1L)

# A TIFF as a chunks data reference (cog_chunks(): the grid of level 0 and
# its overviews, the codec chain of its encoding, and a ref for every tile
# GDAL wrote; an unwritten tile has none), each ref naming `url`.
chunks_ref <- function(f, url) {
  ref <- unclass(cog_chunks(f, url = url))
  ref$url <- NULL
  ref$refs$rows <- lapply(ref$refs$rows, function(r) c(r, list(url = url)))
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
src_zstd <- chunks_ref(fzstd, "fixtures/chunks/uint16-zstd-band.tif")
src_lzw <- chunks_ref(flzw, "fixtures/chunks/float32-lzw.tif")
stopifnot(identical(vapply(src_zstd$codecs, `[[`, "", "name"), c("bytes", "predictor", "zstd")),
          identical(src_zstd$interleave, "separate"),
          identical(vapply(src_lzw$codecs, `[[`, "", "name"), c("bytes", "predictor", "lzw")))
cells_int <- c(
  gdal_cells(fint, data.frame(x = c(0, 1, 2, 127, 128, 255, 200, 299, 256, 299, 0),
                              y = c(0, 0, 0, 127, 0, 5, 150, 199, 128, 130, 199)), 0L),
  gdal_cells(fint, data.frame(x = c(0, 1, 149, 70), y = c(0, 0, 99, 40)), 1L)
)
cells_flt <- gdal_cells(fflt, data.frame(x = c(0, 1, 63, 64, 99, 10), y = c(0, 0, 63, 0, 69, 66)), 0L)
edge_cells <- data.frame(x = c(0, 1, 31, 32, 89, 64, 45, 89), y = c(0, 0, 31, 0, 69, 33, 64, 0))
cells_zstd <- gdal_cells(fzstd, edge_cells, 0L)
cells_lzw <- gdal_cells(flzw, edge_cells, 0L)

# The vendored Zarr v2 array (GDAL's Zarr driver: x, y are array indices).
zdir <- file.path("js", "test", "fixtures", "scenespec", "conformance")
zscene <- jsonlite::fromJSON(file.path(zdir, "zarr-v2-chunks.json"), simplifyVector = FALSE)
cells_zarr <- gdal_cells(file.path(zdir, "tiny.zarr"),
                         data.frame(x = c(0, 1, 9, 10, 23, 5, 19, 23, 20),
                                    y = c(0, 0, 9, 0, 0, 12, 19, 10, 15)), 0L)

# The scene: int16-pixel.tif planned at levels 0 and 1 and drawn over its
# chunks, band 2 through a palette, read by range from "int16-pixel.tif";
# and the same layer planned at level 1 only with its chunks' bytes
# embedded.
cog2 <- cog_info(fint, band = 2L)
s <- scene_add_tiled_raster(scene("EPSG:3031", domain = FALSE), "cog", cog_plan(cog2, "EPSG:3031", levels = 0:1),
                            palette = "ocean", range = c(-20000, 20000), embed = FALSE,
                            url = "int16-pixel.tif", format = "chunks")
stopifnot(identical(s$version, "0.6"), identical(s$layers[[1]]$band, 2L))
blobs <- lapply(scene_blobs(s), b64)
s <- unclass(s)
attr(s, "blobs") <- NULL
attr(s, "files") <- NULL
e <- scene_add_tiled_raster(scene("EPSG:3031", domain = FALSE), "cog", cog_plan(cog2, "EPSG:3031", levels = 1L),
                            palette = "ocean", range = c(-20000, 20000), embed = TRUE,
                            url = "int16-pixel.tif", format = "chunks")
eblobs <- lapply(scene_blobs(e), b64)
e <- unclass(e)
attr(e, "blobs") <- NULL
out <- list(
  scene = s,
  blobs = blobs,
  embedded = list(scene = e, blobs = eblobs),
  cases = list(
    list(name = "zarr v2 float32 deflate (scenespec tiny.zarr)", base = "fixtures/scenespec/conformance",
         source = zscene$data$tiny, cells = cells_zarr),
    list(name = "COG int16, 2 bands pixel, deflate horizontal (int16-pixel.tif)", base = ".",
         source = src_int, cells = cells_int),
    list(name = "GeoTIFF float32 big endian, 2 bands pixel, deflate floating_point (float32-big.tif)",
         base = ".", source = src_flt, cells = cells_flt),
    list(name = "GeoTIFF uint16, 2 bands separate, zstd horizontal (uint16-zstd-band.tif)",
         base = ".", source = src_zstd, cells = cells_zstd),
    list(name = "GeoTIFF float32, lzw floating_point (float32-lzw.tif)",
         base = ".", source = src_lzw, cells = cells_lzw)
  )
)
f <- file.path("js", "test", "chunks-gdal.json")
writeLines(aobcore:::json_value(out, ""), f)
cat("wrote", f, file.size(f), "bytes;", paste(basename(c(fint, fflt, fzstd, flzw)),
    file.size(c(fint, fflt, fzstd, flzw)), "bytes", collapse = "; "), "\n")
