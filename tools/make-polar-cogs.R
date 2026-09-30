# Make the small polar COG fixtures in inst/extdata.
#
#   Rscript tools/make-polar-cogs.R [outdir]
#
# Needs gdalraster (GDAL with the COG driver). Writes two Cloud Optimized
# GeoTIFFs of one synthetic SST-like field (degrees C, stored as Int16 with
# scale 0.001, nodata -32768), 128 x 128 tiles, with overviews:
#
#   polar_3031.tif   EPSG:3031, 400 x 400 cells of 32 km over +-6400 km,
#                    DEFLATE with horizontal predictor, 3 overviews
#                    (200, 100 and 50 cells). Cells north of 40S and on the
#                    "land" mask are nodata.
#   polar_lonlat.tif EPSG:4326, 720 x 100 cells of 0.5 degrees, lon -180..180,
#                    lat -40..-90, LZW with horizontal predictor, 3 overviews
#                    (360 x 50, 180 x 25, 90 x 12). It covers the pole and has
#                    the antimeridian at its edges.
#
# Neither size is a multiple of the tile size, so edge tiles are partial and
# the overviews' pixel sizes are not exact multiples of the base (100 / 50
# cells is fine, 12 rows over 50 degrees is not): the planner must read each
# level's own geotransform.
library(gdalraster)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else file.path("inst", "extdata")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

scale <- 0.001
nodata <- -32768

# The field at lon/lat (degrees). Warm to the north, cold near the pole, with
# six 60-degree sectors (so any rotation or mirror shows), a ring at 60S and a
# warm blob on the antimeridian at 55S (so the seam shows). "Land" (nodata)
# is a lobed ring around the pole that leaves data south of 85S, so the
# sectors can be seen meeting at the pole.
field <- function(lon, lat) {
  t <- (lat + 90) / 50
  v <- -1.8 + 16 * t^1.4
  v <- v + 0.9 * ((floor((lon + 180) / 60) %% 2) - 0.5)
  v <- v + 2.5 * exp(-((lat + 60) / 0.8)^2)
  dlon <- ((lon - 180 + 540) %% 360) - 180
  v <- v + 4 * exp(-(dlon / 8)^2 - ((lat + 55) / 3)^2)
  land <- lat < -72 + 5 * sin(2 * lon * pi / 180) + 3 * cos(5 * lon * pi / 180) & lat > -85
  v[land | lat > -40] <- NA
  v
}

to_raw <- function(v) {
  r <- round(v / scale)
  r[is.na(r)] <- nodata
  as.integer(r)
}

write_cog <- function(file, nx, ny, gt, crs, values, compress) {
  mem <- create("MEM", "", nx, ny, 1L, "Int16", return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(gt)
  mem$setProjection(srs_to_wkt(crs))
  mem$setNoDataValue(1L, nodata)
  mem$setScale(1L, scale)
  mem$setOffset(1L, 0)
  mem$write(1L, 0L, 0L, nx, ny, values)
  if (file.exists(file)) unlink(file)
  createCopy("COG", file, mem, quiet = TRUE, options = c(
    paste0("COMPRESS=", compress), "PREDICTOR=YES", "BLOCKSIZE=128",
    "OVERVIEWS=IGNORE_EXISTING", "OVERVIEW_COUNT=3", "RESAMPLING=NEAREST",
    "OVERVIEW_RESAMPLING=AVERAGE"
  ))
  cat(sprintf("%s: %d bytes\n", file, file.size(file)))
}

# ---- EPSG:3031 ---------------------------------------------------------
n <- 400L
res <- 12800000 / n
cx <- -6400000 + (seq_len(n) - 0.5) * res
cy <- 6400000 - (seq_len(n) - 0.5) * res
xy <- cbind(rep(cx, times = n), rep(cy, each = n))   # row-major, row 0 at the top
ll <- transform_xy(xy, "EPSG:3031", "EPSG:4326")
write_cog(file.path(out, "polar_3031.tif"), n, n, c(-6400000, res, 0, 6400000, 0, -res),
          "EPSG:3031", to_raw(field(ll[, 1], ll[, 2])), "DEFLATE")

# ---- lon/lat -----------------------------------------------------------
nx <- 720L
ny <- 100L
lon <- -180 + (seq_len(nx) - 0.5) * 0.5
lat <- -40 - (seq_len(ny) - 0.5) * 0.5
write_cog(file.path(out, "polar_lonlat.tif"), nx, ny, c(-180, 0.5, 0, -40, 0, -0.5),
          "EPSG:4326", to_raw(field(rep(lon, times = ny), rep(lat, each = nx))), "LZW")
