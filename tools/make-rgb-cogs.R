# Make the small colour COG fixtures in inst/extdata.
#
#   Rscript tools/make-rgb-cogs.R [outdir]
#
# Needs gdalraster (GDAL with the COG driver). Writes two Cloud Optimized
# GeoTIFFs in EPSG:3031, 250 x 250 cells of 32 km over +-4000 km, 128 x 128
# tiles (so edge tiles are partial), with 2 overviews:
#
#   polar_rgba.tif  4-band Byte RGBA (colour interpretation Red, Green, Blue,
#                   Alpha), LZW, pixel interleaved. Four flat quadrants
#                   around the pole, red towards 0 (up), green towards 90E
#                   (right), blue towards 180 (down) and yellow towards 90W
#                   (left), with white rings every 1000 km. Alpha is 0
#                   outside 3800 km of the pole, and 128 in a band from 2400
#                   to 2600 km, so full, half and zero opacity all show.
#   polar_ycbcr.tif 3-band Byte RGB stored as YCbCr JPEG (TIFF photometric
#                   6, quality 85; GDAL's default for a 3-band JPEG COG), so
#                   it has shared JPEGTables in each image. A colour wheel:
#                   hue by angle clockwise from 0 (up), red at 0, green at
#                   120E, blue at 120W, fading to white at the pole, with
#                   dark rings every 1000 km.
library(gdalraster)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else file.path("inst", "extdata")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

n <- 250L
res <- 8000000 / n
cx <- -4000000 + (seq_len(n) - 0.5) * res
cy <- 4000000 - (seq_len(n) - 0.5) * res
x <- rep(cx, times = n)       # row-major, row 0 at the top
y <- rep(cy, each = n)
r <- sqrt(x^2 + y^2)
ang <- (atan2(x, y) * 180 / pi) %% 360   # clockwise from up: longitude east
ring <- abs(r / 1e6 - round(r / 1e6)) < 0.03 & r > 5e5

write_cog <- function(file, bands, interp, options) {
  nb <- length(bands)
  mem <- create("MEM", "", n, n, nb, "Byte", return_obj = TRUE)
  on.exit(mem$close())
  mem$setGeoTransform(c(-4000000, res, 0, 4000000, 0, -res))
  mem$setProjection(srs_to_wkt("EPSG:3031"))
  for (b in seq_len(nb)) {
    mem$setRasterColorInterp(b, interp[b])
    mem$write(b, 0L, 0L, n, n, as.integer(bands[[b]]))
  }
  if (file.exists(file)) unlink(file)
  createCopy("COG", file, mem, quiet = TRUE, options = c(
    "BLOCKSIZE=128", "OVERVIEWS=IGNORE_EXISTING", "OVERVIEW_COUNT=2",
    "OVERVIEW_RESAMPLING=AVERAGE", options
  ))
  cat(sprintf("%s: %d bytes\n", file, file.size(file)))
}

# ---- RGBA, LZW ---------------------------------------------------------
q <- floor(((ang + 45) %% 360) / 90) + 1   # 1 up, 2 right, 3 down, 4 left
pal <- rbind(c(220, 30, 30), c(30, 170, 60), c(40, 70, 220), c(235, 200, 30))
rgb <- pal[q, ]
rgb[ring, ] <- 255
alpha <- ifelse(r > 3800000, 0, ifelse(r > 2400000 & r < 2600000, 128, 255))
write_cog(file.path(out, "polar_rgba.tif"),
          list(rgb[, 1], rgb[, 2], rgb[, 3], alpha), c("Red", "Green", "Blue", "Alpha"),
          c("COMPRESS=LZW", "INTERLEAVE=PIXEL"))

# ---- YCbCr JPEG --------------------------------------------------------
hue <- function(h) {
  # HSV to RGB at full value; saturation grows away from the pole.
  s <- pmin(1, r / 2e6)
  k <- function(m) (m + h / 60) %% 6
  f <- function(m) 1 - s * pmax(0, pmin(k(m), 4 - k(m), 1))
  round(255 * cbind(f(5), f(3), f(1)))
}
wheel <- hue(ang)
wheel[ring, ] <- 40L
write_cog(file.path(out, "polar_ycbcr.tif"),
          list(wheel[, 1], wheel[, 2], wheel[, 3]), c("Red", "Green", "Blue"),
          c("COMPRESS=JPEG", "QUALITY=85"))
