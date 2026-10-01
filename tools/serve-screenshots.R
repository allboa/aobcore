# Screenshots of a scene served by serve_scene() (decision 0006), light and
# dark, and a check of what the browser fetched.
#
#   Rscript tools/serve-screenshots.R [outdir]
#
# Needs aobcore installed (R CMD INSTALL .), gdalraster, httpuv, and Node
# with js/ installed (npm ci; set CHROMIUM_PATH as for js/screenshots.mjs).
# Run from the package root. Serves the polar 3031 COG fixture not embedded
# (embed = FALSE) with the coastline, takes served-polar-cog-3031-light.png
# and -dark.png with js/screenshots.mjs --url (default outdir
# tools/screenshots) while R answers requests, and stops with an error if
# any tile was not fetched as a 206 range or the COG was fetched whole.
library(aobcore)

args <- commandArgs(trailingOnly = TRUE)
out <- normalizePath(if (length(args)) args[[1]] else "tools/screenshots", mustWork = FALSE)
f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
s <- scene("EPSG:3031")
s <- scene_add_tiled_raster(s, "cog", cog_plan(f, "EPSG:3031"), palette = "ocean",
                            embed = FALSE, label = "polar_3031.tif")
s <- scene_add_vector(s, "coast", gdal_vector_stream(
  system.file("extdata", "coastline_south_40s.geojson", package = "aobcore"), "EPSG:3031",
  densify = 0.25), stroke = c(60, 66, 72, 255), stroke_width_px = 1, label = "Coastline (50m)")
srv <- suppressWarnings(serve_scene(s, open = FALSE, title = "polar_3031.tif"),
                        classes = "aobcore_serve_noninteractive")
cat("serving", srv$url, "\n")

log <- tempfile(fileext = ".log")
system2("node", c("js/screenshots.mjs", "--url", "served-polar-cog-3031", srv$url, shQuote(out)),
        stdout = log, stderr = log, wait = FALSE)
t0 <- Sys.time()
done <- function() file.exists(log) && any(readLines(log, warn = FALSE) == "done")
while (!done() && as.numeric(Sys.time() - t0, units = "secs") < 300) httpuv::service(50)
srv$stop()
lines <- readLines(log, warn = FALSE)
cat(lines, sep = "\n")
if (!done()) stop("js/screenshots.mjs did not finish.")
if (any(startsWith(lines, "FAIL"))) stop("A screenshot failed.")

resp <- sub("^ +response ", "", grep("^ +response ", lines, value = TRUE))
parts <- strsplit(resp, " ", fixed = TRUE)
status <- vapply(parts, `[`, "", 1)
bytes <- as.numeric(vapply(parts, `[`, "", 2))
range <- vapply(parts, `[`, "", 3)
path <- vapply(parts, `[`, "", 4)
cog <- grepl("/files/cog/polar_3031[.]tif$", path)
if (!any(cog)) stop("No tile was fetched from the served file.")
if (any(cog & status != "206")) stop("The COG was answered other than 206: ", paste(resp[cog & status != "206"], collapse = "; "))
want <- vapply(strsplit(sub("^bytes=", "", range[cog]), "-"), function(x) diff(as.numeric(x)) + 1, 0)
if (!identical(bytes[cog], want)) stop("A range answered the wrong number of bytes.")
cat(sprintf("ok: %d tile ranges (%.0f bytes in all, the largest %.0f), no whole-file fetch of %.0f bytes\n",
            sum(cog), sum(bytes[cog]), max(bytes[cog]), file.size(f)))
