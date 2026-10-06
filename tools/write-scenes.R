# Write the scenes that tools/screenshots are taken from, as self-contained
# pages and as scene JSON for the scenespec validator.
#
#   Rscript tools/write-scenes.R <outdir>
#
# Needs aobcore installed (R CMD INSTALL .). Writes, for each scene,
# <name>.html (write_scene_html()) and <name>.json (the embedded scene).
#
#   polar-probe  the scene spec 0.1 conformance scene (probe_scene())
#   kinds        every 0.1 layer kind and styling path the probe does not
#                use: points and multipoints with RGBA columns, polygons with
#                holes and a stroke, multilinestrings, a raster in the view
#                CRS with no mesh and NaN nodata, and view.local_origin with
#                origin_subtracted data, in EPSG:3031 away from the pole.
#   polar-cog-*  tiled rasters, scene spec 0.4 since each view has the
#                CRS's domain as bounds (decision 0005): the polar COG fixtures
#                (inst/extdata, tools/make-polar-cogs.R) in EPSG:3031 as
#                cog_scene() / view_cog() build them, whole, at the pole
#                and (3031) far out; polar-cog-lonlat-laea147 is the lon/lat
#                COG in a south polar LAEA with no EPSG code (PROJJSON view).
#   polar-rgb-*  scene spec 0.3 colour images (inst/extdata,
#                tools/make-rgb-cogs.R) as cog_scene() draws them by default:
#                an RGBA LZW COG (alpha 0, 128 and 255) and a YCbCr JPEG COG
#                (tiles joined to the level's JPEGTables), whole and at the
#                pole (full resolution).
#   polar-chunks-* scene spec 0.6 tiled rasters over chunk references
#                (scene_add_tiled_raster(format = "chunks"), aobcore #63):
#                the 3031 deflate COG through a palette and the YCbCr JPEG
#                COG in colour, each chunk's bytes embedded under its own
#                key, so the pages open from file://.
#   polar-legends-popups  scene spec 0.5: the polar probe with legends (a
#                palette ramp with a no-data entry for the SST field, a
#                class legend for land, classes for two sectors and a
#                colour-stop ramp for stations coloured by elevation) and
#                popups (stations on select, showing text, ISO dates and
#                numbers; sectors on point). js/screenshots.mjs also takes
#                it with the first station's popup open.
#   served/*     served pages (decision 0006) in the server's route layout,
#                for js/screenshots.mjs --serve; their scenes are written
#                as served-*.json beside the others.
library(aobcore)
library(nanoarrow)

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "scenes"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

ipc <- function(x) {
  f <- tempfile(fileext = ".arrows")
  write_nanoarrow(x, f)
  readBin(f, "raw", file.size(f))
}

# A FixedSizeList<uint8, 4> RGBA column from an n x 4 integer matrix.
rgba_array <- function(m) {
  child <- nanoarrow_array_modify(
    nanoarrow_array_init(na_uint8()),
    list(length = length(m), buffers = list(NULL, as_nanoarrow_buffer(as.raw(t(m)))))
  )
  nanoarrow_array_modify(
    nanoarrow_array_init(na_fixed_size_list(na_uint8(), 4L)),
    list(length = nrow(m), children = list(child))
  )
}

# A table of a native GeoArrow geometry column plus an RGBA column.
geo_table <- function(wkt, schema, rgba) {
  g <- as_nanoarrow_array(geoarrow::as_geoarrow_vctr(wk::wkt(wkt), schema = schema))
  col <- rgba_array(rgba)
  st <- na_struct(list(geometry = infer_nanoarrow_schema(g), color = infer_nanoarrow_schema(col)))
  nanoarrow_array_modify(nanoarrow_array_init(st), list(length = length(wkt), children = list(geometry = g, color = col)))
}

# Native interleaved GeoArrow in the view CRS: the explicit-data contract
# needs the CRS in the geometry field's metadata, and scene_add_data()
# checks it in IPC bytes (#59).
interleaved <- function(f) f(coord_type = "INTERLEAVED", crs = "EPSG:3031")

# ---- polar probe --------------------------------------------------------
p <- probe_scene()
write_scene_html(p, file = file.path(out, "polar-probe.html"), title = "Polar probe")
writeLines(scene_json(p), file.path(out, "polar-probe.json"))

# ---- kinds --------------------------------------------------------------
# All coordinates in EPSG:3031 metres around the Amery Ice Shelf region.
o <- c(2000000, 800000)
sq <- function(cx, cy, r) {
  sprintf("(%s)", paste(sprintf("%.0f %.0f", cx + r * c(-1, 1, 1, -1, -1), cy + r * c(-1, -1, 1, 1, -1)), collapse = ", "))
}
polys <- c(
  sprintf("POLYGON (%s, %s)", sq(1700000, 1000000, 180000), sq(1700000, 1000000, 80000)),
  sprintf("MULTIPOLYGON ((%s), (%s))", sq(2300000, 1000000, 120000), sq(2300000, 700000, 70000))
)
land <- geo_table(polys, interleaved(geoarrow::geoarrow_multipolygon),
                  rbind(c(218, 160, 120, 230), c(120, 170, 110, 230)))

tracks <- c(
  "MULTILINESTRING ((1500000 500000, 1800000 650000, 2100000 560000), (1500000 450000, 2100000 420000))",
  "MULTILINESTRING ((2400000 1250000, 2550000 900000, 2450000 450000))"
)
lines <- geo_table(tracks, interleaved(geoarrow::geoarrow_multilinestring),
                   rbind(c(200, 40, 40, 255), c(40, 90, 200, 255)))

# Points with local_origin already subtracted.
set.seed(1)
n <- 40
px <- runif(n, 1500000, 2500000) - o[1]
py <- runif(n, 350000, 1250000) - o[2]
ramp <- grDevices::col2rgb(grDevices::hcl.colors(n, "Plasma"))
pts <- geo_table(sprintf("POINT (%.1f %.1f)", px, py), interleaved(geoarrow::geoarrow_point),
                 cbind(t(ramp), 255))
multi <- geo_table(c("MULTIPOINT ((-300000 -250000), (-250000 -250000), (-200000 -250000))"),
                   interleaved(geoarrow::geoarrow_multipoint), rbind(c(20, 20, 20, 255)))

# A raster in the view CRS, no mesh: 40 x 30 cells with a NaN hole.
nx <- 40L
ny <- 30L
xy <- expand.grid(i = seq_len(nx), j = seq_len(ny))
v <- sin(xy$i / 6) + cos(xy$j / 5)
v[xy$i > 15 & xy$i < 22 & xy$j > 10 & xy$j < 18] <- NaN
vals <- ipc(data.frame(value = v))

s <- list(
  version = scene_spec_version(),
  view = list(type = "projected", crs = "EPSG:3031",
              extent = c(1350000, 2650000, 300000, 1350000), local_origin = o),
  data = list(
    field = list(format = "arrow-ipc-stream", blob = "field"),
    land = list(format = "arrow-ipc-stream", blob = "land",
                geometry = list(column = "geometry", encoding = "geoarrow.multipolygon")),
    tracks = list(format = "arrow-ipc-stream", blob = "tracks",
                  geometry = list(column = "geometry", encoding = "geoarrow.multilinestring")),
    stations = list(format = "arrow-ipc-stream", blob = "stations", origin_subtracted = TRUE,
                    geometry = list(column = "geometry", encoding = "geoarrow.point")),
    cluster = list(format = "arrow-ipc-stream", blob = "cluster", origin_subtracted = TRUE,
                   geometry = list(column = "geometry", encoding = "geoarrow.multipoint"))
  ),
  layers = list(
    list(id = "field", kind = "raster", label = "Field (no mesh, NaN nodata)",
         grid = list(crs = "EPSG:3031", extent = c(1400000, 2600000, 350000, 1250000),
                     dim = c(nx, ny), nodata = "NaN"),
         values = "field", palette = list(name = "viridis", range = c(-2, 2))),
    list(id = "land", kind = "polygon", label = "Polygons with holes", data = "land",
         fill = list(column = "color"), stroke = c(40L, 40L, 40L, 255L), stroke_width_px = 1.5),
    list(id = "tracks", kind = "path", label = "Multilinestrings", data = "tracks",
         stroke = list(column = "color"), stroke_width_px = 3),
    list(id = "stations", kind = "point", label = "Points (origin subtracted)", data = "stations",
         fill = list(column = "color"), stroke = c(255L, 255L, 255L, 255L), stroke_width_px = 1, radius_px = 5),
    list(id = "cluster", kind = "point", label = "Multipoint", data = "cluster",
         fill = c(20L, 20L, 20L, 255L), radius_px = 4)
  )
)
blobs <- list(field = vals, land = ipc(land), tracks = ipc(lines), stations = ipc(pts), cluster = ipc(multi))
write_scene_html(s, blobs, file.path(out, "kinds.html"), title = "Scene spec 0.1 layer kinds")
writeLines(scene_json(structure(s, class = "aob_scene")), file.path(out, "kinds.json"))

# ---- tiled COGs (scene spec 0.2) ------------------------------------------
# The polar COG fixtures drawn with view_cog()'s scene, whole and zoomed in
# on the pole, and the 3031 one zoomed far out (so a coarser level is chosen). Tile bytes are embedded, so the pages open from file://.
cogs <- list(
  "polar-cog-3031" = list(file = "polar_3031.tif", extent = NULL),
  "polar-cog-3031-pole" = list(file = "polar_3031.tif", extent = c(-9e5, 9e5, -6e5, 6e5)),
  "polar-cog-3031-far" = list(file = "polar_3031.tif", extent = c(-3e7, 3e7, -3e7, 3e7)),
  "polar-cog-lonlat" = list(file = "polar_lonlat.tif", extent = NULL),
  "polar-cog-lonlat-pole" = list(file = "polar_lonlat.tif", extent = c(-9e5, 9e5, -6e5, 6e5)),
  "polar-cog-lonlat-laea147" = list(file = "polar_lonlat.tif", extent = NULL,
                                    crs = "+proj=laea +lat_0=-90 +lon_0=147 +datum=WGS84")
)
for (name in names(cogs)) {
  x <- cogs[[name]]
  s <- cog_scene(system.file("extdata", x$file, package = "aobcore"), x$crs %||% "EPSG:3031",
                 palette = "ocean", extent = x$extent)
  write_scene_html(s, file = file.path(out, paste0(name, ".html")), title = x$file)
  writeLines(scene_json(s), file.path(out, paste0(name, ".json")))
}

# ---- colour images (scene spec 0.3) ---------------------------------------
rgb <- list(
  "polar-rgb-rgba" = list(file = "polar_rgba.tif", extent = NULL),
  "polar-rgb-rgba-pole" = list(file = "polar_rgba.tif", extent = c(-9e5, 9e5, -6e5, 6e5)),
  "polar-rgb-ycbcr" = list(file = "polar_ycbcr.tif", extent = NULL),
  "polar-rgb-ycbcr-pole" = list(file = "polar_ycbcr.tif", extent = c(-9e5, 9e5, -6e5, 6e5))
)
for (name in names(rgb)) {
  x <- rgb[[name]]
  # No domain, so these stay 0.3 scenes and CI validates 0.3 output too.
  s <- cog_scene(system.file("extdata", x$file, package = "aobcore"), "EPSG:3031",
                 extent = x$extent, domain = FALSE)
  stopifnot(identical(s$version, "0.3"), !is.null(s$layers[[1]]$rgb))
  write_scene_html(s, file = file.path(out, paste0(name, ".html")), title = x$file)
  writeLines(scene_json(s), file.path(out, paste0(name, ".json")))
}
# ---- chunk references (scene spec 0.6) -------------------------------------
chunked <- list(
  "polar-chunks-3031" = list(file = "polar_3031.tif", args = list(palette = "ocean")),
  "polar-chunks-ycbcr" = list(file = "polar_ycbcr.tif", args = list())
)
for (name in names(chunked)) {
  x <- chunked[[name]]
  f <- system.file("extdata", x$file, package = "aobcore")
  s <- do.call(scene_add_tiled_raster, c(list(scene("EPSG:3031"), "chunks", f), x$args,
                                         list(format = "chunks", label = x$file)))
  stopifnot(identical(s$version, "0.6"), identical(s$data$chunks$format, "chunks"),
            any(grepl("^chunks@", names(scene_blobs(s)))))
  write_scene_html(s, file = file.path(out, paste0(name, ".html")), title = x$file)
  writeLines(scene_json(s), file.path(out, paste0(name, ".json")))
}

# ---- legends and popups (scene spec 0.5) -----------------------------------
# Example stations: positions, opening dates and elevations are approximate,
# for illustration.
st <- data.frame(
  name = c("Davis", "Mawson", "Casey", "McMurdo", "Rothera", "Halley", "Vostok",
           "Concordia", "Amundsen-Scott", "Dumont d'Urville"),
  lon = c(77.97, 62.87, 110.53, 166.67, -68.13, -26.66, 106.84, 123.33, 0, 140.0),
  lat = c(-68.58, -67.60, -66.28, -77.85, -67.57, -75.58, -78.46, -75.10, -90, -66.66),
  opened = c("1957-01-13", "1954-02-13", "1969-02-19", "1956-02-16", "1975-01-01",
             "1956-01-06", "1957-12-16", "2005-02-01", "1956-11-20", "1956-04-12"),
  elevation_m = c(15, 10, 40, 10, 16, 35, 3488, 3233, 2835, 40)
)
xy <- gdalraster::transform_xy(cbind(st$lon, st$lat), "EPSG:4326", "EPSG:3031")
# Elevation through a two-stop ramp, written as the RGBA column the layer
# draws and as the legend's stops, from the same numbers.
lo <- c(255, 255, 204, 255)
hi <- c(8, 29, 88, 255)
t <- st$elevation_m / 3500
st_rgba <- round(outer(1 - t, lo) + outer(t, hi))
attr_table <- function(wkt, schema, cols, rgba) {
  g <- as_nanoarrow_array(geoarrow::as_geoarrow_vctr(wk::wkt(wkt), schema = schema))
  arrays <- c(list(geometry = g), lapply(cols, as_nanoarrow_array), list(fill = rgba_array(rgba)))
  sch <- na_struct(lapply(arrays, infer_nanoarrow_schema))
  nanoarrow_array_modify(nanoarrow_array_init(sch), list(length = length(wkt), children = arrays))
}
stations <- attr_table(sprintf("POINT (%.1f %.1f)", xy[, 1], xy[, 2]),
                       interleaved(geoarrow::geoarrow_point),
                       st[c("name", "opened", "elevation_m")], st_rgba)

# Two sectors between 60S and 70S as polygons (vertices along each arc).
sector <- function(lon0, lon1) {
  a <- seq(lon0, lon1, length.out = 40)
  ll <- rbind(cbind(a, -60), cbind(rev(a), -70))
  p <- gdalraster::transform_xy(ll, "EPSG:4326", "EPSG:3031")
  p <- rbind(p, p[1, ])
  sprintf("POLYGON ((%s))", paste(sprintf("%.0f %.0f", p[, 1], p[, 2]), collapse = ", "))
}
sector_rgba <- rbind(c(27, 158, 119, 110), c(117, 112, 179, 110))
sectors <- attr_table(c(sector(20, 80), sector(-80, -20)), interleaved(geoarrow::geoarrow_polygon),
                      data.frame(sector = c("Indian", "Weddell"), lon_from = c(20, -80), lon_to = c(80, -20)),
                      sector_rgba)

lp <- probe_scene()
lp <- scene_add_data(lp, "sectors", ipc(sectors))
lp <- scene_add_layer(lp, "sectors", label = "Sectors", fill = "fill",
                      stroke = c(60L, 66L, 72L, 160L), stroke_width_px = 1,
                      popup = list(columns = c("sector", "lon_from", "lon_to"), trigger = "point"))
lp <- scene_add_data(lp, "stations", ipc(stations))
lp <- scene_add_layer(lp, "stations", label = "Stations (approximate)", fill = "fill",
                      stroke = c(30L, 30L, 30L, 255L), stroke_width_px = 1, radius_px = 6,
                      popup = c("name", "opened", "elevation_m"))
lp <- scene_add_legend(lp, "sst", "SST (degrees C)", na = list(label = "no data", color = c(0, 0, 0, 0)))
lp <- scene_add_legend(lp, "land", classes = list(`Land (50m)` = c(218, 213, 202, 255)))
lp <- scene_add_legend(lp, "sectors", "Sector", classes = list(Indian = sector_rgba[1, ], Weddell = sector_rgba[2, ]))
lp <- scene_add_legend(lp, "stations", "Station elevation (m)",
                       ramp = list(range = c(0, 3500), stops = rbind(lo, hi)))
stopifnot(identical(lp$version, "0.5"))
write_scene_html(lp, file = file.path(out, "polar-legends-popups.html"), title = "Legends and popups")
writeLines(scene_json(lp), file.path(out, "polar-legends-popups.json"))

# ---- served pages (decision 0006) ------------------------------------------
# The linked page a local server answers (no blob scripts, the renderer by
# src, a blob base), written out in the server's route layout so that
# js/screenshots.mjs --serve can draw it over loopback HTTP:
# served/<name>/index.html, aob-renderer.min.js, blob/<encoded key> and
# files/<data id>/<base name>. The scene document is the same as embedded.
# Blob file names are encoded as the renderer's encodeURIComponent() does
# (aobcore:::url_component()), since the stand-in matches paths raw.
#   served-polar-cog-3031        the 3031 COG not embedded (embed = FALSE),
#                                its tiles read by range from files/cog/
#   served-polar-cog-3031-blobs  the polar-cog-3031 scene with embedded tile
#                                bytes, each tile fetched from blob/<key>
write_served <- function(s, name, title) {
  d <- file.path(out, "served", name)
  dir.create(file.path(d, "blob"), recursive = TRUE, showWarnings = FALSE)
  blobs <- scene_blobs(s)
  page <- aobcore:::scene_page(s, blobs, title = title, theme = "auto", mode = "linked")
  con <- file(file.path(d, "index.html"), open = "wb")
  writeLines(c("<!DOCTYPE html>", page), con, useBytes = TRUE)
  close(con)
  file.copy(system.file("renderer", "aob-renderer.min.js", package = "aobcore"), d, overwrite = TRUE)
  for (k in names(blobs)) writeBin(blobs[[k]], file.path(d, "blob", aobcore:::url_component(k)))
  for (id in names(attr(s, "files"))) {
    dir.create(file.path(d, "files", id), recursive = TRUE, showWarnings = FALSE)
    file.copy(attr(s, "files")[[id]]$path, file.path(d, "files", id), overwrite = TRUE)
  }
  writeLines(scene_json(s), file.path(out, paste0(name, ".json")))
}
f3031 <- system.file("extdata", "polar_3031.tif", package = "aobcore")
sv <- scene("EPSG:3031")
sv <- scene_add_tiled_raster(sv, "cog", cog_plan(f3031, "EPSG:3031"), palette = "ocean",
                             embed = FALSE, url = "files/cog/polar_3031.tif", label = "polar_3031.tif")
sv <- scene_add_vector(sv, "coast", gdal_vector_stream(
  system.file("extdata", "coastline_south_40s.geojson", package = "aobcore"), "EPSG:3031",
  densify = 0.25), stroke = c(60, 66, 72, 255), stroke_width_px = 1, label = "Coastline (50m)")
stopifnot(length(attr(sv, "files")) == 1L, !any(grepl("@", names(scene_blobs(sv)))))
write_served(sv, "served-polar-cog-3031", "polar_3031.tif")
write_served(cog_scene(f3031, "EPSG:3031", palette = "ocean"), "served-polar-cog-3031-blobs",
             "polar_3031.tif")

cat("wrote", out, "\n")
