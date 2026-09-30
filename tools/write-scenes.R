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
#   polar-cog-*  scene spec 0.2 tiled rasters: the polar COG fixtures
#                (inst/extdata, tools/make-polar-cogs.R) in EPSG:3031 as
#                cog_scene() / view_cog() build them, whole, at the pole
#                and (3031) far out.
library(aobcore)
library(nanoarrow)

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

interleaved <- function(f) f(coord_type = "INTERLEAVED")

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
  "polar-cog-lonlat-pole" = list(file = "polar_lonlat.tif", extent = c(-9e5, 9e5, -6e5, 6e5))
)
for (name in names(cogs)) {
  x <- cogs[[name]]
  s <- cog_scene(system.file("extdata", x$file, package = "aobcore"), "EPSG:3031",
                 palette = "ocean", extent = x$extent)
  write_scene_html(s, file = file.path(out, paste0(name, ".html")), title = x$file)
  writeLines(scene_json(s), file.path(out, paste0(name, ".json")))
}
cat("wrote", out, "\n")
