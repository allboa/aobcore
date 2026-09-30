coast_path <- function() {
  system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
}

vsimem_files <- function() {
  sort(gdalraster::vsi_read_dir("/vsimem/", recursive = TRUE))
}

check_coast_stream <- function(s) {
  schema <- s$get_schema()
  g <- geometry_info(schema)
  expect_identical(g$column, "geometry")
  expect_true(g$ext %in% c("geoarrow.linestring", "geoarrow.multilinestring"))
  expect_identical(g$coords, "+w:2")
  expect_true("featurecla" %in% names(schema$children))
  df <- as.data.frame(s)
  expect_identical(nrow(df), 170L)
  # Coordinates are polar stereographic metres, not degrees: all south of
  # 40S lies within about 5,800 km of the pole.
  bb <- unclass(wk::wk_bbox(df$geometry))
  expect_true(bb$xmax - bb$xmin > 5e6)
  expect_true(max(abs(unlist(bb[1:4]))) < 6e6)
  invisible(df)
}

test_that("gdal_has_arrow() returns a flag", {
  expect_type(gdal_has_arrow(), "logical")
  expect_length(gdal_has_arrow(), 1L)
})

test_that("the coastline round-trips to EPSG:3031 through the Arrow driver", {
  skip_if_not_installed("gdalraster")
  skip_if_not(gdal_has_arrow(), "GDAL has no Arrow driver")
  before <- vsimem_files()
  s <- gdal_vector_stream(coast_path(), "EPSG:3031", densify = 0.25, route = "gdal")
  expect_identical(vsimem_files(), before)
  check_coast_stream(s)
})

test_that("the coastline round-trips to EPSG:3031 through the R conversion", {
  skip_if_not_installed("gdalraster")
  before <- vsimem_files()
  s <- gdal_vector_stream(coast_path(), "EPSG:3031", densify = 0.25, route = "r")
  expect_identical(vsimem_files(), before)
  check_coast_stream(s)
})

test_that("the coastline scene is valid scene spec 0.1", {
  skip_if_not_installed("gdalraster")
  s <- scene("EPSG:3031")
  coast <- gdal_vector_stream(coast_path(), s$view$crs, densify = 0.25)
  s <- scene_add_vector(s, "coast", coast, stroke = c(60, 66, 72, 255),
                        stroke_width_px = 1, label = "Coastline")
  expect_true(s$data$coast$geometry$encoding %in%
                c("geoarrow.linestring", "geoarrow.multilinestring"))
  expect_identical(s$layers[[1]]$kind, "path")
  expect_valid_scene(s)
  back <- nanoarrow::read_nanoarrow(scene_blobs(s)$coast)
  check_coast_stream(back)
})

test_that("clip and explode pass through to ogr2ogr", {
  skip_if_not_installed("gdalraster")
  s <- gdal_vector_stream(coast_path(), "EPSG:3031", clip = c(-180, -90, 180, -60),
                          explode = TRUE, options = c("-where", "featurecla = 'Coastline'"))
  df <- as.data.frame(s)
  expect_true(nrow(df) > 0L && nrow(df) < 170L)
  # south of 60S is within about 3,400 km of the pole
  bb <- unclass(wk::wk_bbox(df$geometry))
  expect_true(max(abs(unlist(bb[1:4]))) < 3.5e6)
})

test_that("a layer of mixed single and multi lines is never WKB", {
  skip_if_not_installed("gdalraster")
  src <- tempfile(fileext = ".geojson")
  on.exit(unlink(src))
  writeLines(c(
    '{"type": "FeatureCollection", "features": [',
    '{"type": "Feature", "properties": {"n": 1}, "geometry":',
    ' {"type": "LineString", "coordinates": [[0, -70], [10, -70]]}},',
    '{"type": "Feature", "properties": {"n": 2}, "geometry":',
    ' {"type": "MultiLineString", "coordinates": [[[20, -70], [30, -70]]]}}',
    ']}'
  ), src)
  routes <- if (gdal_has_arrow()) c("gdal", "r") else "r"
  for (route in routes) {
    s <- gdal_vector_stream(src, "EPSG:3031", route = route)
    g <- geometry_info(s$get_schema())
    expect_identical(g$ext, "geoarrow.multilinestring")
    expect_identical(g$coords, "+w:2")
    expect_identical(as.data.frame(s)$n, c(1L, 2L))
  }
})

test_that("gdal_vector_stream() checks its arguments", {
  skip_if_not_installed("gdalraster")
  expect_error(gdal_vector_stream(coast_path(), "3031"), "authority:code")
  expect_error(gdal_vector_stream(coast_path(), "EPSG:3031", clip = 1:3), "clip")
  expect_error(gdal_vector_stream(coast_path(), "EPSG:3031", densify = -1), "densify")
  expect_error(gdal_vector_stream(c("a", "b"), "EPSG:3031"), "dsn")
  if (!gdal_has_arrow()) {
    expect_error(gdal_vector_stream(coast_path(), "EPSG:3031", route = "gdal"), "Arrow driver")
  }
})
