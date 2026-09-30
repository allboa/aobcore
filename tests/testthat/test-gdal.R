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
  skip_if_no_gdal()
  skip_if_not(gdal_has_arrow(), "GDAL has no Arrow driver")
  before <- vsimem_files()
  s <- gdal_vector_stream(coast_path(), "EPSG:3031", densify = 0.25, route = "gdal")
  expect_identical(vsimem_files(), before)
  check_coast_stream(s)
})

test_that("the coastline round-trips to EPSG:3031 through the R conversion", {
  skip_if_no_gdal()
  before <- vsimem_files()
  s <- gdal_vector_stream(coast_path(), "EPSG:3031", densify = 0.25, route = "r")
  expect_identical(vsimem_files(), before)
  check_coast_stream(s)
})

test_that("the coastline scene is valid scene spec 0.1", {
  skip_if_no_gdal()
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
  skip_if_no_gdal()
  s <- gdal_vector_stream(coast_path(), "EPSG:3031", clip = c(-180, -90, 180, -60),
                          explode = TRUE, options = c("-where", "featurecla = 'Coastline'"))
  df <- as.data.frame(s)
  expect_true(nrow(df) > 0L && nrow(df) < 170L)
  # south of 60S is within about 3,400 km of the pole
  bb <- unclass(wk::wk_bbox(df$geometry))
  expect_true(max(abs(unlist(bb[1:4]))) < 3.5e6)
})

test_that("a layer of mixed single and multi lines is never WKB", {
  skip_if_no_gdal()
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
  skip_if_no_gdal()
  expect_error(gdal_vector_stream(coast_path(), "3031"), "authority:code")
  expect_error(gdal_vector_stream(coast_path(), "EPSG:3031", clip = 1:3), "clip")
  expect_error(gdal_vector_stream(coast_path(), "EPSG:3031", densify = -1), "densify")
  expect_error(gdal_vector_stream(c("a", "b"), "EPSG:3031"), "dsn")
  if (!gdal_has_arrow()) {
    expect_error(gdal_vector_stream(coast_path(), "EPSG:3031", route = "gdal"), "Arrow driver")
  }
})

test_that("-nlt counts as native only for the six native types", {
  expect_true(gdal_native_type("x", NULL, c("-nlt", "MULTIPOLYGON")))
  expect_true(gdal_native_type("x", NULL, c("-nlt", "linestring25D")))
  expect_true(gdal_native_type("x", NULL, c("-nlt", "POINTZM", "-sql", "select 1")))
  expect_false(gdal_native_type("x", NULL, c("-nlt", "GEOMETRY")))
  expect_false(gdal_native_type("x", NULL, c("-nlt", "GEOMETRYCOLLECTION")))
  expect_false(gdal_native_type("x", NULL, c("-nlt", "CONVERT_TO_CURVE")))
  expect_false(gdal_native_type("x", NULL, c("-nlt", "PROMOTE_TO_MULTI", "-sql", "select 1")))
  expect_identical(nlt_values(c("-nlt", "MultiLineStringZ", "-where", "a", "-nlt")),
                   "MULTILINESTRING")
  skip_if_no_gdal()
  ## PROMOTE_TO_MULTI and CONVERT_TO_LINEAR fall through to the layer's type.
  expect_true(gdal_native_type(coast_path(), NULL, c("-nlt", "PROMOTE_TO_MULTI")))
  expect_true(gdal_native_type(coast_path(), NULL, c("-nlt", "CONVERT_TO_LINEAR")))
})

test_that("-nlt values that are not native types still give native GeoArrow", {
  skip_if_no_gdal()
  routes <- if (gdal_has_arrow()) c("gdal", "r") else "r"
  cases <- list(c("-nlt", "GEOMETRY"), c("-nlt", "PROMOTE_TO_MULTI"),
                c("-nlt", "MULTILINESTRING25D"), c("-nlt", "MULTILINESTRINGZM"))
  for (route in routes) {
    for (o in cases) {
      s <- gdal_vector_stream(coast_path(), "EPSG:3031", options = o, route = route)
      g <- geometry_info(s$get_schema())
      expect_true(g$ext %in% c("geoarrow.linestring", "geoarrow.multilinestring"))
      expect_identical(g$coords, "+w:2")
      df <- as.data.frame(s)
      expect_identical(nrow(df), 170L)
      expect_false(any(is.na(wk::as_wkb(df$geometry))))
    }
  }
})

test_that("both routes write the field's CRS metadata", {
  skip_if_no_gdal()
  skip_if_not(gdal_has_arrow(), "GDAL has no Arrow driver")
  a <- gdal_vector_stream(coast_path(), "EPSG:3031", route = "gdal")$get_schema()
  b <- gdal_vector_stream(coast_path(), "EPSG:3031", route = "r")$get_schema()
  expect_identical(a$children$geometry$metadata, b$children$geometry$metadata)
  expect_match(a$children$geometry$metadata[["ARROW:extension:metadata"]], "PROJJSON|projjson")
})

test_that("a clip that removes every feature gives an empty layer of the layer's type", {
  skip_if_no_gdal()
  routes <- if (gdal_has_arrow()) c("gdal", "r") else "r"
  for (route in routes) {
    s <- gdal_vector_stream(coast_path(), "EPSG:3031", clip = c(0, 0, 1, 1), route = route)
    g <- geometry_info(s$get_schema())
    expect_identical(g$ext, "geoarrow.linestring")
    expect_identical(g$coords, "+w:2")
    expect_identical(nrow(as.data.frame(s)), 0L)
    sc <- scene_add_vector(scene("EPSG:3031"), "coast",
                           gdal_vector_stream(coast_path(), "EPSG:3031", clip = c(0, 0, 1, 1),
                                              route = route),
                           stroke = c(0, 0, 0, 255))
    expect_valid_scene(sc)
  }
})
