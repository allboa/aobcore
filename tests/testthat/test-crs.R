laea_south <- "+proj=laea +lat_0=-90 +lon_0=147 +datum=WGS84 +units=m +no_defs"

test_that("scene_crs() keeps authority:code strings without GDAL", {
  expect_identical(scene_crs("EPSG:3031"), "EPSG:3031")
  expect_identical(scene_crs("OGC:CRS84"), "OGC:CRS84")
  expect_identical(scene_crs(3031), "EPSG:3031")
  expect_identical(scene_crs(3031L), "EPSG:3031")
  expect_error(scene_crs(NA_character_), "single CRS definition")
  expect_error(scene_crs(c(3031, 3413)), "single CRS definition")
})

test_that("scene_crs() reduces a definition that is exactly a code to the code", {
  skip_if_no_gdal()
  expect_identical(scene_crs(gdalraster::srs_to_wkt("EPSG:3031")), "EPSG:3031")
  expect_identical(
    scene_crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +datum=WGS84 +units=m +no_defs"),
    "EPSG:3031"
  )
  # An sf crs object carries its WKT.
  expect_identical(scene_crs(structure(list(input = "x", wkt = gdalraster::srs_to_wkt("EPSG:3413")),
                                       class = "crs")), "EPSG:3413")
})

test_that("scene_crs() carries a CRS with no code as PROJJSON", {
  skip_if_no_gdal()
  crs <- scene_crs(laea_south)
  expect_s3_class(crs, "aob_json")
  expect_true(isTRUE(gdalraster::srs_is_same(crs_wkt(crs), gdalraster::srs_to_wkt(laea_south))))
  # WKT and PROJJSON input give the same CRS; aob_json passes through.
  expect_true(crs_same(scene_crs(gdalraster::srs_to_wkt(laea_south)), crs))
  expect_true(crs_same(scene_crs(unclass(crs)), crs))
  expect_identical(scene_crs(crs), crs)
  expect_match(crs_label(crs), "Lambert Azimuthal Equal Area")
  expect_error(scene_crs("not a crs"), "cannot read the CRS")
})

test_that("a scene in a CRS with no code writes view.crs as a PROJJSON object", {
  skip_if_no_gdal()
  skip_if_not_installed("jsonlite")
  s <- scene(laea_south)
  expect_s3_class(s$view$crs, "aob_json")
  expect_output(print(s), "PROJJSON")
  back <- jsonlite::fromJSON(scene_json(s), simplifyVector = FALSE)
  expect_identical(back$view$crs$type, "ProjectedCRS")
  expect_identical(back$version, "0.1")
  expect_valid_scene(s)
})

test_that("vector producers take a CRS with no code", {
  skip_if_no_gdal()
  s <- scene(laea_south)
  x <- wk::wkt("LINESTRING (0 0, 1000 1000)", crs = laea_south)
  st <- vector_stream(x, laea_south)
  meta <- st$get_schema()$children$geometry$metadata[["ARROW:extension:metadata"]]
  expect_match(meta, "Lambert Azimuthal Equal Area", fixed = TRUE)
  s <- scene_add_vector(s, "line", x)
  expect_valid_scene(s)
  expect_error(vector_stream(wk::wkt("POINT (1 2)", crs = "EPSG:3031"), laea_south),
               "does not reproject")

  coast <- system.file("extdata", "coastline_south_40s.geojson", package = "aobcore")
  for (route in c("r", if (gdal_has_arrow()) "gdal")) {
    g <- gdal_vector_stream(coast, laea_south, densify = 1, route = route)
    df <- as.data.frame(g)
    xy <- wk::wk_coords(df$geometry)
    # Equal area from the pole: 40S lies about 5.8e6 m out, never beyond.
    expect_lt(max(sqrt(xy$x^2 + xy$y^2)), 6.5e6)
    g <- gdal_vector_stream(coast, laea_south, densify = 1, route = route)
    s <- scene_add_vector(scene(laea_south), "coast", g)
    expect_valid_scene(s)
  }
})

test_that("cog_plan() and view_cog() take a CRS with no code", {
  skip_if_no_gdal()
  f <- system.file("extdata", "polar_lonlat.tif", package = "aobcore")
  p <- cog_plan(f, laea_south, levels = 1L)
  expect_s3_class(p$plan$crs, "aob_json")
  expect_output(print(p), "PROJJSON")
  # The plan's CRS spelled differently from the view's is the same CRS.
  s <- scene_add_tiled_raster(scene(gdalraster::srs_to_wkt(laea_south)), "x", p)
  expect_identical(s$layers[[1]]$plan$crs, s$view$crs)
  expect_valid_tiled_scene(s)
  expect_error(scene_add_tiled_raster(scene("EPSG:3031"), "x", p), "the view is in EPSG:3031")

  s <- cog_scene(f, crs = laea_south, levels = 1L)
  expect_s3_class(s$view$crs, "aob_json")
  # South polar, so the coastline comes too.
  expect_length(s$layers, 2L)
  expect_valid_tiled_scene(s)
})
