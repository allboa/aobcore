test_that("scene spec version is 0.1", {
  expect_identical(scene_spec_version(), "0.1")
})

test_that("scene() defaults to a polar projected view", {
  s <- scene()
  expect_s3_class(s, "aob_scene")
  expect_named(s, c("version", "view", "data", "layers"))
  expect_identical(s$version, "0.1")
  expect_identical(s$view, list(type = "projected", crs = "EPSG:3031"))
  expect_length(s$data, 0)
  expect_length(s$layers, 0)
})

test_that("scene() rejects a malformed crs", {
  expect_error(scene(c("EPSG:3031", "EPSG:3413")), "single CRS definition")
  expect_error(scene(NA_character_), "single CRS definition")
  expect_error(scene(""), "single CRS definition")
  expect_error(scene(list()), "single CRS definition")
})

test_that("scene() prints a one-line summary", {
  expect_output(print(scene("EPSG:3413")), "EPSG:3413")
})
