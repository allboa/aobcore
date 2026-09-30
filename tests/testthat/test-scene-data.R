test_that("scene_add_vector() adds a data ref, a blob and a layer", {
  x <- wk::wkt("LINESTRING (0 0, 1000 1000)", crs = "EPSG:3031")
  s <- scene_add_vector(scene(), "line", x, stroke = c(60, 66, 72, 255),
                        stroke_width_px = 1, label = "A line")
  expect_s3_class(s, "aob_scene")
  expect_identical(s$data$line, list(
    format = "arrow-ipc-stream",
    blob = "line",
    geometry = list(column = "geometry", encoding = "geoarrow.linestring")
  ))
  expect_identical(s$layers[[1]], list(
    id = "line", kind = "path", data = "line", label = "A line",
    stroke = c(60L, 66L, 72L, 255L), stroke_width_px = 1
  ))
  blobs <- scene_blobs(s)
  expect_named(blobs, "line")
  expect_type(blobs$line, "raw")
  g <- geometry_info(nanoarrow::read_nanoarrow(blobs$line)$get_schema())
  expect_identical(g$ext, "geoarrow.linestring")
  expect_identical(g$coords, "+w:2")
  expect_valid_scene(s)
})

test_that("scene_add_data() accepts IPC bytes and native streams", {
  x <- wk::wkt("POINT (1 2)", crs = "EPSG:3031")
  bytes <- vector_ipc(vector_stream(x, "EPSG:3031"))
  s <- scene_add_data(scene(), "a", bytes)
  expect_identical(scene_blobs(s)$a, bytes)
  s <- scene_add_data(s, "b", vector_stream(x, "EPSG:3031"))
  expect_identical(s$data$b$geometry$encoding, "geoarrow.point")
  s <- scene_add_layer(s, "a", fill = "rgba", radius_px = 3)
  s <- scene_add_layer(s, "b", id = "b-layer", visible = FALSE)
  expect_identical(s$layers[[1]]$fill, list(column = "rgba"))
  expect_identical(vapply(s$layers, `[[`, "", "kind"), c("point", "point"))
  expect_valid_scene(s)
})

test_that("scene_add_data() uses the view CRS", {
  x <- wk::wkt("POINT (1 2)", crs = "EPSG:3413")
  expect_error(scene_add_data(scene("EPSG:3031"), "a", x), "does not reproject")
  s <- scene_add_data(scene("EPSG:3413"), "a", x)
  expect_valid_scene(s)
})

test_that("scene_add_data() and scene_add_layer() check their inputs", {
  x <- wk::wkt("POLYGON ((0 0, 1 0, 0 1, 0 0))")
  s <- scene_add_data(scene(), "poly", x)
  expect_error(scene_add_data(s, "poly", x), "already in the scene")
  expect_error(scene_add_data(s, "1bad", x), "letter")
  expect_error(scene_add_data(list(), "a", x), "scene")
  expect_error(scene_add_data(s, "raw", as.raw(1:3)))
  expect_error(scene_add_layer(s, "nope"), "not in the scene")
  expect_error(scene_add_layer(s, "poly", kind = "path"), "cannot draw geoarrow.polygon")
  expect_error(scene_add_layer(s, "poly", radius_px = 2), "only to a point")
  expect_error(scene_add_layer(s, "poly", fill = c(1, 2, 3)), "c\\(r, g, b, a\\)")
  expect_error(scene_add_layer(s, "poly", fill = c(1, 2, 3, 256)), "c\\(r, g, b, a\\)")
  expect_error(scene_add_layer(s, "poly", stroke_width_px = -1), "0 or more")
  s <- scene_add_layer(s, "poly", fill = c(200, 80, 40, 128))
  expect_error(scene_add_layer(s, "poly"), "already in the scene")
  line <- scene_add_data(s, "line", wk::wkt("LINESTRING (0 0, 1 1)"))
  expect_error(scene_add_layer(line, "line", fill = c(1, 2, 3, 4)), "path layer")
  expect_valid_scene(s)
})

test_that("an empty scene is valid and writes {} for data", {
  s <- scene()
  expect_valid_scene(s)
  expect_identical(
    scene_json(s),
    "{\"version\":\"0.1\",\"view\":{\"type\":\"projected\",\"crs\":\"EPSG:3031\"},\"data\":{},\"layers\":[]}"
  )
  expect_identical(scene_blobs(s), structure(list(), names = character()))
})

test_that("scene_json() escapes strings and writes ASCII", {
  x <- wk::wkt("POINT (0 0)")
  s <- scene_add_vector(scene(), "pt", x, label = "Quote \" slash \\ tab\t caf\u00e9 \U0001F30D")
  json <- scene_json(s)
  expect_false(grepl("[^\\x01-\\x7f]", json, perl = TRUE))
  expect_match(json, "caf\\u00e9", fixed = TRUE)
  expect_match(json, "\\ud83c\\udf0d", fixed = TRUE)
  skip_if_not_installed("jsonlite")
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(back$layers[[1]]$label, "Quote \" slash \\ tab\t caf\u00e9 \U0001F30D")
  pretty <- scene_json(s, pretty = TRUE)
  expect_match(pretty, "\n  \"view\": {", fixed = TRUE)
  expect_identical(jsonlite::fromJSON(pretty, simplifyVector = FALSE), back)
})

test_that("scene_json() writes numbers exactly enough", {
  s <- scene()
  s$view$center <- c(-1234567.125, 0.1)
  s$view$extent <- c(-3e6, 3e6, -3e6, 3e6)
  json <- scene_json(s)
  skip_if_not_installed("jsonlite")
  back <- jsonlite::fromJSON(json)
  expect_identical(back$view$center, c(-1234567.125, 0.1))
  expect_equal(back$view$extent, c(-3e6, 3e6, -3e6, 3e6))
  s$view$center <- c(Inf, 0)
  expect_error(scene_json(s), "Inf")
})
