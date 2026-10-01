test_that("vector_densify() splits segments evenly to at most max_length", {
  x <- wk::wkt("LINESTRING (0 -60, 90 -60)", crs = "OGC:CRS84")
  d <- vector_densify(x, 0.25)
  expect_s3_class(d, "wk_wkb")
  expect_identical(wk::wk_crs(d), "OGC:CRS84")
  co <- wk::wk_coords(d)
  expect_identical(nrow(co), 361L)
  expect_equal(co$x, seq(0, 90, by = 0.25))
  expect_true(all(co$y == -60))
  expect_identical(nrow(wk::wk_coords(vector_densify(x, 45))), 3L)
  ## A segment already short enough is kept as it is.
  expect_identical(nrow(wk::wk_coords(vector_densify(x, 100))), 2L)
})

test_that("vector_densify() keeps each geometry's type, parts and rings", {
  x <- wk::wkt(c(
    "POINT (1 1)",
    "POLYGON EMPTY",
    "MULTIPOLYGON (((0 0, 2 0, 2 2, 0 0)), ((5 5, 7 5, 7 7, 5 5), (5.1 5.1, 5.2 5.1, 5.2 5.2, 5.1 5.1)))",
    "MULTILINESTRING ((0 0, 0 2), (5 5, 5 5.5))",
    "POLYGON ((0 0, 2 0, 2 2, 0 0))",
    "MULTIPOINT ((0 0), (9 9))",
    "GEOMETRYCOLLECTION (LINESTRING (0 0, 10 0))"
  ))
  d <- vector_densify(x, 1)
  expect_length(d, length(x))
  expect_identical(wk::wk_meta(d)$geometry_type, wk::wk_meta(x)$geometry_type)
  expect_identical(wk::wk_meta(d)$is_empty, wk::wk_meta(x)$is_empty)
  out <- as.character(wk::as_wkt(d))
  expect_identical(out[c(1, 2, 6, 7)], as.character(x[c(1, 2, 6, 7)]))
  expect_identical(out[4], "MULTILINESTRING ((0 0, 0 1, 0 2), (5 5, 5 5.5))")
  ## The diagonal edge (length 2.83) splits in three.
  pc <- wk::wk_coords(d[5])
  expect_identical(nrow(pc), 8L)
  expect_true(all(sqrt(diff(pc$x)^2 + diff(pc$y)^2) <= 1))
  expect_equal(pc$x, c(0, 1, 2, 2, 2, 4 / 3, 2 / 3, 0))
  ## Two parts, the second with its hole (left alone: its edges are short).
  mp <- wk::wk_coords(d[3])
  expect_identical(unique(mp$ring_id), 1:3)
  expect_identical(nrow(mp[mp$ring_id == 3, ]), 4L)
})

test_that("vector_densify() drops Z and M and takes any wk input", {
  z <- wk::wkt("LINESTRING Z (0 0 1, 2 0 1)")
  d <- vector_densify(z, 1)
  expect_false(wk::wk_meta(d)$has_z)
  expect_identical(nrow(wk::wk_coords(d)), 3L)
  xy <- wk::xy(1, 2)
  expect_identical(as.character(wk::as_wkt(vector_densify(xy, 1))), "POINT (1 2)")
  expect_length(vector_densify(wk::wkb(), 1), 0L)
})

test_that("vector_densify() checks its arguments", {
  x <- wk::wkt("LINESTRING (0 0, 1 1)")
  expect_error(vector_densify(x, 0), "max_length")
  expect_error(vector_densify(x, -1), "max_length")
  expect_error(vector_densify(x, c(1, 2)), "max_length")
  expect_error(vector_densify(x, NA_real_), "max_length")
  expect_error(vector_densify(1:3, 1), "cannot be read by wk")
})
