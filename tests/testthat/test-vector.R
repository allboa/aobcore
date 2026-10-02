test_that("vector_stream() writes native interleaved GeoArrow in the view CRS", {
  x <- wk::wkt(c("LINESTRING (0 0, 1000 1000)", "LINESTRING (5 5, 6 6)"), crs = "EPSG:3031")
  s <- vector_stream(x, "EPSG:3031")
  expect_s3_class(s, "nanoarrow_array_stream")
  g <- geometry_info(s$get_schema())
  expect_identical(g$column, "geometry")
  expect_identical(g$ext, "geoarrow.linestring")
  expect_identical(g$coords, "+w:2")
  meta <- s$get_schema()$children$geometry$metadata[["ARROW:extension:metadata"]]
  expect_match(meta, "3031")
  df <- as.data.frame(s)
  expect_identical(nrow(df), 2L)
  expect_equal(unclass(wk::wk_bbox(df$geometry))[1:4],
               list(xmin = 0, ymin = 0, xmax = 1000, ymax = 1000))
})

test_that("each geometry kind gets its native encoding", {
  cases <- list(
    "geoarrow.point" = "POINT (1 2)",
    "geoarrow.polygon" = "POLYGON ((0 0, 1 0, 0 1, 0 0))",
    "geoarrow.multipoint" = c("POINT (1 2)", "MULTIPOINT ((3 4))"),
    "geoarrow.multilinestring" = c("LINESTRING (0 0, 1 1)", "MULTILINESTRING ((0 0, 1 1))"),
    "geoarrow.multipolygon" = "MULTIPOLYGON (((0 0, 1 0, 0 1, 0 0)))"
  )
  for (ext in names(cases)) {
    g <- geometry_info(vector_stream(wk::wkt(cases[[ext]]), "EPSG:3031")$get_schema())
    expect_identical(g$ext, ext)
    expect_identical(g$coords, "+w:2")
  }
})

test_that("attribute columns pass through from a data frame", {
  df <- data.frame(name = c("a", "b"), value = c(1.5, 2.5))
  df$geom <- wk::xy(c(1, 2), c(3, 4), crs = "EPSG:3031")
  s <- vector_stream(df, "EPSG:3031")
  expect_identical(names(s$get_schema()$children), c("name", "value", "geom"))
  expect_identical(geometry_info(s$get_schema())$ext, "geoarrow.point")
  out <- as.data.frame(s)
  expect_identical(out$name, c("a", "b"))
  expect_identical(out$value, c(1.5, 2.5))
})

test_that("an sf-style data frame uses its sf_column", {
  df <- data.frame(label = "x")
  df$other <- wk::wkt("POINT (9 9)")
  df$shape <- wk::wkt("LINESTRING (0 0, 1 1)")
  attr(df, "sf_column") <- "shape"
  s <- vector_stream(df, "EPSG:3031")
  shape <- s$get_schema()$children$shape
  expect_identical(shape$metadata[["ARROW:extension:name"]], "geoarrow.linestring")
  s2 <- vector_stream(df, "EPSG:3031", geometry = "other")
  other <- s2$get_schema()$children$other
  expect_identical(other$metadata[["ARROW:extension:name"]], "geoarrow.point")
  expect_error(vector_stream(df, "EPSG:3031", geometry = "nope"), "not in `x`")
})

test_that("a WKB stream becomes native GeoArrow, never WKB", {
  df <- data.frame(id = 1:2)
  df$geometry <- geoarrow::as_geoarrow_vctr(
    wk::wkt(c("LINESTRING Z (0 0 1, 1 1 1)", "LINESTRING Z (2 2 1, 3 3 1)")),
    schema = geoarrow::geoarrow_wkb()
  )
  wkb <- nanoarrow::as_nanoarrow_array_stream(df)
  expect_identical(geometry_info(wkb$get_schema())$ext, "geoarrow.wkb")
  s <- vector_stream(wkb, "EPSG:3031")
  g <- geometry_info(s$get_schema())
  expect_identical(g$ext, "geoarrow.linestring")
  expect_identical(g$coords, "+w:2")  # Z dropped
  expect_identical(as.data.frame(s)$id, 1:2)
})

test_that("vector_stream() refuses a different CRS and accepts a missing one", {
  expect_error(vector_stream(wk::wkt("POINT (1 2)", crs = "EPSG:4326"), "EPSG:3031"),
               "does not reproject")
  expect_silent(vector_stream(wk::wkt("POINT (1 2)"), "EPSG:3031"))
  expect_silent(vector_stream(wk::wkt("POINT (1 2)", crs = "epsg:3031"), "EPSG:3031"))
  expect_error(vector_stream(wk::wkt("POINT (1 2)"), NA_character_), "single CRS definition")
})

test_that("vector_stream() recognises an equivalent CRS via gdalraster", {
  skip_if_no_gdal()
  wkt <- gdalraster::srs_to_wkt("EPSG:3031")
  expect_silent(vector_stream(wk::wkt("POINT (1 2)", crs = wkt), "EPSG:3031"))
})

test_that("vector_stream() refuses input it cannot encode natively", {
  expect_error(vector_stream(wk::wkt(c("POINT (1 2)", "LINESTRING (0 0, 1 1)")), "EPSG:3031"),
               "no native GeoArrow encoding")
  expect_error(vector_stream(wk::wkt("GEOMETRYCOLLECTION (POINT (1 2))"), "EPSG:3031"),
               "no native GeoArrow encoding")
  expect_error(vector_stream(wk::wkt(character()), "EPSG:3031"), "no geometries")
  expect_error(vector_stream(data.frame(a = 1), "EPSG:3031"), "no geometry column")
  expect_error(vector_stream(1:3, "EPSG:3031"), "cannot be read by wk")
})

test_that("vector_ipc() writes IPC bytes that read back as native GeoArrow", {
  x <- wk::wkt("POLYGON ((0 0, 10 0, 0 10, 0 0))", crs = "EPSG:3031")
  bytes <- vector_ipc(vector_stream(x, "EPSG:3031"))
  expect_type(bytes, "raw")
  back <- nanoarrow::read_nanoarrow(bytes)
  g <- geometry_info(back$get_schema())
  expect_identical(g$ext, "geoarrow.polygon")
  expect_identical(g$coords, "+w:2")
  geom <- as.data.frame(back)$geometry
  expect_identical(unclass(wk::as_wkt(wk::wk_set_crs(geom, NULL))),
                   "POLYGON ((0 0, 10 0, 0 10, 0 0))")
})

test_that("factor columns are written as character (#25)", {
  df <- data.frame(zone = factor(c("b", "a", NA, "b"), levels = c("b", "a", "c")), n = 1:4)
  df$geometry <- wk::wkt(sprintf("POINT (%d 0)", 1:4), crs = "EPSG:3031")
  check <- function(bytes) {
    back <- nanoarrow::read_nanoarrow(bytes)
    sch <- back$get_schema()
    expect_identical(sch$children$zone$format, "u")
    expect_null(sch$children$zone$dictionary)
    expect_identical(geometry_info(sch)$ext, "geoarrow.point")
    out <- as.data.frame(back)
    expect_identical(out$zone, c("b", "a", NA, "b"))
    expect_identical(out$n, 1:4)
  }
  # From a data frame, through vector_stream() and scene_add_vector().
  check(vector_ipc(vector_stream(df, "EPSG:3031")))
  s <- scene_add_vector(scene(), "pts", df, popup = c("zone", "n"))
  check(scene_blobs(s)[[1]])
  # A native stream that already has a dictionary column.
  g <- vector_stream(df["geometry"], "EPSG:3031")
  native <- as.data.frame(g)
  native$zone <- df$zone
  native$n <- df$n
  stream <- nanoarrow::as_nanoarrow_array_stream(native)
  expect_false(is.null(stream$get_schema()$children$zone$dictionary))
  check(vector_ipc(stream))
  # The caller's data frame keeps its factor and its level order.
  expect_identical(levels(df$zone), c("b", "a", "c"))
})

test_that("vector_ipc() refuses WKB, separated coordinates and missing geometry", {
  df <- data.frame(id = 1)
  df$geometry <- geoarrow::as_geoarrow_vctr(wk::wkt("POINT (1 2)"), schema = geoarrow::geoarrow_wkb())
  expect_error(vector_ipc(df), "not native GeoArrow")
  df$geometry <- geoarrow::as_geoarrow_vctr(wk::wkt("POINT (1 2)"), schema = geoarrow::geoarrow_point())
  expect_error(vector_ipc(df), "interleaved")
  expect_error(vector_ipc(data.frame(a = 1)), "no native GeoArrow geometry column")
})

test_that("no geometries give an empty stream of the declared type", {
  v <- geoarrow::as_geoarrow_vctr(wk::wkt(character(), crs = "EPSG:3031"),
                                  schema = geoarrow::geoarrow_polygon())
  s <- vector_stream(v, "EPSG:3031")
  g <- geometry_info(s$get_schema())
  expect_identical(g$ext, "geoarrow.polygon")
  expect_identical(g$coords, "+w:2")
  expect_identical(nrow(as.data.frame(s)), 0L)
  expect_error(vector_stream(wk::wkb(crs = "EPSG:3031"), "EPSG:3031"),
               "no geometries and declares no point, line or polygon type")
})

test_that("rgba_array() builds a FixedSizeList<uint8, 4> colour column", {
  a <- rgba_array(rbind(c(255, 0, 0, 255), c(0, 10, 255, 128)))
  expect_s3_class(a, "nanoarrow_array")
  expect_identical(a$length, 2L)
  sch <- nanoarrow::infer_nanoarrow_schema(a)
  expect_identical(sch$format, "+w:4")
  expect_identical(sch$children[[1]]$format, "C")
  v <- nanoarrow::convert_array(a$children[[1]], integer())
  expect_identical(v, c(255L, 0L, 0L, 255L, 0L, 10L, 255L, 128L))
  a3 <- rgba_array(matrix(c(1L, 2L, 3L), 1))
  expect_identical(nanoarrow::convert_array(a3$children[[1]], integer()), c(1L, 2L, 3L, 255L))
  expect_identical(rgba_array(matrix(integer(), 0, 4))$length, 0L)
  expect_error(rgba_array(matrix(1:2, 1)), "3 or 4 columns")
  expect_error(rgba_array(rbind(c(256, 0, 0, 0))), "0 to 255")
  expect_error(rgba_array(rbind(c(1.5, 0, 0, 0))), "0 to 255")
})
