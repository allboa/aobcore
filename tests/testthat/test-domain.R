test_that("crs_domain() keeps bounded projections whole and cuts divergent ones", {
  skip_if_no_gdal()
  laea <- crs_domain("+proj=laea +lat_0=-90 +lon_0=147 +datum=WGS84")
  expect_s3_class(laea, "aob_domain")
  expect_true(laea$bounded)
  expect_equal(laea$centre_lonlat[2], -90)
  # The whole-Earth disc: radius 2R.
  expect_equal(laea$extent[2], 2 * 6371007, tolerance = 0.01)
  expect_equal(max(laea$reach), 179.5)

  st <- crs_domain("EPSG:3031")
  expect_false(st$bounded)
  # Stretch 2 relative to the pole is reached just past the equator.
  expect_true(all(st$reach > 85 & st$reach < 95))
  expect_equal(st$extent, c(-1, 1, -1, 1) * st$extent[2])

  ortho <- crs_domain("+proj=ortho +lat_0=-90 +datum=WGS84")
  expect_true(ortho$bounded)
  expect_equal(max(ortho$reach), 90)

  merc <- crs_domain("EPSG:3857")
  expect_equal(merc$extent[4], 8.43e6, tolerance = 0.01)
  expect_gt(crs_domain("EPSG:3857", k = 4)$extent[4], 1.3e7)

  expect_identical(crs_domain("OGC:CRS84")$extent, c(-180, 180, -90, 90))
  # Mollweide: the whole map (2 sqrt(2) R by sqrt(2) R), not flagged bounded.
  moll <- crs_domain("+proj=moll +datum=WGS84")
  expect_false(moll$bounded)
  expect_equal(moll$extent[4], sqrt(2) * 6378137, tolerance = 0.01)
  # A false origin moves the centre in CRS units, not in lon/lat.
  utm <- crs_domain("EPSG:32755")
  expect_equal(utm$centre_lonlat, c(147, 0), tolerance = 1e-6)
  expect_equal(utm$centre, c(5e5, 1e7), tolerance = 1e-3)
  expect_output(print(st), "reach")
  expect_error(crs_domain("EPSG:3031", k = 1), "`k`")
})

test_that("scene() takes the domain as view bounds, version 0.4", {
  skip_if_no_gdal()
  s <- scene("EPSG:3031", domain = TRUE)
  expect_identical(s$version, "0.4")
  expect_identical(s$view$bounds, crs_domain("EPSG:3031")$extent)
  expect_output(print(s), "bounded")
  # The option turns the default off; FALSE and explicit extents work too.
  withr_opt <- options(aobcore.domain = TRUE)
  on.exit(options(withr_opt))
  expect_identical(scene()$version, "0.4")
  expect_null(scene(domain = FALSE)$view$bounds)
  expect_identical(scene(domain = FALSE)$version, "0.1")
  expect_identical(scene(domain = c(-1, 1, -2, 2))$view$bounds, c(-1, 1, -2, 2))
  expect_error(scene(domain = c(1, -1, 0, 1)), "`domain`")
  expect_error(scene("EPSG:3413", domain = crs_domain("EPSG:3031")), "the view is in EPSG:3413")
})

test_that("a scene with bounds writes 0.4 JSON the page checks accept", {
  skip_if_no_gdal()
  skip_if_not_installed("jsonlite")
  s <- scene_add_vector(scene("EPSG:3031", domain = TRUE), "l",
                        wk::wkt("LINESTRING (0 0, 1000 1000)", crs = "EPSG:3031"))
  back <- jsonlite::fromJSON(scene_json(s))
  expect_identical(back$version, "0.4")
  expect_length(back$view$bounds, 4L)
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf))
  expect_silent(write_scene_html(s, file = tf))
  plain <- unclass(s)
  plain$version <- "0.3"
  expect_error(write_scene_html(plain, scene_blobs(s), tf), "needs scene spec 0.4")
})

test_that("cog_scene() opens on its data clipped to the domain", {
  skip_if_no_gdal()
  f <- system.file("extdata", "polar_lonlat.tif", package = "aobcore")
  s <- cog_scene(f, "EPSG:3031", levels = 1L, domain = TRUE)
  b <- s$view$bounds
  expect_identical(s$version, "0.4")
  # The lon/lat COG reaches past the domain; the view stops at it, but its
  # tiles are all still planned.
  expect_true(all(s$view$extent[c(1, 3)] >= b[c(1, 3)] & s$view$extent[c(2, 4)] <= b[c(2, 4)]))
  # An explicit view outside the domain widens the bounds to take it in.
  far <- cog_scene(f, "EPSG:3031", levels = 1L, domain = TRUE, extent = c(2e7, 2.1e7, 2e7, 2.1e7))
  expect_gte(far$view$bounds[2], 2.1e7)
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf))
  expect_silent(write_scene_html(far, file = tf))
  bad <- scene("EPSG:3031", domain = TRUE)
  bad$view$extent <- c(3e7, 4e7, 3e7, 4e7)
  expect_error(write_scene_html(bad, file = tf), "does not overlap")
  bad$view$extent <- NULL
  bad$view$center <- c(3e7, 0)
  expect_error(write_scene_html(bad, file = tf), "outside")
  s0 <- cog_scene(f, "EPSG:3031", levels = 1L, domain = FALSE)
  expect_identical(length(s$layers[[1]]$plan$levels[[1]]$tiles),
                   length(s0$layers[[1]]$plan$levels[[1]]$tiles))
})

test_that("crs_domain() fails cleanly, and scene() drops the domain, without proj.db", {
  skip_if_no_gdal()
  # Without the PROJ database a PROJ string still resolves but a transform
  # can crash R (aobcore#21), so the lookup check must stop it first.
  local_mocked_bindings(proj_db_ok = function() FALSE)
  called <- FALSE
  local_mocked_bindings(transform_xy = function(...) {
    called <<- TRUE
    stop("transform_xy() must not be called")
  }, .package = "gdalraster")
  expect_error(crs_domain("+proj=ortho +lat_0=-90 +datum=WGS84"), "proj.db")
  s <- scene("+proj=ortho +lat_0=-90 +datum=WGS84", domain = TRUE)
  expect_null(s$view$bounds)
  expect_false(called)
  # A geographic CRS needs no transform.
  expect_identical(crs_domain("OGC:CRS84")$extent, c(-180, 180, -90, 90))
})
