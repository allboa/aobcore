# Scene spec 0.5: legends (scene_add_legend()) and popups (scene_add_layer()).

stations <- function() {
  df <- data.frame(name = c("Davis", "Mawson", "Casey"), depth_m = c(120, 450, 30),
                   opened = c("1957-01-13", "1954-02-13", "1969-02-16"))
  df$geometry <- wk::wkt(c("POINT (2300000 600000)", "POINT (1300000 1600000)",
                           "POINT (2400000 -1000000)"), crs = "EPSG:3031")
  df
}

# The structural checks of a 0.1 scene, applied to a 0.5 scene with its
# legends and popups taken off (they check every other key).
expect_valid_05 <- function(s) {
  json <- scene_json(s)
  parsed <- jsonlite::fromJSON(json, simplifyVector = TRUE,
                               simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_identical(parsed$version, "0.5")
  rest <- parsed
  rest$version <- "0.1"
  rest$legends <- NULL
  rest$layers <- lapply(rest$layers, function(l) {
    l$popup <- NULL
    l
  })
  expect_identical(scene_spec_problems(rest), character())
  expect_validator_ok(json)
  parsed
}

test_that("a popup makes the scene 0.5 and names its columns as an array", {
  skip_if_not_installed("jsonlite")
  s0 <- scene_add_vector(scene(), "stations", stations())
  expect_identical(scene_spec_version(s0), "0.1")
  s <- scene_add_vector(scene(), "stations", stations(), popup = "name")
  expect_identical(s$version, "0.5")
  expect_identical(scene_spec_version(s), "0.5")
  p <- expect_valid_05(s)
  expect_identical(unlist(p$layers[[1]]$popup$columns), "name")
  expect_match(scene_json(s), "\"popup\":{\"columns\":[\"name\"]}", fixed = TRUE)

  s2 <- scene_add_vector(scene(), "stations", stations(),
                         popup = list(columns = c("depth_m", "name", "opened"), trigger = "point"))
  p2 <- expect_valid_05(s2)
  expect_identical(p2$layers[[1]]$popup$trigger, "point")
  expect_identical(unlist(p2$layers[[1]]$popup$columns), c("depth_m", "name", "opened"))
})

test_that("popup columns must exist, not be the geometry, and the trigger is known", {
  s <- scene_add_data(scene(), "stations", stations())
  expect_error(scene_add_layer(s, "stations", popup = "nope"), "not in data .*nope")
  expect_error(scene_add_layer(s, "stations", popup = c("name", "nope", "zip")), "columns not in data")
  expect_error(scene_add_layer(s, "stations", popup = "geometry"), "geometry column")
  expect_error(scene_add_layer(s, "stations", popup = c("name", "name")), "more than once")
  expect_error(scene_add_layer(s, "stations", popup = character()), "character vector")
  expect_error(scene_add_layer(s, "stations", popup = list(columns = "name", trigger = "click")),
               "\"select\" or \"point\"")
  expect_error(scene_add_layer(s, "stations", popup = list(cols = "name")), "list\\(columns")
  # The geometry check covers any geometry column name.
  x <- wk::wkt("POINT (0 0)", crs = "EPSG:3031")
  df <- data.frame(id = 1)
  df$geom <- x
  s3 <- scene_add_data(scene(), "pt", df)
  expect_identical(s3$data$pt$geometry$column, "geom")
  expect_error(scene_add_layer(s3, "pt", popup = "geom"), "geometry column")
})

test_that("scene_add_legend() writes palette, stops and class legends", {
  skip_if_not_installed("jsonlite")
  p <- probe_scene()
  p <- scene_add_legend(p, "sst", "SST (degrees C)",
                        na = list(label = "no data", color = c(0, 0, 0, 0)))
  p <- scene_add_legend(p, "land", classes = list(`Land (50m)` = c(218, 213, 202, 255)))
  expect_identical(p$version, "0.5")
  expect_identical(p$legends[[1]]$ramp, list(range = c(-2, 13), palette = "ocean"))
  parsed <- jsonlite::fromJSON(scene_json(p), simplifyVector = FALSE)
  expect_identical(parsed$legends[[2]],
                   list(layer = "land", classes = list(list(label = "Land (50m)",
                                                             color = list(218L, 213L, 202L, 255L)))))
  expect_validator_ok(scene_json(p))

  s <- scene_add_vector(scene(), "stations", stations(), popup = c("name", "depth_m"))
  # Matrix stops, evenly spaced.
  s <- scene_add_legend(s, "stations", "Depth (m)",
                        ramp = list(range = c(0, 4000),
                                    stops = rbind(c(255, 255, 204, 255), c(65, 182, 196, 255),
                                                  c(8, 29, 88, 255))))
  st <- s$legends[[1]]$ramp$stops
  expect_identical(vapply(st, function(x) x$at, 0), c(0, 0.5, 1))
  expect_identical(st[[3]]$color, c(8L, 29L, 88L, 255L))
  # Spec-shaped stops, a reversed range, and a second legend on one layer.
  s <- scene_add_legend(s, "stations", ramp = list(range = c(4000, 0), stops = list(
    list(at = 0, color = c(8, 29, 88, 255)), list(at = 0.25, color = c(65, 182, 196, 255)),
    list(at = 1, color = c(255, 255, 204, 255)))))
  expect_identical(vapply(s$legends[[2]]$ramp$stops, function(x) x$at, 0), c(0, 0.25, 1))
  # Classes as list(label, color), and an NA colour alone.
  s <- scene_add_legend(s, "stations", classes = list(list(label = "Open", color = c(1, 2, 3, 255))),
                        na = c(120, 120, 120, 255))
  expect_identical(s$legends[[3]]$na, list(label = "no data", color = c(120L, 120L, 120L, 255L)))
  expect_valid_05(s)
  expect_output(print(s), "3 legends")
})

test_that("scene_add_legend() follows the 0.5 validator's rules", {
  p <- probe_scene()
  s <- scene_add_vector(scene(), "stations", stations())
  two <- rbind(c(0, 0, 0, 255), c(255, 255, 255, 255))
  expect_error(scene_add_legend(p, "nope"), "not in the scene")
  expect_error(scene_add_legend(s, "stations"), "has no palette")
  expect_error(scene_add_legend(p, "sst", ramp = list(range = c(-2, 13), palette = "ocean"),
                                classes = list(a = c(0, 0, 0, 255))), "not both")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(1, 1), stops = two)), "must differ")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, NA), stops = two)), "finite")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1))), "exactly one")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), stops = two,
                                                         palette = "ocean")), "exactly one")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), stops = two[1, , drop = FALSE])),
               "at least two")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), stops = two, at = c(0.1, 1))),
               "first stop must be at 0")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), stops = two, at = c(0, 0.9))),
               "last at 1")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), at = c(0, 1), stops = list(
    list(at = 0, color = c(0, 0, 0, 255)), list(at = 1, color = c(9, 9, 9, 255))))), "not both")
  three <- rbind(two, c(9, 9, 9, 255))
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), stops = three,
                                                         at = c(0, 0, 1))), "increase strictly")
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1),
                                                         stops = rbind(c(0, 0, 0, 256), two[1, ])) ),
               "0 to 255")
  # A palette ramp keys only a palette layer, with its name and range.
  expect_error(scene_add_legend(s, "stations", ramp = list(range = c(0, 1), palette = "ocean")),
               "has no palette")
  expect_error(scene_add_legend(p, "sst", ramp = list(range = c(-2, 13), palette = "viridis")),
               "palette and range of layer")
  expect_error(scene_add_legend(p, "sst", ramp = list(range = c(13, -2), palette = "ocean")),
               "palette and range of layer")
  expect_error(scene_add_legend(p, "sst", ramp = list(range = c(-2, 13), palette = "ocean", at = 1)),
               "goes with `stops`")
  expect_error(scene_add_legend(p, "sst", ramp = list(range = c(-2, 13), colours = "x")), "unknown")
  expect_error(scene_add_legend(p, "sst", title = ""), "non-empty")
  expect_error(scene_add_legend(p, "land", classes = list(c(1, 2, 3, 4))), "named list")
  expect_error(scene_add_legend(p, "land", classes = list(list(label = "a"))), "label = , color")
  expect_error(scene_add_legend(p, "land", classes = list(a = c(1, 2, 3, 4)), na = list(color = c(1, 2, 3, 4))),
               "`na`")
})

test_that("0.5 rejects a layer palette range with equal ends", {
  p <- probe_scene()
  p$layers[[1]]$palette$range <- c(3, 3)
  expect_error(scene_add_legend(p, "land", classes = list(a = c(1, 2, 3, 4))), "equal ends")
})

test_that("scenes without legends or popups keep their lower version", {
  expect_identical(scene_spec_version(probe_scene()), "0.1")
  expect_identical(scene_spec_version(scene(domain = c(-1, 1, -1, 1))), "0.4")
  s <- scene_add_legend(scene_add_vector(scene(), "stations", stations()), "stations",
                        classes = list(a = c(1, 2, 3, 255)))
  s$legends <- NULL
  # A scene once marked 0.5 stays 0.5, as one marked 0.4 stays 0.4.
  expect_identical(scene_spec_version(s), "0.5")
})

test_that("write_scene_html() checks legends and popups", {
  p <- scene_add_legend(probe_scene(), "sst")
  f <- write_scene_html(p, file = tempfile(fileext = ".html"))
  html <- readLines(f, warn = FALSE)
  expect_true(any(grepl("\"legends\":[{\"layer\":\"sst\"", html, fixed = TRUE)))
  plain <- unclass(p)
  plain$version <- "0.4"
  expect_error(write_scene_html(plain, attr(p, "blobs"), tempfile()), "need scene spec 0.5")
  plain$version <- "0.5"
  plain$legends[[1]]$layer <- "nope"
  expect_error(write_scene_html(plain, attr(p, "blobs"), tempfile()), "not in `scene\\$layers`")
  plain$legends[[1]]$layer <- "sst"
  plain$layers[[1]]$popup <- list(columns = list("value"))
  expect_error(write_scene_html(plain, attr(p, "blobs"), tempfile()), "popups are for polygon")
})
