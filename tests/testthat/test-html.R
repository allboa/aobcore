read_page <- function(f) readChar(f, file.size(f), useBytes = TRUE)

script_text <- function(page, open_tag) {
  start <- regexpr(open_tag, page, fixed = TRUE)
  expect_true(start > 0)
  rest <- substring(page, start + nchar(open_tag), nchar(page))
  substr(rest, 1, regexpr("</script>", rest, fixed = TRUE) - 1)
}

test_that("probe_scene() returns the conformance scene and its blobs", {
  p <- probe_scene()
  expect_s3_class(p, "aob_scene")
  expect_named(p, c("version", "view", "data", "layers"))
  expect_identical(p$version, scene_spec_version())
  blobs <- scene_blobs(p)
  expect_named(blobs, c("land", "coast", "graticule", "sst_mesh", "sst_index", "sst_values"))
  for (b in blobs) {
    expect_type(b, "raw")
    # Arrow IPC stream: continuation marker then a schema message
    expect_identical(b[1:4], as.raw(c(0xff, 0xff, 0xff, 0xff)))
  }
  expect_setequal(vapply(p$data, function(d) d$blob, ""), names(blobs))
})

test_that("the probe blobs read as Arrow with native GeoArrow geometry", {
  p <- probe_scene()
  s <- nanoarrow::read_nanoarrow(scene_blobs(p)$coast)
  schema <- nanoarrow::infer_nanoarrow_schema(s)
  expect_identical(schema$children$geometry$metadata[["ARROW:extension:name"]], "geoarrow.linestring")
  s$release()
})

test_that("write_scene_html() writes a self-contained page with scene and blobs", {
  p <- probe_scene()
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  blobs <- scene_blobs(p)
  expect_invisible(out <- write_scene_html(p, file = f))
  expect_identical(out, f)
  page <- read_page(f)
  expect_true(startsWith(page, "<!DOCTYPE html>"))

  # The scene is embedded as JSON, exactly as written by scene_json().
  json <- script_text(page, "<script type=\"application/json\" id=\"aob-scene\">")
  expect_identical(json, scene_json(p))
  expect_match(json, "\"layers\":[{\"id\":\"sst\",\"kind\":\"raster\"", fixed = TRUE)

  # Every blob round-trips through base64.
  for (k in names(blobs)) {
    b64 <- script_text(page, sprintf(
      "<script type=\"application/octet-stream\" data-aob-blob=\"%s\" data-aob-scene=\"aob-scene\">", k))
    expect_identical(b64_decode(b64), blobs[[k]])
  }

  # The renderer is inlined and nothing is loaded from elsewhere.
  expect_match(page, "aob-renderer", fixed = TRUE)
  expect_false(grepl("<script[^>]* src=", page))
  expect_false(grepl("<link[^>]*stylesheet", page))
  expect_false(grepl("[^\\x01-\\x7f]", page, perl = TRUE, useBytes = TRUE))
})

test_that("write_scene_html() sets a fixed theme and a title", {
  p <- probe_scene()
  f <- write_scene_html(p, file = tempfile(fileext = ".html"),
                        title = "Polar <probe>", theme = "dark")
  on.exit(unlink(f))
  page <- read_page(f)
  expect_match(page, "<html lang=\"en\" data-theme=\"dark\">", fixed = TRUE)
  expect_match(page, "<title>Polar &lt;probe&gt;</title>", fixed = TRUE)
  f2 <- write_scene_html(p, file = tempfile(fileext = ".html"))
  on.exit(unlink(f2), add = TRUE)
  expect_match(read_page(f2), "<html lang=\"en\">", fixed = TRUE)
  expect_error(write_scene_html(p, file = tempfile(), theme = "blue"))
})

test_that("an empty scene writes", {
  f <- write_scene_html(scene(), file = tempfile(fileext = ".html"))
  on.exit(unlink(f))
  expect_match(read_page(f), "\"data\":{},\"layers\":[]", fixed = TRUE)
})

test_that("write_scene_html() checks the scene shape", {
  s <- probe_scene()
  b <- scene_blobs(s)
  tf <- tempfile(fileext = ".html")
  expect_error(write_scene_html(unclass(s)[c("version", "view")], b, tf), "missing data, layers")
  s1 <- s
  s1$version <- "0.3"
  expect_error(write_scene_html(s1, b, tf), "must be \"0.1\" or \"0.2\"")
  s2 <- s
  s2$view$type <- "orthographic"
  expect_error(write_scene_html(s2, b, tf), "view\\$type")
  s3 <- s
  s3$layers[[2]]$data <- "nowhere"
  expect_error(write_scene_html(s3, b, tf), "\"nowhere\", which is not in `scene\\$data`")
  s4 <- s
  s4$layers[[1]]$mesh$indices <- "gone"
  expect_error(write_scene_html(s4, b, tf), "\"gone\"")
  s5 <- s
  s5$layers[[3]]$id <- "land"
  expect_error(write_scene_html(s5, b, tf), "used twice")
  s6 <- s
  s6$layers[[1]]$kind <- "heatmap"
  expect_error(write_scene_html(s6, b, tf), "has kind \"heatmap\"")
  s7 <- s
  s7$view$crs <- NULL
  expect_error(write_scene_html(s7, b, tf), "needs `scene\\$view\\$crs`")
  s8 <- s
  s8$layers[[4]]$data <- "sst_values"
  expect_error(write_scene_html(s8, b, tf), "has no `geometry`")
  s9 <- s
  s9$data$land$url <- "land.arrows"
  expect_error(write_scene_html(s9, b, tf), "exactly one of `blob` or `url`")

  expect_error(write_scene_html(s, b[-1], tf), "blob \"land\", which is not in `blobs`")
  expect_error(write_scene_html(s, unname(b), tf), "unique, non-empty names")
  b2 <- b
  b2$land <- "not raw"
  expect_error(write_scene_html(s, b2, tf), "not raw: land")
  expect_false(file.exists(tf))

  b3 <- c(b, list(extra = as.raw(1:3)))
  expect_warning(f <- write_scene_html(s, b3, tf), "left out of the page: extra")
  expect_false(grepl("data-aob-blob=\"extra\"", read_page(f), fixed = TRUE))
  unlink(tf)
})

test_that("url data references need no blob", {
  s <- scene()
  s$data$coast <- list(format = "arrow-ipc-stream", url = "coast.arrows",
                       geometry = list(column = "geometry", encoding = "geoarrow.linestring"))
  s$layers <- list(list(id = "coast", kind = "path", data = "coast"))
  f <- write_scene_html(s, list(), tempfile(fileext = ".html"))
  on.exit(unlink(f))
  expect_match(read_page(f), "\"url\":\"coast.arrows\"", fixed = TRUE)
})

test_that("a scene built with scene_add_vector() writes with its own blobs", {
  x <- wk::wkt("LINESTRING (0 0, 1000000 1000000)", crs = "EPSG:3031")
  s <- scene_add_vector(scene(), "line", x, stroke = c(60, 66, 72, 255))
  f <- write_scene_html(s, file = tempfile(fileext = ".html"))
  on.exit(unlink(f))
  page <- read_page(f)
  b64 <- script_text(page,
    "<script type=\"application/octet-stream\" data-aob-blob=\"line\" data-aob-scene=\"aob-scene\">")
  expect_identical(b64_decode(b64), scene_blobs(s)$line)
  expect_identical(script_text(page, "<script type=\"application/json\" id=\"aob-scene\">"), scene_json(s))
})

test_that("a plain list scene with separate blobs writes", {
  p <- probe_scene()
  f <- write_scene_html(unclass(p)[c("version", "view", "data", "layers")], scene_blobs(p),
                        tempfile(fileext = ".html"))
  on.exit(unlink(f))
  json <- script_text(read_page(f), "<script type=\"application/json\" id=\"aob-scene\">")
  expect_identical(json, scene_json(p))
})

test_that("strings cannot close the script element", {
  s <- structure(list(version = "0.1", view = list(type = "cartesian"),
                      data = structure(list(), names = character()),
                      layers = list(list(id = "a", kind = "path", data = "a",
                                         label = "</script><b>"))),
                 class = "aob_scene")
  json <- page_json(s)
  expect_false(grepl("<", json, fixed = TRUE))
  expect_match(json, "\\u003c/script>\\u003cb>", fixed = TRUE)
})
