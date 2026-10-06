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
  s1$version <- "0.7"
  expect_error(write_scene_html(s1, b, tf), "must be one of \"0.1\", \"0.2\", \"0.3\", \"0.4\", \"0.5\", \"0.6\"")
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

test_that("write_scene_html() pages are byte-identical to the pre-builder page", {
  p <- probe_scene()
  f <- write_scene_html(p, file = tempfile(fileext = ".html"), title = "Polar probe")
  on.exit(unlink(f))
  expect_identical(page_bytes(f), legacy_page(p, title = "Polar probe"))
  ## Blob order is the order of `blobs`, also when it differs from the scene's.
  blobs <- rev(scene_blobs(p))
  f2 <- write_scene_html(p, blobs, file = tempfile(fileext = ".html"), theme = "dark")
  on.exit(unlink(f2), add = TRUE)
  expect_identical(page_bytes(f2), legacy_page(p, blobs, theme = "dark"))
  page <- read_page(f2)
  at <- vapply(names(blobs), function(k) regexpr(sprintf("data-aob-blob=\"%s\"", k), page, fixed = TRUE), 0L)
  expect_true(all(at > 0))
  expect_false(is.unsorted(at))
  x <- wk::wkt(c("POINT (0 0)", "POINT (1e6 1e6)"), crs = "EPSG:3031")
  s <- scene_add_vector(scene(), "pts", x, radius_px = 4)
  f3 <- write_scene_html(s, file = tempfile(fileext = ".html"))
  on.exit(unlink(f3), add = TRUE)
  expect_identical(page_bytes(f3), legacy_page(s))
})

test_that("the linked page links the renderer and names a blob base", {
  p <- probe_scene()
  blobs <- scene_blobs(p)
  page <- scene_page(p, blobs, title = "Polar probe", theme = "auto", mode = "linked")
  expect_type(page, "character")
  expect_length(page, 1L)
  ## The renderer by src, and no inlined renderer.
  expect_match(page, "<script src=\"aob-renderer.min.js\"></script>", fixed = TRUE)
  expect_false(grepl("aob-renderer.min.js\"></script>", sub("<script src=\"aob-renderer.min.js\"></script>", "", page, fixed = TRUE), fixed = TRUE))
  expect_lt(nchar(page, "bytes"), 20000)
  ## No blob scripts; the page div names the blob base.
  expect_false(grepl("data-aob-blob=", page, fixed = TRUE))
  expect_match(page, "<div class=\"aob-page\" data-aob-scene=\"aob-scene\" data-aob-blob-base=\"blob/\"></div>",
               fixed = TRUE)
  ## The same scene JSON as the embedded page.
  json <- script_text(page, "<script type=\"application/json\" id=\"aob-scene\">")
  expect_identical(json, scene_json(p))
  ## The served blob keys, in order.
  keys <- script_text(page, "<script type=\"application/json\" data-aob-blob-keys data-aob-scene=\"aob-scene\">")
  expect_identical(keys, paste0("[\"", paste(names(blobs), collapse = "\",\""), "\"]"))
  expect_false(grepl("[^\\x01-\\x7f]", page, perl = TRUE, useBytes = TRUE))

  ## Keys with reserved characters stay JSON strings, "<" escaped.
  odd <- c("a/b@c+d", "x</script>")
  page2 <- scene_page(scene(), stats::setNames(list(raw(1), raw(1)), odd), title = "t",
                      theme = "dark", mode = "linked")
  expect_match(page2, "[\"a/b@c+d\",\"x\\u003c/script>\"]", fixed = TRUE)
  expect_match(page2, "<html lang=\"en\" data-theme=\"dark\">", fixed = TRUE)
  ## An empty scene lists no keys.
  expect_match(scene_page(scene(), list(), title = "t", theme = "auto", mode = "linked"),
               "data-aob-scene=\"aob-scene\">[]</script>", fixed = TRUE)

  ## The inline mode is the embedded page.
  inline <- scene_page(p, blobs, title = "Polar probe", theme = "auto", mode = "inline")
  expect_identical(charToRaw(paste0("<!DOCTYPE html>\n", inline, "\n")), legacy_page(p, title = "Polar probe"))
})

test_that("a scene spec 0.6 chunks scene is written with its chunk bytes keyed by url", {
  skip_if_no_gdal()
  f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
  cog <- cog_info(f)
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", cog_plan(cog, "EPSG:3031", levels = 3L),
                              palette = "ocean", range = c(-2, 15), embed = FALSE, url = "polar_3031.tif")
  l0 <- cog$levels[[1]]
  lv <- cog$levels[[4]]
  ## The COG's level 3 tiles as chunk refs (a hand-written 0.6 scene: no
  ## producer writes chunks yet).
  s$data$sst <- list(
    format = "chunks", url = "polar_3031.tif",
    grid = list(crs = "EPSG:3031", geotransform = l0$geotransform, dim = l0$dim, chunk_size = l0$tile_size,
                levels = list(list(level = 3L, dim = lv$dim, geotransform = lv$geotransform))),
    dtype = "int16", scale = cog$scale, nodata = cog$nodata,
    codecs = list(list(name = "bytes"), list(name = "predictor", configuration = list(type = "horizontal")),
                  list(name = "deflate")),
    refs = list(rows = lapply(seq_len(nrow(lv$tiles)), function(i) {
      list(level = 3L, col = lv$tiles$col[i], row = lv$tiles$row[i],
           offset = lv$tiles$byte_offset[i], length = lv$tiles$byte_length[i])
    }))
  )
  s$layers[[1]]$plan$levels <- lapply(s$layers[[1]]$plan$levels, function(L) {
    list(level = L$level, pixel_size = L$pixel_size,
         tiles = lapply(L$tiles, function(t) t[c("col", "row", "footprint", "mesh")]))
  })
  expect_identical(scene_spec_version(s), "0.6")
  bytes <- readBin(f, "raw", file.size(f))
  b <- c(scene_blobs(s), list(polar_3031.tif = bytes, unused = as.raw(1:3)))
  tf <- tempfile(fileext = ".html")
  expect_warning(write_scene_html(s, b, tf), "Blobs not used by the scene are left out of the page: unused.")
  html <- paste(readLines(tf, warn = FALSE), collapse = "\n")
  expect_match(html, "\"version\":\"0.6\"", fixed = TRUE)
  expect_match(html, "\"format\":\"chunks\"", fixed = TRUE)
  expect_match(html, "data-aob-blob=\"polar_3031.tif\"", fixed = TRUE)
  expect_match(html, b64_encode(bytes), fixed = TRUE)

  ## The same scene as a plain list: it must say 0.6, and its refs are rows
  ## or a table that is a data id.
  p <- unclass(s)
  attr(p, "blobs") <- NULL
  attr(p, "files") <- NULL
  p$version <- "0.6"
  expect_no_warning(write_scene_html(p, b[names(b) != "unused"], tf))
  p5 <- p
  p5$version <- "0.5"
  expect_error(write_scene_html(p5, b, tf), "The chunks data reference `sst` needs scene spec 0.6.")
  p2 <- p
  p2$data$sst$refs <- list()
  expect_error(write_scene_html(p2, b, tf), "needs `refs` with exactly one of `rows` or `table`")
  p3 <- p
  p3$data$sst$refs <- list(table = "sst_refs")
  expect_error(write_scene_html(p3, b, tf), "names refs table \"sst_refs\", which is not in `scene\\$data`")
  p4 <- p
  p4$data$sst$format <- "zarr"
  expect_error(write_scene_html(p4, b, tf), "which is not a cog or chunks reference")
})
