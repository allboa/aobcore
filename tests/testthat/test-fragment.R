frag_html <- function(tag) as.character(htmltools::renderTags(tag)$html)

test_that("scene_tag() embeds the scene and blobs and attaches the renderer once", {
  p <- probe_scene()
  blobs <- scene_blobs(p)
  tag <- scene_tag(p, id = "s1", height = 300, theme = "light")
  html <- frag_html(tag)
  expect_match(html, "<div class=\"aob-fragment\" style=\"width:100%;height:300px;\" data-theme=\"light\" data-aob-scene=\"s1\"></div>",
               fixed = TRUE)
  json <- regmatches(html, regexpr("(?<=<script type=\"application/json\" id=\"s1\">).*?(?=</script>)",
                                   html, perl = TRUE))
  expect_identical(json, scene_json(p))
  for (k in names(blobs)) {
    open <- sprintf("<script type=\"application/octet-stream\" data-aob-blob=\"%s\" data-aob-scene=\"s1\">", k)
    expect_true(grepl(open, html, fixed = TRUE))
  }
  # No renderer inlined, no socket, no blob base.
  expect_false(grepl("aob-renderer", html, fixed = TRUE))
  expect_false(grepl("data-aob-socket|data-aob-blob-base", html))
  expect_false(grepl("[^\\x01-\\x7f]", html, perl = TRUE, useBytes = TRUE))

  deps <- htmltools::findDependencies(tag)
  expect_length(deps, 1L)
  expect_identical(deps[[1]]$name, "aob-renderer")
  expect_identical(deps[[1]]$script, "aob-renderer.min.js")
  expect_true(file.exists(file.path(deps[[1]]$src$file, "aob-renderer.min.js")))
  expect_match(deps[[1]]$version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")

  # Two scenes in one document: the renderer once, two ids.
  both <- htmltools::renderTags(htmltools::tagList(scene_tag(p), scene_tag(p)))
  expect_length(both$dependencies, 1L)
  ids <- regmatches(both$html, gregexpr("(?<=<div class=\"aob-fragment\" style=\"[^\"]{1,40}\" data-aob-scene=\")[^\"]+",
                                        both$html, perl = TRUE))[[1]]
  expect_length(ids, 2L)
  expect_false(ids[1] == ids[2])
})

test_that("scene_tag() ids do not use the random number generator", {
  set.seed(1)
  a <- stats::runif(1)
  set.seed(1)
  scene_tag(probe_scene())
  expect_identical(stats::runif(1), a)
})

test_that("scene_tag() checks its arguments and the scene", {
  p <- probe_scene()
  expect_error(scene_tag(p, width = "wide"), "CSS size")
  expect_error(scene_tag(p, height = -1), "CSS size")
  expect_error(scene_tag(p, id = "1x"), "`id`")
  expect_error(scene_tag(p, id = "a b"), "`id`")
  expect_error(scene_tag(p, theme = "blue"))
  bad <- p
  bad$layers[[1]]$id <- bad$layers[[2]]$id
  expect_error(scene_tag(bad), "used twice")
  expect_match(frag_html(scene_tag(p, width = "50em", height = "auto", id = "x")),
               "style=\"width:50em;height:auto;\"", fixed = TRUE)
})

test_that("scene_tag() saves to a page with the renderer beside it", {
  x <- wk::wkt("LINESTRING (0 0, 1000000 1000000)", crs = "EPSG:3031")
  s <- scene_add_vector(scene(), "line", x, stroke = c(60, 66, 72, 255))
  d <- tempfile("frag-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  f <- file.path(d, "index.html")
  htmltools::save_html(htmltools::tagList(scene_tag(s, id = "a"), scene_tag(s, id = "b")), f)
  page <- readChar(f, file.size(f), useBytes = TRUE)
  expect_identical(lengths(regmatches(page, gregexpr("aob-renderer.min.js", page, fixed = TRUE))), 1L)
  expect_length(list.files(d, "aob-renderer.min.js", recursive = TRUE), 1L)
})
