test_that("scene_json() keeps coordinates exact", {
  s <- scene()
  s$view$local_origin <- c(-5791903.876384494, 1 / 3)
  s$view$extent <- c(-5791903.876384494, 5791903.876384494, 0.1, 6378137.123456789)
  json <- scene_json(s)
  expect_match(json, "-5791903.876384494", fixed = TRUE)
  skip_if_not_installed("jsonlite")
  back <- jsonlite::fromJSON(json)
  expect_identical(back$view$local_origin, s$view$local_origin)
  expect_identical(back$view$extent, s$view$extent)
})

test_that("the probe scene JSON has the conformance extent", {
  expect_match(scene_json(probe_scene()),
               "\"extent\":[-5791903.876384494,5791903.876384494,-5791903.876384494,5791903.876384494]",
               fixed = TRUE)
})

test_that("base64 round-trips every padding length", {
  for (n in 0:7) {
    x <- as.raw((seq_len(n) * 37L) %% 256L)
    expect_identical(b64_decode(b64_encode(x)), x)
  }
  expect_identical(b64_encode(charToRaw("Man")), "TWFu")
  expect_identical(b64_encode(charToRaw("Ma")), "TWE=")
  expect_identical(b64_encode(charToRaw("M")), "TQ==")
})

test_that("scene_json() writes NA as null without a coercion warning", {
  s <- scene()
  s$view$center <- c(1.5, NA)
  expect_no_warning(json <- scene_json(s))
  expect_match(json, "\"center\":[1.5,null]", fixed = TRUE)
})
