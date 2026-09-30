test_that("to_json() writes objects, arrays and scalars", {
  expect_identical(to_json(list(a = 1, b = "x", c = TRUE, d = NULL)),
                   "{\"a\":1,\"b\":\"x\",\"c\":true,\"d\":null}")
  expect_identical(to_json(list(1L, 2L)), "[1,2]")
  expect_identical(to_json(c(0, 0)), "[0,0]")
  expect_identical(to_json(I(3)), "[3]")
  expect_identical(to_json(list()), "[]")
  expect_identical(to_json(structure(list(), names = character())), "{}")
  expect_identical(to_json(structure(list(a = 1), class = "aob_scene")), "{\"a\":1}")
})

test_that("to_json() keeps numbers exact", {
  # Coordinate-sized values. (Extreme exponents are left out: R's own
  # string-to-double conversion is not correctly rounded on every platform.)
  x <- c(-5791903.876384494, 0.1, 1 / 3, 6378137.123456789, 1.5e-7, 255, -2)
  out <- to_json(x)
  back <- as.numeric(strsplit(gsub("\\[|\\]", "", out), ",")[[1]])
  expect_identical(back, x)
  expect_identical(to_json(255), "255")
  expect_error(to_json(NA_real_), "finite")
  expect_error(to_json(Inf), "finite")
  expect_error(to_json(NA), "NA")
})

test_that("to_json() escapes strings to ASCII that is safe in a script", {
  expect_identical(to_json("a\"b\\c"), "\"a\\\"b\\\\c\"")
  expect_identical(to_json("</script>"), "\"\\u003c/script>\"")
  expect_identical(to_json("line\nbreak\t"), "\"line\\nbreak\\t\"")
  expect_identical(to_json("caf\u00e9"), "\"caf\\u00e9\"")
  expect_identical(to_json("\U0001F30D"), "\"\\ud83c\\udf0d\"")
  expect_identical(to_json(""), "\"\"")
  expect_error(to_json(list(1, b = 2)), "must have a name")
  expect_error(to_json(sum), "Cannot write")
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
