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

# ---- the explicit-data contract, checked in R (#59) ------------------------

# A native stream of `wkt` with no CRS, in either coordinate layout.
no_crs_stream <- function(wkt = "LINESTRING (0 0, 1000 1000)", coord_type = "SEPARATE") {
  df <- data.frame(id = 1L)
  df$geometry <- geoarrow::as_geoarrow_vctr(
    wk::wkt(wkt), schema = geoarrow::na_extension_geoarrow("LINESTRING", coord_type = coord_type)
  )
  nanoarrow::as_nanoarrow_array_stream(df)
}

# The stream with its geometry field's ARROW:extension:metadata replaced.
with_ext_metadata <- function(stream, meta) {
  schema <- stream$get_schema()
  batches <- nanoarrow::collect_array_stream(stream, validate = FALSE)
  field <- schema$children$geometry
  md <- field$metadata
  md[["ARROW:extension:metadata"]] <- meta
  children <- schema$children
  children$geometry <- nanoarrow::nanoarrow_schema_modify(field, list(metadata = md))
  schema <- nanoarrow::nanoarrow_schema_modify(schema, list(children = children))
  batches <- lapply(batches, nanoarrow::nanoarrow_array_set_schema, schema, validate = FALSE)
  nanoarrow::basic_array_stream(batches, schema = schema, validate = FALSE)
}

blob_geometry <- function(s, id) {
  nanoarrow::read_nanoarrow(scene_blobs(s)[[id]])$get_schema()$children$geometry
}

test_that("a native stream with no CRS is given the view CRS", {
  for (coord_type in c("SEPARATE", "INTERLEAVED")) {
    st <- no_crs_stream(coord_type = coord_type)
    expect_identical(field_extension_metadata(st$get_schema()$children$geometry, "g")$crs, NULL)
    s <- scene_add_data(scene(), "line", st)
    g <- blob_geometry(s, "line")
    expect_true(check_field_crs(g, "EPSG:3031", "g"))
    expect_true("EPSG:3031" %in% crs_codes(field_extension_metadata(g, "g")$crs))
    expect_identical(geometry_field(nanoarrow::read_nanoarrow(scene_blobs(s)$line)$get_schema())$layout,
                     tolower(sub("SEPARATE", "SEPARATED", coord_type)))
    expect_valid_scene(s)
  }
})

test_that("a native stream in another CRS is refused", {
  x <- wk::wkt("LINESTRING (0 0, 1000 1000)")
  expect_error(scene_add_data(scene("EPSG:3031"), "a", vector_stream(x, "EPSG:3413")),
               "is EPSG:3413 but the view CRS is EPSG:3031.*does not reproject")
  # The same CRS written as an authority code (any case) is the view's.
  st <- with_ext_metadata(no_crs_stream(), "{\"crs\": \"epsg:3031\", \"crs_type\": \"authority_code\"}")
  s <- scene_add_data(scene(), "a", st)
  expect_identical(field_extension_metadata(blob_geometry(s, "a"), "g")$crs, "epsg:3031")
})

test_that("IPC bytes are checked against the view CRS, not rewritten", {
  bytes <- function(meta) ipc_bytes(with_ext_metadata(no_crs_stream(), meta))
  none <- ipc_bytes(no_crs_stream())
  expect_error(scene_add_data(scene(), "a", none),
               "no CRS in their geometry column.*view CRS is EPSG:3031.*not rewritten")
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": \"EPSG:3413\"}")),
               "is EPSG:3413 but the view CRS is EPSG:3031")
  x <- wk::wkt("POINT (1 2)")
  expect_error(scene_add_data(scene("EPSG:3031"), "a", vector_ipc(vector_stream(x, "EPSG:3413"))),
               "is EPSG:3413 but the view CRS is EPSG:3031")
  ok <- bytes("{\"crs\": \"EPSG:3031\", \"crs_type\": \"authority_code\", \"edges\": \"planar\"}")
  s <- scene_add_data(scene(), "a", ok)
  expect_identical(scene_blobs(s)$a, ok)
  expect_valid_scene(s)
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": \"EPSG:3031\", \"crs_type\": \"projjson\"}")),
               "crs_type .* is projjson but its crs is not a JSON object")
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": {\"name\": \"x\"}, \"crs_type\": \"authority_code\"}")),
               "authority_code but its crs is not")
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": \"EPSG:3031\", \"crs_type\": \"wkt2:2019\"}")),
               "not projjson or authority_code")
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": \"EPSG:3031\", \"edges\": \"spherical\"}")),
               "only planar edges")
  expect_error(scene_add_data(scene(), "a", bytes("[1, 2]")), "not a JSON object")
  # What the page refuses: an authority_code crs that is a PROJJSON object
  # (even one with an EPSG id), and crs_type or edges given as null.
  projjson <- "{\"type\": \"ProjectedCRS\", \"name\": \"P\", \"id\": {\"authority\": \"EPSG\", \"code\": 3031}}"
  expect_error(scene_add_data(scene(), "a", bytes(paste0("{\"crs\": ", projjson, ", \"crs_type\": \"authority_code\"}"))),
               "authority_code but its crs is not")
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": \"EPSG:3031\", \"crs_type\": null}")),
               "crs_type .* is null, not projjson or authority_code")
  expect_error(scene_add_data(scene(), "a", bytes("{\"crs\": \"EPSG:3031\", \"edges\": null}")),
               "edges .* are null; only planar edges")
  expect_identical(scene_blobs(scene_add_data(scene(), "a", bytes(paste0("{\"crs\": ", projjson, ", \"crs_type\": \"projjson\"}"))))$a,
                   bytes(paste0("{\"crs\": ", projjson, ", \"crs_type\": \"projjson\"}")))
  # A view with no CRS needs none.
  v <- scene()
  v$view$crs <- NULL
  expect_identical(scene_blobs(scene_add_data(v, "a", none))$a, none)
})

test_that("the contract's CRS rule: equal JSON values or one authority code", {
  projjson <- "{\"$schema\": \"x\", \"type\": \"ProjectedCRS\", \"name\": \"P\", \"id\": {\"authority\": \"EPSG\", \"code\": 3031}}"
  p <- json_parse(projjson)
  expect_true(crs_values_match(p, "EPSG:3031"))
  expect_true(crs_values_match("epsg:3031", "EPSG:3031"))
  expect_false(crs_values_match(p, "EPSG:3413"))
  # Equal as JSON values, keys in any order and $schema aside, with no id.
  a <- json_parse("{\"name\": \"laea\", \"type\": \"ProjectedCRS\", \"conversion\": {\"x\": [1, 2.5]}}")
  b <- json_parse("{\"$schema\": \"y\", \"conversion\": {\"x\": [1, 2.5]}, \"type\": \"ProjectedCRS\", \"name\": \"laea\"}")
  expect_true(crs_values_match(a, b))
  b$conversion$x[[2]] <- 2.6
  expect_false(crs_values_match(a, b))
  expect_identical(crs_codes(json_parse("{\"ids\": [{\"authority\": \"ESRI\", \"code\": 102020}, {\"authority\": \"EPSG\", \"code\": \"3031\"}]}")),
                   c("ESRI:102020", "EPSG:3031"))
  expect_identical(crs_codes("not a code"), character())
  expect_identical(crs_value_label(a), "\"laea\" (PROJJSON)")
  expect_identical(crs_value_label("+proj=stere\n  +lat_0=-90"), "\"+proj=stere +lat_0=-90\"")
  expect_identical(crs_value_label(3031), "(a JSON number)")
  expect_identical(crs_value_label(list(1, 2)), "(a JSON array)")
  expect_identical(crs_value_label(TRUE), "(a JSON boolean)")
})

test_that("json_parse() reads what json_value() writes", {
  x <- list(a = list(1, 2.5e-3, "q\"u\\o\u00e9\n", TRUE, FALSE), b = structure(list(), names = character()),
            c = list(), d = -12)
  back <- json_parse(json_value(x))
  expect_true(json_same(back, x))
  expect_identical(back$a[[3]], "q\"u\\o\u00e9\n")
  expect_identical(json_parse("[null]"), list(NULL))
  # A repeated key overwrites the earlier value, as JSON.parse does.
  expect_identical(json_parse("{\"a\": 1, \"b\": 2, \"a\": 3}"), list(a = 3, b = 2))
  expect_identical(json_parse("\"\\ud83c\\udf0d\""), "\U0001F30D")
  expect_error(json_parse("{\"a\": }"), "Not JSON")
  expect_error(json_parse("[1, 2"), "Not JSON")
  expect_error(json_parse("[1] 2"), "Not JSON")
})
