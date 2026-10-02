# serve_scene() (decision 0006, aobcore #33 to #35). Requests go over
# 127.0.0.1 through http_req() in helper-serve.R.

test_that("serve_scene() needs httpuv, and says so", {
  local_mocked_bindings(has_httpuv = function() FALSE)
  expect_error(serve_scene(probe_scene(), open = FALSE), "needs the 'httpuv' package")
})

## A served copy of the polar 3031 COG with embed = FALSE, in its own
## directory beside a sibling file.
served_cog <- function() {
  d <- tempfile("aob-serve-")
  dir.create(d)
  f <- file.path(d, "polar_3031.tif")
  file.copy(system.file("extdata", "polar_3031.tif", package = "aobcore"), f)
  writeLines("secret", file.path(d, "sibling.txt"))
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "cog", f, embed = FALSE, range = c(0, 1))
  list(dir = d, file = f, scene = s)
}

test_that("every route answers over 127.0.0.1", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene, title = "Served")
  on.exit(srv$stop(), add = TRUE)
  expect_s3_class(srv, "aob_server")
  expect_match(srv$url, "^http://127[.]0[.]0[.]1:[0-9]+/[0-9a-f]{32}/$")
  expect_identical(srv$url, paste0("http://127.0.0.1:", srv$port, "/", srv$token, "/"))
  expect_true(srv$port >= 20000 && srv$port <= 60000)
  root <- paste0("/", srv$token, "/")

  ## The page: linked, the same document with the file route as the cog url
  ## and no trace of the local path.
  for (p in c(root, paste0(root, "index.html"))) {
    r <- http_req(srv$port, p)
    expect_identical(r$status, 200L)
    expect_match(r$headers[["content-type"]], "^text/html")
    expect_identical(r$headers[["cache-control"]], "no-cache")
    expect_false("access-control-allow-origin" %in% names(r$headers))
    page <- rawToChar(r$body)
    expect_match(page, "<script src=\"aob-renderer.min.js\"></script>", fixed = TRUE)
    expect_match(page, "data-aob-blob-base=\"blob/\"", fixed = TRUE)
    expect_match(page, "\"url\":\"files/cog/polar_3031.tif\"", fixed = TRUE)
    expect_false(grepl("file://", page, fixed = TRUE))
    expect_false(grepl(basename(x$dir), page, fixed = TRUE))
    expect_match(page, "<title>Served</title>", fixed = TRUE)
  }
  ## The scene itself is unchanged.
  expect_match(x$scene$data$cog$url, "^file://")

  r <- http_req(srv$port, paste0(root, "aob-renderer.min.js"))
  expect_identical(r$status, 200L)
  expect_match(r$headers[["content-type"]], "^text/javascript")
  expect_identical(length(r$body), as.integer(file.size(system.file("renderer", "aob-renderer.min.js", package = "aobcore"))))

  r <- http_req(srv$port, paste0(root, "blob/cog_vertices"))
  expect_identical(r$status, 200L)
  expect_identical(r$headers[["content-type"]], "application/vnd.apache.arrow.stream")
  expect_identical(r$body, scene_blobs(x$scene)$cog_vertices)

  r <- http_req(srv$port, paste0(root, "files/cog/polar_3031.tif"))
  expect_identical(r$status, 200L)
  expect_identical(r$headers[["content-type"]], "image/tiff")
  expect_identical(r$body, readBin(x$file, "raw", file.size(x$file)))

  ## The token without its slash redirects.
  r <- http_req(srv$port, paste0("/", srv$token))
  expect_identical(r$status, 301L)
  expect_identical(r$headers[["location"]], root)

  ## GET and HEAD only.
  expect_identical(http_req(srv$port, root, method = "POST")$status, 405L)
  r <- http_req(srv$port, root, method = "HEAD")
  expect_identical(r$status, 200L)
  expect_gt(as.numeric(r$headers[["content-length"]]), 1000)
})

test_that("anything not registered answers 404", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene)
  on.exit(srv$stop(), add = TRUE)
  t <- srv$token
  wrong <- paste(rev(strsplit(t, "")[[1]]), collapse = "")
  paths <- c(
    paste0("/", wrong, "/"), paste0("/", wrong, "/files/cog/polar_3031.tif"), "/", "/index.html",
    paste0("/", t, "/nope"), paste0("/", t, "/blob/"), paste0("/", t, "/blob/nope"),
    paste0("/", t, "/files/cog/"), paste0("/", t, "/files/cog/polar_3031.tif/"),
    paste0("/", t, "/files/../files/cog/polar_3031.tif"),
    paste0("/", t, "/files/cog/../cog/polar_3031.tif"),
    paste0("/", t, "/files/cog/../../../etc/passwd"),
    paste0("/", t, "/files/%2e%2e/polar_3031.tif"), paste0("/", t, "/files/cog/%2e%2e%2fsibling.txt"),
    paste0("/", t, "/files/cog/%2E%2E%2F%2E%2E%2Fetc%2Fpasswd"),
    paste0("/", t, "/files/cog%2fpolar_3031.tif"), paste0("/", t, "/files/cog/polar_3031.tif%2f"),
    paste0("/", t, "/blob/cog_vertices%2f..%2fcog_indices"),
    "/etc/passwd", paste0("/", t, "//etc/passwd"), paste0("/", t, "/files/cog/%2Fetc%2Fpasswd"),
    paste0("/", t, "/files/cog/", utils::URLencode(x$file, reserved = TRUE)),
    paste0("/", t, "/files/cog/sibling.txt"), paste0("/", t, "/files/sst/polar_3031.tif")
  )
  for (p in paths) expect_identical(http_req(srv$port, p)$status, 404L, label = p)
})

test_that("blob keys with reserved characters are one path segment", {
  skip_if_not_installed("httpuv")
  key <- "a/b@c+d e"
  plain <- list(version = "0.1", view = list(type = "cartesian"),
                data = list(v = list(format = "arrow-ipc-file", blob = key)), layers = list())
  bytes <- as.raw(1:10)
  srv <- serve_test(plain, blobs = stats::setNames(list(bytes), key))
  on.exit(srv$stop())
  r <- http_req(srv$port, paste0("/", srv$token, "/blob/a%2Fb%40c%2Bd%20e"))
  expect_identical(r$status, 200L)
  expect_identical(r$headers[["content-type"]], "application/vnd.apache.arrow.file")
  expect_identical(r$body, bytes)
  ## Decoded once: "+" stays "+", and a raw "/" is not part of the key.
  expect_identical(http_req(srv$port, paste0("/", srv$token, "/blob/a/b%40c%2Bd%20e"))$status, 404L)
  expect_identical(http_req(srv$port, paste0("/", srv$token, "/blob/a%2Fb%40c%2Bd+e"))$status, 404L)
  ## Dot-segment keys cannot be served.
  for (k in c(".", "..")) {
    p2 <- plain
    p2$data$v$blob <- k
    expect_error(serve_test(p2, blobs = stats::setNames(list(bytes), k)), "dot segment")
  }
})

test_that("a foreign Host is refused with a warning unless allowed", {
  skip_if_not_installed("httpuv")
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  root <- paste0("/", srv$token, "/")
  expect_identical(http_req(srv$port, root, host = paste0("localhost:", srv$port))$status, 200L)
  expect_warning(r <- http_req(srv$port, root, host = "evil.example:80"),
                 "refused a request with Host \"evil.example:80\".*aobcore.serve_hosts")
  expect_identical(r$status, 403L)
  expect_warning(r <- http_req(srv$port, root, host = "127.0.0.1"), "Host \"127.0.0.1\"")
  expect_identical(r$status, 403L)
  old <- options(aobcore.serve_hosts = c("proxy.example", "evil.example:80"))
  on.exit(options(old), add = TRUE)
  expect_identical(http_req(srv$port, root, host = "evil.example:80")$status, 200L)
})

test_that("tokens and ports leave R's random number generator alone", {
  skip_if_not_installed("httpuv")
  ## Absent before, absent after.
  if (exists(".Random.seed", envir = globalenv())) {
    saved <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", saved, envir = globalenv()))
    rm(".Random.seed", envir = globalenv())
  }
  srv <- serve_test(probe_scene())
  srv$stop()
  expect_false(exists(".Random.seed", envir = globalenv()))
  ## Identical before and after.
  set.seed(42)
  before <- get(".Random.seed", envir = globalenv())
  srv <- serve_test(probe_scene())
  srv$stop()
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  ## The same seed gives different tokens and ports.
  set.seed(1)
  a <- serve_test(probe_scene())
  set.seed(1)
  b <- serve_test(probe_scene())
  on.exit({
    a$stop()
    b$stop()
  }, add = TRUE)
  expect_false(identical(a$token, b$token))
  expect_false(identical(a$port, b$port))
})

test_that("the fallback token source works and says it is weaker", {
  skip_if_not_installed("httpuv")
  old <- options(aobcore.serve_fallback_random = TRUE)
  on.exit(options(old))
  set.seed(7)
  before <- get(".Random.seed", envir = globalenv())
  expect_message(srv <- serve_test(probe_scene()), "weaker source")
  srv$stop()
  expect_message(tokens <- suppressMessages(replicate(5, random_hex(16L))), NA)
  expect_message(random_hex(16L), "weaker source")
  expect_true(all(grepl("^[0-9a-f]{32}$", c(srv$token, tokens))))
  expect_false(anyDuplicated(c(srv$token, tokens)) > 0)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  ## The port bytes are a separate draw: no port is the token's own bytes.
  expect_message(p <- random_ports(20L), "weaker source")
  expect_true(all(p >= 20000L & p <= 60000L))
})

test_that("ports come from bytes of their own", {
  b <- as.raw(c(0, 0, 255, 255, 0x9c, 0x41))
  local_mocked_bindings(random_bytes = function(n, quiet = FALSE) b[seq_len(n)])
  expect_identical(random_ports(3L), 20000L + as.integer(c(0, 65535, 0x9c41) %% 40001))
})

test_that("a relative cog url with no file and no tiles cannot be served", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  f <- system.file("extdata", "polar_3031.tif", package = "aobcore")
  s <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", f, embed = FALSE, url = "sst.tif",
                              range = c(0, 1))
  expect_error(serve_test(s), "`sst` has the relative url \"sst.tif\".*cannot answer")
  expect_error(serve_test(s, files = list()), "`sst` has the relative url")
  ## An embedded layer's base-name url is fine: its tiles are blobs.
  e <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", f, range = c(0, 1))
  srv <- serve_test(e)
  on.exit(srv$stop())
  page <- rawToChar(http_req(srv$port, paste0("/", srv$token, "/"))$body)
  expect_match(page, "\"url\":\"polar_3031.tif\"", fixed = TRUE)
  key <- grep("^sst@", names(scene_blobs(e)), value = TRUE)[1]
  r <- http_req(srv$port, paste0("/", srv$token, "/blob/", url_component(key)))
  expect_identical(r$status, 200L)
  expect_identical(r$headers[["content-type"]], "application/octet-stream")
  expect_identical(r$body, scene_blobs(e)[[key]])
  expect_match(page, key, fixed = TRUE) # listed in data-aob-blob-keys
  ## An explicit absolute url is left alone, file registered or not.
  a <- scene_add_tiled_raster(scene("EPSG:3031"), "sst", f, embed = FALSE,
                              url = "https://example.org/sst.tif", range = c(0, 1))
  srv2 <- serve_test(a)
  on.exit(srv2$stop(), add = TRUE)
  expect_match(rawToChar(http_req(srv2$port, paste0("/", srv2$token, "/"))$body),
               "\"url\":\"https://example.org/sst.tif\"", fixed = TRUE)
})

test_that("a plain-list scene is served with blobs and files given", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  plain <- unclass(x$scene)
  attributes(plain) <- list(names = names(plain))
  srv <- serve_test(plain, blobs = scene_blobs(x$scene), files = attr(x$scene, "files"))
  on.exit(srv$stop(), add = TRUE)
  r <- http_req(srv$port, paste0("/", srv$token, "/files/cog/polar_3031.tif"),
                headers = c(Range = "bytes=0-7"))
  expect_identical(r$status, 206L)
  expect_error(serve_test(plain, files = list(x = list(path = "a"))), "needs `path`, `size` and `mtime`")
  expect_error(serve_test(plain, blobs = scene_blobs(x$scene),
                          files = list(nope = attr(x$scene, "files")$cog)), "not a cog")
})

## ---- Range (#34) ------------------------------------------------------------

test_that("registered files answer single byte ranges", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene)
  on.exit(srv$stop(), add = TRUE)
  path <- paste0("/", srv$token, "/files/cog/polar_3031.tif")
  size <- file.size(x$file)
  all <- readBin(x$file, "raw", size)
  get <- function(range, method = "GET") http_req(srv$port, path, method = method, headers = c(Range = range))

  r <- http_req(srv$port, path)
  expect_identical(r$status, 200L)
  expect_identical(r$headers[["accept-ranges"]], "bytes")
  expect_identical(r$body, all)

  r <- get("bytes=100-199")
  expect_identical(r$status, 206L)
  expect_identical(r$headers[["content-range"]], sprintf("bytes 100-199/%.0f", size))
  expect_identical(r$headers[["accept-ranges"]], "bytes")
  expect_identical(r$body, all[101:200])
  r <- get(sprintf("bytes=%.0f-", size - 300))
  expect_identical(r$status, 206L)
  expect_identical(r$headers[["content-range"]], sprintf("bytes %.0f-%.0f/%.0f", size - 300, size - 1, size))
  expect_identical(r$body, all[(size - 299):size])
  r <- get("bytes=-50")
  expect_identical(r$status, 206L)
  expect_identical(r$headers[["content-range"]], sprintf("bytes %.0f-%.0f/%.0f", size - 50, size - 1, size))
  expect_identical(r$body, all[(size - 49):size])
  ## An end past the file is clipped to it.
  r <- get(sprintf("bytes=%.0f-%.0f", size - 10, size + 100))
  expect_identical(r$status, 206L)
  expect_identical(r$body, all[(size - 9):size])

  ## Past the end, longer than the cap, several ranges, malformed: 416.
  for (rg in c(sprintf("bytes=%.0f-", size), sprintf("bytes=%.0f-%.0f", size + 1, size + 5),
               "bytes=0-1,4-5", "bytes=5-2", "bytes=-0", "bytes=abc", "bytes=-")) {
    r <- get(rg)
    expect_identical(r$status, 416L, label = rg)
    expect_identical(r$headers[["content-range"]], sprintf("bytes */%.0f", size), label = rg)
  }
  old <- options(aobcore.serve_range_max = 64)
  r <- get("bytes=0-64")
  expect_identical(r$status, 416L)
  expect_identical(get("bytes=0-63")$status, 206L)
  options(old)

  ## HEAD gives the length.
  r <- http_req(srv$port, path, method = "HEAD")
  expect_identical(r$status, 200L)
  expect_identical(as.numeric(r$headers[["content-length"]]), size)
  r <- get("bytes=0-9", method = "HEAD")
  expect_identical(r$status, 206L)
  expect_identical(r$headers[["content-length"]], "10")
})

test_that("a file changed or gone after registration is refused", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene)
  on.exit(srv$stop(), add = TRUE)
  path <- paste0("/", srv$token, "/files/cog/polar_3031.tif")
  Sys.setFileTime(x$file, attr(x$scene, "files")$cog$mtime + 60)
  expect_warning(r <- http_req(srv$port, path, headers = c(Range = "bytes=0-9")),
                 "has changed since it was registered")
  expect_identical(r$status, 409L)
  expect_error(serve_test(x$scene), "has changed since it was registered")
  unlink(x$file)
  expect_warning(r <- http_req(srv$port, path), "is gone")
  expect_identical(r$status, 404L)
  expect_error(serve_test(x$scene), "is gone")
})

## ---- lifecycle (#35) --------------------------------------------------------

test_that("the handle prints, stops once and frees its port", {
  skip_if_not_installed("httpuv")
  owned <- tempfile(fileext = ".tif")
  writeLines("x", owned)
  srv <- serve_test(probe_scene(), own = owned)
  expect_output(print(srv), paste0("<aob_server> ", srv$url, "\n  6 blobs, 0 files, running"), fixed = TRUE)
  expect_false(port_free(srv$port))
  expect_true(srv$running())
  expect_true(srv$status()$running)
  expect_true(srv$token %in% vapply(scene_servers(), `[[`, "", "token"))
  srv$stop()
  expect_false(file.exists(owned))
  expect_output(print(srv), "stopped")
  expect_false(srv$running())
  expect_false(srv$status()$running)
  expect_identical(srv$status()$connections, 0L)
  expect_true(port_free(srv$port))
  expect_silent(srv$stop())
  expect_false(srv$token %in% vapply(scene_servers(), `[[`, "", "token"))
  expect_error(serve_test(probe_scene(), server = srv), "has been stopped")
})

test_that("replacing the scene keeps the URL", {
  skip_if_not_installed("httpuv")
  srv <- serve_test(probe_scene(), title = "first")
  on.exit(srv$stop())
  root <- paste0("/", srv$token, "/")
  expect_match(rawToChar(http_req(srv$port, root)$body), "<title>first</title>", fixed = TRUE)
  x <- wk::wkt("POINT (0 0)", crs = "EPSG:3031")
  s2 <- scene_add_vector(scene(), "pt", x)
  same <- serve_test(s2, server = srv, title = "second", port = 1)
  expect_identical(same$url, srv$url)
  page <- rawToChar(http_req(srv$port, root)$body)
  expect_match(page, "<title>second</title>", fixed = TRUE)
  expect_match(page, "\"pt\"", fixed = TRUE)
  ## The old blobs are gone, the new ones served.
  expect_identical(http_req(srv$port, paste0(root, "blob/land"))$status, 404L)
  expect_identical(http_req(srv$port, paste0(root, "blob/pt"))$status, 200L)
  expect_output(print(srv), "1 blobs, 0 files, running")
  expect_error(serve_test(s2, server = list()), "`server` must be")
})

test_that("three servers at once are listed and stopped together", {
  skip_if_not_installed("httpuv")
  stop_scene_servers()
  s <- lapply(1:3, function(i) serve_test(probe_scene(), title = paste("scene", i)))
  expect_length(scene_servers(), 3L)
  expect_setequal(names(scene_servers()), vapply(s, `[[`, "", "url"))
  expect_length(unique(vapply(s, `[[`, 0L, "port")), 3L)
  for (x in s) expect_identical(http_req(x$port, paste0("/", x$token, "/"))$status, 200L)
  expect_identical(stop_scene_servers(), 3L)
  expect_length(scene_servers(), 0L)
  for (x in s) expect_true(port_free(x$port))
  expect_identical(stop_scene_servers(), 0L)
})

test_that("a non-interactive call warns", {
  skip_if_not_installed("httpuv")
  expect_warning(srv <- serve_scene(probe_scene(), open = FALSE), class = "aobcore_serve_noninteractive")
  srv$stop()
  expect_warning(srv <- serve_scene(probe_scene(), open = FALSE), "non-interactive session")
  srv$stop()
})

test_that("servers stop when the session ends and when aobcore is unloaded", {
  skip_if_not_installed("httpuv")
  skip_on_cran()
  d <- tempfile("aob-child-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  run_child <- function(code) {
    script <- file.path(d, "child.R")
    writeLines(code, script)
    old <- Sys.getenv("R_LIBS")
    Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
    on.exit(Sys.setenv(R_LIBS = old))
    out <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = TRUE, stderr = TRUE)
    out
  }
  owned <- file.path(d, "owned-exit.tif")
  port_file <- file.path(d, "port")
  out <- run_child(c(
    "suppressWarnings(library(aobcore))",
    sprintf("writeLines('x', %s)", deparse(owned)),
    sprintf("srv <- suppressWarnings(serve_scene(probe_scene(), open = FALSE, own = %s))", deparse(owned)),
    "rm(srv); invisible(gc())",
    sprintf("writeLines(as.character(scene_servers()[[1]]$port), %s)", deparse(port_file)),
    "cat('serving\\n')"
  ))
  expect_true(any(grepl("serving", out)), label = paste(out, collapse = "\n"))
  expect_false(file.exists(owned))
  expect_true(port_free(as.integer(readLines(port_file))))

  owned2 <- file.path(d, "owned-unload.tif")
  out <- run_child(c(
    "suppressWarnings(library(aobcore))",
    sprintf("writeLines('x', %s)", deparse(owned2)),
    sprintf("srv <- suppressWarnings(serve_scene(probe_scene(), open = FALSE, own = %s))", deparse(owned2)),
    "p <- srv$port",
    "unloadNamespace('aobcore')",
    sprintf("cat('owned', file.exists(%s), '\\n')", deparse(owned2)),
    "s <- tryCatch(httpuv::startServer('127.0.0.1', p, list(call = function(req) NULL)), error = function(e) NULL)",
    "cat('free', !is.null(s), '\\n')"
  ))
  expect_true(any(grepl("owned FALSE", out)), label = paste(out, collapse = "\n"))
  expect_true(any(grepl("free TRUE", out)), label = paste(out, collapse = "\n"))
})

test_that("url_component() encodes as encodeURIComponent() does", {
  expect_identical(url_component(c("a/b@c+d e!*'()~-_.", "\u00e9%", "cog@123+45")),
                   c("a%2Fb%40c%2Bd%20e!*'()~-_.", "%C3%A9%25", "cog%40123%2B45"))
})

## ---- attack cases (security review of #37) ----------------------------------

test_that("a hostile Host is escaped, cut short and warned about once", {
  skip_if_not_installed("httpuv")
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  root <- paste0("/", srv$token, "/")
  evil <- paste0("\033]0;pwned\007\033[2Jx", strrep("y", 200))
  ## Caught with a calling handler: unwinding out of the request handler
  ## (as tryCatch() would) breaks httpuv's response.
  w <- character()
  r <- withCallingHandlers(http_req(srv$port, root, host = evil), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_identical(r$status, 403L)
  expect_length(w, 1L)
  expect_false(grepl("[\001-\037\177]", w))
  expect_match(w, "\\x1b]0;pwned\\x07\\x1b[2Jxyyy", fixed = TRUE)
  expect_match(w, "yyy...\"", fixed = TRUE)
  expect_lt(nchar(w), 600)
  expect_match(w, "Only if this is your IDE proxy's host", fixed = TRUE)
  ## Once per Host per server; another Host warns again.
  expect_no_warning(r <- http_req(srv$port, root, host = evil))
  expect_identical(r$status, 403L)
  expect_warning(http_req(srv$port, root, host = "other.example"), "other.example")
})

test_that("undecodable segments answer 404, not 500", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene)
  on.exit(srv$stop(), add = TRUE)
  t <- srv$token
  for (p in c("/blob/%00", "/blob/cog_vertices%00", "/blob/cog_vertices%00x", "/blob/%ff%fe", "/blob/%zz",
              "/files/%00/polar_3031.tif", "/files/cog/polar_3031.tif%00",
              "/files/cog/%c0%ae%c0%ae", "/files/cog/%"))
    expect_identical(http_req(srv$port, paste0("/", t, p))$status, 404L, label = p)
})

test_that("HEAD sends headers only, with the GET length", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene)
  on.exit(srv$stop(), add = TRUE)
  root <- paste0("/", srv$token, "/")
  for (p in c("", "aob-renderer.min.js", "blob/cog_vertices", "files/cog/polar_3031.tif", "nope")) {
    g <- http_req(srv$port, paste0(root, p))
    h <- http_req(srv$port, paste0(root, p), method = "HEAD")
    expect_identical(h$status, g$status, label = p)
    expect_identical(h$headers[["content-length"]], sprintf("%.0f", length(g$body)), label = p)
    expect_identical(h$trailing, 0L, label = p)
  }
  h <- http_req(srv$port, paste0(root, "files/cog/polar_3031.tif"), method = "HEAD",
                headers = c(Range = "bytes=0-9"))
  expect_identical(h$status, 206L)
  expect_identical(h$headers[["content-length"]], "10")
})

test_that("responses carry nosniff, the page no referrer, and units are case-insensitive", {
  skip_if_not_installed("httpuv")
  skip_if_no_gdal()
  x <- served_cog()
  on.exit(unlink(x$dir, recursive = TRUE))
  srv <- serve_test(x$scene)
  on.exit(srv$stop(), add = TRUE)
  root <- paste0("/", srv$token, "/")
  for (p in c("", "blob/cog_vertices", "files/cog/polar_3031.tif", "nope")) {
    expect_identical(http_req(srv$port, paste0(root, p))$headers[["x-content-type-options"]], "nosniff",
                     label = p)
  }
  expect_match(rawToChar(http_req(srv$port, root)$body),
               "<meta name=\"referrer\" content=\"no-referrer\"/>", fixed = TRUE)
  for (u in c("BYTES=0-9", "Bytes=0-9")) {
    r <- http_req(srv$port, paste0(root, "files/cog/polar_3031.tif"), headers = c(Range = u))
    expect_identical(r$status, 206L, label = u)
    expect_length(r$body, 10L)
  }
  r <- http_req(srv$port, paste0(root, "files/cog/polar_3031.tif"), headers = c(Range = "items=0-9"))
  expect_identical(r$status, 200L)
})

test_that("own takes existing regular files only and deletes them only if unchanged", {
  skip_if_not_installed("httpuv")
  d <- tempfile("aob-own-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  target <- file.path(d, "precious.txt")
  writeLines("keep me", target)
  ## A symbolic link is refused, so its target cannot be deleted.
  link <- file.path(d, "link.tif")
  if (isTRUE(suppressWarnings(file.symlink(target, link)))) {
    expect_error(serve_test(probe_scene(), own = link), "symbolic link")
  }
  ## A path that does not exist yet is refused.
  expect_error(serve_test(probe_scene(), own = file.path(d, "later.tif")), "does not exist")
  expect_error(serve_test(probe_scene(), own = d), "not a regular file")
  ## A relative path is kept absolute: a same-named file in another working
  ## directory is left alone.
  owned <- file.path(d, "own.tif")
  writeLines("tmp", owned)
  other <- tempfile("aob-own-other-")
  dir.create(other)
  on.exit(unlink(other, recursive = TRUE), add = TRUE)
  writeLines("other", file.path(other, "own.tif"))
  old <- setwd(d)
  srv <- tryCatch(serve_test(probe_scene(), own = "own.tif"), finally = setwd(old))
  setwd(other)
  srv$stop()
  setwd(old)
  expect_false(file.exists(owned))
  expect_true(file.exists(file.path(other, "own.tif")))
  ## A file changed since it was given is left, with a warning.
  writeLines("tmp", owned)
  srv <- serve_test(probe_scene(), own = owned)
  Sys.setFileTime(owned, Sys.time() + 120)
  expect_warning(srv$stop(), "Left .* in place")
  expect_true(file.exists(owned))
  expect_true(file.exists(target))
})

test_that("only Unix reads /dev/urandom", {
  body <- paste(deparse(random_bytes), collapse = "\n")
  expect_match(body, ".Platform$OS.type == \"unix\"", fixed = TRUE)
})

test_that("a link is found by resolving it where Sys.readlink() cannot see it", {
  d <- tempfile("aob-link-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  f <- file.path(d, "a.tif")
  writeLines("x", f)
  l <- file.path(d, "l.tif")
  skip_if_not(isTRUE(suppressWarnings(file.symlink(f, l))), "no symbolic links here")
  ## As on Windows, where Sys.readlink() always returns "".
  no_readlink <- is_link
  environment(no_readlink) <- list2env(list(Sys.readlink = function(p) ""),
                                       parent = asNamespace("aobcore"))
  expect_true(no_readlink(l))
  expect_false(no_readlink(f))
  expect_false(no_readlink(file.path(d, "..", basename(d), "a.tif")))
})

test_that("websocket upgrades are refused, closed and print nothing", {
  skip_if_not_installed("httpuv")
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  paths <- c("/", "/no-token-at-all", paste0("/", srv$token), paste0("/", srv$token, "/"),
             paste0("/", srv$token, "/index.html"))
  hosts <- c(paste0("127.0.0.1:", srv$port), "evil.example:80")
  for (path in paths) for (host in hosts) for (upgrade in c("websocket", "WebSocket")) {
    out <- NULL
    msg <- capture.output(type = "message", out <- capture.output(
      expect_no_warning(got <- ws_upgrade(srv$port, path, host = host, upgrade = upgrade))))
    label <- paste(path, host, upgrade)
    ## The answer starts with a refusal, never with an accepted handshake.
    expect_false(got$switched_first, label = label)
    expect_true(startsWith(got$head, "HTTP/1.1 404 Not Found\r\n"), label = label)
    expect_true(grepl("X-Content-Type-Options: nosniff", got$head, fixed = TRUE), label = label)
    expect_identical(c(out, msg), character(), label = label)
  }
  ## Ordinary requests still reach the routes.
  expect_identical(http_req(srv$port, paste0("/", srv$token, "/"))$status, 200L)
  expect_identical(http_req(srv$port, paste0("/", srv$token, "/aob-renderer.min.js"))$status, 200L)
})

test_that("only a websocket Upgrade header counts as an upgrade", {
  expect_true(is_upgrade(list(HTTP_UPGRADE = "websocket")))
  expect_true(is_upgrade(list(HTTP_UPGRADE = "WebSocket")))
  expect_true(is_upgrade(list(HTTP_UPGRADE = "h2c, websocket")))
  expect_false(is_upgrade(list()))
  expect_false(is_upgrade(list(HTTP_UPGRADE = "")))
  expect_false(is_upgrade(list(HTTP_UPGRADE = "h2c")))
  expect_false(is_upgrade(list(HTTP_UPGRADE = "notwebsocket")))
})

## ---- follow-ups from the #37 review (#38) ------------------------------------

test_that("refused-Host warnings stop after five distinct values", {
  skip_if_not_installed("httpuv")
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  root <- paste0("/", srv$token, "/")
  w <- character()
  for (i in 1:8) {
    r <- withCallingHandlers(http_req(srv$port, root, host = paste0("h", i, ".example")),
                             warning = function(c) {
                               w <<- c(w, conditionMessage(c))
                               invokeRestart("muffleWarning")
                             })
    expect_identical(r$status, 403L)
  }
  expect_length(w, 6L)
  expect_match(w[5], "h5.example", fixed = TRUE)
  expect_match(w[6], "further refusals not shown", fixed = TRUE)
  expect_length(srv$state$warned_hosts, 5L)
})

test_that("own = \".\" is refused as not a regular file", {
  skip_if_not_installed("httpuv")
  expect_error(serve_test(probe_scene(), own = "."), "\".\" is not a regular file")
})

test_that("a path in another case is not taken for a link on macOS", {
  d <- tempfile("aob-case-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  f <- file.path(d, "a.tif")
  writeLines("x", f)
  given <- file.path(d, "A.TIF")
  real_dir <- normalizePath(d, winslash = "/")
  ## As on a case-insensitive volume: the given name exists and resolves to
  ## the file's own case.
  fake <- function(sysname) {
    g <- is_link
    environment(g) <- list2env(list(
      Sys.readlink = function(p) "",
      file.exists = function(p) TRUE,
      Sys.info = function() c(sysname = sysname),
      normalizePath = function(p, ...) {
        if (identical(p, given)) file.path(real_dir, "a.tif") else base::normalizePath(p, ...)
      }), parent = asNamespace("aobcore"))
    g
  }
  skip_if(.Platform$OS.type == "windows", "Windows folds case already")
  expect_false(fake("Darwin")(given))
  expect_true(fake("Linux")(given))
})
