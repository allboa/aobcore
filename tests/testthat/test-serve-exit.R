# Servers stopping when the session ends or aobcore is unloaded, with a page
# connected. At exit the sockets are dropped, not closed: httpuv may have
# finalized a socket's handle already, and ws$close() on it crashed R
# (exit status 139 from the exit finalizer).

test_that("the exit finalizer forgets sockets without closing them", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  socks <- lapply(1:2, function(i) ws_open(srv))
  ws_hello(socks[[1]])
  server_stop(srv$state, close_sockets = FALSE)
  for (ws in socks) expect_null(ws$closed)
  expect_identical(length(srv$state$conns), 0L)
  expect_false(exists(srv$token, envir = servers, inherits = FALSE))
  expect_error(srv$selection(), "has stopped")
})

## A child R process serves a scene, connects a websocket to it from the
## same process, then ends as `mode` says. Its exit status, and what the
## socket last read.
run_exit_child <- function(mode) {
  d <- tempfile("aob-exit-child-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  out <- file.path(d, "out")
  log <- file.path(d, "log")
  script <- file.path(d, "child.R")
  writeLines(c(
    sprintf("mode <- %s", deparse(mode)),
    sprintf("out <- %s", deparse(out)),
    "suppressWarnings(library(aobcore))",
    "srv <- suppressWarnings(serve_scene(probe_scene(), open = FALSE))",
    "con <- socketConnection('127.0.0.1', srv$port, blocking = FALSE, open = 'r+b', timeout = 5)",
    "writeBin(charToRaw(paste0('GET /', srv$token, '/ws HTTP/1.1\\r\\nHost: 127.0.0.1:', srv$port,",
    "  '\\r\\nUpgrade: websocket\\r\\nConnection: Upgrade\\r\\nOrigin: http://127.0.0.1:', srv$port,",
    "  '\\r\\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\\r\\nSec-WebSocket-Version: 13\\r\\n\\r\\n')), con)",
    "t0 <- Sys.time()",
    "secs <- function() as.numeric(difftime(Sys.time(), t0, units = 'secs'))",
    "while (srv$connections() < 1L && secs() < 30) httpuv::service(20)",
    "writeLines(paste('connections', srv$connections()), out)",
    "if (mode == 'error') stop('an error in the script')",
    "if (mode == 'unload') {",
    "  unloadNamespace('aobcore')",
    ## Past the 101's headers, the close frame from the unload: 0x88, its
    ## length, then the code.
    "  buf <- raw(); t0 <- Sys.time()",
    "  while (secs() < 10) {",
    "    httpuv::service(20); buf <- c(buf, readBin(con, 'raw', 1e5))",
    "    end <- grepRaw(charToRaw('\\r\\n\\r\\n'), buf, fixed = TRUE)",
    "    rest <- if (length(end)) buf[-seq_len(end + 3L)] else raw()",
    "    if (length(rest) >= 4L) break",
    "  }",
    "  code <- if (length(rest) >= 4L && rest[1] == as.raw(0x88)) as.integer(rest[3]) * 256L + as.integer(rest[4]) else NA",
    "  cat(paste('close', code), file = out, sep = '\\n', append = TRUE)",
    "}",
    "cat('end\\n', file = out, append = TRUE)"
  ), script)
  old <- Sys.getenv("R_LIBS")
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(Sys.setenv(R_LIBS = old), add = TRUE)
  status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                     stdout = log, stderr = log))
  list(status = status, out = readLines_safe(out), log = readLines_safe(log))
}

test_that("a session that ends with a page connected exits cleanly", {
  skip_if_no_ws()
  skip_on_cran()
  r <- run_exit_child("end")
  expect_identical(r$out, c("connections 1", "end"))
  expect_identical(as.integer(r$status), 0L, label = paste(r$log, collapse = "\n"))
})

test_that("a script that stops with an error and a page connected exits with its error", {
  skip_if_no_ws()
  skip_on_cran()
  r <- run_exit_child("error")
  expect_identical(r$out, "connections 1")
  ## Rscript's status for an error (1), not a signal's (128 + n).
  expect_identical(as.integer(r$status), 1L, label = paste(r$log, collapse = "\n"))
  expect_true(any(grepl("an error in the script", r$log, fixed = TRUE)))
})

test_that("unloading aobcore with a page connected closes its socket with 1001", {
  skip_if_no_ws()
  skip_on_cran()
  r <- run_exit_child("unload")
  expect_identical(r$out, c("connections 1", "close 1001", "end"))
  expect_identical(as.integer(r$status), 0L, label = paste(r$log, collapse = "\n"))
})
