# A minimal HTTP/1.1 client for testing serve_scene() in this process: it
# writes the request on a socket and runs httpuv's event loop until the
# whole response has arrived, so the path, Host and Range are sent exactly
# as given (no client-side normalization of "..", "%2e" or "%2f").
http_req <- function(port, path, method = "GET", host = paste0("127.0.0.1:", port),
                     headers = character(), timeout = 20) {
  con <- socketConnection("127.0.0.1", port, blocking = FALSE, open = "r+b", timeout = timeout)
  on.exit(close(con))
  extra <- if (length(headers)) paste0(names(headers), ": ", headers, "\r\n", collapse = "") else ""
  writeBin(charToRaw(paste0(method, " ", path, " HTTP/1.1\r\nHost: ", host, "\r\n", extra,
                            "Connection: close\r\n\r\n")), con)
  buf <- raw()
  t0 <- Sys.time()
  repeat {
    httpuv::service(10)
    chunk <- readBin(con, "raw", 1e7)
    if (length(chunk)) buf <- c(buf, chunk)
    res <- parse_http(buf, method)
    if (!is.null(res) && method == "HEAD") {
      ## A HEAD response has no body: nothing may follow the headers.
      t1 <- Sys.time()
      while (as.numeric(Sys.time() - t1, units = "secs") < 0.3) {
        httpuv::service(10)
        buf <- c(buf, readBin(con, "raw", 1e7))
      }
      end <- grepRaw(charToRaw("\r\n\r\n"), buf, fixed = TRUE)
      res$trailing <- length(buf) - (end + 3L)
      testthat::expect_identical(res$trailing, 0L, label = paste("bytes after HEAD headers for", path))
      return(res)
    }
    if (!is.null(res)) return(res)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) stop("No response to ", path)
  }
}

parse_http <- function(buf, method) {
  end <- grepRaw(charToRaw("\r\n\r\n"), buf, fixed = TRUE)
  if (!length(end)) return(NULL)
  lines <- strsplit(rawToChar(buf[seq_len(end - 1L)]), "\r\n", fixed = TRUE)[[1]]
  status <- as.integer(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  kv <- regmatches(lines[-1], regexpr(": ", lines[-1], fixed = TRUE), invert = TRUE)
  headers <- stats::setNames(vapply(kv, `[`, "", 2), tolower(vapply(kv, `[`, "", 1)))
  body <- buf[-seq_len(end + 3L)]
  n <- as.numeric(headers[["content-length"]] %||% "0")
  if (method != "HEAD" && length(body) < n) return(NULL)
  list(status = status, headers = headers, body = if (method == "HEAD") raw() else body[seq_len(n)])
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# serve_scene() for tests: no browser, and the non-interactive warning
# (every call here is non-interactive) muffled by its class.
serve_test <- function(...) {
  withCallingHandlers(serve_scene(..., open = FALSE),
                      aobcore_serve_noninteractive = function(w) invokeRestart("muffleWarning"))
}

port_free <- function(port) {
  s <- tryCatch(httpuv::startServer("127.0.0.1", port, list(call = function(req) NULL)),
                error = function(e) NULL)
  if (is.null(s)) return(FALSE)
  httpuv::stopServer(s)
  TRUE
}

# A websocket upgrade request on a raw socket: the header block of the
# first response the server sent within `wait` seconds (what a client acts
# on), and whether any of it switched protocols first.
ws_upgrade <- function(port, path, host = paste0("127.0.0.1:", port),
                       upgrade = "websocket", wait = 1) {
  con <- socketConnection("127.0.0.1", port, blocking = FALSE, open = "r+b", timeout = 5)
  on.exit(close(con))
  writeBin(charToRaw(paste0(
    "GET ", path, " HTTP/1.1\r\nHost: ", host, "\r\n",
    "Upgrade: ", upgrade, "\r\nConnection: Upgrade\r\n",
    "Origin: http://evil.example\r\n",
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n")), con)
  buf <- raw()
  t0 <- Sys.time()
  while (as.numeric(Sys.time() - t0, units = "secs") < wait) {
    httpuv::service(10)
    buf <- c(buf, readBin(con, "raw", 1e6))
  }
  end <- grepRaw(charToRaw("\r\n\r\n"), buf, fixed = TRUE)
  if (!length(end)) return(list(head = "", switched_first = FALSE))
  head <- rawToChar(buf[seq_len(end - 1L)])
  list(head = head, switched_first = startsWith(head, "HTTP/1.1 101"))
}
