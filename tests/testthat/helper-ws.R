# Helpers for the websocket of serve_scene() (decision 0007).

# A fake httpuv WebSocket: an environment with `request`, `onMessage()`,
# `onClose()`, `send()` and `close()` that record what they are given.
fake_ws <- function(req) {
  ws <- new.env(parent = emptyenv())
  ws$request <- req
  ws$sent <- character()
  ws$closed <- NULL
  ws$on_message <- NULL
  ws$on_close <- NULL
  ws$onMessage <- function(f) ws$on_message <- f
  ws$onClose <- function(f) ws$on_close <- f
  ws$send <- function(x) ws$sent <- c(ws$sent, x)
  ws$close <- function(code = 1000L, reason = "") {
    if (is.null(ws$closed)) ws$closed <- list(code = as.integer(code), reason = reason)
  }
  ws
}

# The request of an upgrade to `srv`'s socket, with the given parts.
ws_request <- function(srv, path = paste0("/", srv$token, "/ws"),
                       host = paste0("127.0.0.1:", srv$port),
                       origin = paste0("http://127.0.0.1:", srv$port), method = "GET") {
  req <- list(REQUEST_METHOD = method, PATH_INFO = path, HTTP_HOST = host,
              HTTP_UPGRADE = "websocket", HTTP_CONNECTION = "Upgrade")
  req$HTTP_ORIGIN <- origin
  req
}

# Open a fake socket on `srv` as httpuv would (onHeaders, then onWSOpen).
ws_open <- function(srv, ...) {
  req <- ws_request(srv, ...)
  res <- ws_on_headers(srv$state, req)
  ws <- fake_ws(req)
  ws_on_open(srv$state, ws)
  ws$headers_response <- res
  ws
}

# Send a message (a list, as JSON, or a string as is) from the page.
ws_say <- function(ws, x, binary = FALSE) {
  if (is.list(x)) x <- json_value(x)
  ws$on_message(binary, x)
  invisible(ws)
}

ws_hello <- function(ws, scene = 1L, specs = list("0.1", "0.2", "0.3", "0.4", "0.5")) {
  ws_say(ws, list(type = "hello", protocol = 1L, renderer = "0.0.5", specs = specs, scene = scene))
}

# The messages R sent, parsed.
ws_sent <- function(ws) lapply(ws$sent, jsonlite::parse_json, simplifyVector = TRUE)

skip_if_no_ws <- function() {
  testthat::skip_if_not_installed("httpuv")
  testthat::skip_if_not_installed("jsonlite")
}

# ---- a raw websocket client, for a server in another process ---------------

# A masked client frame (RFC 6455 5.2).
ws_frame <- function(text, opcode = 1L) {
  p <- if (is.raw(text)) text else charToRaw(enc2utf8(text))
  n <- length(p)
  len <- if (n < 126) as.raw(0x80 + n) else as.raw(c(0x80 + 126, n %/% 256, n %% 256))
  mask <- as.raw(c(0x12, 0x34, 0x56, 0x78))
  c(as.raw(0x80 + opcode), len, mask, xor(p, rep(mask, length.out = n)))
}

# Server frames (unmasked) in `buf`: a list of opcode and payload.
ws_frames <- function(buf) {
  out <- list()
  i <- 1L
  while (i + 1L <= length(buf)) {
    op <- as.integer(buf[i]) %% 16L
    n <- as.integer(buf[i + 1L]) %% 128L
    h <- 2L
    if (n == 126L) {
      if (i + 3L > length(buf)) break
      n <- as.integer(buf[i + 2L]) * 256L + as.integer(buf[i + 3L])
      h <- 4L
    }
    if (i + h + n - 1L > length(buf)) break
    out[[length(out) + 1L]] <- list(opcode = op, payload = buf[seq_len(n) + i + h - 1L])
    i <- i + h + n
  }
  out
}

close_code <- function(frame) as.integer(frame$payload[1]) * 256L + as.integer(frame$payload[2])

# Bytes read from `con` for `wait` seconds, or until `until(buf)` is TRUE.
read_for <- function(con, wait = 1, until = function(buf) FALSE) {
  buf <- raw()
  t0 <- Sys.time()
  while (as.numeric(Sys.time() - t0, units = "secs") < wait) {
    chunk <- readBin(con, "raw", 1e6)
    if (length(chunk)) buf <- c(buf, chunk) else Sys.sleep(0.02)
    if (until(buf)) break
  }
  buf
}

# Split a raw response into its first header block and what follows it.
split_head <- function(buf) {
  end <- grepRaw(charToRaw("\r\n\r\n"), buf, fixed = TRUE)
  if (!length(end)) return(list(head = "", rest = raw()))
  list(head = rawToChar(buf[seq_len(end - 1L)]), rest = buf[-seq_len(end + 3L)])
}

ws_upgrade_raw <- function(port, path, host = paste0("127.0.0.1:", port), origin = NULL) {
  con <- socketConnection("127.0.0.1", port, blocking = FALSE, open = "r+b", timeout = 5)
  writeBin(charToRaw(paste0(
    "GET ", path, " HTTP/1.1\r\nHost: ", host, "\r\n",
    "Upgrade: websocket\r\nConnection: Upgrade\r\n",
    if (!is.null(origin)) paste0("Origin: ", origin, "\r\n"),
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n")), con)
  con
}

readLines_safe <- function(f) {
  if (!file.exists(f)) return(character())
  ## A file another process still holds open may not be readable on
  ## Windows: that is "nothing yet".
  tryCatch(suppressWarnings(readLines(f, warn = FALSE)), error = function(e) character())
}

secs_since <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
