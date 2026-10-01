#' Serve a scene from a local HTTP server
#'
#' Starts a small HTTP server on `127.0.0.1` that answers the page for
#' `scene`, the bundled renderer, the scene's Arrow blobs and the local files
#' the scene registered (a COG added with `scene_add_tiled_raster(embed =
#' FALSE)`), the files with HTTP range requests. The browser then reads a
#' large local COG tile by tile as it is drawn, instead of carrying its bytes
#' in the page. This is the local server transport of decision 0006 in
#' allboa/design. It needs the 'httpuv' package.
#'
#' **The page.** It is the page [write_scene_html()] writes, from the same
#' builder, with the renderer and blobs linked rather than inlined: the scene
#' document is the same, and blob keys stay blob keys. A registered file
#' whose `url` was not given explicitly is given the relative URL
#' `files/<data id>/<base name>` in the copy that is served (so the served
#' page never holds the local path); the scene itself is not changed. Tile
#' blobs of embedded layers are served as blobs, so one scene can mix
#' embedded and served layers.
#'
#' **Routes**, all under `http://127.0.0.1:<port>/<token>/`: the page (`""`
#' and `index.html`), `aob-renderer.min.js`, `blob/<encoded key>` and
#' `files/<data id>/<base name>`. `/<token>` answers 301 to `/<token>/`;
#' anything else, including a wrong token, answers 404. Only `GET` and
#' `HEAD` are answered (405 otherwise). A file answers a single
#' `Range: bytes=` request with 206 and those bytes (a range past the end, a
#' multi-range request or one longer than `getOption("aobcore.serve_range_max",
#' 64 * 2^20)` bytes answers 416), and a request without `Range` with the
#' whole file. A registered file that has changed since it was registered
#' answers 409, and one that is gone 404, each with a warning in R.
#'
#' **Security.** The server binds the loopback interface only. Every route
#' is under a random 128-bit token, read from `/dev/urandom` where it exists
#' (with a weaker fallback elsewhere, which says so in a message). The port
#' is drawn from separate random bytes, between 20000 and 60000; neither
#' uses R's random number generator, so `.Random.seed` is untouched. A
#' request whose `Host` is not `127.0.0.1:<port>` or `localhost:<port>` is
#' refused (403, with a warning) unless listed in
#' `getOption("aobcore.serve_hosts")`, for an IDE proxy that forwards
#' requests with its own `Host`. There are no CORS headers, and only the
#' registered files can be read. The token is not authentication: anyone
#' who sees the URL can read the scene while it is served.
#'
#' **Lifecycle.** Each call starts its own server, unless `server` names a
#' running one: its scene is then replaced, with the same URL. The handle has
#' `url`, `port`, `token` and `stop()`. Servers are kept in a registry, so
#' losing the handle does not stop one; [scene_servers()] lists them,
#' [stop_scene_servers()] stops them all, and all stop when the R session
#' ends or aobcore is unloaded. httpuv answers requests only while R is idle
#' at the prompt, so a long computation stalls the page; in `Rscript` the
#' server stops when the script ends, and `serve_scene()` warns when the
#' session is not interactive.
#'
#' @param scene A scene, as for [write_scene_html()].
#' @param blobs A named list of raw vectors, as for [write_scene_html()].
#'   Defaults to the blobs the scene carries.
#' @param files Registered local files: a named list, by cog data id, of
#'   records with `path`, `size`, `mtime` and `url_explicit`. Defaults to
#'   those the scene carries (see [scene_add_tiled_raster()]); give it for a
#'   scene that is a plain list.
#' @param port A port number, or `NULL` for a random one.
#' @param open Open the page: in the IDE viewer when `getOption("viewer")`
#'   is set, else with [utils::browseURL()].
#' @param title,theme As for [write_scene_html()].
#' @param server A running server from an earlier `serve_scene()` call, to
#'   serve this scene on instead of starting a new one. `port` is then
#'   ignored.
#' @param own Paths of files the server owns (such as temporary COGs): they
#'   are deleted when it stops. Added to any it already owns.
#' @return A handle of class `"aob_server"`, invisibly: a list with `url`,
#'   `port`, `token` and `stop()`.
#' @seealso [scene_servers()], [stop_scene_servers()].
#' @export
#' @examplesIf interactive() && requireNamespace("httpuv", quietly = TRUE)
#' srv <- serve_scene(probe_scene())
#' srv
#' srv$stop()
serve_scene <- function(scene, blobs = attr(scene, "blobs"), files = attr(scene, "files"),
                        port = NULL, open = interactive(), title = NULL,
                        theme = c("auto", "light", "dark"), server = NULL, own = NULL) {
  if (!has_httpuv()) {
    stop("serve_scene() needs the 'httpuv' package; install it with ",
         "install.packages(\"httpuv\").", call. = FALSE)
  }
  theme <- match.arg(theme)
  title <- title %||% "allonboard scene"
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be a single string.", call. = FALSE)
  }
  if (!is.null(server) && !inherits(server, "aob_server")) {
    stop("`server` must be a server from serve_scene().", call. = FALSE)
  }
  if (!is.null(server) && !isTRUE(server$state$running)) {
    stop("`server` has been stopped; call serve_scene() without it to start a new one.",
         call. = FALSE)
  }
  if (!is.null(own) && (!is.character(own) || anyNA(own))) {
    stop("`own` must be a character vector of paths.", call. = FALSE)
  }
  content <- serve_content(scene, blobs %||% list(), files %||% list(), title, theme)

  if (!is.null(server)) {
    state <- server$state
    serve_set(state, content, own)
    if (isTRUE(open)) open_url(state$url)
    return(invisible(server))
  }

  if (!interactive()) {
    warning(structure(class = c("aobcore_serve_noninteractive", "warning", "condition"), list(
      message = paste0("serve_scene() in a non-interactive session: the server answers ",
                       "only while R is idle, and stops when the session or script ends."),
      call = NULL)))
  }
  token <- random_hex(16L)
  state <- new.env(parent = emptyenv())
  state$token <- token
  state$running <- FALSE
  state$owned <- character()
  state$renderer <- renderer_js()
  serve_set(state, content, own)
  app <- list(call = function(req) serve_request(state, req))
  ## The token's draw already said if the source is the weaker one.
  candidates <- if (is.null(port)) random_ports(20L, quiet = TRUE) else check_port(port)
  handle <- NULL
  for (p in candidates) {
    handle <- keep_seed(tryCatch(httpuv::startServer("127.0.0.1", p, app), error = function(e) NULL))
    if (!is.null(handle)) break
  }
  if (is.null(handle)) {
    stop(if (is.null(port)) "Could not bind any of 20 random ports on 127.0.0.1." else
      paste0("Could not bind port ", port, " on 127.0.0.1."), call. = FALSE)
  }
  state$httpuv <- handle
  state$port <- as.integer(p)
  state$url <- paste0("http://127.0.0.1:", p, "/", token, "/")
  state$running <- TRUE
  srv <- structure(list(url = state$url, port = state$port, token = token,
                        stop = function() server_stop(state), state = state),
                   class = "aob_server")
  assign(token, srv, envir = servers)
  if (isTRUE(open)) open_url(state$url)
  invisible(srv)
}

#' @export
print.aob_server <- function(x, ...) {
  st <- x$state
  cat("<aob_server> ", x$url, "\n", sep = "")
  cat("  ", length(st$blobs), " blobs, ", length(st$files), " files, ",
      if (isTRUE(st$running)) "running" else "stopped", "\n", sep = "")
  invisible(x)
}

#' Servers started by serve_scene()
#'
#' `scene_servers()` lists the servers [serve_scene()] has started and not
#' yet stopped. `stop_scene_servers()` stops them all. All of them also stop
#' when the R session ends and when aobcore is unloaded.
#'
#' @return `scene_servers()`: a list of `"aob_server"` handles, named by URL.
#'   `stop_scene_servers()`: the number stopped, invisibly.
#' @export
#' @examplesIf interactive() && requireNamespace("httpuv", quietly = TRUE)
#' srv <- serve_scene(probe_scene(), open = FALSE)
#' names(scene_servers())
#' stop_scene_servers()
scene_servers <- function() {
  out <- mget(ls(servers, sorted = FALSE), envir = servers)
  out <- out[order(vapply(out, function(s) s$state$started, 0))]
  names(out) <- vapply(out, `[[`, "", "url")
  out
}

#' @rdname scene_servers
#' @export
stop_scene_servers <- function() {
  all <- mget(ls(servers), envir = servers)
  for (s in all) s$stop()
  invisible(length(all))
}

## ---- internals -------------------------------------------------------------

has_httpuv <- function() requireNamespace("httpuv", quietly = TRUE)

## Evaluate `expr` and put .Random.seed back as it was (absent stays
## absent): httpuv::startServer() draws from R's RNG.
keep_seed <- function(expr) {
  g <- globalenv()
  had <- exists(".Random.seed", envir = g, inherits = FALSE)
  if (had) seed <- get(".Random.seed", envir = g, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", seed, envir = g) else
    if (exists(".Random.seed", envir = g, inherits = FALSE)) rm(".Random.seed", envir = g))
  expr
}

## Running servers, by token. A handle's state is reachable from here, so a
## lost handle does not stop its server.
servers <- new.env(parent = emptyenv())

## The page, blobs and files one server answers, checked.
serve_content <- function(scene, blobs, files, title, theme) {
  check_blobs(blobs)
  check_files(files)
  dots <- intersect(names(blobs), c(".", ".."))
  if (length(dots)) {
    stop("A blob key cannot be \"", dots[1], "\": in a URL it is a dot segment, ",
         "which the browser resolves away.", call. = FALSE)
  }
  if (!is.list(scene)) stop("`scene` must be a list.", call. = FALSE)
  served <- scene
  routes <- list()
  for (id in names(files)) {
    rec <- files[[id]]
    ref <- scene$data[[id]]
    if (is.null(ref) || !identical(ref$format, "cog")) {
      stop("A registered file is keyed by data id `", id, "`, which is not a cog in ",
           "`scene$data`.", call. = FALSE)
    }
    check_file_unchanged(id, rec, stop)
    name <- basename(rec$path)
    routes[[id]] <- list(path = rec$path, size = rec$size, mtime = rec$mtime, name = name)
    if (!isTRUE(rec$url_explicit)) {
      served$data[[id]]$url <- paste0("files/", url_component(id), "/", url_component(name))
    }
  }
  used <- check_scene_shape(served, blobs)
  blobs <- blobs[intersect(names(blobs), used)]
  ## A cog with a relative url this server cannot answer: no file behind it
  ## and no embedded tiles.
  for (id in names(served$data)) {
    ref <- served$data[[id]]
    if (!identical(ref$format, "cog") || grepl("^[A-Za-z][A-Za-z0-9+.-]*:", ref$url)) next
    if (!is.null(routes[[id]]) && !isTRUE(files[[id]]$url_explicit)) next
    if (any(startsWith(names(blobs), paste0(id, "@")))) next
    stop("The cog data reference `", id, "` has the relative url \"", ref$url,
         "\", which the server cannot answer: no file is registered for it and its ",
         "tiles are not embedded. Add the layer with `embed = FALSE` (or `embed = TRUE`), ",
         "or give an absolute `url`.", call. = FALSE)
  }
  ## A blob's content type is that of the data reference naming it; tile
  ## bytes are named by none.
  types <- vapply(names(blobs), function(k) {
    fmt <- NULL
    for (ref in served$data) if (identical(ref$blob, k)) fmt <- ref$format
    switch(fmt %||% "", "arrow-ipc-file" = "application/vnd.apache.arrow.file",
           "arrow-ipc-stream" = "application/vnd.apache.arrow.stream",
           "application/octet-stream")
  }, "")
  page <- scene_page(served, blobs, title = title, theme = theme, mode = "linked")
  list(page = charToRaw(paste0("<!DOCTYPE html>\n", page, "\n")), blobs = blobs,
       types = types, files = routes)
}

serve_set <- function(state, content, own) {
  state$page <- content$page
  state$blobs <- content$blobs
  state$types <- content$types
  state$files <- content$files
  state$started <- state$started %||% as.numeric(Sys.time())
  if (length(own)) state$owned <- unique(c(state$owned, normalizePath(own, winslash = "/", mustWork = FALSE)))
  invisible(state)
}

check_files <- function(files) {
  if (!is.list(files) || (length(files) && (is.null(names(files)) || any(!nzchar(names(files)))))) {
    stop("`files` must be a named list of file records.", call. = FALSE)
  }
  for (id in names(files)) {
    rec <- files[[id]]
    if (!is.list(rec) || !is.character(rec$path) || length(rec$path) != 1L ||
        !is.numeric(rec$size) || !inherits(rec$mtime, "POSIXct")) {
      stop("The file record `", id, "` needs `path`, `size` and `mtime` ",
           "(see scene_add_tiled_raster(embed = FALSE)).", call. = FALSE)
    }
  }
  invisible()
}

## NULL when the file is as registered, else "gone" or "changed", after
## calling `signal` with a message (stop() when serving, warning() on a
## request).
check_file_unchanged <- function(id, rec, signal) {
  if (!file.exists(rec$path)) {
    signal("The registered file of `", id, "` (\"", rec$path, "\") is gone.", call. = FALSE)
    return("gone")
  }
  info <- file.info(rec$path, extra_cols = FALSE)
  if (!identical(info$size, rec$size) || !identical(info$mtime, rec$mtime)) {
    signal("The registered file of `", id, "` (\"", rec$path, "\") has changed since it ",
           "was registered, so its tile plan is out of date; add the layer again.",
           call. = FALSE)
    return("changed")
  }
  NULL
}

serve_request <- function(state, req) {
  method <- req$REQUEST_METHOD
  path <- req$PATH_INFO
  host <- req$HTTP_HOST %||% ""
  allowed <- c(paste0(c("127.0.0.1:", "localhost:"), state$port),
               as.character(getOption("aobcore.serve_hosts")))
  if (!host %in% allowed) {
    warning("aobcore server ", state$url, " refused a request with Host \"", host,
            "\". If an IDE proxy forwards to it, allow that host with ",
            "options(aobcore.serve_hosts = \"", host, "\").", call. = FALSE)
    return(serve_response(403L, "Forbidden"))
  }
  if (!method %in% c("GET", "HEAD")) {
    return(serve_response(405L, "Method not allowed", headers = list(Allow = "GET, HEAD")))
  }
  root <- paste0("/", state$token)
  if (identical(path, root)) {
    return(serve_response(301L, "", headers = list(Location = paste0(root, "/"))))
  }
  if (!startsWith(path, paste0(root, "/"))) return(not_found())
  rest <- substring(path, nchar(root) + 2L)
  if (rest %in% c("", "index.html")) {
    return(serve_response(200L, state$page, "text/html; charset=utf-8"))
  }
  if (identical(rest, "aob-renderer.min.js")) {
    return(serve_response(200L, state$renderer, "text/javascript; charset=utf-8"))
  }
  if (startsWith(rest, "blob/")) {
    seg <- substring(rest, 6L)
    if (!nzchar(seg) || grepl("/", seg, fixed = TRUE)) return(not_found())
    key <- utils::URLdecode(seg)
    if (!key %in% names(state$blobs)) return(not_found())
    return(serve_response(200L, state$blobs[[key]], state$types[[key]]))
  }
  if (startsWith(rest, "files/")) {
    parts <- strsplit(rest, "/", fixed = TRUE)[[1]]
    if (length(parts) != 3L || grepl("/$", rest)) return(not_found())
    id <- utils::URLdecode(parts[2])
    name <- utils::URLdecode(parts[3])
    if (!id %in% names(state$files)) return(not_found())
    f <- state$files[[id]]
    if (!identical(name, f$name)) return(not_found())
    return(serve_file(id, f, req$HTTP_RANGE))
  }
  not_found()
}

## One registered file, whole or one byte range.
serve_file <- function(id, f, range) {
  bad <- check_file_unchanged(id, f, warning)
  if (identical(bad, "gone")) return(not_found())
  if (identical(bad, "changed")) return(serve_response(409L, "Conflict: the file has changed"))
  size <- f$size
  type <- if (grepl("[.]tiff?$", f$name, ignore.case = TRUE)) "image/tiff" else "application/octet-stream"
  base <- list(`Accept-Ranges` = "bytes")
  if (is.null(range) || !nzchar(range) || !startsWith(range, "bytes=")) {
    return(list(status = 200L,
                headers = c(base, list(`Content-Type` = type, `Cache-Control` = "no-cache")),
                body = list(file = f$path)))
  }
  r <- parse_range(substring(range, 7L), size, getOption("aobcore.serve_range_max", 64 * 2^20))
  if (is.null(r)) {
    return(serve_response(416L, "Range not satisfiable",
                          headers = c(base, list(`Content-Range` = paste0("bytes */", size)))))
  }
  con <- file(f$path, open = "rb")
  on.exit(close(con))
  seek(con, r[1])
  body <- readBin(con, "raw", r[2] - r[1] + 1)
  serve_response(206L, body, type, headers = c(base, list(
    `Content-Range` = sprintf("bytes %.0f-%.0f/%.0f", r[1], r[2], size))))
}

## c(first, last) byte of a single range spec against a file of `size`
## bytes, or NULL when it cannot be answered with exactly those bytes:
## several ranges, a malformed one, one past the end, or one longer than
## `cap`.
parse_range <- function(spec, size, cap) {
  m <- regmatches(spec, regexec("^([0-9]*)-([0-9]*)$", spec))[[1]]
  if (length(m) != 3L || (!nzchar(m[2]) && !nzchar(m[3]))) return(NULL)
  if (nzchar(m[2])) {
    a <- as.numeric(m[2])
    b <- if (nzchar(m[3])) min(as.numeric(m[3]), size - 1) else size - 1
    if (nzchar(m[3]) && as.numeric(m[3]) < a) return(NULL)
  } else {
    n <- as.numeric(m[3])
    if (n == 0) return(NULL)
    a <- max(size - n, 0)
    b <- size - 1
  }
  if (a >= size || b < a || b - a + 1 > cap) return(NULL)
  c(a, b)
}

serve_response <- function(status, body, type = "text/plain; charset=utf-8", headers = list()) {
  list(status = status,
       headers = c(list(`Content-Type` = type, `Cache-Control` = "no-cache"), headers),
       body = body)
}

not_found <- function() serve_response(404L, "Not found")

server_stop <- function(state) {
  if (isTRUE(state$running)) {
    state$running <- FALSE
    keep_seed(try(httpuv::stopServer(state$httpuv), silent = TRUE))
  }
  if (exists(state$token, envir = servers, inherits = FALSE)) rm(list = state$token, envir = servers)
  if (length(state$owned)) {
    unlink(state$owned[file.exists(state$owned)])
    state$owned <- character()
  }
  invisible()
}

open_url <- function(url) {
  viewer <- getOption("viewer")
  if (is.function(viewer)) viewer(url) else utils::browseURL(url)
}

check_port <- function(port) {
  if (!is.numeric(port) || length(port) != 1L || is.na(port) || port != round(port) ||
      port < 1 || port > 65535) {
    stop("`port` must be a single port number, 1 to 65535.", call. = FALSE)
  }
  as.integer(port)
}

## encodeURIComponent(): every byte but A-Z a-z 0-9 - _ . ! ~ * ' ( )
## percent-encoded (UTF-8).
url_component <- function(x) {
  keep <- as.integer(charToRaw("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()"))
  vapply(x, function(s) {
    b <- as.integer(charToRaw(enc2utf8(s)))
    paste0(ifelse(b %in% keep, vapply(b, function(v) rawToChar(as.raw(v)), ""),
                  sprintf("%%%02X", b)), collapse = "")
  }, "", USE.NAMES = FALSE)
}

## ---- random bytes without R's RNG -----------------------------------------

## `n` random bytes from /dev/urandom, or from the weaker fallback where it
## does not exist (or when the internal option aobcore.serve_fallback_random
## is TRUE, for tests). Neither touches .Random.seed.
random_bytes <- function(n, quiet = FALSE) {
  if (!isTRUE(getOption("aobcore.serve_fallback_random")) && file.exists("/dev/urandom")) {
    con <- file("/dev/urandom", open = "rb", raw = TRUE)
    on.exit(close(con))
    b <- readBin(con, "raw", n)
    if (length(b) == n) return(b)
  }
  if (!quiet) message("aobcore: no /dev/urandom here, so the server token and port come from a ",
          "weaker source (the time, process id, a temporary file name and a memory ",
          "address). They guard against other web pages, much less against other ",
          "users of this computer.")
  fallback_bytes(n)
}

random_hex <- function(n) paste(format(as.hexmode(as.integer(random_bytes(n))), width = 2), collapse = "")

## Candidate ports in 20000 to 60000, from their own random bytes (not the
## token's), so a visible port says nothing about the token.
random_ports <- function(k, quiet = FALSE) {
  b <- as.integer(random_bytes(2L * k, quiet = quiet))
  20000L + as.integer((b[c(TRUE, FALSE)] * 256L + b[c(FALSE, TRUE)]) %% 40001L)
}

fallback_counter <- new.env(parent = emptyenv())

## Hashes what varies between calls and sessions: the time to the
## microsecond, the process id, proc.time(), a tempfile() name, the address
## of a fresh environment and a call counter. The hash is four 32-bit FNV-1a
## lanes over the serialized inputs, each lane seeded differently, extended
## by rehashing for more than 16 bytes. Not cryptographic.
fallback_bytes <- function(n) {
  fallback_counter$i <- (fallback_counter$i %||% 0) + 1
  input <- serialize(list(format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"), as.numeric(Sys.time()),
                          Sys.getpid(), proc.time(), tempfile(), format(new.env()),
                          fallback_counter$i), NULL)
  out <- raw()
  round <- 0L
  while (length(out) < n) {
    for (lane in 0:3) {
      h <- fnv1a(c(as.raw(c(lane, round)), input, out))
      out <- c(out, as.raw(c(h %/% 2^24, (h %/% 2^16) %% 256, (h %/% 2^8) %% 256, h %% 256)))
    }
    round <- round + 1L
  }
  out[seq_len(n)]
}

## 32-bit FNV-1a, exactly, in doubles: the multiply by the FNV prime
## (16777619 = 2^24 + 403) is split so no product passes 2^53.
fnv1a <- function(bytes) {
  h <- 2166136261
  for (b in as.integer(bytes)) {
    lo <- h %% 256
    h <- h - lo + bitwXor(as.integer(lo), b)
    h <- ((h * 403) %% 2^32 + (h %% 2^8) * 2^24) %% 2^32
  }
  h
}
