#' Selections from a served page: the websocket and protocol 1
#'
#' A server from [serve_scene()] also takes a websocket at
#' `http://127.0.0.1:<port>/<token>/ws`, through which the page sends the
#' viewer's selection and its settled view back to R (decision 0007 in
#' allboa/design). It needs the 'jsonlite' package; without it the page has
#' no socket and the functions below are errors.
#'
#' **What R gets.** The handle from [serve_scene()] has:
#'
#' - `selection()`: the latest selection, a data frame with columns `layer`
#'   (layer id) and `row` (1-based row of that layer's Arrow data, every
#'   record batch in stream order), with the attributes `at` (the pressed
#'   point, in view CRS units or `c(lon, lat)` on a globe, or `NULL`),
#'   `trigger`, `connection`, `seq`, `time` and `scene` (the scene serial it
#'   was made on). It has zero rows when nothing is selected.
#' - `view_state()`: the latest settled view, a list with `extent`
#'   (`c(xmin, xmax, ymin, ymax)`), `zoom`, `units_per_pixel` and `size_px`
#'   (or `center` and `zoom` on a globe), `connection`, `seq`, `time` and
#'   `scene`; `NULL` before the first.
#' - `wait(type = c("select", "view"), timeout = Inf)`: runs httpuv's event
#'   loop until a message of that type arrives after the call, then returns
#'   what `selection()` or `view_state()` returns; `NULL`, with a message,
#'   after `timeout` seconds. An interrupt ends it.
#' - `on(type, f)`: calls `f(message)` for each accepted message of that type
#'   (`"hello"`, `"select"` or `"view"`; `message` is the parsed JSON with
#'   `connection` added), and returns a function that removes it. `f` runs
#'   from the event loop whenever R is idle; an error in it becomes a
#'   warning. Inside `f`, `wait()` is an error, and `selection()`,
#'   `view_state()` and `connections()` read the state without running the
#'   loop.
#' - `connections()`: how many pages are connected.
#'
#' `selection()`, `view_state()` and `connections()` first run every
#' callback that is ready, without blocking (`later::run_now(0, all = TRUE)`
#' until none is left, at most 1000 rounds), so a selection made just
#' before the call is counted, as are all the messages queued while R was
#' busy. Inside an `on()` callback they skip that and read the state as it
#' is. All are errors once the server has stopped: the selection lives
#' in the server and goes with it. Selections from several pages (tabs) on
#' one server are the server's: the last message wins.
#'
#' **Protocol 1.** Each message is one JSON object in a text frame, with a
#' string `type`; fields not listed are ignored, and an unknown type is
#' ignored. The page sends `hello` (`protocol`, `renderer`, `specs`,
#' `scene`), then `select` (`scene`, `seq`, `trigger`, `items`: a list of
#' `layer` and `rows`, 0-based Arrow rows, and `at`) and `view` (`scene`,
#' `seq`, `extent`, `zoom`, `units_per_pixel`, `size_px`, or `center` and
#' `zoom`). A `select` is the page's whole selection. R answers `hello`
#' with `hello` (`protocol`, `connection`, `scene`, `spec`, `select`: the
#' layers the page may let the viewer select, `max_message`), and sends
#' `reload` (`scene`) when [serve_scene()] replaces the scene. A `select` or
#' `view` for an older scene serial, or with a `seq` not above the last
#' from that page, is dropped without a warning (the page is told to
#' reload); one with bad fields (an unknown or unselectable layer, a row out
#' of range or not a whole number) is dropped with a warning, once per page
#' and type. The scene serial is in the served page as
#' `data-aob-scene-serial`, and the socket's URL as `data-aob-socket`.
#'
#' **Checks.** An upgrade is accepted only for `GET /<token>/ws` (else 404),
#' with an allowed `Host` (as for every route, else 403) and an `Origin` of
#' `http://127.0.0.1:<port>`, `http://localhost:<port>`, or `http://H` or
#' `https://H` for a value `H` of `getOption("aobcore.serve_hosts")` (else
#' 403), compared without regard to case. A missing `Origin` is refused. An
#' `Origin` refusal warns in R, the value escaped and cut to 80 bytes, for
#' the first 5 distinct values per server, then once more to say further
#' refusals are not shown. A browser leaves a default port out of `Origin`,
#' so a proxy host listed as `host:443` should also be listed as `host`.
#' httpuv 1.6.17 switches protocols even after a refusal, so the same checks
#' run again, first, when the socket opens, which closes a refused socket at
#' once (1008) and keeps nothing of it. A client that hangs up during a
#' refused upgrade can make httpuv print `Warning in rm(list =
#' wsconn_address(handle), envir = private$wsconns) : object '...' not
#' found`; it comes from httpuv's own bookkeeping, is harmless and leaves no
#' state. A refused socket is closed by R at once, but httpuv keeps the
#' connection until the client hangs up.
#'
#' **Close codes.** 1000 normal; 1001 the server is stopping; 1003 a binary
#' frame; 1007 not UTF-8, not JSON, or not an object with a string `type`;
#' 1008 refused, or a message before `hello`; 1009 a message over
#' `getOption("aobcore.ws_max_message", 2^20)` bytes; 1011 an error in R;
#' 1013 more than 8 pages at once; 4000 a protocol other than 1. Each
#' closing but 1000, 1001 and a refusal warns in R; these warnings, a
#' spec mismatch and dropped messages are given for the first 5 per
#' server, then once more to say further ones are not shown. The 1013 cap
#' warns once per server. Eight sockets that open and never send `hello`
#' hold the cap until they close. httpuv ends a text frame at its first NUL byte, so R
#' never sees what follows one; the rest of the message is checked as
#' usual.
#'
#' A socket message only sets R's record of the selection and view; it
#' never evaluates anything, names a file or reaches the scene.
#'
#' @name serve_scene_socket
#' @seealso [serve_scene()].
#' @examplesIf interactive() && requireNamespace("httpuv", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE)
#' srv <- serve_scene(probe_scene())
#' ## Click a feature in the page, then:
#' sel <- srv$wait("select", timeout = 60)
#' srv$selection()
#' srv$stop()
NULL

## ---- internals -------------------------------------------------------------

ws_protocol <- 1L
ws_max_connections <- 8L
vector_kinds <- c("point", "path", "polygon")

has_jsonlite <- function() requireNamespace("jsonlite", quietly = TRUE)

## Package-wide flags: whether the missing-jsonlite note was given this
## session, and how deeply srv$on() callbacks are running (any server's: a
## nested httpuv::service() from inside one is what the guard prevents).
ws_flags <- new.env(parent = emptyenv())
ws_flags$told_jsonlite <- FALSE
ws_flags$depth <- 0L

tell_no_jsonlite <- function() {
  if (isTRUE(ws_flags$told_jsonlite)) return(invisible())
  ws_flags$told_jsonlite <- TRUE
  message("aobcore: the served page cannot send selections back to R without the ",
          "'jsonlite' package; install it with install.packages(\"jsonlite\").")
}

ws_max_message <- function() {
  v <- getOption("aobcore.ws_max_message", 2^20)
  if (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 1) {
    stop("getOption(\"aobcore.ws_max_message\") must be a positive number of bytes.", call. = FALSE)
  }
  floor(v)
}

## The layer ids a page may let the viewer select.
select_layers <- function(scene, select) {
  ids <- vapply(scene$layers, function(l) as.character(l$id %||% NA), "")
  kinds <- vapply(scene$layers, function(l) as.character(l$kind %||% ""), "")
  vec <- ids[kinds %in% vector_kinds & !is.na(ids)]
  if (is.null(select)) return(vec)
  if (!is.character(select) || anyNA(select)) {
    stop("`select` must be NULL or a character vector of layer ids.", call. = FALSE)
  }
  bad <- setdiff(select, vec)
  if (length(bad)) {
    stop("`select` names `", bad[1], "`, which is not a vector layer of the scene",
         if (length(vec)) paste0(" (", paste0("`", vec, "`", collapse = ", "), ")") else
           " (it has none)", ".", call. = FALSE)
  }
  unique(select)
}

## A warning about a page or a socket, from the server at `state`: the
## first 5 per server are given, then one saying further ones are not
## shown, so a page that misbehaves in a loop cannot flood the console.
## Origin refusals have their own record (warn_origin()).
ws_warn <- function(state, ...) {
  n <- state$ws_warnings %||% 0L
  state$ws_warnings <- n + 1L
  if (n < 5L) {
    warning("aobcore server ", state$url, " ", ..., call. = FALSE)
  } else if (n == 5L) {
    warning("aobcore server ", state$url, " has more websocket warnings; ",
            "further ones not shown.", call. = FALSE)
  }
  invisible()
}

ws_init <- function(state) {
  state$conns <- new.env(parent = emptyenv())
  state$next_conn <- 0L
  state$received <- c(hello = 0, select = 0, view = 0)
  state$handlers <- list()
  state$next_handler <- 0L
  state$warned_origins <- character()
  state$origins_capped <- FALSE
  state$warned_cap <- FALSE
  state$ws_warnings <- 0L
  state$view <- NULL
  invisible(state)
}

## A scene served, first or replacing: its serial, the selectable layers
## (with the data behind each, to bound rows), the spec version for hello;
## the selection is cleared and connected pages are told to reload.
ws_new_scene <- function(state, scene, select, serial, socket) {
  state$serial <- serial
  state$socket <- socket
  state$select <- select
  state$spec <- tryCatch(scene_spec_version(scene), error = function(e) NA_character_)
  src <- list()
  for (l in scene$layers) {
    if (!isTRUE(l$id %in% select)) next
    ref <- scene$data[[l$data %||% ""]]
    src[[l$id]] <- if (identical(ref$format, "arrow-ipc-stream") && is.character(ref$blob)) ref$blob
  }
  state$select_src <- src
  state$nrows <- new.env(parent = emptyenv())
  state$selection <- empty_selection(serial)
  for (conn in ws_conn_list(state)) {
    if (isTRUE(conn$hello)) ws_send(conn, list(type = "reload", scene = serial))
  }
  invisible(state)
}

empty_selection <- function(serial) {
  out <- data.frame(layer = character(), row = integer(), stringsAsFactors = FALSE)
  attr(out, "scene") <- serial
  out
}

ws_conn_list <- function(state) {
  if (!is.environment(state$conns)) return(list())
  ids <- ls(state$conns)
  out <- mget(ids, envir = state$conns)
  out[order(as.integer(ids))]
}

## NULL when an upgrade request may become a socket, else why not: a list
## with the status to answer and, for a Host or Origin refusal, the value.
## The path is checked first, so a wrong path is a silent 404 whatever its
## headers.
ws_refusal <- function(state, req) {
  if (!isTRUE(state$socket) || !isTRUE(state$running)) return(list(status = 404L, what = "path"))
  if (!identical(req$REQUEST_METHOD, "GET") ||
      !identical(req$PATH_INFO, paste0("/", state$token, "/ws"))) {
    return(list(status = 404L, what = "path"))
  }
  host <- req$HTTP_HOST %||% ""
  if (!host_allowed(state, host)) return(list(status = 403L, what = "host", value = host))
  origin <- req$HTTP_ORIGIN
  if (!origin_allowed(state, origin)) return(list(status = 403L, what = "origin", value = origin))
  NULL
}

origin_allowed <- function(state, origin) {
  if (!is.character(origin) || length(origin) != 1L || is.na(origin)) return(FALSE)
  hosts <- as.character(getOption("aobcore.serve_hosts"))
  allowed <- c(paste0("http://", c("127.0.0.1:", "localhost:"), state$port),
               if (length(hosts)) paste0(c("http://", "https://"), rep(hosts, each = 2L)))
  tolower(origin) %in% tolower(allowed)
}

warn_origin <- function(state, origin) {
  missing <- !is.character(origin) || length(origin) != 1L || is.na(origin)
  key <- if (missing) "\001missing" else origin
  if (key %in% state$warned_origins) return(invisible())
  if (length(state$warned_origins) >= 5L) {
    if (!isTRUE(state$origins_capped)) {
      state$origins_capped <- TRUE
      warning("aobcore server ", state$url, " refused another websocket for its Origin; ",
              "further refusals not shown.", call. = FALSE)
    }
    return(invisible())
  }
  state$warned_origins <- c(state$warned_origins, key)
  if (missing) {
    warning("aobcore server ", state$url, " refused a websocket with no Origin header.",
            call. = FALSE)
  } else {
    warning("aobcore server ", state$url, " refused a websocket from Origin ",
            safe_text(origin, 80L), ". Only if this is your IDE proxy's origin, allow its host ",
            "(without \"http://\" or \"https://\") in options(aobcore.serve_hosts = ...).",
            call. = FALSE)
  }
}

## onHeaders: an upgrade that fails a check is answered 404 or 403 (and the
## Host and Origin refusals warned about here, since httpuv may skip
## onWSOpen for a client that has hung up); anything else goes on.
ws_on_headers <- function(state, req) {
  if (!is_upgrade(req)) return(NULL)
  tryCatch({
    r <- ws_refusal(state, req)
    if (is.null(r)) return(NULL)
    if (identical(r$what, "host")) warn_host(state, r$value)
    if (identical(r$what, "origin")) warn_origin(state, r$value)
    if (r$status == 404L) not_found() else serve_response(403L, "Forbidden")
  }, error = function(e) {
    ws_warn(state, "refused a websocket after an error in R: ",
            safe_text(conditionMessage(e), 200L))
    serve_response(403L, "Forbidden")
  })
}

## onWSOpen: the checks again, first of all, since httpuv opens the socket
## even after onHeaders refused it. A refused socket is closed silently
## (onHeaders warned) before anything is registered or counted.
ws_on_open <- function(state, ws) {
  ok <- tryCatch(is.null(ws_refusal(state, ws$request)), error = function(e) FALSE)
  if (!ok) {
    ws_try_close(ws, 1008L, "refused")
    return(invisible())
  }
  tryCatch(ws_accept(state, ws), error = function(e) {
    ws_warn(state, "closed a websocket after an error in R: ",
            safe_text(conditionMessage(e), 200L))
    ws_try_close(ws, 1011L, "error in R")
  })
  invisible()
}

ws_try_close <- function(ws, code, reason) {
  tryCatch(ws$close(code, reason), error = function(e) NULL)
  invisible()
}

ws_accept <- function(state, ws) {
  if (length(state$conns) >= ws_max_connections) {
    ## Once per server: a page that retries would otherwise warn each time.
    if (!isTRUE(state$warned_cap)) {
      state$warned_cap <- TRUE
      warning("aobcore server ", state$url, " refused a page: at most ", ws_max_connections,
              " pages can be connected at once. Close a tab showing it. Further pages ",
              "refused for this are not reported.", call. = FALSE)
    }
    ws_try_close(ws, 1013L, "too many connections")
    return(invisible())
  }
  state$next_conn <- state$next_conn + 1L
  conn <- new.env(parent = emptyenv())
  conn$id <- state$next_conn
  conn$ws <- ws
  conn$hello <- FALSE
  conn$seq <- -Inf
  conn$warned <- character()
  conn$open <- TRUE
  ws$onMessage(function(binary, message) ws_on_message(state, conn, binary, message))
  ws$onClose(function() ws_forget(state, conn))
  assign(as.character(conn$id), conn, envir = state$conns)
  invisible()
}

ws_forget <- function(state, conn) {
  tryCatch({
    conn$open <- FALSE
    key <- as.character(conn$id)
    if (is.environment(state$conns) && exists(key, envir = state$conns, inherits = FALSE)) {
      rm(list = key, envir = state$conns)
    }
  }, error = function(e) NULL)
  invisible()
}

## Close one page's socket, forgetting it at once, with a warning when
## `why` is given (fixed text: never the page's bytes unescaped).
ws_close <- function(state, conn, code, reason, why = NULL) {
  ws_forget(state, conn)
  ws_try_close(conn$ws, code, reason)
  if (!is.null(why)) {
    ws_warn(state, "closed the socket of page ", conn$id, " (", code, "): ", why)
  }
  invisible()
}

ws_close_all <- function(state, code, reason) {
  for (conn in ws_conn_list(state)) ws_close(state, conn, code, reason)
  invisible()
}

## Forget every page without calling its socket: for the exit finalizer,
## when a socket's handle may already be gone.
ws_forget_all <- function(state) {
  for (conn in ws_conn_list(state)) ws_forget(state, conn)
  invisible()
}

ws_send <- function(conn, x) {
  tryCatch(conn$ws$send(json_value(x)), error = function(e) NULL)
  invisible()
}

ws_on_message <- function(state, conn, binary, message) {
  if (!isTRUE(conn$open)) return(invisible())
  tryCatch(ws_message(state, conn, binary, message), error = function(e) {
    ws_close(state, conn, 1011L, "error in R",
             paste0("an error in R: ", safe_text(conditionMessage(e), 200L)))
  })
  invisible()
}

ws_message <- function(state, conn, binary, message) {
  if (isTRUE(binary) || !is.character(message) || length(message) != 1L) {
    return(ws_close(state, conn, 1003L, "binary frames are not accepted",
                    "it sent a binary frame"))
  }
  cap <- ws_max_message()
  size <- nchar(message, type = "bytes", allowNA = TRUE)
  if (is.na(size) || size > cap) {
    return(ws_close(state, conn, 1009L, "message too large", paste0(
      "it sent a message of ", format(size, scientific = FALSE), " bytes, over the ",
      format(cap, scientific = FALSE), " of getOption(\"aobcore.ws_max_message\")")))
  }
  if (!validUTF8(message)) {
    return(ws_close(state, conn, 1007L, "not UTF-8", "it sent text that is not UTF-8"))
  }
  msg <- tryCatch(jsonlite::parse_json(message, simplifyVector = FALSE),
                  error = function(e) NULL)
  type <- if (is.list(msg) && !is.null(names(msg))) msg[["type"]]
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    return(ws_close(state, conn, 1007L, "not a message",
                    "it sent text that is not a JSON object with a string type"))
  }
  if (!isTRUE(conn$hello) && !identical(type, "hello")) {
    return(ws_close(state, conn, 1008L, "hello first", "it sent a message before hello"))
  }
  switch(type,
         hello = ws_hello(state, conn, msg),
         select = ws_select(state, conn, msg),
         view = ws_view(state, conn, msg),
         invisible())
}

## A whole number, as one JSON number.
is_whole <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)
}

## A JSON array of `n` finite numbers, as a double vector, or NULL.
num_vec <- function(x, n) {
  if (!is.list(x) || !is.null(names(x)) || length(x) != n) return(NULL)
  if (!all(vapply(x, function(v) is.numeric(v) && length(v) == 1L && is.finite(v), TRUE))) return(NULL)
  as.numeric(unlist(x))
}

ws_hello <- function(state, conn, msg) {
  p <- msg[["protocol"]]
  if (!(is.numeric(p) && length(p) == 1L && isTRUE(p == ws_protocol))) {
    return(ws_close(state, conn, 4000L, "protocol", paste0(
      "the page speaks another protocol than ", ws_protocol,
      ", so it needs a newer (or older) aobcore")))
  }
  first <- !isTRUE(conn$hello)
  conn$hello <- TRUE
  specs <- msg[["specs"]]
  if (first && is.list(specs) && length(specs) &&
      all(vapply(specs, function(s) is.character(s) && length(s) == 1L, TRUE)) &&
      !is.na(state$spec) && !state$spec %in% unlist(specs)) {
    shown <- vapply(utils::head(unlist(specs), 10L), safe_text, "", max = 20L)
    ws_warn(state, "page ", conn$id, "'s renderer draws scene spec ",
            paste(shown, collapse = ", "), ", but the scene is ", state$spec, ".")
  }
  ws_send(conn, list(type = "hello", protocol = ws_protocol, connection = conn$id,
                     scene = state$serial, spec = state$spec, select = as.list(state$select),
                     max_message = ws_max_message()))
  sc <- msg[["scene"]]
  if (is_whole(sc) && sc != state$serial) ws_send(conn, list(type = "reload", scene = state$serial))
  ws_accepted(state, conn, "hello", msg)
}

## A bad field: the message is dropped, with a warning once per page and
## type.
ws_drop <- function(state, conn, type, why) {
  if (!type %in% conn$warned) {
    conn$warned <- c(conn$warned, type)
    ws_warn(state, "dropped a ", type, " message from page ", conn$id,
            ": ", why, ". Further bad ", type, " messages from that page are dropped ",
            "without a warning.")
  }
  invisible()
}

## The scene serial and seq of a select or view: TRUE to go on. A message
## for another scene is dropped without a warning and the page told to
## reload; one whose seq is not above the page's last is stale.
ws_current <- function(state, conn, type, msg) {
  sc <- msg[["scene"]]
  seq <- msg[["seq"]]
  if (!is_whole(sc) || !is_whole(seq)) {
    ws_drop(state, conn, type, "its scene or seq is not a whole number")
    return(FALSE)
  }
  if (sc != state$serial) {
    ws_send(conn, list(type = "reload", scene = state$serial))
    return(FALSE)
  }
  if (seq <= conn$seq) return(FALSE)
  TRUE
}

## Rows in a selectable layer's Arrow data, counted once per scene; NA
## when the layer's data is not an Arrow stream blob this server holds.
ws_layer_rows <- function(state, layer) {
  if (exists(layer, envir = state$nrows, inherits = FALSE)) return(get(layer, envir = state$nrows))
  key <- state$select_src[[layer]]
  n <- NA_real_
  if (is.character(key) && key %in% names(state$blobs)) {
    n <- tryCatch({
      s <- nanoarrow::read_nanoarrow(state$blobs[[key]])
      on.exit(s$release())
      total <- 0
      while (!is.null(a <- s$get_next())) total <- total + a$length
      total
    }, error = function(e) NA_real_)
  }
  assign(layer, n, envir = state$nrows)
  n
}

ws_select <- function(state, conn, msg) {
  if (!ws_current(state, conn, "select", msg)) return(invisible())
  items <- msg[["items"]]
  if (!is.list(items) || !is.null(names(items))) {
    return(ws_drop(state, conn, "select", "its items are not a list"))
  }
  rows <- list()
  for (it in items) {
    layer <- if (is.list(it)) it[["layer"]]
    if (!is.character(layer) || length(layer) != 1L || is.na(layer)) {
      return(ws_drop(state, conn, "select", "an item has no layer id"))
    }
    if (!layer %in% state$select) {
      return(ws_drop(state, conn, "select", paste0("layer ", safe_text(layer, 80L),
                                                  " is not a selectable layer")))
    }
    r <- it[["rows"]]
    if (!is.list(r) || !is.null(names(r)) || !all(vapply(r, is_whole, TRUE))) {
      return(ws_drop(state, conn, "select", paste0("the rows of layer ", safe_text(layer, 80L),
                                                  " are not all whole numbers")))
    }
    r <- as.numeric(unlist(r))
    n <- ws_layer_rows(state, layer)
    if (any(r < 0) || (!is.na(n) && any(r >= n))) {
      return(ws_drop(state, conn, "select", paste0("a row of layer ", safe_text(layer, 80L),
                                                  " is out of range")))
    }
    rows[[layer]] <- c(rows[[layer]], r)
  }
  at <- msg[["at"]]
  if (!is.null(at)) {
    at <- num_vec(at, 2L)
    if (is.null(at)) return(ws_drop(state, conn, "select", "its at is not two numbers"))
  }
  trigger <- msg[["trigger"]]
  if (!is.null(trigger) && !(is.character(trigger) && length(trigger) == 1L && !is.na(trigger))) {
    return(ws_drop(state, conn, "select", "its trigger is not a string"))
  }
  conn$seq <- msg[["seq"]]
  ## intersect() with NULL (no items) is NULL, which would drop the layer
  ## column from an empty selection.
  layers <- intersect(state$select, names(rows) %||% character())
  rows <- lapply(rows[layers], function(r) sort(unique(r)) + 1)
  rr <- unlist(rows, use.names = FALSE) %||% numeric()
  if (all(rr <= .Machine$integer.max)) rr <- as.integer(rr)
  out <- data.frame(layer = rep(layers, lengths(rows)), row = rr, stringsAsFactors = FALSE)
  attr(out, "at") <- at
  attr(out, "trigger") <- trigger
  attr(out, "connection") <- conn$id
  attr(out, "seq") <- conn$seq
  attr(out, "time") <- Sys.time()
  attr(out, "scene") <- state$serial
  state$selection <- out
  ws_accepted(state, conn, "select", msg)
}

ws_view <- function(state, conn, msg) {
  if (!ws_current(state, conn, "view", msg)) return(invisible())
  out <- list()
  ext <- msg[["extent"]]
  ctr <- msg[["center"]]
  if (!is.null(ext)) {
    out$extent <- num_vec(ext, 4L)
    if (is.null(out$extent)) return(ws_drop(state, conn, "view", "its extent is not four numbers"))
  } else if (!is.null(ctr)) {
    out$center <- num_vec(ctr, 2L)
    if (is.null(out$center)) return(ws_drop(state, conn, "view", "its center is not two numbers"))
  } else {
    return(ws_drop(state, conn, "view", "it has neither extent nor center"))
  }
  for (f in c("zoom", "units_per_pixel")) {
    v <- msg[[f]]
    if (is.null(v)) next
    if (!(is.numeric(v) && length(v) == 1L && is.finite(v))) {
      return(ws_drop(state, conn, "view", paste0("its ", f, " is not a number")))
    }
    out[[f]] <- as.numeric(v)
  }
  if (!is.null(msg[["size_px"]])) {
    out$size_px <- num_vec(msg[["size_px"]], 2L)
    if (is.null(out$size_px)) return(ws_drop(state, conn, "view", "its size_px is not two numbers"))
  }
  conn$seq <- msg[["seq"]]
  out$connection <- conn$id
  out$seq <- conn$seq
  out$time <- Sys.time()
  out$scene <- state$serial
  state$view <- out
  ws_accepted(state, conn, "view", msg)
}

## Count an accepted message (for wait()) and run the on() callbacks for
## its type, with the guard set.
ws_accepted <- function(state, conn, type, msg) {
  state$received[[type]] <- state$received[[type]] + 1
  hs <- Filter(function(h) identical(h$type, type), state$handlers)
  if (!length(hs)) return(invisible())
  msg$connection <- conn$id
  old <- ws_flags$depth
  ws_flags$depth <- old + 1L
  on.exit(ws_flags$depth <- old)
  for (h in hs) {
    tryCatch(h$f(msg), error = function(e) {
      warning("aobcore server ", state$url, ": an on(\"", type, "\") callback failed: ",
              conditionMessage(e), call. = FALSE)
    })
  }
  invisible()
}

## ---- the handle's selection functions -------------------------------------

ws_usable <- function(state) {
  if (!isTRUE(state$running)) {
    stop("This server has stopped, and its selection and view went with it. ",
         "Serve the scene again with serve_scene().", call. = FALSE)
  }
  if (!isTRUE(state$socket)) {
    stop("This server takes no websocket, so its page cannot send selections back to R: ",
         "that needs the 'jsonlite' package. Install it and serve the scene again.",
         call. = FALSE)
  }
  invisible()
}

## Run every callback that is due, without blocking, unless an on()
## callback is running (the loop is then already running it), so that all
## the messages queued while R was busy are counted, not only the first.
## httpuv::service() will not do: service(NA) runs one callback, and
## service(0) runs the loop until something pauses it, which from here
## would never return. The loop is later's, which httpuv uses; the cap
## keeps a page that sends without pause from holding R here.
ws_service0 <- function() {
  if (ws_flags$depth > 0L) return(invisible())
  for (i in seq_len(1000L)) if (!later::run_now(0, all = TRUE)) break
  invisible()
}

ws_selection <- function(state) {
  ws_usable(state)
  ws_service0()
  ws_usable(state)
  state$selection
}

ws_view_state <- function(state) {
  ws_usable(state)
  ws_service0()
  ws_usable(state)
  state$view
}

ws_connections <- function(state) {
  if (!isTRUE(state$running) || !isTRUE(state$socket)) return(0L)
  ws_service0()
  length(state$conns)
}

ws_wait <- function(state, type = c("select", "view"), timeout = Inf) {
  type <- match.arg(type)
  if (ws_flags$depth > 0L) {
    stop("srv$wait() cannot be called from inside an srv$on() callback, which httpuv's ",
         "event loop is already running; read srv$selection() there instead.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout < 0) {
    stop("`timeout` must be a number of seconds (Inf to wait for ever).", call. = FALSE)
  }
  ws_usable(state)
  start <- state$received[[type]]
  t0 <- Sys.time()
  repeat {
    if (!isTRUE(state$running)) {
      stop("The server stopped while waiting for a ", type, " message.", call. = FALSE)
    }
    if (state$received[[type]] > start) {
      ## Take in whatever else was queued with it, so the answer is the
      ## latest, as selection() would give.
      ws_service0()
      return(if (type == "select") state$selection else state$view)
    }
    left <- timeout - as.numeric(Sys.time() - t0, units = "secs")
    if (left <= 0) {
      message("No ", type, " message from the page within ", format(timeout), " seconds.")
      return(NULL)
    }
    ## Never 0, which would run the loop for ever (see ws_service0()).
    httpuv::service(max(1, min(100, ceiling(left * 1000))))
  }
}

ws_on <- function(state, type, f) {
  if (!is.character(type) || length(type) != 1L || !type %in% c("hello", "select", "view")) {
    stop("`type` must be \"hello\", \"select\" or \"view\".", call. = FALSE)
  }
  if (!is.function(f)) stop("`f` must be a function of one argument, the message.", call. = FALSE)
  ws_usable(state)
  state$next_handler <- state$next_handler + 1L
  id <- state$next_handler
  state$handlers[[length(state$handlers) + 1L]] <- list(id = id, type = type, f = f)
  invisible(function() {
    state$handlers <- Filter(function(h) h$id != id, state$handlers)
    invisible()
  })
}
