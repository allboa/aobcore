# The websocket of serve_scene() and protocol 1 (decision 0007, aobcore #39
# and #40). Most tests drive the app's onHeaders and onWSOpen with a fake
# `ws` (helper-ws.R); the last checks real upgrades against a server in a
# child R process.

## ---- the route and its checks (#39) ----------------------------------------

test_that("the socket's page carries its serial and socket URL; embedded pages neither", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  page <- rawToChar(http_req(srv$port, paste0("/", srv$token, "/"))$body)
  expect_match(page, "data-aob-scene-serial=\"1\"", fixed = TRUE)
  expect_match(page, "data-aob-socket=\"ws\"", fixed = TRUE)
  ## /<token>/ws without an upgrade is no route.
  expect_identical(http_req(srv$port, paste0("/", srv$token, "/ws"))$status, 404L)
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  write_scene_html(probe_scene(), file = f)
  html <- paste(readLines(f), collapse = "\n")
  expect_false(grepl("data-aob-socket", html, fixed = TRUE))
  expect_false(grepl("data-aob-scene-serial", html, fixed = TRUE))
})

test_that("an allowed upgrade is accepted and counted", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  for (origin in c(paste0("http://127.0.0.1:", srv$port), paste0("http://localhost:", srv$port),
                   paste0("HTTP://LocalHost:", srv$port))) {
    ws <- ws_open(srv, origin = origin)
    expect_null(ws$headers_response, label = origin)
    expect_null(ws$closed, label = origin)
    expect_true(is.function(ws$on_message))
  }
  expect_identical(srv$connections(), 3L)
  ## Not a websocket upgrade: left to the HTTP routes.
  expect_null(ws_on_headers(srv$state, list(REQUEST_METHOD = "GET", PATH_INFO = "/")))
})

test_that("each refusal answers in onHeaders and closes first in onWSOpen", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  t <- srv$token
  wrong <- paste(rev(strsplit(t, "")[[1]]), collapse = "")
  cases <- list(
    list(args = list(path = paste0("/", wrong, "/ws")), status = 404L, warn = NA),
    list(args = list(path = paste0("/", t, "/ws/")), status = 404L, warn = NA),
    list(args = list(path = paste0("/", t, "/")), status = 404L, warn = NA),
    list(args = list(path = paste0("/", t, "/ws/x")), status = 404L, warn = NA),
    list(args = list(path = "/ws"), status = 404L, warn = NA),
    list(args = list(method = "POST"), status = 404L, warn = NA),
    ## A wrong path is silent whatever its headers.
    list(args = list(path = paste0("/", wrong, "/ws"), host = "evil.example", origin = "http://evil.example"),
         status = 404L, warn = NA),
    list(args = list(host = "evil.example:80"), status = 403L, warn = "Host \"evil.example:80\""),
    list(args = list(origin = "http://evil.example"), status = 403L, warn = "Origin \"http://evil.example\""),
    list(args = list(origin = paste0("https://127.0.0.1:", srv$port)), status = 403L, warn = "Origin"),
    list(args = list(origin = paste0("http://127.0.0.1:", srv$port + 1L)), status = 403L, warn = "Origin"),
    list(args = list(origin = "null"), status = 403L, warn = "Origin \"null\""),
    list(args = list(origin = NULL), status = 403L, warn = "no Origin header")
  )
  for (k in cases) {
    label <- paste(deparse(k$args), collapse = "")
    req <- do.call(ws_request, c(list(srv), k$args))
    if (is.na(k$warn)) {
      expect_no_warning(res <- ws_on_headers(srv$state, req))
    } else {
      expect_warning(res <- ws_on_headers(srv$state, req), k$warn, fixed = TRUE)
    }
    expect_identical(res$status, k$status, label = label)
    expect_identical(res$headers$`X-Content-Type-Options`, "nosniff")
    ## httpuv opens the socket anyway; onWSOpen closes it, silently, before
    ## anything is registered or counted.
    ws <- fake_ws(req)
    expect_no_warning(ws_on_open(srv$state, ws))
    expect_identical(ws$closed$code, 1008L, label = label)
    expect_null(ws$on_message, label = label)
    expect_null(ws$on_close, label = label)
    expect_length(ws$sent, 0L)
  }
  ## A refused socket leaves no connection or number behind.
  expect_identical(srv$connections(), 0L)
  expect_identical(srv$state$next_conn, 0L)
  ws <- ws_open(srv)
  expect_null(ws$closed)
  ws_hello(ws)
  expect_identical(ws_sent(ws)[[1]]$connection, 1L)
})

test_that("serve_hosts values are allowed as http and https origins", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  old <- options(aobcore.serve_hosts = c("proxy.example", "ide.example:443"))
  on.exit(options(old), add = TRUE)
  for (o in c("http://proxy.example", "https://proxy.example", "HTTPS://Proxy.Example",
              "https://ide.example:443")) {
    ws <- ws_open(srv, host = "proxy.example", origin = o)
    expect_null(ws$headers_response, label = o)
    expect_null(ws$closed, label = o)
  }
  ## A listed host:443 does not match the Origin a browser sends for it.
  expect_warning(ws <- ws_open(srv, host = "proxy.example", origin = "https://ide.example"), "Origin")
  expect_identical(ws$headers_response$status, 403L)
  expect_identical(ws$closed$code, 1008L)
  expect_warning(ws <- ws_open(srv, host = "proxy.example", origin = "https://proxy.example.evil"), "Origin")
  expect_identical(ws$closed$code, 1008L)
})

test_that("Origin refusals are escaped, cut short and capped at five values", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  evil <- paste0("http://\033]0;pwned\007\033[2Jx", strrep("y", 200))
  w <- character()
  collect <- function(expr) withCallingHandlers(expr, warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  collect(ws_on_headers(srv$state, ws_request(srv, origin = evil)))
  expect_length(w, 1L)
  expect_false(grepl("[\001-\037\177]", w))
  expect_match(w, "\\x1b]0;pwned\\x07\\x1b[2Jxyyy", fixed = TRUE)
  expect_match(w, "yyy...\"", fixed = TRUE)
  expect_lt(nchar(w), 600)
  expect_match(w, "aobcore.serve_hosts", fixed = TRUE)
  expect_match(w, "Only if this is your IDE proxy's origin", fixed = TRUE)
  ## Once per value.
  collect(ws_on_headers(srv$state, ws_request(srv, origin = evil)))
  expect_length(w, 1L)
  ## Five distinct values, then one note, then nothing.
  for (i in 1:8) collect(ws_on_headers(srv$state, ws_request(srv, origin = paste0("http://o", i, ".example"))))
  expect_length(w, 6L)
  expect_match(w[5], "o4.example", fixed = TRUE)
  expect_match(w[6], "further refusals not shown", fixed = TRUE)
  ## Host refusals are still warned about (they have their own record).
  expect_warning(ws_on_headers(srv$state, ws_request(srv, host = "h.example")), "Host \"h.example\"")
})

test_that("binary, malformed and oversized messages close with their codes", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  close_with <- function(x, code, warn, binary = FALSE, hello = TRUE) {
    srv$state$ws_warnings <- 0L # each case warns; the per-server cap is tested below
    ws <- ws_open(srv)
    if (hello) ws_hello(ws)
    expect_warning(ws_say(ws, x, binary = binary), warn)
    expect_identical(ws$closed$code, code, label = paste(warn, code))
    ws
  }
  close_with(as.raw(1:4), 1003L, "binary frame", binary = TRUE)
  for (x in c("not json", "{\"type\":", "[1, 2]", "{}", "{\"type\": 1}", "{\"type\": null}",
              "\"select\"", "null", "{\"types\": \"select\"}")) {
    ws <- close_with(x, 1007L, "not a JSON object with a string type")
    expect_false(identical(ws$closed$code, 1011L))
  }
  bad <- rawToChar(as.raw(c(0x7b, 0x22, 0x74, 0x22, 0x3a, 0x22, 0xff, 0xfe, 0x22, 0x7d)))
  close_with(bad, 1007L, "not UTF-8")
  close_with(bad, 1007L, "not UTF-8", hello = FALSE)
  ## A message before hello.
  close_with(list(type = "select", scene = 1L, seq = 1L, items = list()), 1008L, "before hello",
             hello = FALSE)
  close_with(list(type = "future"), 1008L, "before hello", hello = FALSE)
  ## Another protocol.
  close_with(list(type = "hello", protocol = 2L), 4000L, "another protocol", hello = FALSE)
  close_with(list(type = "hello"), 4000L, "another protocol", hello = FALSE)
  ## Too large: the cap is an option.
  old <- options(aobcore.ws_max_message = 100)
  on.exit(options(old), add = TRUE)
  close_with(paste0("{\"type\":\"view\",\"pad\":\"", strrep("x", 200), "\"}"), 1009L,
             "over the 100 of getOption")
  ## Every closed page is forgotten.
  expect_identical(srv$connections(), 0L)
})

test_that("an error in R closes with 1011 and a warning, not through httpuv", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  ws <- ws_open(srv)
  ws_hello(ws)
  local_mocked_bindings(ws_select = function(...) stop("boom"))
  expect_warning(ws_say(ws, list(type = "select", scene = 1L, seq = 1L, items = list())),
                 "an error in R: \"boom\"")
  expect_identical(ws$closed$code, 1011L)
})

test_that("at most eight pages connect at once", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  socks <- lapply(1:8, function(i) ws_open(srv))
  expect_identical(srv$connections(), 8L)
  expect_warning(ninth <- ws_open(srv), "at most 8 pages")
  expect_identical(ninth$closed$code, 1013L)
  expect_null(ninth$on_message)
  ## Warned once per server, even when pages come and go.
  expect_no_warning(tenth <- ws_open(srv))
  expect_identical(tenth$closed$code, 1013L)
  socks[[3]]$on_close()
  expect_identical(srv$connections(), 7L)
  again <- ws_open(srv)
  expect_null(again$closed)
  expect_identical(srv$connections(), 8L)
  for (i in 1:30) {
    expect_no_warning(extra <- ws_open(srv))
    expect_identical(extra$closed$code, 1013L)
    again$on_close()
    again <- ws_open(srv)
  }
})

test_that("warnings about pages are capped per server", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  w <- character()
  for (i in 1:20) {
    ws <- ws_open(srv)
    withCallingHandlers(ws_say(ws, "not json"), warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    })
    expect_identical(ws$closed$code, 1007L)
  }
  expect_length(w, 6L)
  expect_match(w[1:5], "closed the socket of page")
  expect_match(w[6], "further ones not shown", fixed = TRUE)
  ## Dropped messages and spec mismatches count toward the same cap.
  ws <- ws_open(srv)
  expect_no_warning(ws_hello(ws, specs = list("9.9")))
  expect_no_warning(ws_say(ws, list(type = "select", scene = 1L, seq = 1L,
                                    items = list(list(layer = "nope", rows = list())))))
  ## Another server has its own.
  srv2 <- serve_test(probe_scene())
  on.exit(srv2$stop(), add = TRUE)
  expect_warning(ws_say(ws_open(srv2), "not json"), "closed the socket")
})

test_that("reads take in every queued message, not only the first", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  ws <- ws_open(srv)
  ws_hello(ws)
  queue <- function(from) {
    for (i in from + 0:2) local({
      k <- i
      later::later(function() {
        ws_say(ws, list(type = "select", scene = 1L, seq = k, items = list(list(layer = "land", rows = list(k)))))
      }, 0)
    })
  }
  queue(1L)
  expect_identical(srv$selection()$row, 4L)
  queue(4L)
  expect_identical(srv$connections(), 1L)
  expect_identical(srv$selection()$row, 7L)
  queue(7L)
  later::later(function() {
    ws_say(ws, list(type = "view", scene = 1L, seq = 10L, extent = list(0, 1, 0, 1)))
  }, 0)
  expect_identical(srv$view_state()$seq, 10L)
  expect_identical(srv$selection()$row, 10L)
  ## wait() returns the latest of what arrived together.
  queue(11L)
  expect_identical(srv$wait(timeout = 5)$row, 14L)
})

test_that("stop() closes every socket with 1001", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  socks <- lapply(1:3, function(i) ws_open(srv))
  ws_hello(socks[[1]])
  srv$stop()
  for (ws in socks) expect_identical(ws$closed$code, 1001L)
  expect_identical(srv$connections(), 0L)
  expect_error(srv$selection(), "has stopped")
  expect_error(srv$view_state(), "has stopped")
  expect_error(srv$wait(timeout = 0), "has stopped")
  expect_error(srv$on("select", identity), "has stopped")
})

## ---- protocol 1 and the selection API (#40) --------------------------------

test_that("hello is answered with the connection, serial, spec, layers and cap", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  ws1 <- ws_open(srv)
  ws2 <- ws_open(srv)
  ws_hello(ws2)
  ws_hello(ws1)
  h <- ws_sent(ws2)[[1]]
  expect_identical(h$type, "hello")
  expect_identical(h$protocol, 1L)
  expect_identical(h$connection, 2L)
  expect_identical(h$scene, 1L)
  expect_identical(h$spec, "0.1")
  expect_identical(h$select, c("land", "graticule", "coast"))
  expect_identical(h$max_message, 1048576L)
  expect_identical(ws_sent(ws1)[[1]]$connection, 1L)
  ## No raster layer is selectable, and the JSON is ASCII.
  expect_false(grepl("sst", ws2$sent[1], fixed = TRUE))
  ## A renderer that does not draw the scene's spec version is named.
  ws3 <- ws_open(srv)
  expect_warning(ws_hello(ws3, specs = list("0.2", "9.9")), "draws scene spec \"0.2\", \"9.9\", but the scene is 0.1")
  ## A hello from an older scene is told to reload.
  ws4 <- ws_open(srv)
  ws_hello(ws4, scene = 0L)
  expect_identical(ws_sent(ws4)[[2]], list(type = "reload", scene = 1L))
  ## An unknown type after hello is ignored.
  ws_say(ws1, list(type = "future", x = 1))
  expect_null(ws1$closed)
  expect_length(ws1$sent, 1L)
})

test_that("serve_scene(select =) chooses the selectable layers", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene(), select = "coast")
  on.exit(srv$stop())
  ws <- ws_open(srv)
  ws_hello(ws)
  expect_identical(ws_sent(ws)[[1]]$select, "coast")
  srv2 <- serve_test(probe_scene(), select = character())
  on.exit(srv2$stop(), add = TRUE)
  ws <- ws_open(srv2)
  ws_hello(ws)
  expect_match(ws$sent[1], "\"select\":[]", fixed = TRUE)
  expect_error(serve_test(probe_scene(), select = "sst"), "`sst`, which is not a vector layer")
  expect_error(serve_test(probe_scene(), select = "nope"), "`nope`, which is not a vector layer")
  expect_error(serve_test(probe_scene(), select = NA_character_), "character vector of layer ids")
  expect_error(serve_test(probe_scene(), select = 1), "character vector of layer ids")
})

test_that("a selection arrives as 1-based Arrow rows with its attributes", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  expect_identical(nrow(srv$selection()), 0L)
  expect_null(srv$view_state())
  ws <- ws_open(srv)
  ws_hello(ws)
  ws_say(ws, list(type = "select", scene = 1L, seq = 12L, trigger = "click",
                  items = list(list(layer = "coast", rows = list(17L, 3L)),
                               list(layer = "land", rows = list(0L))),
                  at = list(1520345.2, -1834001.7)))
  sel <- srv$selection()
  expect_identical(sel$layer, c("land", "coast", "coast"))
  expect_identical(sel$row, c(1L, 4L, 18L))
  expect_identical(attr(sel, "at"), c(1520345.2, -1834001.7))
  expect_identical(attr(sel, "trigger"), "click")
  expect_identical(attr(sel, "connection"), 1L)
  expect_identical(attr(sel, "seq"), 12L)
  expect_identical(attr(sel, "scene"), 1L)
  expect_s3_class(attr(sel, "time"), "POSIXct")
  ## A stale seq is dropped silently.
  expect_no_warning(ws_say(ws, list(type = "select", scene = 1L, seq = 12L, trigger = "clear",
                                    items = list())))
  expect_identical(nrow(srv$selection()), 3L)
  ## An empty selection is no selection; at may be absent.
  ws_say(ws, list(type = "select", scene = 1L, seq = 13L, trigger = "clear", items = list()))
  sel <- srv$selection()
  expect_identical(nrow(sel), 0L)
  expect_null(attr(sel, "at"))
  expect_identical(attr(sel, "trigger"), "clear")
  ## Last message wins across pages.
  ws2 <- ws_open(srv)
  ws_hello(ws2)
  ws_say(ws2, list(type = "select", scene = 1L, seq = 1L, trigger = "toggle",
                   items = list(list(layer = "graticule", rows = list(16L)))))
  sel <- srv$selection()
  expect_identical(sel$row, 17L)
  expect_identical(attr(sel, "connection"), 2L)
})

test_that("bad fields drop the message with one warning per page and type", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene(), select = c("land", "coast"))
  on.exit(srv$stop())
  sel <- function(items, seq, ...) list(type = "select", scene = 1L, seq = seq, items = items, ...)
  cases <- list(
    list(sel(list(list(layer = "sst", rows = list(0L))), 1L), "\"sst\" is not a selectable layer"),
    list(sel(list(list(layer = "graticule", rows = list(0L))), 1L), "\"graticule\" is not a selectable"),
    list(sel(list(list(layer = "coast", rows = list(170L))), 1L), "out of range"),
    list(sel(list(list(layer = "coast", rows = list(-1L))), 1L), "out of range"),
    list(sel(list(list(layer = "coast", rows = list(1.5))), 1L), "not all whole numbers"),
    list(sel(list(list(layer = "coast", rows = list("1"))), 1L), "not all whole numbers"),
    list(sel(list(list(rows = list(1L))), 1L), "no layer id"),
    list(sel(list(layer = "coast"), 1L), "items are not a list"),
    list(sel(list(), 1L, at = list(1)), "at is not two numbers"),
    list(sel(list(), 1L, trigger = 3L), "trigger is not a string"),
    list(list(type = "select", scene = "1", seq = 1L, items = list()), "scene or seq")
  )
  for (k in cases) {
    srv$state$ws_warnings <- 0L # the per-server cap is tested below
    ws <- ws_open(srv)
    ws_hello(ws)
    expect_warning(ws_say(ws, k[[1]]), k[[2]], fixed = TRUE)
    expect_null(ws$closed)
    expect_identical(nrow(srv$selection()), 0L)
    ## The next bad select from that page is dropped without a warning, and
    ## a good one is taken.
    expect_no_warning(ws_say(ws, k[[1]]))
    ws_say(ws, sel(list(list(layer = "coast", rows = list(169L))), 5L))
    expect_identical(srv$selection()$row, 170L)
    ws_say(ws, sel(list(), 6L))
    ws$on_close()
  }
  ## The warning is per type: a bad view still warns once.
  srv$state$ws_warnings <- 0L
  ws <- ws_open(srv)
  ws_hello(ws)
  expect_warning(ws_say(ws, sel(list(list(layer = "x", rows = list())), 1L)), "dropped a select")
  expect_warning(ws_say(ws, list(type = "view", scene = 1L, seq = 2L, zoom = 1)), "dropped a view")
  expect_no_warning(ws_say(ws, list(type = "view", scene = 1L, seq = 3L, extent = list(1, 2))))
})

test_that("views are kept, orthographic and globe", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  ws <- ws_open(srv)
  ws_hello(ws)
  ws_say(ws, list(type = "view", scene = 1L, seq = 13L, extent = list(-2.1e6, 2.3e6, -1.9e6, 1.6e6),
                  zoom = -12.4, units_per_pixel = 3810.2, size_px = list(1152L, 720L), extra = "x"))
  v <- srv$view_state()
  expect_identical(v$extent, c(-2.1e6, 2.3e6, -1.9e6, 1.6e6))
  expect_identical(v$zoom, -12.4)
  expect_identical(v$units_per_pixel, 3810.2)
  expect_identical(v$size_px, c(1152, 720))
  expect_identical(v$connection, 1L)
  expect_identical(v$seq, 13L)
  expect_identical(v$scene, 1L)
  expect_null(v$extra)
  ws_say(ws, list(type = "view", scene = 1L, seq = 14L, center = list(0, -90), zoom = 2))
  v <- srv$view_state()
  expect_identical(v$center, c(0, -90))
  expect_null(v$extent)
  ## A stale seq is dropped.
  ws_say(ws, list(type = "view", scene = 1L, seq = 14L, center = list(10, 10), zoom = 2))
  expect_identical(srv$view_state()$center, c(0, -90))
})

test_that("replacing the scene gives a new serial, clears the selection and reloads pages", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  ws <- ws_open(srv)
  ws_hello(ws)
  quiet <- ws_open(srv) # no hello yet: not told
  ws_say(ws, list(type = "select", scene = 1L, seq = 1L, items = list(list(layer = "coast", rows = list(0L)))))
  ws_say(ws, list(type = "view", scene = 1L, seq = 2L, extent = list(0, 1, 0, 1), zoom = 0))
  expect_identical(nrow(srv$selection()), 1L)
  x <- wk::wkt(c("POINT (0 0)", "POINT (1 1)"), crs = "EPSG:3031")
  serve_test(scene_add_vector(scene(), "pt", x), server = srv)
  expect_identical(ws_sent(ws)[[2]], list(type = "reload", scene = 2L))
  expect_length(quiet$sent, 0L)
  sel <- srv$selection()
  expect_identical(nrow(sel), 0L)
  expect_identical(attr(sel, "scene"), 2L)
  page <- rawToChar(http_req(srv$port, paste0("/", srv$token, "/"))$body)
  expect_match(page, "data-aob-scene-serial=\"2\"", fixed = TRUE)
  ## Messages for the old serial are dropped without a warning, and the
  ## page told to reload again.
  expect_no_warning(ws_say(ws, list(type = "select", scene = 1L, seq = 3L,
                                    items = list(list(layer = "coast", rows = list(0L))))))
  expect_identical(nrow(srv$selection()), 0L)
  expect_identical(ws_sent(ws)[[3]], list(type = "reload", scene = 2L))
  expect_no_warning(ws_say(ws, list(type = "view", scene = 1L, seq = 4L, extent = list(9, 9, 9, 9))))
  expect_identical(srv$view_state()$extent, c(0, 1, 0, 1))
  ## The new scene's layers and rows apply.
  ws_say(ws, list(type = "select", scene = 2L, seq = 5L, items = list(list(layer = "pt", rows = list(1L)))))
  expect_identical(srv$selection()$row, 2L)
  expect_warning(ws_say(ws, list(type = "select", scene = 2L, seq = 6L,
                                 items = list(list(layer = "pt", rows = list(2L))))), "out of range")
  expect_identical(ws_sent(quiet), list())
  ws_hello(quiet, scene = 1L)
  expect_identical(ws_sent(quiet)[[2]], list(type = "reload", scene = 2L))
})

test_that("wait() returns the next message, or NULL with a message at the timeout", {
  skip_if_no_ws()
  skip_if_not_installed("later")
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  ws <- ws_open(srv)
  ws_hello(ws)
  ## A message already received is not "next".
  ws_say(ws, list(type = "select", scene = 1L, seq = 1L, items = list(list(layer = "land", rows = list(0L)))))
  later::later(function() {
    ws_say(ws, list(type = "select", scene = 1L, seq = 2L, items = list(list(layer = "land", rows = list(5L)))))
  }, 0.2)
  t0 <- Sys.time()
  sel <- srv$wait()
  expect_identical(sel$row, 6L)
  expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 5)
  later::later(function() {
    ws_say(ws, list(type = "view", scene = 1L, seq = 3L, extent = list(0, 1, 0, 1), zoom = 1))
  }, 0.1)
  expect_identical(srv$wait("view", timeout = 10)$extent, c(0, 1, 0, 1))
  ## A view does not end a wait for a select.
  later::later(function() {
    ws_say(ws, list(type = "view", scene = 1L, seq = 4L, extent = list(0, 2, 0, 2), zoom = 1))
  }, 0.05)
  expect_message(res <- srv$wait("select", timeout = 0.4), "No select message from the page within 0.4 seconds")
  expect_null(res)
  expect_identical(srv$view_state()$extent, c(0, 2, 0, 2))
  expect_error(srv$wait("nope"), "should be one of")
  expect_error(srv$wait(timeout = -1), "number of seconds")
})

test_that("on() calls back, can be removed, and guards the loop", {
  skip_if_no_ws()
  srv <- serve_test(probe_scene())
  on.exit(srv$stop())
  got <- list()
  off <- srv$on("select", function(m) got[[length(got) + 1L]] <<- m)
  ws <- ws_open(srv)
  ws_hello(ws)
  ws_say(ws, list(type = "select", scene = 1L, seq = 1L, trigger = "click",
                  items = list(list(layer = "land", rows = list(2L)))))
  expect_length(got, 1L)
  expect_identical(got[[1]]$trigger, "click")
  expect_identical(got[[1]]$connection, 1L)
  ## Dropped messages call nothing.
  ws_say(ws, list(type = "select", scene = 1L, seq = 1L, items = list()))
  expect_length(got, 1L)
  off()
  ws_say(ws, list(type = "select", scene = 1L, seq = 2L, items = list()))
  expect_length(got, 1L)
  ## An error in a callback is a warning, and the page stays.
  srv$on("view", function(m) stop("callback broke"))
  expect_warning(ws_say(ws, list(type = "view", scene = 1L, seq = 3L, extent = list(0, 1, 0, 1))),
                 "on\\(\"view\"\\) callback failed: callback broke")
  expect_null(ws$closed)
  ## Inside a callback wait() is an error; selection() reads without
  ## running the loop.
  inner <- NULL
  srv$on("select", function(m) {
    inner <<- list(sel = srv$selection(), n = srv$connections(),
                   wait = tryCatch(srv$wait(timeout = 1), error = conditionMessage))
  })
  local_mocked_bindings(service = function(...) stop("the loop ran inside a callback"),
                        .package = "httpuv")
  ws_say(ws, list(type = "select", scene = 1L, seq = 4L, items = list(list(layer = "coast", rows = list(7L)))))
  expect_identical(inner$sel$row, 8L)
  expect_identical(inner$n, 1L)
  expect_match(inner$wait, "cannot be called from inside an srv\\$on\\(\\) callback")
  expect_error(srv$on("nope", identity), "\"hello\", \"select\" or \"view\"")
  expect_error(srv$on("select", 1), "must be a function")
})

test_that("socket traffic leaves R's random number generator alone", {
  skip_if_no_ws()
  set.seed(3)
  before <- get(".Random.seed", envir = globalenv())
  srv <- serve_test(probe_scene())
  ws <- ws_open(srv)
  ws_hello(ws)
  ws_say(ws, list(type = "select", scene = 1L, seq = 1L, items = list(list(layer = "land", rows = list(1L)))))
  srv$selection()
  srv$view_state()
  expect_message(srv$wait(timeout = 0.05), "No select")
  srv$stop()
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("without jsonlite the page is served with no socket", {
  skip_if_not_installed("httpuv")
  local_mocked_bindings(has_jsonlite = function() FALSE)
  old <- ws_flags$told_jsonlite
  ws_flags$told_jsonlite <- FALSE
  on.exit(ws_flags$told_jsonlite <- old, add = TRUE)
  expect_message(srv <- serve_test(probe_scene()), "without the 'jsonlite' package")
  on.exit(srv$stop(), add = TRUE)
  ## Said once per session (other messages, such as the weaker token
  ## source on Windows, may still come).
  said <- character()
  srv2 <- withCallingHandlers(serve_test(probe_scene()), message = function(m) {
    said <<- c(said, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_false(any(grepl("jsonlite", said, fixed = TRUE)))
  srv2$stop()
  page <- rawToChar(http_req(srv$port, paste0("/", srv$token, "/"))$body)
  expect_false(grepl("data-aob-socket", page, fixed = TRUE))
  expect_match(page, "data-aob-scene-serial=\"1\"", fixed = TRUE)
  ## Every upgrade is refused, the right one too, and its socket closed.
  ws <- ws_open(srv)
  expect_identical(ws$headers_response$status, 404L)
  expect_identical(ws$closed$code, 1008L)
  expect_null(ws$on_message)
  expect_error(srv$selection(), "needs the 'jsonlite' package")
  expect_error(srv$wait(), "needs the 'jsonlite' package")
  expect_identical(srv$connections(), 0L)
  ## `select` is still checked.
  expect_error(serve_test(probe_scene(), select = "nope"), "not a vector layer")
})

## ---- real upgrades against a server in a child process ---------------------

test_that("real upgrades answer 101, 403 and 404, and protocol 1 runs over the wire", {
  skip_if_no_ws()
  skip_on_cran()
  d <- tempfile("aob-ws-child-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  info <- file.path(d, "info")
  stop_file <- file.path(d, "stop")
  sel_file <- file.path(d, "sel")
  log <- file.path(d, "out.log")
  err <- file.path(d, "err.log")
  script <- file.path(d, "child.R")
  writeLines(c(
    "suppressWarnings(library(aobcore))",
    "srv <- suppressWarnings(serve_scene(probe_scene(), open = FALSE))",
    sprintf("srv$on('select', function(m) writeLines(paste(srv$selection()$row, collapse = ','), %s))",
            deparse(sel_file)),
    ## Written whole, then renamed, so the parent never reads half of it.
    sprintf("writeLines(c(srv$port, srv$token), %s)", deparse(paste0(info, ".tmp"))),
    sprintf("file.rename(%s, %s)", deparse(paste0(info, ".tmp")), deparse(info)),
    "t0 <- Sys.time()",
    sprintf(paste0("while (!file.exists(%s) && ",
                   "as.numeric(difftime(Sys.time(), t0, units = 'secs')) < 60) httpuv::service(20)"),
            deparse(stop_file)),
    "srv$stop()",
    "cat('stopped\\n')"
  ), script)
  old <- Sys.getenv("R_LIBS")
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = log, stderr = err, wait = FALSE)
  Sys.setenv(R_LIBS = old)
  on.exit(writeLines("stop", stop_file), add = TRUE, after = FALSE)
  t0 <- Sys.time()
  while (length(readLines_safe(info)) < 2L && secs_since(t0) < 60) Sys.sleep(0.1)
  x <- readLines_safe(info)
  skip_if(length(x) < 2L, paste("child server did not start:", paste(c(readLines_safe(log), readLines_safe(err)), collapse = "\n")))
  port <- as.integer(x[1])
  token <- x[2]
  path <- paste0("/", token, "/ws")
  origin <- paste0("http://127.0.0.1:", port)
  wrong <- paste0("/", paste(rev(strsplit(token, "")[[1]]), collapse = ""), "/ws")

  status_of <- function(head) as.integer(strsplit(head, " ", fixed = TRUE)[[1]][2])

  ## Refusals: the refusal comes first, then httpuv's 101, then the close
  ## from onWSOpen (1008).
  for (k in list(list(path = path, origin = "http://evil.example", status = 403L),
                 list(path = path, origin = NULL, status = 403L),
                 list(path = path, origin = origin, host = "evil.example:80", status = 403L),
                 list(path = wrong, origin = origin, status = 404L),
                 list(path = paste0("/", token, "/"), origin = origin, status = 404L))) {
    con <- ws_upgrade_raw(port, k$path, host = k$host %||% paste0("127.0.0.1:", port), origin = k$origin)
    buf <- read_for(con, 1.5)
    close(con)
    first <- split_head(buf)
    label <- paste(k$path, k$origin %||% "(no origin)", k$host %||% "")
    expect_identical(status_of(first$head), k$status, label = label)
    ## If httpuv's 101 followed, its socket was closed with 1008 and no
    ## message from R was sent on it. On the wire httpuv 1.6.17 writes the
    ## refusal's headers, the 101's headers, then the refusal's body, then
    ## frames.
    at <- grepRaw(charToRaw("HTTP/1.1 101"), first$rest, fixed = TRUE)
    if (length(at)) {
      n <- as.integer(sub("(?s).*Content-Length: ([0-9]+).*", "\\1", first$head, perl = TRUE))
      after <- split_head(first$rest[at:length(first$rest)])$rest
      frames <- ws_frames(after[-seq_len(n)])
      expect_gte(length(frames), 1L)
      expect_identical(frames[[1]]$opcode, 8L, label = label)
      expect_identical(close_code(frames[[1]]), 1008L, label = label)
    }
  }

  ## Accepted: 101, then hello both ways and a selection.
  con <- ws_upgrade_raw(port, path, origin = origin)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  buf <- read_for(con, 3, until = function(b) length(grepRaw(charToRaw("\r\n\r\n"), b, fixed = TRUE)) > 0)
  first <- split_head(buf)
  expect_identical(status_of(first$head), 101L)
  writeBin(ws_frame("{\"type\":\"hello\",\"protocol\":1,\"specs\":[\"0.1\"],\"scene\":1}"), con)
  got <- read_for(con, 5, until = function(b) length(ws_frames(b)) > 0)
  frames <- ws_frames(c(first$rest, got))
  expect_identical(frames[[1]]$opcode, 1L)
  hello <- jsonlite::parse_json(rawToChar(frames[[1]]$payload), simplifyVector = TRUE)
  expect_identical(hello$type, "hello")
  expect_identical(hello$connection, 1L)
  writeBin(ws_frame(paste0("{\"type\":\"select\",\"scene\":1,\"seq\":1,\"trigger\":\"click\",",
                           "\"items\":[{\"layer\":\"coast\",\"rows\":[0,4,99]}]}")), con)
  t0 <- Sys.time()
  while (!file.exists(sel_file) && secs_since(t0) < 20) Sys.sleep(0.05)
  expect_identical(readLines_safe(sel_file), "1,5,100")
  ## A binary frame is closed with 1003.
  writeBin(ws_frame(as.raw(1:3), opcode = 2L), con)
  got <- read_for(con, 5, until = function(b) any(vapply(ws_frames(b), function(f) f$opcode == 8L, TRUE)))
  fr <- Filter(function(f) f$opcode == 8L, ws_frames(got))
  expect_length(fr, 1L)
  expect_identical(close_code(fr[[1]]), 1003L)
  close(con)

  ## stop() closes an open socket with 1001.
  con <- ws_upgrade_raw(port, path, origin = paste0("http://localhost:", port))
  buf <- read_for(con, 3, until = function(b) length(grepRaw(charToRaw("\r\n\r\n"), b, fixed = TRUE)) > 0)
  expect_identical(status_of(split_head(buf)$head), 101L)
  writeLines("stop", stop_file)
  got <- read_for(con, 10, until = function(b) any(vapply(ws_frames(b), function(f) f$opcode == 8L, TRUE)))
  fr <- Filter(function(f) f$opcode == 8L, ws_frames(c(split_head(buf)$rest, got)))
  expect_length(fr, 1L)
  if (length(fr)) expect_identical(close_code(fr[[1]]), 1001L)
  close(con)
  t0 <- Sys.time()
  while (!any(grepl("stopped", readLines_safe(log))) && secs_since(t0) < 20) Sys.sleep(0.1)
  out <- c(readLines_safe(log), readLines_safe(err))
  expect_true(any(grepl("stopped", out)), label = paste(out, collapse = "\n"))
  ## Nothing printed by httpuv's own try().
  expect_false(any(grepl("Error in try|attempt to apply", out)), label = paste(out, collapse = "\n"))
})
