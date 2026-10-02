#' Write a scene to a self-contained HTML page
#'
#' Writes one HTML file that draws `scene` with the bundled renderer. The
#' scene is embedded as JSON, each Arrow IPC blob is embedded as base64 text,
#' and the renderer JavaScript is inlined, so the page works offline and loads
#' nothing from a CDN.
#'
#' The scene is checked cheaply before writing: the top-level fields, the
#' version, the view type, that layer ids are unique, that every data id a
#' layer uses is defined in `scene$data`, that every `blob` key has a
#' blob, and that legends name layers in the scene and popups are on vector
#' layers. Full validation against the scene spec JSON Schema happens outside
#' R (see the allboa/scenespec validator).
#'
#' A tiled raster added with `embed = FALSE` from a local COG (see
#' [scene_add_tiled_raster()]) has no tile bytes in the page and a `file://`
#' URL a page opened from disk cannot range-request, so writing it warns
#' (unless its `url` was given explicitly); serve it with [serve_scene()] or
#' embed the layer.
#'
#' @param scene A scene from [scene()] and [scene_add_data()] or
#'   [scene_add_vector()], which carries its blobs; or a plain list following
#'   scene spec 0.1 to 0.5, with `version`, `view`, `data` and `layers`. It is written
#'   with [scene_json()].
#' @param blobs A named list of raw vectors, each an Arrow IPC stream or file,
#'   named by the `blob` keys used in `scene$data`. Defaults to the blobs the
#'   scene carries ([scene_blobs()]).
#' @param file Path of the HTML file to write.
#' @param title Page title. Defaults to the first layer label, or
#'   `"allonboard scene"`.
#' @param theme `"auto"` follows the browser's light or dark preference;
#'   `"light"` or `"dark"` fixes it. The page also has a theme button.
#' @return The path of the written file, invisibly.
#' @export
#' @examples
#' f <- write_scene_html(probe_scene(), file = tempfile(fileext = ".html"))
#' file.size(f)
#'
#' x <- wk::wkt("LINESTRING (0 0, 1000000 1000000)", crs = "EPSG:3031")
#' s <- scene_add_vector(scene(), "line", x, stroke = c(60, 66, 72, 255))
#' f2 <- write_scene_html(s, file = tempfile(fileext = ".html"))
#' \dontrun{
#' utils::browseURL(f)
#' }
write_scene_html <- function(scene, blobs = attr(scene, "blobs"), file = tempfile(fileext = ".html"),
                             title = NULL, theme = c("auto", "light", "dark")) {
  theme <- match.arg(theme)
  if (is.null(blobs)) {
    blobs <- list()
  }
  check_blobs(blobs)
  used <- check_scene_shape(scene, blobs)
  blobs <- blobs[intersect(names(blobs), used)]
  if (is.null(title)) {
    title <- "allonboard scene"
  }
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be a single string.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a single path.", call. = FALSE)
  }

  warn_unfetchable_files(scene)
  html <- scene_page(scene, blobs, title = title, theme = theme, mode = "inline")
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(c("<!DOCTYPE html>", html), con, useBytes = TRUE)
  invisible(file)
}

## The page for a scene, as one UTF-8 string without the doctype line. One
## builder for every transport, so they cannot drift (decision 0006):
##
## "inline" (write_scene_html()): every blob as base64 in a
##   <script data-aob-blob>, in the order of `blobs`, and the renderer
##   inlined. The page needs nothing else.
## "linked" (a served page): no blob scripts. The page <div> carries
##   data-aob-blob-base="blob/", so the renderer fetches a blob it was not
##   given from "blob/" plus encodeURIComponent(key), relative to the page;
##   the keys the server delivers are listed in one JSON script
##   (data-aob-blob-keys), which tells a tiled raster which tiles have a
##   blob; and the renderer is loaded from "aob-renderer.min.js" beside
##   the page. A served page also carries the scene serial its server gave
##   it (data-aob-scene-serial, `serial`) and, when the server takes a
##   websocket, data-aob-socket="ws": the socket's URL relative to the page
##   (decision 0007). Inline pages carry neither.
##
## `blobs` has been checked (check_blobs(), check_scene_shape()) and holds
## only the blobs the scene uses; in "linked" mode only its names are used.
scene_page <- function(scene, blobs, title, theme, mode = c("inline", "linked"),
                       serial = NULL, socket = FALSE) {
  mode <- match.arg(mode)
  sid <- "aob-scene"
  linked <- mode == "linked"
  renderer <- if (linked) {
    htmltools::tags$script(src = "aob-renderer.min.js")
  } else {
    htmltools::tags$script(htmltools::HTML(renderer_js()))
  }
  page <- htmltools::tags$html(
    lang = "en",
    `data-theme` = if (theme != "auto") theme,
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(title),
      ## A served page asks the server for no favicon (it would be a 404
      ## outside the token).
      if (linked) htmltools::tags$link(rel = "icon", href = "data:,"),
      ## Nor does it send the token in a Referer to anything it links to.
      if (linked) htmltools::tags$meta(name = "referrer", content = "no-referrer"),
      htmltools::tags$style(htmltools::HTML(page_css))
    ),
    htmltools::tags$body(
      scene_element(scene, blobs, sid, linked = linked, serial = serial, socket = socket,
                    class = "aob-page"),
      renderer
    )
  )
  enc2utf8(as.character(htmltools::doRenderTags(page)))
}

## The scene's element, its JSON script and its blob scripts (or, linked,
## the list of blob keys), tied together by `sid`: the part of a page that
## draws one scene, shared by scene_page() and scene_tag(). `...` are more
## attributes of the element (class, style, data-theme).
scene_element <- function(scene, blobs, sid, linked = FALSE, serial = NULL, socket = FALSE,
                          ...) {
  blob_tags <- if (linked) {
    keys <- as.list(names(blobs) %||% character())
    htmltools::tags$script(
      type = "application/json", `data-aob-blob-keys` = NA, `data-aob-scene` = sid,
      htmltools::HTML(gsub("<", "\\u003c", json_value(keys), fixed = TRUE))
    )
  } else {
    lapply(names(blobs), function(k) {
      htmltools::tags$script(
        type = "application/octet-stream", `data-aob-blob` = k, `data-aob-scene` = sid,
        htmltools::HTML(b64_encode(blobs[[k]]))
      )
    })
  }
  htmltools::tagList(
    htmltools::tags$div(..., `data-aob-scene` = sid,
                        `data-aob-blob-base` = if (linked) "blob/",
                        `data-aob-scene-serial` = if (linked && !is.null(serial)) format(serial),
                        `data-aob-socket` = if (linked && isTRUE(socket)) "ws"),
    htmltools::tags$script(type = "application/json", id = sid, htmltools::HTML(page_json(scene))),
    blob_tags
  )
}

## A page written to disk cannot range-request a local file, so a scene
## with a registered file (scene_add_tiled_raster(embed = FALSE)) whose url
## was not given explicitly draws nothing for that layer from file://.
warn_unfetchable_files <- function(scene) {
  files <- attr(scene, "files")
  if (!length(files)) return(invisible())
  ids <- names(files)[!vapply(files, function(f) isTRUE(f$url_explicit), logical(1))]
  ids <- ids[ids %in% names(scene$data)]
  if (length(ids)) {
    warning("The page cannot read the local COG of ",
            paste0("`", ids, "`", collapse = ", "), " from disk (",
            paste0("\"", vapply(ids, function(id) scene$data[[id]]$url, ""), "\"", collapse = ", "),
            "): serve the scene with serve_scene() or add the layer with `embed = TRUE`.",
            call. = FALSE)
  }
  invisible()
}

## The scene document for a <script> element: scene_json() output with
## every "<" (only ever inside a JSON string) escaped, so no "</script>" can
## end the element early.
page_json <- function(scene) {
  if (!inherits(scene, "aob_scene")) {
    scene <- structure(scene, class = "aob_scene")
  }
  gsub("<", "\\u003c", scene_json(scene), fixed = TRUE)
}

## A whole page is the renderer's own: its root takes the renderer's colour
## scheme (--aob-scheme, light or dark with the theme). A fragment in a host
## page leaves the host's root alone (decision 0009).
page_css <- paste(
  "html { color-scheme: var(--aob-scheme, light); }",
  "html, body { height: 100%; margin: 0; }",
  "body { background: var(--aob-bg, #eef2f4); }",
  ".aob-page { height: 100%; }",
  sep = "\n"
)

renderer_path <- function() {
  f <- system.file("renderer", "aob-renderer.min.js", package = "aobcore")
  if (!nzchar(f)) {
    stop("The bundled renderer is missing from the installed package.", call. = FALSE)
  }
  f
}

renderer_js <- function() {
  f <- renderer_path()
  readChar(f, file.size(f), useBytes = TRUE)
}

check_blobs <- function(blobs) {
  if (!is.list(blobs) || is.data.frame(blobs)) {
    stop("`blobs` must be a named list of raw vectors.", call. = FALSE)
  }
  if (length(blobs) == 0L) {
    return(invisible())
  }
  nms <- names(blobs)
  if (is.null(nms) || anyNA(nms) || any(nms == "") || anyDuplicated(nms)) {
    stop("`blobs` must have unique, non-empty names.", call. = FALSE)
  }
  bad <- !vapply(blobs, is.raw, logical(1))
  if (any(bad)) {
    stop("Every blob must be a raw vector of Arrow IPC bytes; not raw: ",
         paste(nms[bad], collapse = ", "), ".", call. = FALSE)
  }
  invisible()
}

## A cheap shape check: enough to catch a scene that cannot draw, not a
## replacement for the JSON Schema.
check_scene_shape <- function(scene, blobs) {
  fail <- function(...) stop(..., call. = FALSE)
  if (!is.list(scene)) fail("`scene` must be a list.")
  missing <- setdiff(c("version", "view", "data", "layers"), names(scene))
  if (length(missing)) {
    fail("`scene` is missing ", paste(missing, collapse = ", "), ".")
  }
  if (!is.character(scene$version) || length(scene$version) != 1L ||
      !scene$version %in% scene_spec_versions) {
    fail("`scene$version` must be one of \"", paste(scene_spec_versions, collapse = "\", \""), "\".")
  }
  view <- scene$view
  if (!is.list(view) || !is.character(view$type) || length(view$type) != 1L ||
      !view$type %in% c("projected", "cartesian", "globe")) {
    fail("`scene$view$type` must be one of \"projected\", \"cartesian\" or \"globe\".")
  }
  if (view$type != "cartesian" && is.null(view$crs)) {
    fail("A ", view$type, " view needs `scene$view$crs`.")
  }
  if (!is.null(view$bounds)) {
    b <- view$bounds
    if (!is.numeric(b) || length(b) != 4L || anyNA(b) || !(b[1] < b[2] && b[3] < b[4])) {
      fail("`scene$view$bounds` must be c(xmin, xmax, ymin, ymax) with xmin < xmax and ymin < ymax.")
    }
    if (view$type == "globe") fail("A globe view has no `bounds`.")
    if (!is.null(view$extent) && !extents_overlap(view$extent, b)) {
      fail("`scene$view$extent` does not overlap `scene$view$bounds`, so the camera could ",
           "never show it; widen the bounds or build the scene with `domain = FALSE`.")
    }
    ce <- view$center
    if (!is.null(ce) && !(ce[1] >= b[1] && ce[1] <= b[2] && ce[2] >= b[3] && ce[2] <= b[4])) {
      fail("`scene$view$center` lies outside `scene$view$bounds`.")
    }
    if (!spec_at_least(scene$version, "0.4") && !inherits(scene, "aob_scene")) {
      fail("`scene$view$bounds` needs scene spec 0.4.")
    }
  }
  data <- scene$data
  if (!is.list(data) || (length(data) > 0L && is.null(names(data)))) {
    fail("`scene$data` must be a named list of data references.")
  }
  ids <- names(data)
  used_blobs <- character()
  for (id in ids) {
    ref <- data[[id]]
    if (!is.list(ref)) fail("Data reference `", id, "` must be a list.")
    if (identical(ref$format, "cog")) {
      if (!is.null(ref$blob) || is.null(ref$url)) {
        fail("The cog data reference `", id, "` needs a `url` and no `blob`.")
      }
      next
    }
    has_blob <- !is.null(ref$blob)
    if (has_blob == !is.null(ref$url)) {
      fail("Data reference `", id, "` needs exactly one of `blob` or `url`.")
    }
    if (has_blob) {
      if (!ref$blob %in% names(blobs)) {
        fail("Data reference `", id, "` uses blob \"", ref$blob, "\", which is not in `blobs`.")
      }
      used_blobs <- c(used_blobs, ref$blob)
    }
  }
  layers <- scene$layers
  if (!is.list(layers) || !is.null(names(layers))) {
    fail("`scene$layers` must be an unnamed list of layers.")
  }
  need <- function(layer_id, id) {
    if (!is.character(id) || length(id) != 1L || !id %in% ids) {
      fail("Layer `", layer_id, "` uses data id \"", format(id), "\", which is not in `scene$data`.")
    }
  }
  seen <- character()
  for (layer in layers) {
    lid <- layer$id
    if (!is.character(lid) || length(lid) != 1L) fail("Every layer needs a string `id`.")
    if (lid %in% seen) fail("Layer id `", lid, "` is used twice.")
    seen <- c(seen, lid)
    kind <- layer$kind
    if (identical(kind, "raster")) {
      need(lid, layer$values)
      if (!is.null(layer$mesh)) {
        need(lid, layer$mesh$vertices)
        need(lid, layer$mesh$indices)
      }
    } else if (identical(kind, "tiled_raster")) {
      if (identical(scene$version, "0.1") && !inherits(scene, "aob_scene")) {
        fail("Layer `", lid, "` is a tiled raster, which needs scene spec 0.2 or later.")
      }
      if (uses_spec_03(layer) && !spec_at_least(scene$version, "0.3") && !inherits(scene, "aob_scene")) {
        fail("Layer `", lid, "` uses `rgb` or JPEG tiles, which need scene spec 0.3.")
      }
      need(lid, layer$source)
      if (!identical(data[[layer$source]]$format, "cog")) {
        fail("Layer `", lid, "` draws `", layer$source, "`, which is not a cog.")
      }
      need(lid, layer$plan$mesh$vertices)
      need(lid, layer$plan$mesh$indices)
      ## Embedded tile bytes (see scene_add_tiled_raster()).
      for (lv in layer$plan$levels) for (t in lv$tiles) {
        key <- tile_blob_key(layer$source, t$byte_offset, t$byte_length)
        if (key %in% names(blobs)) used_blobs <- c(used_blobs, key)
      }
    } else if (is.character(kind) && length(kind) == 1L && kind %in% c("polygon", "path", "point")) {
      need(lid, layer$data)
      if (is.null(data[[layer$data]]$geometry)) {
        fail("Layer `", lid, "` draws data `", layer$data, "`, which has no `geometry`.")
      }
    } else {
      fail("Layer `", lid, "` has kind \"", format(kind), "\"; expected polygon, path, point, raster or tiled_raster.")
    }
    if (!is.null(layer$popup) && !kind %in% c("polygon", "path", "point")) {
      fail("Layer `", lid, "` has a popup; popups are for polygon, path and point layers.")
    }
  }
  ## Scene spec 0.5: legends key layers in the scene.
  if (!is.null(scene$legends)) {
    if (!is.list(scene$legends) || !is.null(names(scene$legends))) {
      fail("`scene$legends` must be an unnamed list of legends.")
    }
    for (lg in scene$legends) {
      if (!is.character(lg$layer) || length(lg$layer) != 1L || !lg$layer %in% seen) {
        fail("A legend keys layer \"", format(lg$layer), "\", which is not in `scene$layers`.")
      }
    }
  }
  if (uses_spec_05(scene) && !spec_at_least(scene$version, "0.5") && !inherits(scene, "aob_scene")) {
    fail("Legends and popups need scene spec 0.5.")
  }
  unused <- setdiff(names(blobs), used_blobs)
  if (length(unused)) {
    warning("Blobs not used by the scene are left out of the page: ",
            paste(unused, collapse = ", "), ".", call. = FALSE)
  }
  unique(used_blobs)
}
