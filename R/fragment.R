#' A scene as an HTML fragment for a document or an app
#'
#' Returns `scene` as an 'htmltools' fragment that draws it inside a host
#' page: a knitted document (R Markdown, Quarto), a Shiny app or any page
#' built with 'htmltools'. The fragment is a sized `<div>` with the scene
#' JSON and every Arrow IPC blob embedded as base64, as
#' [write_scene_html()] embeds them, and the bundled renderer attached as an
#' [htmltools::htmlDependency()] named `"aob-renderer"`. A host that resolves
#' dependencies (rmarkdown, Quarto, Shiny, [htmltools::save_html()]) carries
#' the renderer once however many scenes it shows; each scene carries its
#' own data (allboa/design decision 0009).
#'
#' The checks are those of [write_scene_html()], including the warning for a
#' local COG registered with `embed = FALSE`, which a fragment cannot read
#' either. A fragment opens no socket and sends no selection.
#'
#' **Late fragments.** A fragment inserted after the host page has loaded
#' (Shiny's `renderUI()` or `insertUI()`, say) draws when it is inserted:
#' the fragment ends with a one-line script that calls the renderer's
#' `aob.boot()`, which draws every scene element not yet drawn. A host that
#' inserts the HTML without running its scripts calls `aob.boot()` itself.
#'
#' **Theme.** The host's own theme is not touched: `theme = "light"` or
#' `"dark"` fixes the fragment's colours, and `"auto"` follows the browser's
#' preference. The theme button in the fragment changes that fragment only,
#' and the renderer sets no `color-scheme` or theme on the host's root
#' element.
#'
#' @inheritParams write_scene_html
#' @param width,height CSS sizes of the fragment, such as `"100%"` or
#'   `"480px"`; a bare number is pixels.
#' @param id The id that ties the fragment's element to its scripts. `NULL`
#'   (the default) makes one unique in the session, without using the
#'   random number generator.
#' @return An [htmltools::tagList()] with the renderer dependency attached.
#' @export
#' @examples
#' x <- wk::wkt("LINESTRING (0 0, 1000000 1000000)", crs = "EPSG:3031")
#' s <- scene_add_vector(scene(), "line", x, stroke = c(60, 66, 72, 255))
#' tag <- scene_tag(s, height = 300)
#' f <- tempfile(fileext = ".html")
#' htmltools::save_html(htmltools::tagList(htmltools::tags$h1("A scene"), tag), f)
scene_tag <- function(scene, blobs = attr(scene, "blobs"), width = "100%", height = "480px",
                      theme = c("auto", "light", "dark"), id = NULL) {
  theme <- match.arg(theme)
  if (is.null(blobs)) {
    blobs <- list()
  }
  check_blobs(blobs)
  used <- check_scene_shape(scene, blobs)
  blobs <- blobs[intersect(names(blobs), used)]
  if (is.null(id)) {
    id <- fragment_id()
  }
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[A-Za-z][A-Za-z0-9_-]*$", id)) {
    stop("`id` must be a single string of letters, digits, \"-\" and \"_\", starting with a letter.",
         call. = FALSE)
  }
  style <- paste0("width:", css_size(width, "width"), ";height:", css_size(height, "height"), ";")
  warn_unfetchable_files(scene)
  htmltools::attachDependencies(
    htmltools::tagList(
      scene_element(scene, blobs, id, class = "aob-fragment", style = style,
                    `data-theme` = if (theme != "auto") theme),
      ## Draw it now when the renderer is already loaded: a fragment
      ## inserted after the page loaded (Shiny's renderUI(), say) is
      ## otherwise never booted. A renderer loaded later boots it itself.
      htmltools::tags$script(htmltools::HTML(fragment_boot_js))
    ),
    renderer_dependency()
  )
}

fragment_boot_js <- "if (window.aob && window.aob.boot) window.aob.boot();"

## The bundled renderer as an htmltools dependency: hosts that resolve
## dependencies include it once per document, by name.
renderer_dependency <- function() {
  htmltools::htmlDependency(
    name = "aob-renderer", version = renderer_version(),
    src = dirname(renderer_path()), script = "aob-renderer.min.js",
    all_files = FALSE
  )
}

## The renderer's version, from the bundle's banner
## ("/* aob-renderer 0.0.5: ...").
renderer_version <- function() {
  first <- readLines(renderer_path(), n = 1L, warn = FALSE)
  v <- regmatches(first, regexpr("(?<=^/\\* aob-renderer )[0-9]+(\\.[0-9]+)*", first, perl = TRUE))
  if (!length(v)) {
    stop("The bundled renderer has no version in its banner.", call. = FALSE)
  }
  v
}

## A fragment id unique in the session: a counter, the process id and the
## time, so knitting a document does not change its random numbers.
fragment_ids <- new.env(parent = emptyenv())
fragment_ids$n <- 0L
fragment_id <- function() {
  fragment_ids$n <- fragment_ids$n + 1L
  ms <- floor(as.numeric(Sys.time()) * 1000) %% 2^31
  sprintf("aob-%x-%x-%d", Sys.getpid(), as.integer(ms), fragment_ids$n)
}

## A CSS size: a number is pixels; a string is passed on when it looks like
## one CSS length or percentage (or "auto").
css_size <- function(x, what) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 0) {
    return(paste0(format(x, scientific = FALSE), "px"))
  }
  ok <- is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^(auto|[0-9]*\\.?[0-9]+(px|%|em|rem|vh|vw|in|cm|mm|pt))$", x)
  if (!ok) {
    stop("`", what, "` must be a number of pixels or a CSS size such as \"100%\" or \"480px\".",
         call. = FALSE)
  }
  x
}
