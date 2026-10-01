# The page write_scene_html() wrote before the shared page builder (aobcore
# 3502f60, R/html.R), kept verbatim as a reference: decision 0006 requires
# embedded pages to stay byte-identical, blob order included.
legacy_page <- function(scene, blobs = attr(scene, "blobs"), title = "allonboard scene",
                        theme = "auto") {
  if (is.null(blobs)) blobs <- list()
  used <- aobcore:::check_scene_shape(scene, blobs)
  blobs <- blobs[intersect(names(blobs), used)]
  sid <- "aob-scene"
  blob_tags <- lapply(names(blobs), function(k) {
    htmltools::tags$script(
      type = "application/octet-stream", `data-aob-blob` = k, `data-aob-scene` = sid,
      htmltools::HTML(aobcore:::b64_encode(blobs[[k]]))
    )
  })
  page <- htmltools::tags$html(
    lang = "en",
    `data-theme` = if (theme != "auto") theme,
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(aobcore:::page_css))
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "aob-page", `data-aob-scene` = sid),
      htmltools::tags$script(type = "application/json", id = sid,
                             htmltools::HTML(aobcore:::page_json(scene))),
      blob_tags,
      htmltools::tags$script(htmltools::HTML(aobcore:::renderer_js()))
    )
  )
  html <- enc2utf8(as.character(htmltools::doRenderTags(page)))
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  con <- file(f, open = "wb")
  writeLines(c("<!DOCTYPE html>", html), con, useBytes = TRUE)
  close(con)
  readBin(f, "raw", file.size(f))
}

page_bytes <- function(f) readBin(f, "raw", file.size(f))
