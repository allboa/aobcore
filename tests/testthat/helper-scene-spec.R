# Scenes in these tests carry no view domain (scene spec 0.4 bounds) unless
# a test asks for one, so the version rules for 0.1 to 0.3 are tested as
# before; test-domain.R covers the default domain.
options(aobcore.domain = FALSE)

# A structural check of a scene against scene spec 0.1, with no node or
# JSON Schema validator. It covers the rules the producers here can break:
# the top-level shape, the view, data references (format, exactly one of
# blob or url, a native GeoArrow geometry encoding) and vector layers (known
# keys, ids, data that resolves, a kind that can draw the encoding). The
# full check is allboa/scenespec scripts/validate.js; the PR says where it
# was run. Returns a character vector of problems, empty when valid.
scene_spec_problems <- function(x) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  id_ok <- function(id) {
    is.character(id) && length(id) == 1L && grepl("^[A-Za-z][A-Za-z0-9_.-]*$", id) &&
      nchar(id) <= 128L
  }
  crs_ok <- function(crs) {
    (is.character(crs) && length(crs) == 1L &&
       grepl("^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$", crs)) ||
      (is.list(crs) && is.character(crs$type)) ||
      (inherits(crs, "aob_json") && grepl("^[[:space:]]*[{].*\"type\"", crs))
  }
  color_ok <- function(col) {
    if (is.list(col)) return(identical(names(col), "column") && is.character(col$column))
    is.numeric(col) && length(col) == 4L && all(col == round(col)) && all(col >= 0 & col <= 255)
  }
  kinds <- list(
    point = c("geoarrow.point", "geoarrow.multipoint"),
    path = c("geoarrow.linestring", "geoarrow.multilinestring"),
    polygon = c("geoarrow.polygon", "geoarrow.multipolygon")
  )
  layer_keys <- list(
    point = c("fill", "stroke", "stroke_width_px", "radius_px"),
    path = c("stroke", "stroke_width_px"),
    polygon = c("fill", "stroke", "stroke_width_px")
  )

  extra <- setdiff(names(x), c("$schema", "version", "view", "data", "layers"))
  if (length(extra)) add("unknown top-level keys: ", paste(extra, collapse = ", "))
  missing <- setdiff(c("version", "view", "data", "layers"), names(x))
  if (length(missing)) return(c(p, paste0("missing: ", paste(missing, collapse = ", "))))
  if (!identical(x$version, "0.1")) add("version must be \"0.1\"")

  v <- x$view
  if (!is.list(v) || !isTRUE(v$type %in% c("projected", "cartesian", "globe"))) {
    add("view.type must be projected, cartesian or globe")
  } else {
    if (length(setdiff(names(v), c("type", "crs", "center", "extent", "local_origin")))) {
      add("unknown view keys")
    }
    if (v$type %in% c("projected", "globe") && is.null(v$crs)) add("view.crs is required")
    if (!is.null(v$crs) && !crs_ok(v$crs)) add("view.crs is malformed")
  }

  if (!is.list(x$data) || (length(x$data) && is.null(names(x$data)))) {
    add("data must be an object keyed by id")
  }
  for (id in names(x$data)) {
    d <- x$data[[id]]
    if (!id_ok(id)) add("data id \"", id, "\" is malformed")
    if (length(setdiff(names(d), c("format", "blob", "url", "geometry", "origin_subtracted")))) {
      add("data ", id, ": unknown keys")
    }
    if (!isTRUE(d$format %in% c("arrow-ipc-stream", "arrow-ipc-file"))) add("data ", id, ": bad format")
    if (is.null(d$blob) == is.null(d$url)) add("data ", id, ": needs exactly one of blob or url")
    if (!is.null(d$blob) && !(is.character(d$blob) && nzchar(d$blob))) add("data ", id, ": bad blob")
    if (!is.null(d$geometry)) {
      g <- d$geometry
      if (length(setdiff(names(g), c("column", "encoding", "crs")))) add("data ", id, ": unknown geometry keys")
      if (!(is.character(g$column) && nzchar(g$column))) add("data ", id, ": geometry.column missing")
      if (!isTRUE(g$encoding %in% unlist(kinds))) add("data ", id, ": geometry.encoding not native GeoArrow")
      if (!is.null(g$crs) && !identical(g$crs, v$crs)) add("data ", id, ": geometry.crs differs from view.crs")
    }
  }

  if (!is.list(x$layers) || !is.null(names(x$layers))) add("layers must be an array")
  seen <- character()
  for (i in seq_along(x$layers)) {
    l <- x$layers[[i]]
    where <- paste0("layer ", i, ": ")
    if (!id_ok(l$id)) add(where, "bad id")
    if (isTRUE(l$id %in% seen)) add(where, "duplicate id")
    seen <- c(seen, l$id)
    if (!isTRUE(l$kind %in% names(kinds))) {
      add(where, "kind must be point, path or polygon")
      next
    }
    allowed <- c("id", "kind", "label", "visible", "data", layer_keys[[l$kind]])
    extra <- setdiff(names(l), allowed)
    if (length(extra)) add(where, "keys not allowed: ", paste(extra, collapse = ", "))
    d <- x$data[[l$data]]
    if (is.null(d)) {
      add(where, "data \"", l$data, "\" is not defined")
    } else if (!isTRUE(d$geometry$encoding %in% kinds[[l$kind]])) {
      add(where, l$kind, " cannot draw ", d$geometry$encoding)
    }
    for (k in intersect(c("fill", "stroke"), names(l))) {
      if (!color_ok(l[[k]])) add(where, k, " is not a color")
    }
    for (k in intersect(c("stroke_width_px", "radius_px"), names(l))) {
      if (!(is.numeric(l[[k]]) && length(l[[k]]) == 1L && l[[k]] >= 0)) add(where, k, " is bad")
    }
    if (!is.null(l$label) && !(is.character(l$label) && length(l$label) == 1L)) add(where, "bad label")
    if (!is.null(l$visible) && !(is.logical(l$visible) && length(l$visible) == 1L)) add(where, "bad visible")
  }
  p
}

# Check the scene both as an R list and, when jsonlite is available, as the
# JSON scene_json() writes, parsed back without simplification.
expect_valid_scene <- function(s) {
  expect_identical(scene_spec_problems(unclass(s)), character())
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    parsed <- jsonlite::fromJSON(scene_json(s), simplifyVector = TRUE,
                                 simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_identical(scene_spec_problems(parsed), character())
  }
}

# The extension name and coordinate layout of a stream's geometry column.
geometry_info <- function(schema) {
  for (child in schema$children) {
    ext <- child$metadata[["ARROW:extension:name"]]
    if (!is.null(ext)) {
      node <- child
      while (length(node$children) && !startsWith(node$format, "+w:")) {
        node <- node$children[[1]]
      }
      return(list(column = child$name, ext = ext, coords = node$format))
    }
  }
  NULL
}

# gdalraster is installed and its GDAL can resolve EPSG codes. Some binary
# builds cannot find their PROJ database (proj.db); skip there rather than
# fail, since that is an installation problem, not a producer one.
skip_if_no_gdal <- function() {
  skip_if_not_installed("gdalraster")
  ok <- !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
  skip_if_not(ok, "gdalraster cannot resolve EPSG:3031 (PROJ database not found)")
}

# A structural check of a scene spec 0.2 or 0.3 scene with tiled raster layers:
# keys and required fields of cog references, tiled_raster layers, plans,
# levels, encodings and tiles, following scene-0.2.schema.json (and 0.3's
# rgb and jpeg additions: a 0.2 scene must use neither), plus the
# validator's cross-checks (plan.crs is view.crs, windows fit their tile and
# the level grid, mesh row runs do not overlap). Vector layers are checked by
# scene_spec_problems() on the scene with its tiled layers removed.
tiled_spec_problems <- function(x) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  only <- function(obj, keys, where) {
    extra <- setdiff(names(obj), keys)
    if (length(extra)) add(where, ": keys not allowed: ", paste(extra, collapse = ", "))
  }
  need <- function(obj, keys, where) {
    miss <- setdiff(keys, names(obj))
    if (length(miss)) add(where, ": missing ", paste(miss, collapse = ", "))
  }
  if (!isTRUE(x$version %in% c("0.2", "0.3"))) add("version must be \"0.2\" or \"0.3\"")
  v03 <- identical(x$version, "0.3")
  tiled <- vapply(x$layers, function(l) identical(l$kind, "tiled_raster"), TRUE)
  rest <- x
  rest$version <- "0.1"
  rest$layers <- x$layers[!tiled]
  rest$data <- x$data[!vapply(x$data, function(d) identical(d$format, "cog"), TRUE)]
  p <- c(p, scene_spec_problems(rest))
  codecs <- c("none", "deflate", "lzw", "zstd", "lerc", "lerc_deflate", "lerc_zstd", "webp", "packbits",
              if (v03) "jpeg")
  dtypes <- c("uint8", "int8", "uint16", "int16", "uint32", "int32", "float32", "float64")
  for (l in x$layers[tiled]) {
    w <- paste0("layer ", l$id)
    only(l, c("id", "kind", "label", "visible", "source", "plan", "palette", if (v03) "rgb"), w)
    need(l, c("id", "kind", "source", "plan"), w)
    if (is.null(l$palette) == is.null(l$rgb)) add(w, ": needs exactly one of palette and rgb")
    if (!is.null(l$rgb)) {
      only(l$rgb, c("bands", "alpha", "range"), paste(w, "rgb"))
      if (length(l$rgb$bands) != 3L) add(w, ": rgb needs 3 bands")
      if (!is.null(l$rgb$alpha) && l$rgb$alpha %in% l$rgb$bands) add(w, ": alpha is a colour band")
    }
    src <- x$data[[l$source]]
    if (!identical(src$format, "cog")) add(w, ": source is not a cog")
    only(src, c("format", "url"), paste(w, "source"))
    if (!(is.character(src$url) && nzchar(src$url))) add(w, ": cog needs a url")
    only(l$palette, c("name", "range"), paste(w, "palette"))
    pl <- l$plan
    only(pl, c("crs", "coverage", "planned_for", "selection", "mesh", "levels"), paste(w, "plan"))
    need(pl, c("crs", "coverage", "mesh", "levels"), paste(w, "plan"))
    if (!identical(pl$crs, x$view$crs)) add(w, ": plan crs is not view crs")
    if (identical(pl$coverage, "all_levels") && is.null(pl$selection$rule)) add(w, ": needs selection")
    if (identical(pl$coverage, "view") && (is.null(pl$planned_for) || length(pl$levels) != 1L)) {
      add(w, ": a view plan needs planned_for and one level")
    }
    only(pl$mesh, c("vertices", "indices", "position_column", "uv_column", "index_column"), paste(w, "mesh"))
    for (id in c(pl$mesh$vertices, pl$mesh$indices)) {
      d <- x$data[[id]]
      if (is.null(d) || !is.null(d$geometry) || !identical(d$format, "arrow-ipc-stream")) {
        add(w, ": mesh table ", id, " is not a plain Arrow table")
      }
    }
    runs <- NULL
    for (lv in pl$levels) {
      lw <- paste0(w, " level ", lv$level)
      only(lv, c("level", "grid", "pixel_size", "encoding", "tiles"), lw)
      need(lv, c("level", "grid", "pixel_size", "encoding", "tiles"), lw)
      only(lv$grid, c("crs", "extent", "dim", "nodata"), paste(lw, "grid"))
      enc <- lv$encoding
      only(enc, c("codec", "predictor", "dtype", "byte_order", "samples_per_pixel", "planar",
                  "band", "scale", "offset", if (v03) "jpeg_tables"), paste(lw, "encoding"))
      spp <- enc$samples_per_pixel %||% 1
      if (!is.null(l$rgb)) {
        if (!is.null(enc$band)) add(lw, ": rgb layer with encoding.band")
        if (max(c(l$rgb$bands, l$rgb$alpha)) > spp) add(lw, ": rgb band out of range")
        if (!identical(enc$planar %||% "interleaved", "interleaved")) add(lw, ": rgb needs interleaved")
        if (!identical(enc$dtype, "uint8") && is.null(l$rgb$range)) add(lw, ": rgb needs a range")
      }
      if (identical(enc$codec, "jpeg")) {
        if (!identical(enc$dtype, "uint8") || !spp %in% c(1, 3) ||
            !identical(enc$predictor %||% "none", "none") ||
            !identical(enc$planar %||% "interleaved", "interleaved")) add(lw, ": bad jpeg encoding")
      } else if (!is.null(enc$jpeg_tables)) add(lw, ": jpeg_tables without jpeg")
      if (!isTRUE(enc$codec %in% codecs)) add(lw, ": bad codec")
      if (!isTRUE(enc$dtype %in% dtypes)) add(lw, ": bad dtype")
      if ((enc$band %||% 1) > (enc$samples_per_pixel %||% 1)) add(lw, ": band out of range")
      if (!(is.numeric(lv$pixel_size) && lv$pixel_size > 0)) add(lw, ": bad pixel_size")
      for (t in lv$tiles) {
        tw <- paste0(lw, " tile ", t$col, "/", t$row)
        only(t, c("col", "row", "byte_offset", "byte_length", "size", "window", "footprint", "mesh"), tw)
        need(t, c("col", "row", "byte_offset", "byte_length", "size", "footprint", "mesh"), tw)
        if (!(t$byte_length >= 1)) add(tw, ": empty")
        win <- t$window %||% list(x = 0, y = 0, width = t$size[1], height = t$size[2])
        if (win$x + win$width > t$size[1] || win$y + win$height > t$size[2]) add(tw, ": window outside tile")
        if (t$col * t$size[1] + win$x + win$width > lv$grid$dim[1] ||
            t$row * t$size[2] + win$y + win$height > lv$grid$dim[2]) add(tw, ": runs past the grid")
        fp <- t$footprint
        if (!(length(fp) == 4L && fp[1] < fp[2] && fp[3] < fp[4])) add(tw, ": bad footprint")
        if (t$mesh$index_count %% 3 != 0) add(tw, ": index_count not a multiple of 3")
        runs <- rbind(runs, c(t$mesh$first_vertex, t$mesh$vertex_count, t$mesh$first_index,
                              t$mesh$index_count))
      }
    }
    for (k in c(1L, 3L)) {
      if (is.null(runs) || nrow(runs) < 2L) next
      o <- order(runs[, k])
      if (any(utils::head(runs[o, k] + runs[o, k + 1L], -1) > runs[o, k][-1])) add(w, ": mesh runs overlap")
    }
  }
  p
}

# Check a 0.2 scene structurally (as an R list and as parsed JSON) and, when
# AOB_SCENESPEC names an allboa/scenespec checkout with its node modules
# installed, with its validator (scripts/validate.js).
expect_valid_tiled_scene <- function(s) {
  expect_identical(tiled_spec_problems(unclass(s)), character())
  json <- scene_json(s)
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    parsed <- jsonlite::fromJSON(json, simplifyVector = TRUE,
                                 simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    expect_identical(tiled_spec_problems(parsed), character())
  }
  dir <- Sys.getenv("AOB_SCENESPEC")
  node <- Sys.which("node")
  if (nzchar(dir) && nzchar(node) && file.exists(file.path(dir, "scripts", "validate.js"))) {
    f <- tempfile(fileext = ".json")
    on.exit(unlink(f))
    writeLines(json, f)
    out <- suppressWarnings(system2(node, c(file.path(dir, "scripts", "validate.js"), f),
                                    stdout = TRUE, stderr = TRUE))
    expect_null(attr(out, "status"), label = paste(out, collapse = "\n"))
    expect_match(out[1], "^valid")
  }
}
