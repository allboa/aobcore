#' Densify lines and polygon edges
#'
#' Adds vertices along each segment of the lines and polygon rings in `x`
#' so that no segment is longer than `max_length`, in the units of `x`'s
#' coordinates. Segments are split evenly and linearly in those
#' coordinates: an edge in lon/lat stays straight in lon/lat (as GDAL's
#' `-segmentize` does), it is not a great circle. Densifying before a
#' change of CRS makes edges curve as they should in the view, for example
#' along a parallel in a polar view.
#'
#' This is the wk densify that allboa/design decision 0004 names as the
#' default home for in-memory data: it works on any 'wk' handleable input
#' and needs no GDAL. Points and multipoints are returned unchanged, as are
#' geometry collections (flatten them first, with [wk::wk_flatten()]),
#' empty geometries and missing ones. Empty parts and rings inside a
#' geometry that is densified are dropped, and so are its Z and M values.
#' The result is always [wk::wkb()], whatever the class of `x` (an `sfc`
#' becomes wkb with the same CRS). Non-finite coordinates are an error, as
#' is a densify that would make more than 100 million vertices.
#'
#' @param x Geometry: anything 'wk' can handle, such as an `sfc`,
#'   [wk::wkb()] or [wk::wkt()].
#' @param max_length The longest segment to keep, a single positive number
#'   in the units of `x`.
#' @return A [wk::wkb()] vector of the same length as `x`, with `x`'s CRS.
#' @export
#' @examples
#' x <- wk::wkt("LINESTRING (0 -60, 90 -60)", crs = "OGC:CRS84")
#' d <- vector_densify(x, 0.25)
#' nrow(wk::wk_coords(d))
vector_densify <- function(x, max_length) {
  if (!is.numeric(max_length) || length(max_length) != 1L || is.na(max_length) ||
      !is.finite(max_length) || max_length <= 0) {
    stop("`max_length` must be a single positive number.", call. = FALSE)
  }
  if (!wk::is_handleable(x)) {
    stop("`x` cannot be read by wk (class ", paste(class(x), collapse = "/"), ").",
         call. = FALSE)
  }
  crs <- wk::wk_crs(x)
  g <- wk::wk_set_crs(wk::as_wkb(x), NULL)
  meta <- wk::wk_meta(g)
  ## Not wk_meta()'s is_empty, which is TRUE when only the first part or
  ## ring is empty.
  has_coords <- wk::wk_count(g)$n_coord > 0
  out <- unclass(g)
  ## 2 linestring, 3 polygon, 5 multilinestring, 6 multipolygon.
  for (type in c(2L, 3L, 5L, 6L)) {
    idx <- which(meta$geometry_type == type & has_coords)
    if (!length(idx)) next
    out[idx] <- unclass(densify_type(g[idx], type, max_length))
  }
  wk::wkb(out, crs = crs)
}

## The most vertices vector_densify() makes in one call.
densify_max <- 1e8

## Densify geometries that are all of one type (2, 3, 5 or 6), none empty,
## and rebuild them as that type, one feature per input.
densify_type <- function(g, type, max_length) {
  g <- wk::wk_drop_m(wk::wk_drop_z(g))
  co <- wk::wk_coords(g)
  n <- nrow(co)
  f <- co$feature_id
  p <- co$part_id
  r <- co$ring_id
  ## A segment runs from row i to row i + 1 within one ring or line.
  same <- c(f[-1L] == f[-n] & p[-1L] == p[-n] & r[-1L] == r[-n], FALSE)
  dx <- c(diff(co$x), 0)
  dy <- c(diff(co$y), 0)
  dx[!same] <- 0
  dy[!same] <- 0
  if (!all(is.finite(co$x) & is.finite(co$y))) {
    stop("`x` has coordinates that are not finite, so it cannot be densified.", call. = FALSE)
  }
  pieces <- ceiling(sqrt(dx[same]^2 + dy[same]^2) / max_length)
  if (sum(pieces) > densify_max) {
    stop("Densifying every ", max_length, " would make more than ",
         format(densify_max, big.mark = ",", scientific = FALSE),
         " vertices; use a larger `max_length`.", call. = FALSE)
  }
  k <- rep(1L, n)
  k[same] <- pmax(1L, as.integer(pieces))
  i <- rep(seq_len(n), k)
  t <- (sequence(k) - 1) / rep(k, k)
  xy <- wk::xy(co$x[i] + t * dx[i], co$y[i] + t * dy[i])
  f <- f[i]
  p <- p[i]
  r <- r[i]
  ## Rebuild multi types part by part, then gather the parts per feature.
  multi <- type %in% c(5L, 6L)
  part <- if (multi) cumsum(c(TRUE, f[-1L] != f[-length(f)] | p[-1L] != p[-length(p)])) else f
  single <- if (type %in% c(2L, 5L)) {
    wk::wk_linestring(xy, feature_id = part)
  } else {
    wk::wk_polygon(xy, feature_id = part, ring_id = r)
  }
  if (!multi) return(single)
  part_feature <- f[!duplicated(part)]
  wk::wk_collection(single, geometry_type = type, feature_id = part_feature)
}
