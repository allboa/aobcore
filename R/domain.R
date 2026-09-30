#' The default view domain of a CRS
#'
#' How much of a view CRS is worth showing, measured from its centre
#' (allboa/design decision 0005). Starting at the projection centre,
#' `crs_domain()` walks outward along `bearings` great-circle directions in
#' `step` degree steps and stops, on each, at the first step that the
#' projection cannot transform or stretches more than `k` times the first
#' step on that bearing. The domain is the region the end points enclose; its
#' bounding box is the default view of a scene with no data and the region
#' its camera is kept within ([scene()]'s `domain`, `view.bounds` in scene
#' spec 0.4).
#'
#' A projection that never passes the stretch limit (seams aside: a single
#' long step, such as a jump to the far edge of a world map, only ends the
#' walk) is `bounded` and keeps
#' its natural edge: Lambert azimuthal equal area gives the whole-Earth disc,
#' orthographic stops at the horizon. A divergent one is cut where it becomes
#' unreasonable: with `k = 2`, south polar stereographic ends just past the
#' equator, Web Mercator at 60 degrees north and south. Pseudocylindrical
#' world maps (Mollweide, Robinson, Equal Earth) stretch near their outer
#' edge past the poles, so they report `bounded = FALSE` although their
#' extent is still the whole map.
#'
#' The centre is the CRS's natural origin: its false easting and northing
#' taken back to longitude and latitude (a pole for a polar CRS). A
#' geographic CRS is measured in degrees, so its domain is the whole world,
#' `c(-180, 180, -90, 90)`. Requires the 'gdalraster' package.
#'
#' The domain limits the camera only. Data outside it are still planned and
#' drawn when the camera reaches them.
#'
#' @param crs A view CRS, as [scene_crs()] accepts.
#' @param k The largest stretch along a bearing, relative to the centre, that
#'   stays in the domain. Must be above 1.
#' @param bearings The number of directions to walk.
#' @param step The step along each bearing, in degrees of arc.
#' @return A list of class `"aob_domain"`: `crs`, `k`, `centre` (the centre
#'   in CRS units), `centre_lonlat`, `extent` (`c(xmin, xmax, ymin, ymax)` in
#'   CRS units), `outline` (a matrix of the end points, one row per bearing),
#'   `reach` (the angular distance reached on each bearing, degrees) and
#'   `bounded`.
#' @export
#' @examplesIf requireNamespace("gdalraster", quietly = TRUE) && !inherits(try(gdalraster::srs_to_wkt("EPSG:3031"), silent = TRUE), "try-error")
#' crs_domain("EPSG:3031")
#' crs_domain("+proj=laea +lat_0=-90 +lon_0=147 +datum=WGS84")
crs_domain <- function(crs, k = 2, bearings = 72L, step = 0.5) {
  need_gdalraster("crs_domain()")
  crs <- scene_crs(crs)
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || !(k > 1)) {
    stop("`k` must be a single number above 1.", call. = FALSE)
  }
  bearings <- as.integer(bearings)
  if (length(bearings) != 1L || is.na(bearings) || bearings < 4L) {
    stop("`bearings` must be a single whole number, 4 or more.", call. = FALSE)
  }
  if (!is.numeric(step) || length(step) != 1L || is.na(step) || !(step > 0 && step <= 10)) {
    stop("`step` must be a single number of degrees above 0 and at most 10.", call. = FALSE)
  }
  wkt <- crs_wkt(crs)
  if (isTRUE(gdalraster::srs_is_geographic(wkt))) {
    return(new_domain(crs, k, c(0, 0), c(0, 0), c(-180, 180, -90, 90),
                      cbind(c(-180, 180, 180, -180), c(-90, -90, 90, 90)),
                      rep(180, 4L), TRUE))
  }
  ll <- gdalraster::srs_to_wkt("OGC:CRS84")
  fwd <- function(lonlat) {
    tryCatch(suppressMessages(suppressWarnings(gdalraster::transform_xy(lonlat, ll, wkt))),
             error = function(e) matrix(NA_real_, nrow(lonlat), 2L))
  }
  inv <- function(xy) {
    tryCatch(suppressMessages(suppressWarnings(gdalraster::transform_xy(xy, wkt, ll))),
             error = function(e) matrix(NA_real_, nrow(xy), 2L))
  }

  origin <- crs_false_origin(crs)
  c_ll <- inv(matrix(origin, 1L))
  if (!all(is.finite(c_ll))) c_ll <- inv(matrix(0, 1L, 2L))
  if (!all(is.finite(c_ll))) {
    stop("Cannot find the centre of ", crs_label(crs), ".", call. = FALSE)
  }
  c_ll <- as.numeric(c_ll)
  c_xy <- as.numeric(fwd(matrix(c_ll, 1L)))

  d <- seq(0, 180, by = step)
  b <- seq(0, 2 * pi, length.out = bearings + 1L)[-(bearings + 1L)]
  pts <- sphere_destination(c_ll, rep(b, each = length(d)), rep(d, bearings))
  xy <- fwd(pts)
  n <- length(d)
  outline <- matrix(NA_real_, bearings, 2L)
  reach <- numeric(bearings)
  stretched <- logical(bearings)
  for (i in seq_len(bearings)) {
    p <- xy[(i - 1L) * n + seq_len(n), , drop = FALSE]
    seg <- sqrt(diff(p[, 1])^2 + diff(p[, 2])^2)
    ok <- is.finite(seg)
    over <- ok & seg > k * seg[1]
    stop_at <- which(!ok | over)
    last <- if (length(stop_at)) stop_at[1] else n
    if (!is.finite(seg[1]) || last < 2L) {
      stop("Cannot measure ", crs_label(crs), " around its centre.", call. = FALSE)
    }
    ## A stop counts against `bounded` only when the stretch lasts: one
    ## long step with normal steps after it is a seam (the edge of a world
    ## map), not divergence.
    j <- stop_at[1]
    stretched[i] <- length(stop_at) > 0L && over[j] &&
      (j == length(seg) || !isTRUE(seg[j + 1L] <= k * seg[1]))
    outline[i, ] <- p[last, ]
    reach[i] <- d[last]
  }
  pts_all <- rbind(outline, c_xy)
  extent <- c(range(pts_all[, 1]), range(pts_all[, 2]))
  new_domain(crs, k, c_xy, c_ll, extent, outline, reach, !any(stretched))
}

#' @export
print.aob_domain <- function(x, ...) {
  e <- x$extent
  cat("<domain> ", crs_label(x$crs), ", k = ", x$k, if (x$bounded) ", bounded", "\n", sep = "")
  cat("  centre ", format(x$centre_lonlat[1], digits = 6), ", ",
      format(x$centre_lonlat[2], digits = 6), " (lon, lat)\n", sep = "")
  cat("  extent x ", format(e[1], digits = 6), " to ", format(e[2], digits = 6),
      ", y ", format(e[3], digits = 6), " to ", format(e[4], digits = 6), "\n", sep = "")
  cat("  reach ", min(x$reach), " to ", max(x$reach), " degrees from the centre\n", sep = "")
  invisible(x)
}

## ---- internals -------------------------------------------------------------

new_domain <- function(crs, k, centre, centre_lonlat, extent, outline, reach, bounded) {
  structure(list(crs = crs, k = k, centre = as.numeric(centre),
                 centre_lonlat = as.numeric(centre_lonlat), extent = as.numeric(extent),
                 outline = unname(outline), reach = reach, bounded = bounded),
            class = "aob_domain")
}

## The false easting and northing of a projected CRS (0, 0 when it has
## none), read from its PROJJSON conversion parameters.
crs_false_origin <- function(crs) {
  j <- gdalraster::srs_to_projjson(crs_wkt(crs))
  value <- function(names) {
    re <- paste0("\"name\"[[:space:]]*:[[:space:]]*\"(", paste(names, collapse = "|"),
                 ")\"[[:space:]]*,[[:space:]]*\"value\"[[:space:]]*:[[:space:]]*([-+0-9.eE]+)")
    m <- regmatches(j, regexec(re, j))[[1]]
    if (length(m) == 3L) as.numeric(m[3]) else 0
  }
  c(value(c("False easting", "Easting at false origin", "Easting at projection centre")),
    value(c("False northing", "Northing at false origin", "Northing at projection centre")))
}

## Points at angular distance `d` (degrees) along bearing `b` (radians,
## clockwise from north) from `from` (lon, lat in degrees), on a sphere. At
## a pole, bearings are meridians.
sphere_destination <- function(from, b, d) {
  rad <- pi / 180
  lat1 <- from[2] * rad
  lon1 <- from[1] * rad
  dr <- d * rad
  if (abs(from[2]) >= 90 - 1e-9) {
    lat2 <- sign(from[2]) * (pi / 2 - dr)
    lon2 <- b
  } else {
    lat2 <- asin(pmin(1, pmax(-1, sin(lat1) * cos(dr) + cos(lat1) * sin(dr) * cos(b))))
    lon2 <- lon1 + atan2(sin(b) * sin(dr) * cos(lat1), cos(dr) - sin(lat1) * sin(lat2))
  }
  lon <- ((lon2 / rad + 180) %% 360) - 180
  cbind(lon, lat2 / rad)
}
