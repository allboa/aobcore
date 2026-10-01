#' @keywords internal
"_PACKAGE"

## Importing from geoarrow loads its namespace with this one, which
## registers the geoarrow Arrow extension types with nanoarrow, so streams
## read here convert geometry columns to geoarrow vectors.
#' @importFrom geoarrow as_geoarrow_vctr
NULL

## Every server serve_scene() started stops when the session ends (a
## finalizer run at exit) and when aobcore is unloaded (decision 0006).
.onLoad <- function(libname, pkgname) {
  reg.finalizer(servers, function(e) {
    for (s in mget(ls(e), envir = e)) try(s$stop(), silent = TRUE)
  }, onexit = TRUE)
  invisible()
}

.onUnload <- function(libpath) {
  for (s in mget(ls(servers), envir = servers)) try(s$stop(), silent = TRUE)
  invisible()
}
