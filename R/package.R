#' @keywords internal
"_PACKAGE"

## Importing from geoarrow loads its namespace with this one, which
## registers the geoarrow Arrow extension types with nanoarrow, so streams
## read here convert geometry columns to geoarrow vectors.
#' @importFrom geoarrow as_geoarrow_vctr
NULL

## Every server serve_scene() started stops when the session ends (a
## finalizer run at exit) and when aobcore is unloaded (decision 0006).
## At exit the pages' sockets are dropped, not closed: httpuv finalizes
## each socket's handle at exit too, and may already have, so calling
## ws$close() then crashes R (httpuv 1.6.17). Unloading during a session
## stops them as stop() does, closing each socket with 1001.
.onLoad <- function(libname, pkgname) {
  reg.finalizer(servers, function(e) {
    for (s in mget(ls(e), envir = e)) try(server_stop(s$state, close_sockets = FALSE), silent = TRUE)
  }, onexit = TRUE)
  invisible()
}

.onUnload <- function(libpath) {
  for (s in mget(ls(servers), envir = servers)) try(s$stop(), silent = TRUE)
  invisible()
}
