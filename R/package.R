#' @keywords internal
"_PACKAGE"

## Importing from geoarrow loads its namespace with this one, which
## registers the geoarrow Arrow extension types with nanoarrow, so streams
## read here convert geometry columns to geoarrow vectors.
#' @importFrom geoarrow as_geoarrow_vctr
NULL
