#' @keywords internal
"_PACKAGE"

## Importing from geoarrow loads its namespace with this one, which
## registers the geoarrow Arrow extension types with nanoarrow, so streams
## read here convert geometry columns to geoarrow vectors.
#' @importFrom geoarrow as_geoarrow_vctr
NULL

## htmltools is declared for the embed transport, which is not written yet;
## this import keeps R CMD check from flagging it as unused until then.
#' @importFrom htmltools tags
NULL
