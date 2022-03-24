#' Deprecated functions
#'
#' @param ... Arguments to the deprecated function.
#'
#' @details
#' - `pkAnalysesAsDataFrame` is now [`pkAnalysesToDataFrame`].
#' - `populationAsDataFrame` is now [`populationToDataFrame`].
#'
#' @rdname ospsuite_deprecated
#' @name ospsuite_deprecated
NULL


#' @rdname ospsuite_deprecated
#' @export
pkAnalysesAsDataFrame <- function(...) {
  .Deprecated("pkAnalysesToDataFrame")
  pkAnalysesToDataFrame(...)
}

#' @rdname ospsuite_deprecated
#' @export
populationAsDataFrame <- function(...) {
  .Deprecated("populationToDataFrame")
  populationToDataFrame(...)
}
