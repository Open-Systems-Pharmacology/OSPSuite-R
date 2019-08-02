
#' Create an enumeration to be used instead of arbitraty values in code.
#'
#' @param enumValues
#'
#' @return the Enum created
#' @examples
#'
#' # Without predefined values
#' Color <- enum(c("Red", "Blue", "Green"))
#' myColor <- Color$Red
#'
#' # With predefined values
#' Symbol <- enum(c(Diamond = 1, Triangle = 2, Circle = 2))
#' mySymbol <- Symbol$Diamond
enum <- function(enumValues) {
  myEnum <- as.list(enumValues)
  enumNames <- names(myEnum)
  if (is.null(enumNames)) {
    names(myEnum) <- myEnum
  } else if ("" %in% enumNames) {
    stop("The enumValues has some but not all names assigned. They must be all assigned or none assigned")
  }
  return(myEnum)
}
