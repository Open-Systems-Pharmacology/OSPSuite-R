#' Transforms a single .NET object  or a list of .NET Object to their correpsonding wrapper class in R.
#' Note that if the object is a single object, NULL will be returned if the .NET object is null. This allows semantic equivalence between .NET and R
#'
#' @param netObject The .NET object instances (single or list) to wrapp
#' @param class The class definition that will be used to convert the parameter
#'
#' @return The wrapped object (single or list)
toObjectType <- function(netObject, class) {
  if (!is.list(netObject)) {
    return(if (is.null(netObject)) NULL else class$new(netObject))
  }
  sapply(c(netObject), function(x) {
    class$new(x)
  })
}



`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}


#' Mimic the ternary operator  \code{a ? x : y} behavior in other languages
#' If \code{condition} is not null, returns \code{outputIfNotNull} otherwise \code{outputIfNull}
#'
#' @param condition The .NET object instances (single or list) to wrapp
#' @param outputIfNotNull The class definition that will be used to convert the parameter
#' @param outputIfNull The class definition that will be used to convert the parameter
ifNotNull <- function(condition, outputIfNotNull, outputIfNull) {
  if (!is.null(condition)) {
    outputIfNotNull
  } else {
    outputIfNull
  }
}
