#' Transforms a single .NET object  or a list of .NET Object to their correpsonding wrapper class in R.
#' Note that if the object is a single object, NULL will be returned if the .NET object is null. This allows semantic equivalence between .NET and R
#'
#' @param netObject The .NET object instances (single or list) to wrapp
#' @param class The class definition that will be used to convert the parameter
#'
#' @return The wrapped object (single or list)
toObjectType <- function(netObject, class) {
  if (!is.list(netObject)) {
    return(ifNotNull(netObject, class$new(ref = netObject)))
  }
  sapply(c(netObject), function(x) {
    class$new(ref = x)
  })
}


# Convenience function to avoid testing for null. It returns the first object if it is not null otherwise the second object
#
# @param lhs Object that will be returned if not NULL
# @param rhs Object that will be returned if \code{lhs} is NULL. It maybe well be NULL
#
# @return The first parameter if it is not NULL otherwise the second parameter
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
ifNotNull <- function(condition, outputIfNotNull, outputIfNull = NULL) {
  if (!is.null(condition)) {
    outputIfNotNull
  } else {
    outputIfNull
  }
}

#' This is required to ensure that we have no issue using the mu symbol in different OS
#' See https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/476 for details
#' @import stringr
encodeUnit <- function(unit) {
  
  #TODO maybe we don't even need to load from .NET
  # dimensionTask <- getNetTask("DimensionTask")
  # ospsuiteEnv$muSymbol <- rClr::clrGet(dimensionTask, "MuSymbol")
  mu <- ospsuiteEnv$muSymbol
  unit <- enc2utf8(unit)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xce, 0xbc))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xc2, 0xb5))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(0xb5)), mu)
}
