#' Transforms a single .NET object  or a list of .NET Object to their corresponding wrapper class in R.
#' Note that if the object is a single object, NULL will be returned if the .NET object is null. This allows semantic equivalence between .NET and R
#'
#' @param netObject The .NET object instances (single or list) to wrap
#' @param class The class definition that will be used to convert the parameter
#'
#' @return The wrapped object (single or list)
#' @keywords internal
toObjectType <- function(netObject, class) {
  if (!is.list(netObject)) {
    return(ifNotNull(netObject, class$new(ref = netObject)))
  }
  sapply(c(netObject), function(x) {
    class$new(ref = x)
  })
}


#' This is required to ensure that we have no issue using the mu symbol in different OS
#' See https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/476 for details
#' @param unit Unit to encode
#' @import stringr
#' @keywords internal
encodeUnit <- function(unit) {
  mu <- ospsuiteEnv$muSymbol
  unit <- enc2utf8(unit)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xce, 0xbc))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xc2, 0xb5))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(0xb5)), mu)
}

#' Retrieves the name of the constant in the specified enumeration that has the specified value.
#'
#' @inheritParams rClr::clrGetEnumNames
#' @param enumValue The value of a particular enumerated constant in terms of its underlying type. Typically an integer.
#'
#' @return A string containing the name of the enumerated constant in enumType whose value is enumValue; or null if no such constant is found.
#' @keywords internal
netEnumName <- function(enumType, enumValue) {
  netTypeObj <- rClr::clrGetType(enumType)
  rClr::clrCallStatic("System.Enum", methodName = "GetName", netTypeObj, enumValue)
}


#' Clears the memory used by all underlying objects
#' @details The function aims at clearing the memory used by object references
#' allocated during some workflows. The memory should typically be freed automatically.
#' when the system is under memory pressure or when the garbage collection is kicking
#' in. However it may be necessary sometimes to explicitly start the garbage collection
#' process
#'
#' @param clearSimulationsCache optional - Should the simulation cache also be cleared? Default is `FALSE`
#'
#' @examples
#' # This will clear the memory and also clear the simulations cache but leave
#' # the environment intact.
#' clearMemory(clearSimulationsCache = TRUE)
#'
#' @export
clearMemory <- function(clearSimulationsCache = FALSE) {
  if (clearSimulationsCache) {
    resetSimulationCache()
  }

  # performs R GC first
  gc()

  # then forces .NET garbage collection
  rClr::clrCallStatic("OSPSuite.R.Api", "ForceGC")
  invisible()
}
