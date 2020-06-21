
#' Returns a list containing all properties `propertyName` from the .NET objects `netObjects`
#'
#' @param netObjects List of .NET object
#' @param  propertyName Property name that should be retrieved from the `netObjects`
getPropertyValues <- function(netObjects, propertyName) {
  sapply(netObjects, function(x) getPropertyValue(x, propertyName))
}

#' Returns the value of property named `propertyName` from .NET object instance `netObject`
#'
#' @param netObject .NET object
#' @param  propertyName Property name that should be retrieved from the `netObject`
getPropertyValue <- function(netObject, propertyName) {
  rClr::clrGet(netObject, name = propertyName)
}
