#' Returns a list containing all properties `propertyName` from the .NET objects `netObjects`
#'
#' @param netObjects List of .NET object
#' @param  propertyName Property name that should be retrieved from the `netObjects`
#' @keywords internal
.getPropertyValues <- function(netObjects, propertyName) {
  sapply(netObjects, function(x) .getPropertyValue(x, propertyName))
}

#' Returns the value of property named `propertyName` from .NET object instance `netObject`
#'
#' @param netObject .NET object
#' @param  propertyName Property name that should be retrieved from the `netObject`
#' @keywords internal
.getPropertyValue <- function(netObject, propertyName) {
  netObject$get(name = propertyName)
}
