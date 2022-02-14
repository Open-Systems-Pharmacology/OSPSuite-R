
#' Converts a list of .NET `ParameterValue` into a list with 2 entries: `paths`, `values`.
#' A 3rd optional entry `units` will be defined if the parameter `addUnits` is set to `TRUE`.
#' Note: Units are only available for .NET object of type `ParameterValueWithUnit`
#'
#' @param netParameterValues List of.NET `ParameterValue` or `ParameterValueWithUnit`
#' @param  addUnits If `TRUE`, a a third list will be returned containing the units in which the parameters are defined. Default is `FALSE`
#'
#' @return A list with 3 sublist: `paths`, `values`, and optionally `units` containing the corresponding values from each parameter value
parameterValueListFrom <- function(netParameterValues, addUnits = FALSE) {
  parameterList <- list(
    paths = getPropertyValues(netParameterValues, "ParameterPath"),
    values = getPropertyValues(netParameterValues, "Value")
  )

  if (addUnits) {
    parameterList$units <- getPropertyValues(netParameterValues, "Unit")
  }

  return(parameterList)
}
