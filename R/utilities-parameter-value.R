
#' Converts a list of .NET \code{ParameterValue} into a list with 2 entries: `paths`, `values`.
#' A 3rd optional entry `units` will be defined if the parameter `addUnits`` is set to \code{TRUE}.
#' Note: Untis are only available for .NET object of type \code{ParameterValueWithUnit}
#'
#' @param netParameterValues List of.NET \code{ParameterValue} or \code{ParameterValueWithUnit}
#'
#' @return A list with 3 sublist: `paths`, `values`, and optionally `units` containing the corresponding values from each parameter value
parameterValueListFrom <- function(netParameterValues, addUnits = FALSE) {
  parameterList <- list(
    paths = getPropertyValues(netParameterValues, "ParameterPath"),
    values = getPropertyValues(netParameterValues, "Value")
  )

  if(addUnits){
    parameterList <- c(parameterList, list(units = getPropertyValues(netParameterValues, "Unit")))
  }

  return(parameterList)
}
