
#' Converts a list of .NET \code{ParameterValue} into a list with 3 entries: `paths`, `values`, and `percentiles`.
#'
#' @param netParameterValues List of.NET \code{ParameterValue}
#'
#' @return A list with 3 sublist: `paths`, `values`, and `percentiles` containing the corresponding values from each parameter value
parameterValueListFrom <- function(netParameterValues) {
  list(
    paths = getPropertyValues(netParameterValues, "ParameterPath"),
    values = getPropertyValues(netParameterValues, "Value")
  )
}
