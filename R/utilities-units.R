withDimensionExtensions <- "OSPSuite.Core.Domain.WithDimensionExtensions"

#' Converts a value given in a specified unit into the base unit of a quantity
#'
#' @param quantity Instance of a quantity from which the base unit will be retrieved
#' @param values Value in unit (single or vector)
#' @param unit Unit of value
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter("Organism|Liver|Volume", sim)
#'
#' # Converts the value in unit (1000 ml) to the base unit (l) => 1
#' valueInBaseUnit <- toBaseUnit(par, 1000, "ml")
#'
#' valuesInBaseUnit <- toBaseUnit(par, c(1000, 2000, 3000), "ml")
#' @export
toBaseUnit <- function(quantity, values, unit) {
  validateIsNumeric(values)
  validateHasUnit(quantity, unit)

  values <- c(values)
  sapply(values, function(value) {
    rClr::clrCallStatic(withDimensionExtensions, "ConvertToBaseUnit", quantity$ref, value, unit)
  })
}


#' Converts a value given in base unit of a quantity into a target unit
#'
#' @param quantity Instance of a quantity from which the base unit will be retrieved
#' @param values Value in base unit (single or vector)
#' @param targetUnit Unit to convert to
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter("Organism|Liver|Volume", sim)
#'
#' # Converts the value in base unit (1L) to ml => 1000
#' valueInMl <- toUnit(par, 1, "ml")
#'
#' valuesInMl <- toUnit(par, c(1, 5, 5), "ml")
#' @export
toUnit <- function(quantity, values, targetUnit) {
  validateIsNumeric(values)
  validateHasUnit(quantity, targetUnit)
  values <- c(values)
  sapply(values, function(value) {
    rClr::clrCallStatic(withDimensionExtensions, "ConvertToUnit", quantity$ref, value, targetUnit)
  })
}
