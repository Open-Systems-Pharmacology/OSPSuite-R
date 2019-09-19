withDimensionExtensions <- "OSPSuite.Core.Domain.WithDimensionExtensions"


#' Converts a value given in base unit of a quantity into a target unit
#'
#' @param quantity Instance of a quantity from which the base unit will be retrieved
#' @param value Value in base unit.
#' @param targetUnit Unit to convert to
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter(sim, "Organism|Liver|Volume")
#'
#' # Converts the value in base unit (1L) to ml => 1000
#' valueInMl <- toUnit(par, 1, "ml")
#' @export
toUnit <- function(quantity, value, targetUnit) {
  validateIsNumeric(value)
  validateHasUnit(quantity, targetUnit)

  rClr::clrCallStatic(withDimensionExtensions, "ConvertToUnit", quantity$ref, value, targetUnit)
}

#' Converts a value given in a specified unit into the base unit of a quantity
#'
#' @param quantity Instance of a quantity from which the base unit will be retrieved
#' @param value Value in unit.
#' @param targetUnit Unit of value
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter(sim, "Organism|Liver|Volume")
#'
#' # Converts the value in unit (1000 ml) to the base unit (l) => 1
#' valueInBaseUnit <- toBaseUnit(par, 1000, "ml")
#' @export
toBaseUnit <- function(quantity, value, unit) {
  validateIsNumeric(value)
  validateHasUnit(quantity, unit)

  rClr::clrCallStatic(withDimensionExtensions, "ConvertToBaseUnit", quantity$ref, value, unit)
}
