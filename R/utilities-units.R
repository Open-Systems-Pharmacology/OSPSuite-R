R_WITH_DIMENSION_EXTENSION <- "OSPSuite.R.Extensions.WithDimensionExtensions"

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
  validateIsOfType(quantity, Quantity)
  validateIsNumeric(values)
  validateHasUnit(quantity, unit)
  unitIndex <-  as.integer(quantity$unitIndexOf(unit))
  values <- c(values)
  sapply(values, function(value) {
    rClr::clrCallStatic(R_WITH_DIMENSION_EXTENSION, "ConvertToBaseUnit", quantity$ref, value,unitIndex)
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
  validateIsOfType(quantity, Quantity)
  validateIsNumeric(values)
  validateHasUnit(quantity, targetUnit)
  targetUnitIndex <-  as.integer(quantity$unitIndexOf(targetUnit))
  values <- c(values)
  sapply(values, function(value) {
    rClr::clrCallStatic(R_WITH_DIMENSION_EXTENSION, "ConvertToUnit", quantity$ref, value,targetUnitIndex)
  })
}

#' Converts a value given in base unit of a quantity into the display unit of a quantity
#'
#' @param quantity Instance of a quantity from which the base unit will be retrieved
#' @param values Value in base unit (single or vector)
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' par <- getParameter("Organism|Liver|Volume", sim)
#'
#' # Converts the value in base unit (1L) to display unit
#' valueInMl <- toDisplayUnit(par, 1)
#'
#' valuesInDisplayUnit <- toDisplayUnit(par, c(1, 5, 5))
#' @export
toDisplayUnit <- function(quantity, values) {
  validateIsOfType(quantity, Quantity)
  toUnit(quantity, values, quantity$displayUnit)
}
