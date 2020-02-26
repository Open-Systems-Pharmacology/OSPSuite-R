WITH_DIMENSION_EXTENSION <- "OSPSuite.Core.Domain.WithDimensionExtensions"

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
  unit <- enc2utf8(unit)
  validateHasUnit(quantity, unit)
  values <- c(values)
  sapply(values, function(value) {
    rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "ConvertToBaseUnit", quantity$ref, value, unit)
  })
}


#' Converts a value given in base unit of a quantity into a target unit
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Value in base unit (single or vector)
#' @param targetUnit Unit to convert to
#' @param molWeight Optional molecule weight to use when converting from molar to mass amount or concentration
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
toUnit <- function(quantityOrDimension, values, targetUnit, molWeight = NULL) {
  validateIsOfType(quantityOrDimension, c(Quantity, "character"))
  validateIsNumeric(values)
  validateIsNumeric(molWeight, nullAllowed =  TRUE)
  targetUnit <- enc2utf8(targetUnit)
  dimension <- quantityOrDimension
  if(isOfType(quantityOrDimension, Quantity))
    dimension<- quantityOrDimension$dimension

  values <- c(values)
  dimensionTask <- getNetTask("DimensionTask")
  if(is.null(molWeight))
    rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values)
  else
    rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values, molWeight)
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


#' Returns the name of all available dimensions defined in the OSPSuite platform
#'
#' @examples
#' dims <- allAvailableDimensions()
#'
#' @export
allAvailableDimensions <- function(){
  dimensionTask <- getNetTask("DimensionTask")
  rClr::clrCall(dimensionTask, "AllAvailableDimensionNames")
}
