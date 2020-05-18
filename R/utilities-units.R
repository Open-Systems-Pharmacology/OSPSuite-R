#' Converts a value given in a specified unit into the base unit of a quantity
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Value in unit (single or vector)
#' @param unit Unit of value
#' @param molWeight Optional molecule weight to use when converting, for example,  from molar to mass amount or concentration

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
toBaseUnit <- function(quantityOrDimension, values, unit, molWeight = NULL) {
  validateIsOfType(quantityOrDimension, c(Quantity, "character"))
  validateIsNumeric(values)
  validateIsNumeric(molWeight, nullAllowed = TRUE)
  unit <- enc2utf8(unit)
  dimension <- quantityOrDimension
  if (isOfType(quantityOrDimension, Quantity)) {
    dimension <- quantityOrDimension$dimension
  }
  values <- c(values)
  dimensionTask <- getNetTask("DimensionTask")
  if (is.null(molWeight)) {
    rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, unit, values)
  } else {
    rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, unit, values, molWeight)
  }
}


#' Converts a value given in base unit of a quantity into a target unit
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Value in base unit (single or vector)
#' @param targetUnit Unit to convert to
#' @param molWeight Optional molecule weight to use when converting, for example,  from molar to mass amount or concentration
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
  validateIsNumeric(molWeight, nullAllowed = TRUE)
  targetUnit <- enc2utf8(targetUnit)
  dimension <- quantityOrDimension
  if (isOfType(quantityOrDimension, Quantity)) {
    dimension <- quantityOrDimension$dimension
  }

  values <- c(values)
  dimensionTask <- getNetTask("DimensionTask")
  if (is.null(molWeight)) {
    rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values)
  } else {
    rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values, molWeight)
  }
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
#' @export
allAvailableDimensions <- function() {
  dimensionTask <- getNetTask("DimensionTask")
  rClr::clrCall(dimensionTask, "AllAvailableDimensionNames")
}

#' Returns the name of dimension that can be used to support the given unit or null if the dimension cannot be found
#'
#' @param unit Unit used to find the corresponding dimension
#'
#' @examples
#' dim <- getDimensionForUnit("mg")
#' @export
getDimensionForUnit <- function(unit) {
  validateIsString(unit)
  unit <- enc2utf8(unit)
  dimensionTask <- getNetTask("DimensionTask")
  dim <- rClr::clrCall(dimensionTask, "DimensionForUnit", unit)
  ifNotNull(dim, rClr::clrGet(dim, "Name"))
}
