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
  unit <- encodeUnit(unit)
  dimension <- quantityOrDimension
  dimensionTask <- getNetTask("DimensionTask")

  if (isOfType(quantityOrDimension, Quantity)) {
    dimension <- quantityOrDimension$dimension
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }

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
  targetUnit <- encodeUnit(targetUnit)
  dimension <- quantityOrDimension
  values <- c(values)
  dimensionTask <- getNetTask("DimensionTask")

  if (isOfType(quantityOrDimension, Quantity)) {
    dimension <- quantityOrDimension$dimension
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }

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
  unit <- encodeUnit(unit)
  dimensionTask <- getDimensionTask()
  dim <- rClr::clrCall(dimensionTask, "DimensionForUnit", unit)
  ifNotNull(dim, rClr::clrGet(dim, "Name"))
}

#' Returns a vector containing all units defined in the dimension
#'
#' @param dimension Name of dimension for which units should be returned
#'
#' @examples
#' units <- getUnitsForDimension("Mass")
#' @export
getUnitsForDimension <- function(dimension) {
  validateIsString(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "AllAvailableUnitNamesForDimension", dimension)
}


#' Return an instance of the .NET Task `DimensionTask`
#' This is purely for optimization purposes
#'
#' @return An instance of the Task
getDimensionTask <- function() {
  dimensionTask <- ospsuiteEnv$dimensionTask
  if (is.null(dimensionTask)) {
    dimensionTask <- getNetTask("DimensionTask")
    ospsuiteEnv$dimensionTask <- dimensionTask
  }
  return(dimensionTask)
}



#'Loop through dimensions and build a list containing an enum of all units available for each dimension
#' @return enum of all units for each dimension
#' @export
getUnitsEnum <- function(){
  dimensions <- allAvailableDimensions()[!(allAvailableDimensions() %in% c("CV mmHg*s²/ml","Time²","Flow²"))]
  units <- lapply(dimensions,function(dimension){enum(getUnitsForDimension(dimension = dimension))})
  names(units) <- dimensions
  return(units)
}

#' #'Function to return an enum of all available dimensions
#' @return enum of all dimensions
#' @export
getDimensionsEnum <- function(){
   dimensions <- enum(allAvailableDimensions()[!(allAvailableDimensions() %in% c("CV mmHg*s²/ml","Time²","Flow²"))])
   return(dimensions)
}
