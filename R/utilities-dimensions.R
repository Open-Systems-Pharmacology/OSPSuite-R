#' Dimension existence
#'
#' @param dimension String name of the dimension.
#' @details Returns \code{TRUE} if the provided dimension is supported otherwise \code{FALSE}
#' @export
hasDimension <- function(dimension) {
  validateIsString(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "HasDimension", enc2utf8(dimension))
}

#' Validate dimension
#'
#' @param dimension String name of the dimension.
#' @details Check if the provided dimension is supported. If not, throw an error
#' @export
validateDimension <- function(dimension) {
  validateIsString(dimension)
  if (!existsDimension(dimension)) {
    stop(messages$errorDimensionNotSupported(dimension))
  }
}

#' Unit existence
#'
#' @param unit String name of the unit
#' @param dimension String name of the dimension.
#' @details Check if the unit is valid for the dimension.
#' @export
hasUnit <- function(unit, dimension) {
  validateIsString(unit)
  validateDimension(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "HasUnit", enc2utf8(dimension),  enc2utf8(unit))
}

#' Validate unit
#'
#' @param unit String name of the unit
#' @param dimension String name of the dimension.
#' @details Check if the unit is valid for the dimension. If not, throw an error
#' @export
validateUnit <- function(unit, dimension) {
  if (!hasUnit(unit, dimension)) {
    stop(messages$errorUnitNotSupported(unit, dimension))
  }
}

#' Get base unit of a dimension
#'
#' @param dimension Dimension for which the base unit is returned.
#'
#' @return String name of the base unit.
#' @export
getBaseUnit <- function(dimension) {
  validateDimension(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "BaseUnitFor", enc2utf8(dimension))
}

#' Calculate factor for converting values of one dimension into another
#'
#' @param dimension1 String name of the first dimension
#' @param dimension2 String name fo the second dimension
#' @param mw Double value of the molecular weight of the molecule. Used
#' for conversion between molar and mass concentrations or amounts. Can be \code{NULL}
#' for other dimensions (default)
#'
#' @return A numerical factor the values of the first dimensions must be multiplied by to
#' be converted to second dimension. The conversion assumes values being in base units
#' of both dimensions.
#' If the values cannot be transformed between the dimensions, an error is thrown.
#' @export
dimensionsConversionFactor <- function(dimension1, dimension2, mw = NULL) {
  # No conversion if the dimensions are equall
  validateIsString(c(dimension1, dimension2))
  if (dimension1 == dimension2) {
    return(1)
  }

  # Dimensionless <-> Fraction
  if (dimension1 == Dimensions$Dimensionless) {
    if (dimension2 == Dimensions$Fraction) {
      return(1)
    }
  }
  if (dimension1 == Dimensions$Fraction) {
    if (dimension2 == Dimensions$Dimensionless) {
      return(1)
    }
  }

  # Concentration (molar) <-> Concentration (mass)
  if (dimension1 == Dimensions$`Concentration (molar)`) {
    if (dimension2 == Dimensions$`Concentration (mass)`) {
      if (is.null(mw)) {
        stop(messages$errorCannotConvertDimensionsNoMW(dimension1, dimension2))
      }
      return(1 * 1e-6 * mw * 1e-3)
    }
  }
  if (dimension1 == Dimensions$`Concentration (mass)`) {
    if (dimension2 == Dimensions$`Concentration (molar)`) {
      if (is.null(mw)) {
        stop(messages$errorCannotConvertDimensionsNoMW(dimension1, dimension2))
      }
      return(1 * 1e3 / mw * 1e6)
    }
  }

  # Amount (molar) <-> Mass
  if (dimension1 == Dimensions$Amount) {
    if (dimension2 == Dimensions$Mass) {
      if (is.null(mw)) {
        stop(messages$errorCannotConvertDimensionsNoMW(dimension1, dimension2))
      }
      return(1 * 1e-6 * mw * 1e-3)
    }
  }
  if (dimension1 == Dimensions$Mass) {
    if (dimension2 == Dimensions$Amount) {
      if (is.null(mw)) {
        stop(messages$errorCannotConvertDimensionsNoMW(dimension1, dimension2))
      }
      return(1 * 1e3 / mw * 1e6)
    }
  }

  stop(messages$errorCannotConvertDimensions(dimension1, dimension2))
}

#' Get conversion factor from one unit to another
#'
#' @param fromUnit String name of the unit to convert from
#' @param toUnit String name of the unit to convert to
#' @param dimension Dimension of the units
#'
#' @return Numerical value. A value in \code{fromUnit} multiplied  by the factor
#' corresponds to the value in \code{toUnit}
#' @export
getUnitConversionFactor <- function(fromUnit, toUnit, dimension) {
  validateDimension(dimension)
  validateUnit(fromUnit, dimension)
  validateUnit(toUnit, dimension)

  dimensionTask <- getDimensionTask()
  dimension <- rClr::clrCall(dimensionTask, "DimensionByName", enc2utf8(dimension))
  fromUnitObj <- rClr::clrCall(dimension, "UnitOrDefault", enc2utf8(fromUnit))
  toUnitObj <- rClr::clrCall(dimension, "UnitOrDefault", enc2utf8(toUnit))
  convFac <- rClr::clrCall(dimension, "UnitValueToBaseUnitValue", fromUnitObj, 1)
  convFac <- convFac * rClr::clrCall(dimension, "BaseUnitValueToUnitValue", toUnitObj, 1)
  return(convFac)
}

#' #' Supported dimensions.
#' #'
#' #' @include enum.R
#' #'
#' #' @export
#' Dimensions <- enum(allAvailableDimensions())
