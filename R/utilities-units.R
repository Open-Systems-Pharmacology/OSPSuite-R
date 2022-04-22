#' Dimension existence
#'
#' @param dimension String name of the dimension.
#' @details Returns `TRUE` if the provided dimension is supported otherwise `FALSE`
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
  if (!hasDimension(dimension)) {
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
  rClr::clrCall(dimensionTask, "HasUnit", enc2utf8(dimension), encodeUnit(unit))
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

#' Check if quantity can be represented in the unit
#'
#' @param quantity `Quantity` object
#' @param unit Unit name to check for
#'
#' @return
#' If validations are successful, `NULL` is returned. Otherwise, error is
#' signaled.
validateHasUnit <- function(quantity, unit) {
  validateIsOfType(quantity, "Quantity")
  validateIsString(unit)
  if (quantity$hasUnit(unit)) {
    return()
  }
  stop(messages$errorUnitNotDefined(quantity$name, quantity$dimension, unit))
}

#' Get base unit of a dimension
#'
#' @param dimension Dimension (string name) for which the base unit is returned.
#'
#' @return String name of the base unit.
#' @export
getBaseUnit <- function(dimension) {
  validateDimension(dimension)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "BaseUnitFor", enc2utf8(dimension))
}

#' Converts a value given in a specified unit into the base unit of a quantity
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Value in unit (single or vector)
#' @param unit Unit of value
#' @param molWeight Optional molecule weight to use when converting, for example,  from molar to mass amount or concentration. If `molWeightUnit` is not specified, `molWeight` is assumed to be in kg/µmol
#' @param molWeightUnit Unit of the molecular weight value. If `NULL` (default), kg/µmol is assumed.

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
toBaseUnit <- function(quantityOrDimension, values, unit, molWeight = NULL, molWeightUnit = NULL) {
  validateIsOfType(quantityOrDimension, c("Quantity", "character"))
  validateIsNumeric(values, nullAllowed = TRUE)
  validateIsNumeric(molWeight, nullAllowed = TRUE)
  unit <- encodeUnit(unit)
  dimension <- quantityOrDimension
  dimensionTask <- getNetTask("DimensionTask")

  # covers all NULL or NA
  if (all(is.na(values))) {
    return(values)
  }

  # ensure that we are dealing with an list of values seen as number (and not integer)
  values <- as.numeric(c(values))

  if (isOfType(quantityOrDimension, "Quantity")) {
    dimension <- quantityOrDimension$dimension
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }


  if (is.null(molWeight)) {
    return(rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, unit, values))
  }

  # Convert molWeight value to base unit if a unit is provided
  if (!is.null(molWeightUnit)) {
    molWeight <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", ospDimensions$`Molecular weight`, molWeightUnit, molWeight)
  }
  rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, unit, values, molWeight)
}

#' Converts a value given in base unit of a quantity into a target unit
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#' @param values Values to convert (single or vector). If `sourceUnit` is not specified, `values` are in the base unit of the dimension
#' @param targetUnit Unit to convert to
#' @param sourceUnit Optional Name of the unit to convert from. If `NULL` (default), the values are assumed to be in base unit.
#' @param molWeight Optional molecular weight to use when converting, for example,  from molar to mass amount or concentration. If `molWeightUnit` is not specified, `molWeight` is assumed to be in kg/µmol
#' @param molWeightUnit Optional Unit of the molecular weight value. If `NULL` (default), kg/µmol is assumed.
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
#'
#' # Converts a numerical value in from mmol/l to mg/dl
#' valuesInMgDl <- toUnit(ospDimensions$`Concentration (molar)`, 5,
#'   targetUnit = "mmol/l",
#'   sourceUnit = "mg/dl", molWeight = 180, molWeightUnit = "g/mol"
#' )
#' @export
toUnit <- function(quantityOrDimension,
                   values,
                   targetUnit,
                   sourceUnit = NULL,
                   molWeight = NULL,
                   molWeightUnit = NULL) {
  validateIsOfType(quantityOrDimension, c("Quantity", "character"))
  validateIsNumeric(values, nullAllowed = TRUE)
  validateIsNumeric(molWeight, nullAllowed = TRUE)
  targetUnit <- encodeUnit(targetUnit)
  dimension <- quantityOrDimension
  dimensionTask <- getNetTask("DimensionTask")


  # covers all NULL or NA
  if (all(is.na(values))) {
    return(values)
  }

  if (!is.null(sourceUnit)) {
    sourceUnit <- encodeUnit(sourceUnit)
  }

  if (isOfType(quantityOrDimension, "Quantity")) {
    dimension <- quantityOrDimension$dimension
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }

  # ensure that we are dealing with an list of values seen as number (and not integer)
  values <- as.numeric(c(values))


  if (is.null(molWeight)) {
    # Convert values to base unit first if the source unit is provided
    if (!is.null(sourceUnit)) {
      values <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, sourceUnit, values)
    }

    return(rClr::clrCall(dimensionTask, "ConvertToUnit", dimension, targetUnit, values))
  }

  # Convert molWeight value to base unit if a unit is provided
  if (!is.null(molWeightUnit)) {
    molWeight <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", ospDimensions$`Molecular weight`, molWeightUnit, molWeight)
  }

  # Convert values to base unit first if the source unit is provided
  if (!is.null(sourceUnit)) {
    values <- rClr::clrCall(dimensionTask, "ConvertToBaseUnit", dimension, sourceUnit, values, molWeight)
  }

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
  validateIsOfType(quantity, "Quantity")
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
  rClr::clrCall(dimensionTask, "AllAvailableUnitNamesFor", enc2utf8(dimension))
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

#' Returns the an instance of the dimension with the given name if found or NULL otherwise
#'
#' @param name Name of dimension that should be retrieved
#'
#' @examples
#' dim <- getDimensionByName("Time")
#' @export
getDimensionByName <- function(name) {
  validateIsString(name)
  dimensionTask <- getDimensionTask()
  rClr::clrCall(dimensionTask, "DimensionByName", enc2utf8(name))
}


#' Loop through dimensions and build a list containing an enum of all units available for each dimension
#' @return enum of all units for each dimension
#' @keywords internal
getUnitsEnum <- function() {
  dimensions <- allAvailableDimensions()
  units <- lapply(dimensions, function(dimension) {
    x <- getUnitsForDimension(dimension = dimension)
    return(enum(replace(x, x == "", "Unitless")))
  })
  names(units) <- sapply(dimensions, function(str) {
    str <- gsub(pattern = "[(]", replacement = "[", x = str)
    str <- gsub(pattern = "[)]", replacement = "]", x = str)
  })
  return(units)
}

#' #'Function to return an enum of all available dimensions
#' @return enum of all dimensions
#' @keywords internal
getDimensionsEnum <- function() {
  enum(allAvailableDimensions())
}

#' Supported dimensions defined as a named list
#'
#' ospDimensions$Mass => "Mass"
#' @export
ospDimensions <- list()

#' Supported units defined as a named list of lists
#'
#' ospUnits$Mass$kg => "kg"
#' @export
ospUnits <- list()

initializeDimensionAndUnitLists <- function() {
  # This initializes the two lists in the parent environment which is the package environments
  ospDimensions <<- getDimensionsEnum()
  ospUnits <<- getUnitsEnum()
}


#' Convert data frame to common units
#'
#' @description
#'
#' When multiple (observed and/or simulated) datasets are present in a data
#' frame, they are likely to have different units. This function helps to
#' convert them to a common unit specified by the user.
#'
#' This is especially helpful while plotting since the quantities from different
#' datasets to be plotted on the X-and Y-axis need to have same units to be
#' meaningfully compared.
#'
#' @note
#'
#' Molecular weight is **required** for the conversion between certain
#' dimensions (`Amount`, `Mass`, `Concentration (molar)`, and `Concentration
#' (mass)`). Therefore, if molecular weight is missing for these dimension, the
#' unit conversion will fail.
#'
#' @return A data frame with measurement columns transformed to have common units.
#'
#' @param data A data frame (or a tibble).
#' @param xUnit,yUnit Target units for `xValues` and `yValues`, respectively. If
#'   not specified (`NULL`), first of the existing units in the respective
#'   columns (`xUnit` and `yUnit`) will be selected as the common unit. For
#'   available dimensions and units, see `ospsuite::ospDimensions` and
#'   `ospsuite::ospUnits`, respectively.
#'
#' @seealso toUnit
#'
#' @examples
#'
#' # small dataframe to illustrate the conversion
#' (df <- dplyr::tibble(
#'   dataType = c(rep("simulated", 3), rep("observed", 3)),
#'   xValues = c(0, 14.482, 28.965, 0, 1, 2),
#'   xUnit = "min",
#'   xDimension = "Time",
#'   yValues = c(1, 1, 1, 1, 1, 1),
#'   yUnit = c("mol", "mol", "mol", "g", "g", "g"),
#'   yDimension = c("Amount", "Amount", "Amount", "Mass", "Mass", "Mass"),
#'   yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
#'   yErrorUnit = c("mol", "mol", "mol", "g", "g", "g"),
#'   molWeight = c(10, 10, 20, 20, 20, 10)
#' ))
#'
#' # default conversion
#' ospsuite:::.unitConverter(df)
#'
#' # customizing conversion with specified unit(s)
#' ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$h)
#' ospsuite:::.unitConverter(df, yUnit = ospUnits$Mass$kg)
#' ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$s, yUnit = ospUnits$Amount$mmol)
#'
#' @keywords internal
.unitConverter <- function(data, xUnit = NULL, yUnit = NULL) {

  # target units --------------------------

  # No validation of inputs for this non-exported function.
  # All validation will take place in the `DataCombined` class itself.

  # The observed and simulated data should have the same units for
  # visual/graphical comparison.
  #
  # Therefore, if target units are not specified by the user, we need to choose
  # one ourselves. For no special reason, the first element from a vector of
  # unique units will be selected: one for X-axis, and one for Y-axis, i.e.
  xTargetUnit <- xUnit %||% unique(data$xUnit)[[1]]
  yTargetUnit <- yUnit %||% unique(data$yUnit)[[1]]

  # internal --------------------------

  # The strategy is to split the data frame (using `split()`) for each source
  # unit and carry out conversion separately per data frame. This is the most
  # performant options since there can only be as many expensive calls to
  # `toUnit()`/`{rClr}` as there are source units.
  #
  # The problem occurs when source units are missing (`NA`). The `toUnit()`
  # function can handle them but not `split()`, which would drop the entire
  # section of the data frame corresponding to `NA` and thus there will be loss
  # of data when a data frame is split into a list of data frames.
  #
  # The trick is to create copies of source unit and molecular weight columns
  # and fill in the missing values with something other than `NA`. Using these
  # new columns with `split()` makes sure that the parts of a data frame where
  # source units are missing won't be dropped. Note that the original columns
  # containing source units remain unchanged.
  #
  # These newly created columns are removed before the converted data frame is
  # returned to the user.

  # Add suffix `Split` to the following columns:
  # `xUnit`, `yUnit`, `yErrorUnit`, and `molWeight`
  data <- transform(
    data,
    xUnitSplit = as.character(data$xUnit),
    yUnitSplit = as.character(data$yUnit),
    molWeightSplit = as.character(data$molWeight)
  )

  # Replace missing values in these new columns with `"missing"`, so that
  # `split()` won't remove the corresponding portion of the data frame.
  data$xUnitSplit[is.na(data$xUnitSplit)] <- "missing"
  data$yUnitSplit[is.na(data$yUnitSplit)] <- "missing"
  data$molWeightSplit[is.na(data$molWeightSplit)] <- "missing"

  # Do the same if errors are present
  if ("yErrorValues" %in% names(data)) {
    data <- transform(data, yErrorUnitSplit = as.character(data$yErrorUnit))
    data$yErrorUnitSplit[is.na(data$yErrorUnitSplit)] <- "missing"
  }

  # `split()` will change the row order of the data frame depending on the
  # alphabetical order of the levels of the variable used to split the data
  # frame into a list.
  #
  # Therefore, an internal row identifier is kept to restore the original
  # data frame row order before data is returned.
  data <- dplyr::mutate(data, .rowidInternal = dplyr::row_number())

  # splitting data frames and unit conversions --------------------------

  # Split data frame to a list, mutate the unit column, and rebind.

  # xUnit
  xDataList <- .removeEmptyDataFrame(split(data, data$xUnitSplit))
  data <- dplyr::bind_rows(lapply(xDataList, .xUnitConverter, xTargetUnit))

  # yUnit
  yDataList <- .removeEmptyDataFrame(split(data, list(data$yUnitSplit, data$molWeightSplit)))
  data <- dplyr::bind_rows(lapply(yDataList, .yUnitConverter, yTargetUnit))

  # yUnit error
  if ("yErrorValues" %in% names(data)) {
    yErrorDataList <- .removeEmptyDataFrame(split(data, list(data$yErrorUnitSplit, data$molWeightSplit)))
    data <- dplyr::bind_rows(lapply(yErrorDataList, .yErrorUnitConverter, yTargetUnit))
    data <- subset(data, select = -c(yErrorUnitSplit))
  }

  # clean up and return --------------------------

  # Restore the original row order using the internal row id
  data <- data[order(data$.rowidInternal), , drop = FALSE]

  # Remove all columns that were added only for internal workings of the function.
  data <- subset(data, select = -c(xUnitSplit, yUnitSplit, molWeightSplit, .rowidInternal))

  return(data)
}

#' Remove empty data frames from a list of data frames
#'
#' @description
#'
#' Remove empty data frames sometimes produced due to the non-existent
#' combination of source unit and molecular weight.
#'
#' @param x A list of data frames.
#'
#' @examples
#'
#' # Create a list of data frames
#' (ls <- split(mtcars, list(mtcars$vs, mtcars$cyl)))
#'
#' # Remove element data frames with 0 rows
#' ospsuite:::.removeEmptyDataFrame(ls)
#'
#' @keywords internal
.removeEmptyDataFrame <- function(x) Filter(function(data) nrow(data) > 0L, x)


#' @keywords internal
#' @noRd
.xUnitConverter <- function(xData, xTargetUnit) {
  xData$xValues <- toUnit(
    quantityOrDimension = xData$xDimension[[1]],
    values = xData$xValues,
    targetUnit = xTargetUnit,
    sourceUnit = xData$xUnit[[1]]
  )

  xData$xUnit <- xTargetUnit

  return(xData)
}

#' @keywords internal
#' @noRd
.yUnitConverter <- function(yData, yTargetUnit) {
  yData$yValues <- toUnit(
    quantityOrDimension = yData$yDimension[[1]],
    values = yData$yValues,
    targetUnit = yTargetUnit,
    sourceUnit = yData$yUnit[[1]],
    molWeight = yData$molWeight[[1]],
    molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
  )

  yData$yUnit <- yTargetUnit

  return(yData)
}

#' @keywords internal
#' @noRd
.yErrorUnitConverter <- function(yData, yTargetUnit) {
  yData$yErrorValues <- toUnit(
    quantityOrDimension = yData$yDimension[[1]],
    values = yData$yErrorValues,
    targetUnit = yTargetUnit,
    sourceUnit = yData$yErrorUnit[[1]],
    molWeight = yData$molWeight[[1]],
    molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
  )

  yData$yErrorUnit <- yTargetUnit

  return(yData)
}
