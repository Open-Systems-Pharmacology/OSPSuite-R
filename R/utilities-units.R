#' Dimension existence
#'
#' @param dimension String name of the dimension.
#' @details Returns `TRUE` if the provided dimension is supported otherwise `FALSE`
#' @export
hasDimension <- function(dimension) {
  validateIsString(dimension)
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dimensionTask$call("HasDimension", dimension)
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
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dimensionTask$call("HasUnit", dimension, .encodeUnit(unit))
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
.validateHasUnit <- function(quantity, unit) {
  validateIsOfType(quantity, "Quantity")
  validateIsString(unit)
  if (quantity$hasUnit(unit)) {
    return()
  }
  stop(messages$errorUnitNotDefined(quantity$name, quantity$dimension, unit))
}

#' Get base unit of a dimension
#'
#' @param quantityOrDimension Instance of a quantity from which the dimension will be retrieved or name of dimension
#'
#' @return String name of the base unit.
#' @export
getBaseUnit <- function(quantityOrDimension) {
  if (isOfType(quantityOrDimension, "Quantity")) {
    dimension <- quantityOrDimension$dimension
  } else {
    dimension <- quantityOrDimension
  }
  validateDimension(dimension)
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dimensionTask$call("BaseUnitFor", dimension)
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

  # Get the base unit of the dimension and call `toUnit()`
  baseUnit <- getBaseUnit(quantityOrDimension)

  toUnit(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = baseUnit,
    sourceUnit = unit,
    molWeight = molWeight,
    molWeightUnit = molWeightUnit
  )
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

  # covers all NULL or NA
  if (all(is.na(values))) {
    return(values)
  }

  targetUnit <- .encodeUnit(targetUnit)

  if (!is.null(sourceUnit)) {
    sourceUnit <- .encodeUnit(sourceUnit)

    # If source and target units are equal, return early
    if (sourceUnit == targetUnit) {
      return(values)
    }
  }

  dimension <- quantityOrDimension
  if (isOfType(quantityOrDimension, "Quantity")) {
    dimension <- quantityOrDimension$dimension
  }
  baseUnit <- getBaseUnit(dimension)

  # Return early
  # If no source unit is defined and target is the base unit
  if (is.null(sourceUnit) && targetUnit == baseUnit) {
    return(values)
  }

  if (all(is.na(molWeight))) {
    molWeight <- NULL
  }

  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  # ensure that we are dealing with an list of values seen as number (and not integer)
  values <- as.numeric(c(values))

  # Case - no molecular weight is provided
  if (is.null(molWeight)) {
    # Convert values to base unit first if the source unit is provided
    if (!is.null(sourceUnit)) {
      values <- dimensionTask$call("ConvertToBaseUnit", dimension, sourceUnit, values)
    }
    # Return early if target unit is the base unit
    if (targetUnit == baseUnit) {
      return(values)
    }

    return(dimensionTask$call("ConvertToUnit", dimension, targetUnit, values))
  }

  # Case - molecular weight is provided
  # Convert molWeight value to base unit if a unit is provided
  if (!is.null(molWeightUnit)) {
    molWeight <- dimensionTask$call("ConvertToBaseUnit", ospDimensions$`Molecular weight`, molWeightUnit, molWeight)
  }

  # Convert values to base unit first if the source unit is provided
  if (!is.null(sourceUnit)) {
    values <- dimensionTask$call("ConvertToBaseUnit", dimension, sourceUnit, values, molWeight)
  }
  # Return early if target unit is the base unit
  if (targetUnit == baseUnit) {
    return(values)
  }

  dimensionTask$call("ConvertToUnit", dimension, targetUnit, values, molWeight)
}

#' @title Convert base unit to display unit
#'
#' @description
#'
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

#' @title List all available dimensions in the `OSPSuite` platform
#'
#' @return
#'
#' Returns the names of all available dimensions defined in the `OSPSuite`
#' platform.
#'
#' @examples
#'
#' allAvailableDimensions()
#' @export
allAvailableDimensions <- function() {
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dimensionTask$call("AllAvailableDimensionNames")
}

#' @title Get dimension for a given unit
#'
#' @return
#'
#' Returns the name of dimension that can be used to support the given unit or
#' `NULL` if the dimension cannot be found.
#'
#' @param unit Unit used to find the corresponding dimension.
#'
#' @examples
#'
#' getDimensionForUnit("mg")
#' @export
getDimensionForUnit <- function(unit) {
  validateIsString(unit)
  unit <- .encodeUnit(unit)
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dim <- dimensionTask$call("DimensionForUnit", unit)
  ifNotNull(dim, dim$get("Name"))
}

#' @title Get units for a given dimension
#'
#' @return
#'
#' Returns a vector containing all units defined in the dimension
#'
#' @param dimension Name of dimension for which units should be returned
#'
#' @examples
#'
#' getUnitsForDimension("Mass")
#' @export
getUnitsForDimension <- function(dimension) {
  validateIsString(dimension)
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dimensionTask$call("AllAvailableUnitNamesFor", dimension)
}

#' @title Get dimension by name
#'
#' @return
#'
#' Returns the an instance of the dimension with the given name if found or `NULL`
#' otherwise.
#'
#' @param name Name of dimension that should be retrieved
#'
#' @examples
#'
#' getDimensionByName("Time")
#' @export
getDimensionByName <- function(name) {
  validateIsString(name)
  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  dimensionTask$call("DimensionByName", name)
}


#' @title Create a list of all units available for each dimension
#'
#' @details
#'
#' Loop through dimensions and build a list containing an enum of all units
#' available for each dimension
#'
#' @return enum of all units for each dimension
#'
#' @examples
#'
#' ospsuite:::.getUnitsEnum()
#' @keywords internal
.getUnitsEnum <- function() {
  dimensions <- allAvailableDimensions()
  errors <- c()
  units <- lapply(dimensions, function(dimension) {
    x <- tryCatch(
      {
        # on some systems, we have issues loading units because of encoding
        # see https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/923#issuecomment-1119442789
        getUnitsForDimension(dimension = dimension)
      },
      error = function(cond) {
        errors <<- c(errors, dimension)
        # making sure that in this case, the user sees that something went wrong
        return(c("Unavailable"))
      }
    )
    return(enum(replace(x, x == "", "Unitless")))
  })

  if (length(errors) > 0L) {
    message(messages$errorLoadingUnitsForDimension(errors))
  }

  names(units) <- sapply(dimensions, function(str) {
    str <- gsub(pattern = "[(]", replacement = "[", x = str)
    str <- gsub(pattern = "[)]", replacement = "]", x = str)
  })

  return(units)
}

#' @title Function to return an enum of all available dimensions
#'
#' @return enum of all dimensions
#'
#' @examples
#'
#' ospsuite:::.getDimensionsEnum()
#' @keywords internal
.getDimensionsEnum <- function() {
  enum(allAvailableDimensions())
}


#' @title Supported dimensions defined as a named list
#'
#' @details
#' ospDimensions$Mass => "Mass"
#'
#' @export
ospDimensions <- NULL

#' Supported units defined as a named list of lists
#'
#' ospUnits$Mass$kg => "kg"
#' @export
ospUnits <- NULL

#' parse OSPSuite.Dimensions.xml containing dimensions and units
#'
#' @return An XML document
#' @import xml2
.parseDimensionsXML <- function() {
  # Read the XML file
  xmlFile <- system.file("lib/OSPSuite.Dimensions.xml", package = "ospsuite")
  xmlData <- xml2::read_xml(xmlFile)

  return(xmlData)
}

#' get OSP Dimensions from OSPSuite.Dimensions.xml data
#'
#' @param xmlData XML data from `.parseDimensionsXML()`
#'
#' @return a list of supported dimensions
.getOspDimensions <- function(xmlData) {
  ospDimensions <- list()

  dimensionsNodes <- xmlData %>%
    xml2::xml_find_all(".//Dimension")

  for (dimNode in dimensionsNodes) {
    name <- xml2::xml_attr(dimNode, "name")
    ospDimensions[[name]] <- name
  }

  ospDimensions[["Dimensionless"]] <- "Dimensionless"

  ospDimensions <- ospDimensions[order(names(ospDimensions))]

  return(ospDimensions)
}

#' get OSP Units from OSPSuite.Dimensions.xml data
#'
#' @param xmlData XML data from `.parseDimensionsXML()`
#'
#' @return a list of supported units
.getOspUnits <- function(xmlData) {
  # Extract information from the XML
  dimensionsNodes <- xmlData %>%
    xml2::xml_find_all(".//Dimension")

  ospUnits <- list()

  for (dim in dimensionsNodes) {
    dim_name <- dim %>%
      xml2::xml_attr("name") %>%
      gsub(pattern = "[(]", replacement = "[") %>%
      gsub(pattern = "[)]", replacement = "]")
    dim_units <- dim %>%
      xml2::xml_find_all(".//Unit")

    unit_list <- list()

    for (unit in dim_units) {
      unit_name <- unit %>% xml2::xml_attr("name")
      # if unit_name equals "" replace by Unitless
      if (unit_name == "") {
        unit_name <- "Unitless"
      }
      unit_list[[unit_name]] <- unit_name
    }
    ospUnits[[dim_name]] <- unit_list
  }

  ospUnits[["Dimensionless"]] <- list("Unitless" = "Unitless")

  ospUnits <- ospUnits[order(names(ospUnits))]

  return(ospUnits)
}

.initializeDimensionAndUnitLists <- function() {
  # This initializes the two lists in the parent environment which is the package environments
  xmlData <- .parseDimensionsXML()

  utils::assignInMyNamespace("ospDimensions", .getOspDimensions(xmlData))
  utils::assignInMyNamespace("ospUnits", .getOspUnits(xmlData))
}

#' Convert a data frame to common units
#'
#' @param data A data frame (or a tibble) from `DataCombined$toDataFrame()`.
#' @inheritParams convertUnits
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
#' @keywords internal
.unitConverter <- function(data, xUnit = NULL, yUnit = NULL) {
  # No validation of inputs for this non-exported function.
  # All validation will take place in the `DataCombined` class itself.

  # early return --------------------------

  # Return early if there are only unique units present in the provided data and
  # `xUnit` and `yUnit` arguments are `NULL`. This helps avoid expensive and
  # redundant computations.
  #
  # *DO NOT* use short-circuiting `&&` logical operator here.
  if (length(unique(data$xUnit)) == 1L & is.null(xUnit) &
    length(unique(data$yUnit)) == 1L & is.null(yUnit)) {
    return(data)
  }

  # target units --------------------------

  # The observed and simulated data should have the same units for
  # visual/graphical comparison.
  #
  # Therefore, if target units are not specified by the user, we need to choose
  # one ourselves. The most frequent units will be selected: one for X-axis, and
  # one for Y-axis. If multiple units are tied in terms of their frequency, the
  # first will be selected.
  xTargetUnit <- xUnit %||% .extractMostFrequentUnit(data, unitColumn = "xUnit")
  yTargetUnit <- yUnit %||% .extractMostFrequentUnit(data, unitColumn = "yUnit")

  # Strategy --------------------------

  # The strategy is to split the data frame (using `split()`) for each source
  # unit and carry out conversion separately per data frame. This is the most
  # performant option since there can only be as many expensive calls to
  # `toUnit()`/`{rSharp}` as there are source units.
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

  # internal --------------------------

  # `yErrorUnit` column won't be present when only simulated datasets are
  # entered, but it can be assumed to be the same as `yUnit`.
  #
  # If there is no `yErrorValues` column in the entered data frame, it doesn't
  # make sense for this function to introduce a new column called `yErrorUnit`.
  if ((any(colnames(data) == "yErrorValues")) &&
    !(any(colnames(data) == "yErrorUnit"))) {
    data <- dplyr::mutate(data, yErrorUnit = yUnit)
  }

  # Add suffix `Split` to the following columns:
  # `xUnit`, `yUnit`, `yErrorUnit`, `molWeight`
  data <- dplyr::mutate(
    data,
    dplyr::across(
      .cols = dplyr::matches("Unit$|Weight$"), # use pattern matching to select columns
      .fns = as.character,
      .names = "{.col}Split" # = original column name + Split suffix
    )
  )

  # Replace missing values in these new columns with `"missing"`, so that
  # `split()` won't remove the corresponding portion of the data frame.
  data <- dplyr::mutate(
    data,
    dplyr::across(
      .cols = dplyr::matches("Split$"), # use pattern matching to select columns
      .fns = function(x) tidyr::replace_na(x, "missing")
    )
  )

  # `split()` will change the row order of the data frame depending on the
  # alphabetical order of the levels of the variable used to split the data
  # frame into a list.
  #
  # Therefore, an internal row identifier is kept to restore the original
  # data frame row order before the data is returned.
  data <- dplyr::mutate(data, .rowidInternal = dplyr::row_number())

  # splitting data frames and unit conversions --------------------------

  # Split data frame to a list, mutate the unit column using the corresponding
  # `*UnitConverter()`, and then rebind.
  #
  # The `_dfr` variant of `purrr::map()` signals this intent:
  # It will return a single data frame. This data frame is created by binding
  # row-wise resulting data frames from mapping the given function `.f` to each
  # element data frame in the list provided to `.x`.

  # xUnit
  xDataList <- .removeEmptyDataFrame(split(data, data$xUnitSplit))
  data <- purrr::map_dfr(
    .x = xDataList,
    .f = function(data) .xUnitConverter(data, xTargetUnit)
  )

  # yUnit
  yDataList <- .removeEmptyDataFrame(split(data, list(data$yUnitSplit, data$molWeightSplit)))
  data <- purrr::map_dfr(
    .x = yDataList,
    .f = function(data) .yUnitConverter(data, yTargetUnit)
  )

  # yUnit error
  if (any(colnames(data) == "yErrorValues")) {
    yErrorDataList <- .removeEmptyDataFrame(split(data, list(data$yErrorUnitSplit, data$molWeightSplit)))

    data <- purrr::map_dfr(
      .x = yErrorDataList,
      .f = function(data) .yErrorUnitConverter(data, yTargetUnit)
    )
  } else {
    # For some reason, if the user dataset doesn't have error values, but
    # still have columns about error units, update them as well. The quantity
    # and its error should always have the same unit in the final data frame.
    if (any(colnames(data) == "yErrorUnit")) {
      data <- dplyr::mutate(data, yErrorUnit = yUnit)
    }
  }

  # clean up and return --------------------------

  # Restore the original row order using the internal row id
  data <- dplyr::arrange(data, .rowidInternal)

  # Remove all columns that were added only for internal workings of the function.
  data <- dplyr::select(data, -dplyr::matches("Split$|.rowidInternal"))

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
#' @keywords internal
.removeEmptyDataFrame <- function(x) purrr::keep(x, function(data) nrow(data) > 0L)


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

  if (any(colnames(yData) == "lloq")) {
    yData$lloq <- toUnit(
      quantityOrDimension = yData$yDimension[[1]],
      values = yData$lloq,
      targetUnit = yTargetUnit,
      sourceUnit = yData$yUnit[[1]],
      molWeight = yData$molWeight[[1]],
      molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
    )
  }

  yData$yUnit <- yTargetUnit

  return(yData)
}

#' @keywords internal
#' @noRd
.yErrorUnitConverter <- function(yData, yTargetUnit) {
  # If error type is geometric, conversion of `yValues` to different units
  # should not trigger conversion of error values (and units)
  if (any(colnames(yData) == "yErrorType") &&
    !is.na(unique(yData$yErrorType)) &&
    unique(yData$yErrorType) == DataErrorType$GeometricStdDev) {
    return(yData)
  }

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


#' Find the most common units
#'
#' @inheritParams .unitConverter
#' @param unitColumn The name of the column containing units (e.g. `xUnit`).
#'
#' @examples
#'
#' df <- dplyr::tibble(
#'   xValues = c(15, 30, 60),
#'   xUnit = "min",
#'   xDimension = "Time",
#'   yValues = c(0.25, 45, 78),
#'   yUnit = c("", "%", "%"),
#'   yErrorUnit = c("", "%", "%"),
#'   yDimension = "Fraction",
#'   molWeight = 10
#' )
#'
#' ospsuite:::.extractMostFrequentUnit(df, unitColumn = "xUnit")
#' ospsuite:::.extractMostFrequentUnit(df, unitColumn = "yUnit")
#'
#' @keywords internal
.extractMostFrequentUnit <- function(data, unitColumn) {
  # Converting to argument to symbol makes sure that both ways of specifying
  # arguments will be treated the same way:
  # - unquoted (`unitColumn = xUnit`)
  # - quoted (`unitColumn = "xUnit"`)
  unitColumn <- rlang::ensym(unitColumn)

  # Create a new data frame with frequency for each unit
  unitUsageFrequency <- data %>%
    # The embrace operator (`{{`) captures the user input and evaluates it in
    # the current data frame.
    dplyr::group_by({{ unitColumn }}) %>%
    dplyr::tally(name = "unitFrequency")

  mostFrequentUnit <- unitUsageFrequency %>%
    # Select only the row(s) with maximum frequency.
    #
    # In case of ties, there can be more than one row. In such cases, setting
    # `with_ties = FALSE` make sure that only the first row (and the
    # corresponding) unit will be selected.
    #
    # Do *not* select randomly as that would introduce randomness in plotting
    # functions with each run of the plotting function defaulting to a different
    # unit.
    dplyr::slice_max(unitFrequency, n = 1L, with_ties = FALSE) %>%
    # Remove the frequency column, which is not useful outside the context of
    # this function.
    dplyr::select(-unitFrequency)

  return(mostFrequentUnit[[1]])
}
