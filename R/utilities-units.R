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

#' Check if unit is supported in the OSPSuite platform
#'
#' @param unit String name of the unit
#' @details Returns `TRUE` if the provided unit is supported in the OSPSuite platform, otherwise `FALSE`
#'
#' @return Logical: `TRUE` if the unit is supported, `FALSE` otherwise.
#'
#' @examples
#' isSupportedUnit("kg")
#' isSupportedUnit("invalid_unit")
#'
#' @export
isSupportedUnit <- function(unit) {
  validateIsString(unit)
  validateIsOfLength(unit, 1)
  return(unit %in% unlist(ospUnits))
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
toBaseUnit <- function(
  quantityOrDimension,
  values,
  unit,
  molWeight = NULL,
  molWeightUnit = NULL
) {
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

#' Converts values from one unit to another
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
toUnit <- function(
  quantityOrDimension,
  values,
  targetUnit,
  sourceUnit = NULL,
  molWeight = NULL,
  molWeightUnit = NULL
) {
  # covers all NULL or NA
  if (all(is.na(values))) {
    return(values)
  }

  # Input validation
  validateIsOfType(quantityOrDimension, c("Quantity", "character"))
  validateIsNumeric(values, nullAllowed = TRUE)
  validateIsNumeric(molWeight, nullAllowed = TRUE)

  # Ensure dimension is a string
  dimension <- quantityOrDimension
  if (isOfType(quantityOrDimension, "Quantity")) {
    dimension <- quantityOrDimension$dimension
  }
  baseUnit <- getBaseUnit(dimension)

  targetUnit <- .encodeUnit(targetUnit)
  # If no source unit is provided, use base unit as source unit
  if (is.null(sourceUnit)) {
    sourceUnit <- baseUnit
  } else {
    sourceUnit <- .encodeUnit(sourceUnit)
  }

  # Return early
  # If target and source units are equal, no conversion required
  if (targetUnit == sourceUnit) {
    return(values)
  }

  dimensionTask <- .getCoreTaskFromCache("DimensionTask")
  # ensure that we are dealing with double precision numeric values
  if (!is.double(values)) {
    values <- as.numeric(values)
  }

  ## Have to differentiate between with molecular weight and without because it would be
  ## different calls to the `DimensionTask`.

  # If MolWeight is na, convert it to NULL
  if (!is.null(molWeight) && is.na(molWeight)) {
    molWeight <- NULL
  }
  # Case - no molecular weight is provided
  if (is.null(molWeight)) {
    # Convert values to base unit first if the source unit is not the base unit
    if (sourceUnit != baseUnit) {
      values <- dimensionTask$call(
        "ConvertToBaseUnit",
        dimension,
        sourceUnit,
        values
      )
    }

    return(dimensionTask$call("ConvertToUnit", dimension, targetUnit, values))
  }

  # Case - molecular weight is provided
  # Convert molWeight value to base unit if a unit is provided
  if (
    !is.null(molWeightUnit) &&
      molWeightUnit != ospUnits[["Molecular weight"]][["kg/\u00b5mol"]]
  ) {
    molWeight <- dimensionTask$call(
      "ConvertToBaseUnit",
      ospDimensions$`Molecular weight`,
      molWeightUnit,
      molWeight
    )
  }

  # Convert values to base unit first if the source unit is not the base unit
  if (sourceUnit != baseUnit) {
    values <- dimensionTask$call(
      "ConvertToBaseUnit",
      dimension,
      sourceUnit,
      values,
      molWeight
    )
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
#' @param values Value (single or vector). If `unit` is not specified, values are assumed to be in base unit
#' @param unit Optional Name of the unit to convert from. If `NULL` (default), the values are assumed to be in base unit.
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
#'
#' # Converts the value in ml to display unit
#' valueInDisplayUnit <- toDisplayUnit(par, 1000, "ml")
#' @export
toDisplayUnit <- function(quantity, values, unit = NULL) {
  validateIsOfType(quantity, "Quantity")
  toUnit(quantity, values, quantity$displayUnit, sourceUnit = unit)
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
    return(enum(x))
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

  dimensionsNodes <- xmlData |>
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
  dimensionsNodes <- xmlData |>
    xml2::xml_find_all(".//Dimension")

  ospUnits <- list()

  for (dim in dimensionsNodes) {
    dim_name <- dim |>
      xml2::xml_attr("name") |>
      gsub(pattern = "[(]", replacement = "[") |>
      gsub(pattern = "[)]", replacement = "]")
    dim_units <- dim |>
      xml2::xml_find_all(".//Unit")

    unit_list <- list()

    for (unit in dim_units) {
      unit_name <- unit |> xml2::xml_attr("name")
      # If unit_name equals "" use "Unitless" as key but keep "" as value.
      if (unit_name == "") {
        unit_list[["Unitless"]] <- ""
      } else {
        unit_list[[unit_name]] <- unit_name
      }
    }
    ospUnits[[dim_name]] <- unit_list
  }

  ospUnits[["Dimensionless"]] <- list("Unitless" = "")

  ospUnits <- ospUnits[order(names(ospUnits))]

  return(ospUnits)
}

.initializeDimensionAndUnitLists <- function() {
  # This initializes the two lists in the parent environment which is the package environments
  xmlData <- .parseDimensionsXML()

  utils::assignInMyNamespace("ospDimensions", .getOspDimensions(xmlData))
  utils::assignInMyNamespace("ospUnits", .getOspUnits(xmlData))
}

#' Convert the value columns of a data frame to given target units
#'
#' @param data A data frame (or a tibble) from `DataCombined$toDataFrame()`,
#'   containing only one y dimension, so that the target units fit all of its
#'   source units.
#' @param xTargetUnit Character string, the target unit for `xValues`.
#' @param yTargetUnit Character string, the target unit for `yValues`.
#'
#' @seealso toUnit
#' @keywords internal
#' @noRd
.convertDataToUnits <- function(data, xTargetUnit, yTargetUnit) {
  # No validation of inputs for this non-exported function.
  # All validation takes place in `.selectUnitsForConversion()`.

  # Strategy --------------------------

  # Use data.table for efficient in-place grouped operations. By grouping rows
  # with the same source unit and molecular weight, we make exactly as many
  # calls to `toUnit()`/`{rSharp}` as there are distinct source-unit groups,
  # while modifying columns in-place without reorganizing the data frame.
  #
  # data.table's `:=` by-group assignment preserves the original row order and
  # handles NA values in grouping variables naturally (NAs form their own
  # group), so there is no need for temporary sentinel columns or a row-order
  # identifier.

  # internal --------------------------

  # `yErrorUnit` column won't be present when only simulated datasets are
  # entered, but it can be assumed to be the same as `yUnit`.
  #
  # If there is no `yErrorValues` column in the entered data frame, it doesn't
  # make sense for this function to introduce a new column called `yErrorUnit`.
  if (
    (any(colnames(data) == "yErrorValues")) &&
      !(any(colnames(data) == "yErrorUnit"))
  ) {
    data <- dplyr::mutate(data, yErrorUnit = yUnit)
  }

  # unit conversions --------------------------

  # Convert to data.table for efficient in-place grouped operations.
  # `as.data.table()` creates a copy when the input is a tibble/data.frame,
  # so the caller's object is never modified by reference.
  data <- as.data.table(data)

  # xUnit: convert xValues in-place, grouped by (xDimension, xUnit)
  data[,
    xValues := toUnit(
      quantityOrDimension = xDimension[[1L]],
      values = xValues,
      targetUnit = xTargetUnit,
      sourceUnit = xUnit[[1L]]
    ),
    by = .(xDimension, xUnit)
  ]
  data[, xUnit := xTargetUnit]

  # yUnit: convert yValues (and lloq if present) in-place,
  # grouped by (yDimension, yUnit, molWeight)
  if ("lloq" %in% colnames(data)) {
    data[,
      c("yValues", "lloq") := list(
        toUnit(
          quantityOrDimension = yDimension[[1L]],
          values = yValues,
          targetUnit = yTargetUnit,
          sourceUnit = yUnit[[1L]],
          molWeight = molWeight[[1L]],
          molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
        ),
        toUnit(
          quantityOrDimension = yDimension[[1L]],
          values = lloq,
          targetUnit = yTargetUnit,
          sourceUnit = yUnit[[1L]],
          molWeight = molWeight[[1L]],
          molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
        )
      ),
      by = .(yDimension, yUnit, molWeight)
    ]
  } else {
    data[,
      yValues := toUnit(
        quantityOrDimension = yDimension[[1L]],
        values = yValues,
        targetUnit = yTargetUnit,
        sourceUnit = yUnit[[1L]],
        molWeight = molWeight[[1L]],
        molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
      ),
      by = .(yDimension, yUnit, molWeight)
    ]
  }
  data[, yUnit := yTargetUnit]

  # yError: convert yErrorValues and update yErrorUnit in-place, grouped by
  # (yDimension, yErrorUnit, molWeight).
  # GeometricStdDev error values are dimensionless ratios and must be skipped.
  if ("yErrorValues" %in% colnames(data)) {
    if ("yErrorType" %in% colnames(data)) {
      data[
        is.na(yErrorType) | yErrorType != DataErrorType$GeometricStdDev,
        c("yErrorValues", "yErrorUnit") := list(
          toUnit(
            quantityOrDimension = yDimension[[1L]],
            values = yErrorValues,
            targetUnit = yTargetUnit,
            sourceUnit = yErrorUnit[[1L]],
            molWeight = molWeight[[1L]],
            molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
          ),
          rep(yTargetUnit, .N)
        ),
        by = .(yDimension, yErrorUnit, molWeight)
      ]
    } else {
      data[,
        c("yErrorValues", "yErrorUnit") := list(
          toUnit(
            quantityOrDimension = yDimension[[1L]],
            values = yErrorValues,
            targetUnit = yTargetUnit,
            sourceUnit = yErrorUnit[[1L]],
            molWeight = molWeight[[1L]],
            molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
          ),
          rep(yTargetUnit, .N)
        ),
        by = .(yDimension, yErrorUnit, molWeight)
      ]
    }
  } else {
    # No error values, but sync yErrorUnit to yUnit if the column exists.
    if ("yErrorUnit" %in% colnames(data)) {
      data[, yErrorUnit := yUnit]
    }
  }

  # Convert back to tibble.
  return(dplyr::as_tibble(data))
}

#' Convert a data frame to common units
#'
#' @description
#'
#' Selects the target units with `.selectUnitsForConversion()` (one per y
#' dimension) and converts the data to them with `.convertUnitsToSelection()`.
#' This is the single entry point of the unit conversion used by
#' `convertUnits()`, `calculateResiduals()` and the plot functions, so that all
#' of them produce the same units for the same data.
#'
#' The dimensions `Concentration (mass)` and `Concentration (molar)` are merged
#' and converted to a single target unit (converting between them requires the
#' molecular weight). After a conversion between mass-based and molar
#' concentration units, the `yDimension` column is updated to the dimension of
#' the target unit.
#'
#' @param data A data frame (or a tibble) from `DataCombined$toDataFrame()`.
#' @param xUnit,yUnit Target units for `xValues` and `yValues`, respectively.
#'   If not specified (`NULL`), the most frequent unit in the respective column
#'   is selected, preferring observed data over simulated data. `yUnit` applies
#'   to the first y dimension of the data (concentration dimensions first); the
#'   remaining y dimensions keep their most frequent unit.
#' @param yUnits A list of target units per y dimension, overriding `yUnit`.
#'   Entry `i` applies to the `i`-th y dimension in display order
#'   (concentration dimensions first). Used by the plot functions to map
#'   `yUnit` to the primary and `y2Unit` to the secondary y axis.
#' @param maxAllowedYDimensions An integer, the maximum number of y dimensions
#'   allowed after merging the concentration dimensions, or `NULL` for no
#'   limit.
#'
#' @return A tibble with the data converted to common units.
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
#' # default conversion: one target unit per y dimension
#' ospsuite:::.unitConverter(df)
#'
#' # customizing conversion with specified unit(s)
#' ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$h)
#' ospsuite:::.unitConverter(df, yUnit = ospUnits$Mass$kg)
#' ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$s, yUnit = ospUnits$Amount$mmol)
#' @keywords internal
.unitConverter <- function(
  data,
  xUnit = NULL,
  yUnit = NULL,
  yUnits = NULL,
  maxAllowedYDimensions = NULL
) {
  validateIsOfType(data, "data.frame", FALSE)

  # Return early if there is no data to convert
  if (nrow(data) == 0L) {
    return(dplyr::as_tibble(data))
  }

  unitSelection <- .selectUnitsForConversion(
    data = data,
    xUnit = xUnit,
    yUnits = yUnits %||% list(yUnit),
    maxAllowedYDimensions = maxAllowedYDimensions
  )

  return(dplyr::as_tibble(.convertUnitsToSelection(unitSelection)))
}

#' Select target units for the x and y values
#'
#' @description
#'
#' Determines a target unit for `xValues` and one target unit per y dimension,
#' and splits the data by y dimension. This is the single place where target
#' units are selected for the plot functions, `convertUnits()` and
#' `calculateResiduals()`, so that all of them produce the same units for the
#' same data.
#'
#' The dimensions `Concentration (mass)` and `Concentration (molar)` are
#' treated as one convertible group and merged (converting between them
#' requires the molecular weight).
#'
#' User-specified units take precedence. Otherwise, the most frequent unit is
#' selected, preferring observed over simulated data (see
#' `.extractMostFrequentUnit()`).
#'
#' @param data A data frame (or a tibble) from `DataCombined$toDataFrame()`.
#' @param xUnit Character string, target unit for `xValues`. If `NULL`, the
#'   most frequent `xUnit` in the data is selected.
#' @param yUnits A list of character strings, target units for `yValues`.
#'   Entry `i` applies to the `i`-th y dimension in display order (concentration
#'   dimensions first, see `dataByDimension`). Entries that are `NULL` fall
#'   back to the most frequent unit of the dimension.
#' @param maxAllowedYDimensions An integer, the maximum number of y dimensions
#'   allowed after merging the concentration dimensions, or `NULL` for no
#'   limit.
#'
#' @return A list with three entries:
#'
#' - `xUnit`: the selected target unit for `xValues`,
#' - `dataByDimension`: the data split by (merged) y dimension, ordered with
#'   the concentration dimensions first,
#' - `yUnits`: a named character vector of the target unit per (merged) y
#'   dimension.
#'
#' @keywords internal
#' @noRd
.selectUnitsForConversion <- function(
  data,
  xUnit = NULL,
  yUnits = NULL,
  maxAllowedYDimensions = NULL
) {
  validateIsOfType(data, "data.frame", FALSE)
  if (!is.null(maxAllowedYDimensions)) {
    validateIsInteger(maxAllowedYDimensions, FALSE)
  }

  xTargetUnit <- xUnit %||% .extractMostFrequentUnit(data, "xUnit")

  # `as.data.table()` copies a data frame or tibble, so the object of the
  # caller is not modified by the conversions applied later on. A data.table
  # is returned as is.
  dataByDimension <- split(as.data.table(data), by = "yDimension")
  dimensionsToMerge <- c("Concentration (mass)", "Concentration (molar)")

  # Merge the concentration dimensions if they both exist: their units can be
  # converted into each other via the molecular weight, so they share one
  # target unit.
  if (all(dimensionsToMerge %in% names(dataByDimension))) {
    dataByDimension[[dimensionsToMerge[1]]] <- rbindlist(
      dataByDimension[dimensionsToMerge]
    )
    dataByDimension[[dimensionsToMerge[2]]] <- NULL
  }

  # Raise an error when there are more y dimensions left than allowed
  if (
    !is.null(maxAllowedYDimensions) &&
      length(dataByDimension) > maxAllowedYDimensions
  ) {
    stop(messages$plotTooManyYDimension(unique(data$yDimension)))
  }

  # Order the dimensions with the concentration dimensions first: the first
  # entries receive the user-specified y units and are displayed on the
  # primary y axis of a plot.
  orderedDimNames <- c(
    names(dataByDimension)[names(dataByDimension) %in% dimensionsToMerge],
    names(dataByDimension)[!names(dataByDimension) %in% dimensionsToMerge]
  )
  dataByDimension <- dataByDimension[orderedDimNames]

  yTargetUnits <- vapply(seq_along(dataByDimension), function(dimIdx) {
    userYUnit <- if (dimIdx <= length(yUnits)) {
      yUnits[[dimIdx]]
    } else {
      NULL
    }
    userYUnit %||% .extractMostFrequentUnit(dataByDimension[[dimIdx]], "yUnit")
  }, character(1))
  names(yTargetUnits) <- orderedDimNames

  return(list(
    xUnit = xTargetUnit,
    dataByDimension = dataByDimension,
    yUnits = yTargetUnits
  ))
}

#' Convert data to the units selected by `.selectUnitsForConversion()`
#'
#' @description
#'
#' Converts the data of each dimension to the selected target unit via
#' `.convertDataToUnits()` (split by `yErrorType` where a dimension mixes error
#' types) and updates the `yUnit` and `yDimension` columns accordingly — a
#' conversion between `Concentration (mass)` and `Concentration (molar)` also
#' changes the dimension.
#'
#' @param unitSelection The list returned by `.selectUnitsForConversion()`.
#'
#' @return A data.table with all dimensions converted to their target units,
#'   ordered with the concentration dimensions first, so that they end up on
#'   the primary y axis of a plot.
#'
#' @keywords internal
#' @noRd
.convertUnitsToSelection <- function(unitSelection) {
  convertedData <- lapply(
    names(unitSelection$dataByDimension),
    function(dimName) {
      dt <- unitSelection$dataByDimension[[dimName]]
      yTargetUnit <- unitSelection$yUnits[[dimName]]

      # A dimension with mixed error types is split because geometric standard
      # deviations must not be converted like arithmetic errors.
      if ("yErrorType" %in% names(dt) && uniqueN(dt$yErrorType) > 1) {
        dt <- rbindlist(
          lapply(split(dt, by = "yErrorType"), function(dtSplit) {
            .convertDataToUnits(
              data = dtSplit,
              xTargetUnit = unitSelection$xUnit,
              yTargetUnit = yTargetUnit
            )
          })
        )
      } else {
        dt <- .convertDataToUnits(
          data = dt,
          xTargetUnit = unitSelection$xUnit,
          yTargetUnit = yTargetUnit
        )
      }
      data.table::setDT(dt)

      # Set unit and dimension explicitly: this also covers the case that
      # `.convertDataToUnits()` returned the data unchanged (only one unit
      # present), and updates the dimension after a conversion between mass and
      # molar concentration units.
      dt[, yUnit := yTargetUnit]
      dt[, yDimension := getDimensionForUnit(yTargetUnit)]
      return(dt)
    }
  )
  names(convertedData) <- names(unitSelection$dataByDimension)

  result <- rbindlist(convertedData)
  result[, xUnit := unitSelection$xUnit][]

  return(result)
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
.removeEmptyDataFrame <- function(x) {
  purrr::keep(x, function(data) nrow(data) > 0L)
}


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
  if (
    any(colnames(yData) == "yErrorType") &&
      !is.na(unique(yData$yErrorType)) &&
      unique(yData$yErrorType) == DataErrorType$GeometricStdDev
  ) {
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


#' Find the most frequent unit in the data
#'
#' When a `dataType` column is present, counts are done per dataset (not per
#' time point) and observed data is preferred over simulated. This ensures that
#' a sparse observed dataset is not outvoted by a dense simulation time grid.
#' Without `dataType`, falls back to a plain row count.
#'
#' @param data A data.frame or data.table.
#' @param unitColumn Character string: column name containing units (e.g. `"xUnit"`).
#'
#' @return A single character scalar - the most frequent unit.
#' @keywords internal
#' @noRd
.extractMostFrequentUnit <- function(data, unitColumn) {
  dt <- data.table::as.data.table(data)

  dedupCols <- c("group", "name", "dataType")
  if (all(dedupCols %in% names(dt))) {
    dedup <- unique(dt, by = c(dedupCols, unitColumn))

    observedUnits <- dedup[dataType == "observed", .N, by = c(unitColumn)]
    if (nrow(observedUnits) > 0) {
      data.table::setorderv(observedUnits, "N", order = -1)
      return(observedUnits[[unitColumn]][1])
    }

    simulatedUnits <- dedup[dataType == "simulated", .N, by = c(unitColumn)]
    data.table::setorderv(simulatedUnits, "N", order = -1)
    return(simulatedUnits[[unitColumn]][1])
  }

  # Fallback: naive row count with alphabetical tiebreaker.
  unitCounts <- dt[, .N, by = c(unitColumn)]
  data.table::setorderv(unitCounts, c("N", unitColumn), order = c(-1, 1))
  return(unitCounts[[unitColumn]][1])
}
