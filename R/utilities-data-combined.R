#' Convert datasets in `DataCombined` to common units
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
#' @param dataCombined A single instance of `DataCombined` class.
#' @param xUnit,yUnit Target units for `xValues` and `yValues`, respectively. If
#'   not specified (`NULL`), first of the existing units in the respective
#'   columns (`xUnit` and `yUnit`) will be selected as the common unit. For
#'   available dimensions and units, see `ospsuite::ospDimensions` and
#'   `ospsuite::ospUnits`, respectively.
#'
#' @return
#'
#' In the returned tibble data frame, the following columns will always be present:
#'
#' name - group - dataType - xValues - xDimension - xUnit - yValues -
#' yErrorValues - yDimension - yUnit - yErrorType - yErrorUnit - molWeight
#'
#' Importantly, the `xUnit` and `yUnit` columns will have unique entries.
#'
#' @family data-combined
#'
#' @examples
#' # simulated data
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simResults <- runSimulations(sim)[[1]]
#' outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#'
#' # observed data
#' obsData <- lapply(
#'   c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
#'   function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
#' )
#' names(obsData) <- lapply(obsData, function(x) x$name)
#'
#'
#' # Create a new instance of `DataCombined` class
#' myDataCombined <- DataCombined$new()
#'
#' # Add simulated results
#' myDataCombined$addSimulationResults(
#'   simulationResults = simResults,
#'   quantitiesOrPaths = outputPath,
#'   groups = "Aciclovir PVB"
#' )
#'
#' # Add observed data set
#' myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")
#'
#' convertUnits(
#'   myDataCombined,
#'   xUnit = ospUnits$Time$s,
#'   yUnit = ospUnits$`Concentration [mass]`$`µg/l`
#' )
#'
#' @export
convertUnits <- function(dataCombined, xUnit = NULL, yUnit = NULL) {
  .validateScalarDataCombined(dataCombined)

  # Extract combined data frame
  combinedData <- dataCombined$toDataFrame()

  # Getting all units on the same scale
  combinedData <- .unitConverter(combinedData, xUnit, yUnit)

  return(combinedData)
}

#' Calculate residuals for datasets in `DataCombined`
#'
#' @details
#'
#' To compute residuals, for every simulated dataset in a given group, there
#' should also be a corresponding observed dataset. If this is not the case, the
#' corresponding observed or simulated datasets will be removed.
#'
#' When multiple (observed and/or simulated) datasets are present in
#' `DataCombined`, they are likely to have different units. The `xUnit` and
#' `yUnit` arguments help you specify a common unit to convert them to.
#'
#' @param scaling A character specifying the scale for residual calculation. Supported values:
#'   - `"linear"` or `tlf::Scaling$lin` or `tlf::Scaling$identity`: Linear residuals (Simulated - Observed)
#'   - `"log"` or `tlf::Scaling$log`: Logarithmic residuals (log(Simulated) - log(Observed))
#'   - `"ratio"`: Ratio residuals (Observed / Simulated)
#'   
#'   For `"log"` scale, undefined values (e.g., log(0)) will result in NaN and trigger a warning.
#'   These points are excluded from the results.
#' @inheritParams convertUnits
#'
#' @return
#'
#' In the returned tibble data frame, the following columns will always be present:
#'
#' xValues - xUnit - xDimension - yValuesObserved - yUnit - yDimension -
#' yErrorValues - yErrorType - yErrorUnit - yValuesSimulated - residualValues
#'
#' @family data-combined
#'
#' @examples
#' # simulated data
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simResults <- runSimulations(sim)[[1]]
#' outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#'
#' # observed data
#' obsData <- lapply(
#'   c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
#'   function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
#' )
#' names(obsData) <- lapply(obsData, function(x) x$name)
#'
#'
#' # Create a new instance of `DataCombined` class
#' myDataCombined <- DataCombined$new()
#'
#' # Add simulated results
#' myDataCombined$addSimulationResults(
#'   simulationResults = simResults,
#'   quantitiesOrPaths = outputPath,
#'   groups = "Aciclovir PVB"
#' )
#'
#' # Add observed data set
#' myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")
#'
#' calculateResiduals(myDataCombined, scaling = tlf::Scaling$lin)
#' @export
calculateResiduals <- function(
  dataCombined,
  scaling,
  xUnit = NULL,
  yUnit = NULL
) {
  nameSimulated <- NULL

  .validateScalarDataCombined(dataCombined)
  
  # Validate scaling parameter
  validScales <- c("linear", "log", "ratio", tlf::Scaling$lin, tlf::Scaling$log, tlf::Scaling$identity)
  if (!scaling %in% validScales) {
    stop(sprintf(
      "Invalid scaling parameter: '%s'. Must be one of: 'linear', 'log', 'ratio', or tlf::Scaling values.",
      scaling
    ))
  }

  # Validation has already taken place in the calling plotting function
  combinedData <- dataCombined$toDataFrame()

  # Remove the observed and simulated datasets which can't be paired.
  combinedData <- .removeUnpairableDatasets(combinedData)

  # Return early if there are no pair-able datasets present
  if (nrow(combinedData) == 0L) {
    warning(messages$residualsCanNotBeComputed())
    return(NULL)
  }

  # Getting all datasets to have the same units.
  combinedData <- .unitConverter(combinedData, xUnit, yUnit)

  # Create observed versus simulated paired data using interpolation for each
  # grouping level and combine the resulting data frames in a row-wise manner.
  #
  # Both of these routines will be carried out by `dplyr::group_modify()`.
  pairedData <- combinedData %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .extractResidualsToTibble(.x, scaling)) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(group, name, nameSimulated)

  return(pairedData)
}

#' Created observed versus simulated paired data
#'
#' @param data A data frame from `DataCombined$toDataFrame()`, which has been
#'   further tidied using `.removeUnpairableDatasets()` and then
#'   `.unitConverter()` functions.
#'
#' @keywords internal
.extractResidualsToTibble <- function(data, scaling) {
  observedList <- split(dplyr::filter(data, dataType == "observed"), ~name)
  simulatedList <- split(dplyr::filter(data, dataType == "simulated"), ~name)

  obsSimPairs <- tidyr::expand_grid(
    observedName = names(observedList),
    simulatedName = names(simulatedList)
  )

  resultList <- vector("list", nrow(obsSimPairs))

  for (i in seq_len(nrow(obsSimPairs))) {
    observedName <- obsSimPairs$observedName[[i]]
    simulatedName <- obsSimPairs$simulatedName[[i]]

    observedData <- observedList[[observedName]]
    simulatedData <- simulatedList[[simulatedName]]

    # If available, error values will be useful for plotting error bars in the
    # scatter plot. Even if not available, add missing values to be consistent.
    if ("yErrorValues" %in% colnames(data)) {
      yErrorValues <- data$yErrorValues[data$dataType == "observed"]
    } else {
      yErrorValues <- rep(NA_real_, nrow(observedData))
    }

    # Most of the columns in the observed data frame should also be included in
    # the paired data frame for completeness.
    pairedData <- dplyr::select(
      observedData,
      # Identifier column
      name,
      # Everything related to the X-variable
      "xValues",
      "xUnit",
      "xDimension",
      dplyr::matches("^x"),
      # Everything related to the Y-variable
      "yValuesObserved" = "yValues",
      "yUnit",
      "yDimension",
      dplyr::matches("^y"),
      # lower limit of quantification
      "lloq"
    )

    pairedData$nameSimulated <- simulatedName

    # Add predicted values

    # Interpolation with stats::approx requires at least 2 simulated points.
    # With 2 or more points, perform linear interpolation.
    if (nrow(simulatedData) >= 2) {
      # `rule = 1` returning NA for any observed points outside the simulated x-range
      interpolatedYValues <- stats::approx(
        x = simulatedData$xValues,
        y = simulatedData$yValues,
        xout = observedData$xValues,
        rule = 1
      )$y
    } else if (nrow(simulatedData) == 1) {
      # With exactly 1 simulated point, assign the simulated yValue to observed xValues that exactly match the simulated xValue.
      interpolatedYValues <- ifelse(
        observedData$xValues == simulatedData$xValues,
        simulatedData$yValues,
        NA_real_
      )
    } else {
      # No simulated data points: all interpolated values should be NA.
      interpolatedYValues <- rep(NA_real_, length(observedData$xValues))
    }

    pairedData$yValuesSimulated <- interpolatedYValues

    # Residual computation will depend on the scaling.
    if (scaling %in% c(tlf::Scaling$lin, tlf::Scaling$identity, "linear")) {
      # Linear scale: Simulated - Observed
      pairedData <- dplyr::mutate(
        pairedData,
        residualValues = yValuesSimulated - yValuesObserved
      )
    } else if (scaling %in% c("ratio")) {
      # Ratio scale: Observed / Predicted
      pairedData <- dplyr::mutate(
        pairedData,
        residualValues = yValuesObserved / yValuesSimulated
      )
    } else {
      # Log scale: log(Simulated / Observed) = log(Simulated) - log(Observed)
      # Do NOT add epsilon - let log(0) produce NaN
      pairedData$residualValues <- log(pairedData$yValuesSimulated) - log(pairedData$yValuesObserved)
    }

    # some residual values might turn out to be NA (for example, when extrapolating)
    # they are not returned in the output tibble
    resultList[[i]] <- dplyr::filter(pairedData, !is.na(residualValues))
  }

  pairedData <- dplyr::bind_rows(resultList)
  
  # Count and warn about undefined residuals (NaN or Inf) for log/ratio scales
  if (!scaling %in% c(tlf::Scaling$lin, tlf::Scaling$identity, "linear")) {
    nanCount <- sum(is.nan(pairedData$residualValues))
    infCount <- sum(is.infinite(pairedData$residualValues))
    
    if (nanCount > 0 || infCount > 0) {
      warning(messages$undefinedResidualsWarning(nanCount, infCount))
    }
  }
  
  return(pairedData)
}

#' Remove unpairable datasets for computing residuals
#'
#' @description
#'
#' Computing residuals by definition requires that data should be in pairs, i.e.
#' for every simulated dataset in a given group, there should also be a
#' corresponding observed dataset.
#'
#' To this end, current function removes the following datasets:
#'
#' - Datasets which haven't been assigned to any group.
#' - Datasets that are not part of a pair (i.e. a simulated dataset without
#'   observed dataset partner, and vice versa).
#'
#' @param data A data frame returned by `DataCombined$toDataFrame()`.
#'
#' @examples
#'
#' df <- dplyr::tribble(
#'   ~name, ~dataType, ~group,
#'   "Sim1", "Simulated", "GroupA",
#'   "Sim2", "Simulated", "GroupA",
#'   "Obs1", "Observed", "GroupB",
#'   "Obs2", "Observed", "GroupB",
#'   "Sim3", "Simulated", "GroupC",
#'   "Obs3", "Observed", "GroupC",
#'   "Sim4", "Simulated", "GroupD",
#'   "Obs4", "Observed", "GroupD",
#'   "Obs5", "Observed", "GroupD",
#'   "Sim5", "Simulated", "GroupE",
#'   "Sim6", "Simulated", "GroupE",
#'   "Obs7", "Observed", "GroupE",
#'   "Sim7", "Simulated", "GroupF",
#'   "Sim8", "Simulated", "GroupF",
#'   "Obs8", "Observed", "GroupF",
#'   "Obs9", "Observed", "GroupF",
#'   "Sim9", "Simulated", NA,
#'   "Obs10", "Observed", NA
#' )
#'
#' # original
#' df
#'
#' # transformed
#' ospsuite:::.removeUnpairableDatasets(df)
#'
#' @keywords internal
.removeUnpairableDatasets <- function(data) {
  # How many rows were originally present?
  originalDatasets <- unique(data$name)

  # Remove datasets that don't belong to any group.
  data <- dplyr::filter(data, !is.na(group))

  # Remove groups (and the datasets therein) with only one type (either only
  # observed or only simulated) of dataset.
  data <- data %>%
    dplyr::group_by(group) %>%
    dplyr::filter(length(unique(dataType)) > 1L) %>%
    dplyr::ungroup()

  # How many rows are left after filtering?
  finalDatasets <- unique(data$name)

  # Inform the user about which (if any) datasets were removed.
  if (length(finalDatasets) < length(originalDatasets)) {
    missingDatasets <- originalDatasets[!originalDatasets %in% finalDatasets]

    message(messages$printMultipleEntries(
      header = messages$datasetsToGroupNotFound(),
      entries = missingDatasets
    ))
  }

  return(data)
}

#' Validate that single instance of `DataCombined`
#'
#' @examples
#' ospsuite:::.validateScalarDataCombined(DataCombined$new()) # okay
#' # ospsuite:::.validateScalarDataCombined(list(DataCombined$new(), DataCombined$new())) # error
#'
#' @keywords internal
.validateScalarDataCombined <- function(dataCombined) {
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L)
}


#' Validate arguments provided as vectors
#'
#' @details
#'
#' Cleaning an argument provided as (atomic or generic) vector involves:
#'
#' - Checking that it is of expected length.
#' - Checking for `NULL` or other special constants (`NaN`, `Inf`, `NA` of the
#'   wrong type) and standardizing them to `NA` of desired data type.
#' - Checking that each element in the vector is of expected data type.
#' - Making sure that an atomic vector is always returned, irrespective of if
#'   the input was a list or an atomic vector.
#'
#' @param arg A vector of arguments.
#' @param expectedLength An integer to denote the expected length of the vector.
#' @inheritParams ospsuite.utils::validateIsOfType
#'
#' @return
#' An atomic vector of desired data type.
#'
#' @examples
#' ospsuite:::.cleanVectorArgs(list(1, 2, NA, NULL), 4L, "numeric")
#' @keywords internal
.cleanVectorArgs <- function(arg = NULL, expectedLength = NULL, type) {
  # Return early if no argument was specified
  if (is.null(arg)) {
    return(NULL)
  }

  # Check that the argument is not empty
  validateIsNotEmpty(arg)

  # validate the length of vector arguments
  if (!is.null(expectedLength)) {
    validateIsOfLength(arg, expectedLength)
  }

  # convert `NULL`s or logical `NA`s to `NA` of required type

  # Note that `purrr::map()` will return a list
  arg <- purrr::map(arg, function(x) toMissingOfType(x, type))

  # validate the type of arguments

  # `nullAllowed = TRUE` is necessary because `NULL` in vector arguments is
  # used to specify no change for the corresponding dataset
  validateIsOfType(arg, type, nullAllowed = TRUE)

  # arguments are still in a list
  # flatten them to an atomic vector of required type
  arg <- flattenList(arg, type)

  return(arg)
}
