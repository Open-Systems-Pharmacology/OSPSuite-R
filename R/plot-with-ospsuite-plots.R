#' @title Create Time Profile Plot
#'
#' @description
#' Creates a time profile plot for given data.
#'
#' This function generates a time profile plot using ggplot2, where the data
#' is grouped by a column named "group".
#'
#' @details
#' ## Automatic Unit Conversion
#'
#' When using a `DataCombined` object, the function automatically converts mixed
#' units to a common unit. The target unit is determined by the most frequently
#' occurring unit in the observed data (or simulated data if no observed data
#' exists). Concentration dimensions (`Concentration (mass)` and `Concentration
#' (molar)`) are treated as compatible and can be converted between each other
#' if molecular weight is available.
#'
#' ## Mixed Error Types
#'
#' The function automatically handles data containing different error type
#' specifications:
#' - If all data uses the same error type (`ArithmeticStdDev` or `GeometricStdDev`),
#'   it is passed directly to the plotting function.
#' - If data contains **mixed error types**, they are automatically converted to
#'   `yMin`/`yMax` bounds:
#'   - `ArithmeticStdDev`: `yMin = yValues - yErrorValues`, `yMax = yValues + yErrorValues`
#'   - `GeometricStdDev`: `yMin = yValues / yErrorValues`, `yMax = yValues * yErrorValues`
#' - For custom error types (not `ArithmeticStdDev` or `GeometricStdDev`), provide
#'   error bounds directly in `yMin` and `yMax` columns.
#'
#' @param plotData An object of class `DataCombined` or a `data.table`. If a
#'   `data.table`, it must include the following:
#'   - `xValues`: Numeric time points.
#'   - `yValues`: Observed or simulated values (numeric).
#'   - `group`: Grouping variable (factor or character).
#'   - `name`: Name for the dataset (factor or character).
#'   - `xUnit`: Unit of the x-axis values (character).
#'   - `yUnit`: Unit of the y-axis values (character).
#'   - `dataType`: Specifies data type—either `observed` or `simulated`.
#'   - Optional:
#'     - `yErrorType`: Type of y error, relative to `ospsuite::DataErrorType`.
#'     - `yErrorValues`: Numeric error values.
#'     - `yMin`, `yMax`: Custom ranges for y-axis instead of error types.
#'     - `IndividualId`: Used for aggregation of simulated population data.
#' @param metaData A list containing metadata for the plot. If NULL, a default
#'   list is constructed from the data. Expected structure includes information
#'   about dimensions and units for both x and y axes.
#' @param mapping A ggplot2 aesthetic mapping object. Default is
#'   `ggplot2::aes()`. This is added or replaces the default mapping constructed
#'   by the data.
#' @param observedMapping A ggplot2 aesthetic mapping for observed data. Default
#'   is the same as `mapping`.
#' @param aggregation The type of the aggregation of simulated data. One of
#'   `quantiles` (Default), `arithmetic` or `geometric` (full list in
#'   `ospsuite::DataAggregationMethods`). Will replace `yValues` by the median,
#'   arithmetic or geometric average and add a set of upper and lower bounds
#'   (`yMin` and `yMax`). It is only applied if the simulated data represents a
#'   population.
#' @param quantiles A numerical vector with quantile values (Default: `c(0.05,
#'   0.50, 0.95)`) to be plotted. Ignored if `aggregation` is not `quantiles`.
#' @param nsd Optional parameter specifying the number of standard deviations to
#'   plot above and below the mean (used for error bars when aggregation is
#'   "arithmetic" or "geometric"). Ignored if `aggregation` is  `quantiles`.
#' @param showLegendPerDataset Controls display of separate legend entries for
#'   individual datasets. One of:
#'   - `"none"` (default): No per-dataset differentiation. Only group-level legend.
#'   - `"all"`: Differentiate both observed (via `shape`) and simulated (via `linetype`).
#'   - `"observed"`: Differentiate only observed data via different shapes.
#'   - `"simulated"`: Differentiate only simulated data via different line types.
#'
#'   User-provided `mapping` and `observedMapping` will override internal settings.
#'   A warning is issued if the override removes per-dataset differentiation.
#' @param ... Additional arguments passed to `ospsuite.plots::plotTimeProfile`.
#'
#' @return A `ggplot2` plot object representing the time profile.
#' @export
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a time profile plot for the provided data
#' plotTimeProfile(convertUnits(
#'   myDataCombined,
#'   xUnit = ospUnits$Time$h,
#'   yUnit = ospUnits$`Concentration [mass]`$`mg/l`
#' ))
#'
#' # Show individual dataset names for observed data only
#' plotTimeProfile(manyObsDC, showLegendPerDataset = "observed")
#'
#' # Show individual dataset names for simulated data only
#' plotTimeProfile(manySimDC, showLegendPerDataset = "simulated")
#'
#' # Show individual dataset names for both observed and simulated
#' plotTimeProfile(manyObsSimDC, showLegendPerDataset = "all")
#' }
plotTimeProfile <- function(
  plotData, # nolint
  metaData = NULL,
  mapping = ggplot2::aes(),
  observedMapping = mapping,
  aggregation = "quantiles",
  quantiles = ospsuite.plots::getOspsuite.plots.option(
    ospsuite.plots::OptionKeys$Percentiles
  )[c(1, 3, 5)],
  nsd = 1,
  showLegendPerDataset = "none",
  ...
) {
  # initialize variables used for data.table to avoid messages during checks
  yDimension <- yUnit <- dataType <- NULL

  checkmate::assertChoice(
    showLegendPerDataset,
    choices = c("none", "all", "observed", "simulated")
  )

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = FALSE,
    aggregation = aggregation,
    quantiles = quantiles,
    nsd = nsd
  )
  # validate field used only for timeprofile
  checkmate::assertNames(names(plotData), must.include = c("xUnit"))

  # Capture additional arguments
  additionalArgs <- list(...)

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData, nYunit = 2)
  }

  # get Mappings for simulated and observed data
  mappingAdjusted = .getMappingForTimeprofiles(
    plotData = plotData,
    metaData = metaData,
    userMapping = mapping,
    showLegendPerDataset = showLegendPerDataset,
    dataType = 'simulated'
  )
  observedMappingAdjusted = .getMappingForTimeprofiles(
    plotData = plotData,
    metaData = metaData,
    userMapping = observedMapping,
    showLegendPerDataset = showLegendPerDataset,
    dataType = 'observed'
  )

  if (any(names(metaData) %in% "y2")) {
    if (!("yDimension" %in% names(plotData))) {
      plotData[, yDimension := ""]
      for (yUnitLoop in unique(plotData$yUnit)) {
        plotData[
          yUnitLoop == yUnit,
          yDimension := ospsuite::getDimensionForUnit(yUnitLoop)
        ]
      }
    }
  }
  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotTimeProfile,
    args = c(
      list(
        data = plotData[dataType == "simulated"],
        mapping = mappingAdjusted,
        observedMapping = observedMappingAdjusted,
        metaData = metaData,
        observedData = plotData[dataType == "observed"]
      ),
      additionalArgs
    )
  )

  return(plotObject)
}

#' @title Plot Predicted vs Observed Values
#'
#' @description
#' Plots predicted vs observed data, grouped by "group".
#'
#' This function visualizes the relationship between predicted and observed
#' values, allowing for easy identification of discrepancies.
#'
#' @inheritParams plotTimeProfile
#'
#' @param xyScale A character string specifying the scale for the x and y-axis.
#'   Default is `log`.
#' @param predictedAxis A character string specifying which axis to use for
#'   predicted values. Options are `"x"` (predicted on x-axis, observed on
#'   y-axis) or `"y"` (default, predicted on y-axis, observed on x-axis).
#' @param comparisonLineVector A named list generated by the function
#'   `ospsuite.plots::getFoldDistanceList`. This list contains fold distances,
#'   where each entry represents a fold and its reciprocal. The identity fold
#'   (1) will be included if specified in `getFoldDistanceList`.
#' @param ... Additional arguments passed to `ospsuite.plots::plotYVsX`.
#'
#' @return A `ggplot2` plot object representing predicted vs observed values,
#'   including aesthetics for the x and y axes.
#' @export
#'
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a predicted vs observed plot for the provided data
#' plotPredictedVsObserved(myDataCombined)
#'
#' # Generate an observed vs predicted plot (swap axes)
#' plotPredictedVsObserved(myDataCombined, predictedAxis = "y")
#' }
plotPredictedVsObserved <- function(
  plotData, # nolint
  metaData = NULL,
  mapping = ggplot2::aes(),
  xyScale = "log",
  predictedAxis = "y",
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(folds = c(2)),
  ...
) {
  # Validate predictedAxis parameter
  predictedAxis <- match.arg(predictedAxis, choices = c("x", "y"))
  observedAxis <- setdiff(c("x", "y"), predictedAxis)

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = xyScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- .getMappingForPredictedVsObserved(
    plotData = plotData,
    userMapping = mapping,
    predictedAxis = predictedAxis
  )

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData[[predictedAxis]] <- metaData$yValues
    metaData[[observedAxis]] <- metaData$yValues
  }
  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotYVsX,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        residualScale = NULL,
        metaData = metaData,
        xScale = xyScale,
        yScale = xyScale,
        comparisonLineVector = comparisonLineVector,
        observedDataDirection = observedAxis,
        yDisplayAsAbsolute = TRUE,
        asSquarePlot = TRUE
      ),
      additionalArgs
    )
  )

  commonLabel <- plotObject$labels[[observedAxis]]
  plotObject$labels[[observedAxis]] <-
    paste(c(commonLabel, "observed"), collapse = "\n")
  plotObject$labels[[predictedAxis]] <-
    paste(c(commonLabel, "predicted"), collapse = "\n")

  return(plotObject)
}

#' @title Plot Residuals vs Covariate
#'
#' @description
#' Plots residuals vs a covariate (time, observed, or predicted values),
#' grouped by "group".
#'
#' This function visualizes the residuals against time, observed, or predicted
#' (simulated) values, helping to assess model performance.
#'
#' @details
#' ## Residual Calculation
#'
#' Residuals are calculated by pairing observed and simulated data at matching
#' time points within the same group. Only datasets that can be paired (i.e.,
#' have corresponding observed and simulated values) are included in the
#' residual plot. The function automatically removes unpaired datasets with a
#' warning, converts units to ensure consistent comparisons, and computes
#' residuals as the difference between observed and predicted values.
#'
#' ## Residual Scales
#'
#' The `residualScale` parameter controls how residuals are displayed:
#' - `linear`: Absolute residuals (Observed - Predicted). Values centered around
#'   zero indicate good model fit. Useful for normally distributed errors.
#' - `log`: Log-transformed residuals, calculated as log(Observed / Predicted).
#'   Values centered around zero indicate good fit. Preferred for log-normally
#'   distributed data or when errors are proportional to magnitude.
#' - `ratio`: Ratio of observed to predicted (Observed / Predicted). Values
#'   centered around 1.0 indicate good fit. Useful for understanding relative
#'   prediction error.
#'
#' @inheritParams plotTimeProfile
#'
#' @param residualScale Either "linear", "log", or "ratio" method for computing
#'   residuals. Default is `log`.
#' @param xAxis A character string specifying what to display on the x-axis.
#'   Options are `"time"` (time points from xValues), `"observed"` (observed
#'   values, default), or `"predicted"` (predicted/simulated values).
#' @param ... Additional arguments passed to `ospsuite.plots::plotResVsCov`.
#'
#' @return A `ggplot2` plot object representing residuals vs time, observed, or
#'   predicted values.
#' @export
#'
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a residuals vs observed plot for the provided data
#' plotResidualsVsCovariate(convertUnits(
#'   myDataCombined,
#'   xUnit = ospUnits$Time$h,
#'   yUnit = ospUnits$`Concentration [mass]`$`µg/l`
#' ))
#'
#' # Generate a residuals vs predicted plot
#' plotResidualsVsCovariate(myDataCombined, xAxis = "predicted")
#'
#' # Generate a residuals vs time plot
#' plotResidualsVsCovariate(myDataCombined, xAxis = "time")
#' }
plotResidualsVsCovariate <- function(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  residualScale = "log",
  xAxis = "observed",
  ...
) {
  predicted <- NULL

  # Validate xAxis parameter
  xAxis <- match.arg(xAxis, choices = c("time", "observed", "predicted"))

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  # Set x-axis mapping based on xAxis parameter
  xMapping <- if (xAxis == "time") {
    ggplot2::aes(x = xValues)
  } else if (xAxis == "observed") {
    ggplot2::aes(x = yValues)
  } else {
    ggplot2::aes(x = predicted)
  }

  mapping <- .getMappingForResiduals(xMapping = xMapping, userMapping = mapping)

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotResVsCov,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  # add observed or predicted to x-Axis label
  if (xAxis != "time") {
    plotObject@labels['x'] <- paste0(plotObject@labels['x'], '\n', xAxis)
  }

  return(plotObject)
}

#' @title Plot Residuals Histogram
#'
#' @description
#' Plots residuals as a histogram, grouped by "group".
#'
#' This function generates a histogram of the residuals, providing a visual
#' representation of their distribution.
#'
#' @inheritParams plotTimeProfile
#'
#' @param residualScale Either "linear", "log", or "ratio" method for computing
#'   residuals. Default is `log`.
#' @param distribution parameter passed to `ospsuite.plots::plotHistogram`.
#' @param ... Additional arguments passed to `ospsuite.plots::plotHistogram`.
#'
#' @return A `ggplot2` plot object representing the histogram of residuals.
#' @export
#'
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a histogram of residuals with default settings
#' plotResidualsAsHistogram(myDataCombined)
#'
#' # Generate a histogram with linear scale
#' plotResidualsAsHistogram(myDataCombined, residualScale = "linear")
#' }
plotResidualsAsHistogram <- function(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  distribution = "normal",
  residualScale = "log",
  ...
) {
  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- .getMappingForResiduals(
    xMapping = ggplot2::aes(),
    userMapping = mapping
  )

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }
  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotHistogram,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale,
        distribution = distribution
      ),
      additionalArgs
    )
  )

  return(plotObject)
}


#' @title Plot Quantile-Quantile Plot
#'
#' @description
#' Plots a Quantile-Quantile plot, grouped by "group".
#'
#' This function visualizes the distribution of predicted vs observed values
#' using a Q-Q plot.
#'
#' @inheritParams plotTimeProfile
#'
#' @param residualScale Either "linear", "log", or "ratio" method for computing
#'   residuals. Default is `log`.
#' @param ... Additional arguments passed to `ospsuite.plots::plotQQ`.
#'
#' @return A `ggplot2` plot object representing the Q-Q plot.
#' @export
#'
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a Q-Q plot with default settings
#' plotQuantileQuantilePlot(myDataCombined)
#'
#' # Generate a Q-Q plot with linear scale
#' plotQuantileQuantilePlot(myDataCombined, residualScale = "linear")
#' }
plotQuantileQuantilePlot <- function(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  residualScale = "log",
  ...
) {
  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- .getMappingForResiduals(
    xMapping = ggplot2::aes(),
    userMapping = mapping
  )

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotQQ,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}

#' Validates observed data and converts it to appropriate format.
#'
#' This function checks the input data for required columns and formats it for
#' plotting. If predicted values are needed, it calculates them based on the
#' observed and simulated data.
#'
#' @inheritParams plotTimeProfile
#'
#' @param predictedIsNeeded If TRUE, predicted values are calculated if not
#'   already present in the data. If FALSE, predicted values are not calculated
#'   and only data validation and aggregation are performed.
#' @param scaling A character string specifying the scale for the data. Used in
#'   the conversion process.
#'
#' @return A `data.table` with data formatted for plotting.
#' @keywords internal
#' @noRd
.validateAndConvertData <- function(
  plotData,
  predictedIsNeeded,
  scaling = NULL,
  aggregation = NULL,
  quantiles = NULL,
  nsd = 1
) {
  # initialize variables used for data.table to avoid messages during checks
  dataType <- predicted <- NULL

  if ("DataCombined" %in% class(plotData)) {
    plotData <- plotData$toDataFrame() |>
      data.table::setDT()

    if (nrow(plotData) == 0) {
      stop(messages$plotNoDataAvailable())
    }
  } else {
    validateIsOfType(plotData, "data.frame", nullAllowed = FALSE)
    plotData <- setDT(plotData)
    checkmate::assertNames(
      names(plotData),
      must.include = c("xValues", "yValues", "group", "dataType")
    )
  }

  if (predictedIsNeeded & !('predicted' %in% names(plotData))) {
    plotData <- .convertUnitsForPlot(plotData, maxAllowedYDimensions = 1)

    plotData <- .calculateResidualsForPlot(
      plotData = plotData,
      scaling = scaling
    )
    if (is.null(plotData) || nrow(plotData) == 0) {
      stop(messages$plotNoDataAvailable())
    }
    plotData <- data.table::setDT(plotData)
    plotData <- plotData |>
      data.table::setnames(
        old = c("yValuesSimulated", "yValuesObserved"),
        new = c("predicted", "yValues")
      ) |>
      dplyr::mutate(dataType = "observed")
  } else {
    plotData <- .convertUnitsForPlot(plotData, maxAllowedYDimensions = 2)
  }

  # check for inconsistent error types
  plotData <- .convertInconsistentErrorTypes(plotData)

  # Aggregate only for Timeprofiles
  if (!predictedIsNeeded) {
    plotData <- .aggregateSimulatedData(
      plotData = plotData,
      aggregation = aggregation,
      quantiles = quantiles,
      nsd = nsd
    )
  }

  # create a copy, so changes to columns will stay inside function
  plotData <- data.table::copy(data.table::setDT(plotData))

  if (predictedIsNeeded) {
    if (!("predicted" %in% names(plotData))) {
      stop(messages$plotMissingColumnPredicted())
    } else {
      plotData <- plotData[dataType == "observed"]
    }
    plotData <- plotData[!is.na(predicted)]
  }

  if (nrow(plotData) == 0) {
    stop(messages$plotNoDataAvailable())
  }

  return(plotData)
}

#' Get Most Frequent Unit
#'
#' This function retrieves the most frequently occurring unit from a specified
#' column in a given dataset, prioritizing observed data types. If no observed
#' units are available, it will return the most frequent simulated unit instead.
#'
#' @param data A data.table or data.frame containing the data. It must include
#'   the columns 'group', 'name', 'yUnit', 'xUnit', and 'dataType'.
#' @param unitColumn A character string specifying the column name from which to
#'   extract the most frequent unit. This should be either 'yUnit' or 'xUnit'.
#'
#' @return The most frequent observed unit from the specified column, or the
#'   most frequent simulated unit if no observed units are available.
#' @keywords internal
#' @noRd
.getMostFrequentUnit <- function(data, unitColumn) {
  dataType <- NULL

  # count per group and not per timepoint
  dt <- data[, c("group", "name", "dataType", unitColumn), with = FALSE] |>
    unique()

  # Filter for observed data first
  observedUnits <- dt[dataType == "observed", .N, by = c(unitColumn)] |>
    setorderv(cols = c("N"), order = -1)

  # If no observed units, check simulated
  if (nrow(observedUnits) == 0) {
    simulatedUnits <- dt[dataType == "simulated", .N, by = c(unitColumn)] |>
      setorderv(cols = c("N"), order = -1)
    return(simulatedUnits[[unitColumn]][1])
  }

  return(observedUnits[[unitColumn]][1])
}

#' Convert Units for Plotting
#'
#' This function automatically converts mixed units in the provided plot data to
#' a common unit, enabling plotting of data from different sources with
#' different unit specifications.
#'
#' @details ## Unit Selection The target unit is determined by the most
#'   frequently occurring unit in the data, prioritizing observed data over
#'   simulated data. This ensures that observed data (typically the reference)
#'   dictates the display unit.
#'
#'   ## Concentration Dimension Handling `Concentration (mass)` and
#'   `Concentration (molar)` dimensions are treated as compatible and merged for
#'   unit conversion purposes. This allows plotting of data where some datasets
#'   use mass-based units (e.g., mg/L) and others use molar units (e.g.,
#'   µmol/L), provided molecular weight is available.
#'
#'   ## Dimension Validation An error is raised if the data contains more
#'   y-dimensions than allowed by the plot type (e.g., time profile plots
#'   support up to 2 y-dimensions for dual y-axis support).
#'
#' @param plotData A data.frame containing the data to be plotted.
#' @param maxAllowedYDimensions An integer indicating the maximum number of
#'   y-dimensions allowed in the plot. If more than this number is found, an
#'   error is raised.
#'
#' @return A data.table containing the converted units and merged dimensions,
#'   ordered with concentration dimensions at the top (for primary y-axis
#'   placement).
#'
#' @keywords internal
#' @noRd
.convertUnitsForPlot <- function(plotData, maxAllowedYDimensions) {
  xUnit <- yUnit <- yDimension <- NULL

  validateIsOfType(plotData, "data.frame", FALSE)
  if (nrow(plotData) == 0) {
    return(plotData)
  }
  plotData <- setDT(plotData)
  validateIsInteger(maxAllowedYDimensions, FALSE)

  xUnitStr <- .getMostFrequentUnit(plotData, "xUnit")

  plotDataByDimensions <- split(plotData, by = "yDimension")
  dimensionsToMerge <- c("Concentration (mass)", "Concentration (molar)")

  # Merge Concentration dimensions if they exist
  if (all(dimensionsToMerge %in% names(plotDataByDimensions))) {
    plotDataByDimensions[[dimensionsToMerge[
      1
    ]]] <- rbindlist(plotDataByDimensions[dimensionsToMerge])
    plotDataByDimensions[[dimensionsToMerge[2]]] <- NULL
  }

  # Check for maximum allowed Y dimensions
  if (length(plotDataByDimensions) > maxAllowedYDimensions) {
    stop(messages$plotTooManyYDimension(unique(plotData$yDimension)))
  }

  # Convert units for each dimension
  convertedData <- lapply(plotDataByDimensions, function(dt) {
    yUnitStr <- .getMostFrequentUnit(dt, "yUnit")
    if ('yErrorType' %in% names(dt) && uniqueN(dt$yErrorType) > 1) {
      dt <- rbindlist(lapply(split(dt, by = 'yErrorType'), function(dtSplit) {
        .unitConverter(data = dtSplit, xUnit = xUnitStr, yUnit = yUnitStr)
      }))
    } else {
      dt <- .unitConverter(data = dt, xUnit = xUnitStr, yUnit = yUnitStr)
    }
    dt[, yUnit := yUnitStr]
    dt[, yDimension := ospsuite::getDimensionForUnit(yUnitStr)]
    return(dt)
  })

  # Order the list to have Concentration at the top,
  # that ensures it will be displayed on the primary axis at the timeprofile plots
  orderedData <- c(
    convertedData[dimensionsToMerge],
    convertedData[!names(convertedData) %in% dimensionsToMerge]
  )

  result <- rbindlist(orderedData)
  result[, xUnit := xUnitStr][]

  return(result)
}

#' Convert Mixed Error Types to yMin/yMax Bounds
#'
#' This function handles datasets containing multiple error type specifications
#' by converting them to a unified `yMin`/`yMax` format.
#'
#' @details When data contains only one error type, no conversion is performed
#' and the original `yErrorType`/`yErrorValues` are preserved for the plotting
#' function to handle natively.
#'
#' When multiple error types are present:
#' - `ArithmeticStdDev`: converted to `yMin = yValues - yErrorValues`, `yMax = yValues + yErrorValues`
#' - `GeometricStdDev`: converted to `yMin = yValues / yErrorValues`, `yMax = yValues * yErrorValues`
#' - Custom types: must already have `yMin`/`yMax` columns provided
#'
#' After conversion, `yErrorValues` and `yErrorType` are set to NA to prevent
#' duplicate error bar rendering.
#'
#' @param plotData A data.table containing the columns yValues, yErrorValues,
#'   and yErrorType.
#' @return A modified data.table with yMin and yMax columns when mixed error
#'   types are detected.
#'
#' @keywords internal
#' @noRd
.convertInconsistentErrorTypes <- function(plotData) {
  yErrorType <- yMin <- yMax <- NULL

  # nothing to do
  if (!("yErrorType" %in% names(plotData))) {
    return(plotData)
  }

  if ("yErrorType" %in% names(plotData)) {
    if (any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))) {
      checkmate::assertNames(
        names(plotData),
        must.include = c("yErrorValues"),
        .var.name = "columns needed for yErrorValues"
      )
    }
    # check if error values for custom error types are in columns yMin yMax
    tmp <- plotData[
      !is.na(yErrorType) &
        !(yErrorType %in% unlist(ospsuite::DataErrorType))
    ]
    if (nrow(tmp) > 0) {
      if (!all(c('yMin', 'yMax') %in% names(plotData))) {
        stop(messages$plotWrongColumnsForCustomErrorType(tmp$yErrorType))
      }
      tmp <- tmp[(is.na(yMin) | is.na(yMax)) & !is.na(yErrorValues)]
      if (nrow(tmp) > 0) {
        stop(messages$plotWrongColumnsForCustomErrorType(tmp$yErrorType))
      }
    }
  }

  # Check if there are multiple unique yErrorType values
  if (uniqueN(plotData[!is.na(yErrorType), yErrorType]) > 1) {
    if (!"yMin" %in% names(plotData)) {
      plotData[, yMin := NA_real_]
    }
    if (!"yMax" %in% names(plotData)) {
      plotData[, yMax := NA_real_]
    }
    plotData[
      yErrorType == DataErrorType$GeometricStdDev,
      `:=`(
        yMin = yValues / yErrorValues,
        yMax = yValues * yErrorValues,
        yErrorValues = NA_real_,
        yErrorType = NA_character_
      )
    ]
    plotData[
      yErrorType == DataErrorType$ArithmeticStdDev,
      `:=`(
        yMin = yValues - yErrorValues,
        yMax = yValues + yErrorValues,
        yErrorValues = NA_real_,
        yErrorType = NA_character_
      )
    ]
  }

  return(plotData)
}

#' @title Construct Metadata for Time Profile
#'
#' @description
#' Generates metadata for time profile plots based on the provided plot data.
#' Extracts and validates the dimensions and units for both x and y values,
#' ensuring that there is no ambiguity in the units.
#'
#' @details
#' The function checks for ambiguities in the x and y units and retrieves the
#' corresponding dimensions. If two y units are provided, it constructs separate
#' metadata for each. The resulting metadata is returned as a list, which
#' includes dimensions and units for both x and y values.
#'
#' @param plotData A data.table containing the following relevant columns:
#'   - `xUnit`: The unit of the x-axis values.
#'   - `xDimension`: (optional) The dimension of the x values, if already
#'     specified.
#'   - `yUnit`: The unit(s) of the y-axis values (can be one or two units).
#'   - `yDimension`: (optional) The dimension of the y values, if already
#'     specified.
#' @param nYunit Maximal number of different y-units that the plot can handle.
#'
#' @return A list containing metadata with the following structure:
#'   - `xValues`: A list with `dimension` and `unit` for the x-axis.
#'   - `yValues`: A list with `dimension` and `unit` for the primary y-axis.
#'   - `y2`: (optional) A list with `dimension` and `unit` for the secondary
#'     y-axis if applicable.
#' @keywords internal
#' @noRd
.constructMetDataForTimeProfile <- function(plotData, nYunit = 1) {
  xUnit <- unique(plotData$xUnit)
  if (length(xUnit) > 1) {
    stop(messages$plotUnitConsistency())
  }
  if ("xDimension" %in% names(plotData)) {
    xDimension <- unique(plotData$xDimension)
  } else {
    xDimension <- ospsuite::getDimensionForUnit(xUnit)
  }
  yUnit <- unique(plotData$yUnit)
  if (length(yUnit) > nYunit) {
    stop(messages$plotUnitConsistency())
  }
  if ("yDimension" %in% names(plotData)) {
    yDimension <- unique(plotData$yDimension)
  } else {
    yDimension <- unlist(lapply(yUnit, function(yUnit) {
      ospsuite::getDimensionForUnit(yUnit)
    }))
  }

  metaData <- list(
    xValues = list(
      dimension = xDimension,
      unit = xUnit
    ),
    yValues = list(
      dimension = yDimension[1],
      unit = yUnit[1]
    )
  )
  if (length(yUnit) == 2) {
    metaData[["y2"]] <- list(
      dimension = yDimension[2],
      unit = yUnit[2]
    )
  }

  return(metaData)
}

#' Calculate Residuals for Plotting
#'
#' This function computes the residuals from observed and simulated datasets,
#' ensuring that only pairable datasets are considered. It utilizes the
#' `.removeUnpairableDatasets` function to filter the input data and then
#' extracts residuals grouped by a specified variable.
#'
#' @param plotData A data.table containing observed and simulated datasets,
#'   along with a grouping variable.
#' @param scaling A character specifying scale: either `lin` (linear) or `log`
#'   (logarithmic).
#'
#' @return A data.table containing the residuals for each group, along with the
#'   relevant identifiers. Returns NULL if no pairable datasets are found.
#' @keywords internal
#' @noRd
.calculateResidualsForPlot <- function(plotData, scaling) {
  lloq <- nameSimulated <- NULL

  # functions called below needs column lloq
  if (!('lloq' %in% names(plotData))) {
    plotData[, lloq := NA]
  }

  # Remove the observed and simulated datasets which can't be paired.
  plotData <- .removeUnpairableDatasets(plotData)

  # Return early if there are no pair-able datasets present
  if (nrow(plotData) == 0L) {
    warning(messages$residualsCanNotBeComputed())
    return(NULL)
  }

  pairedData <- plotData %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .extractResidualsToTibble(.x, scaling)) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(group, name, nameSimulated)

  return(pairedData)
}

#' Creates mapping for plotData.
#'
#' This function generates a mapping for the plotting based on the provided plot
#' data and metadata.
#'
#' @param plotData Data to map.
#' @param metaData A list with metadata for plotData.
#' @param userMapping Mapping provided by the user; this will update the
#'   internal mapping.
#'
#' @return A mapping object for ggplot2.
#' @keywords internal
#' @noRd
.getMappingForTimeprofiles <- function(
  plotData,
  metaData,
  userMapping,
  showLegendPerDataset,
  dataTypeFilter
) {
  # initialize variables used for data.table to avoid warnings during checks
  xValues <- yValues <- group <- yMin <- yMax <- lloq <- NULL

  # initialize mapping
  mapping <- ggplot2::aes(x = xValues, y = yValues)

  plotData <- plotData[dataType == dataTypeFilter]

  if (nrow(plotData) == 0) {
    if (showLegendPerDataset == dataTypeFilter) {
      warning(messages$plotShowLegendPerDatasetHasNoEffect(
        dataType = dataTypeFilter
      ))
    }
    return(mapping)
  }

  # add default groupby
  if (!("groupby" %in% names(mapping))) {
    if (any(!is.na(plotData$group))) {
      mapping <- structure(
        utils::modifyList(
          ggplot2::aes(
            groupby = group,
            group = interaction(group, name)
          ),
          mapping
        ),
        class = "uneval"
      )
    } else {
      mapping <- structure(
        c(mapping, ggplot2::aes(groupby = name)),
        class = "uneval"
      )
    }
  }

  if (
    showLegendPerDataset %in%
      c("all", "simulated") &
      dataTypeFilter == "simulated"
  ) {
    # For simulated data, add linetype mapping to show individual datasets
    mapping <- structure(
      utils::modifyList(mapping, ggplot2::aes(linetype = name)),
      class = "uneval"
    )
  }

  if (
    showLegendPerDataset %in%
      c("all", "observed") &
      dataTypeFilter == "observed"
  ) {
    # For observed data, add shape mapping to show individual datasets
    mapping <- structure(
      utils::modifyList(mapping, ggplot2::aes(shape = name)),
      class = "uneval"
    )
  }

  # delete columns not needed
  plotData <- plotData[,
    which(colSums(is.na(plotData)) != nrow(plotData)),
    with = FALSE
  ]

  # set error mapping
  if (
    "yErrorType" %in%
      names(plotData) &&
      any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))
  ) {
    if (
      any(
        plotData[["yErrorType"]] == ospsuite::DataErrorType$ArithmeticStdDev,
        na.rm = TRUE
      )
    ) {
      mapping <- structure(
        c(mapping, ggplot2::aes(error = yErrorValues)),
        class = "uneval"
      )
    }

    if (
      any(
        plotData[["yErrorType"]] == ospsuite::DataErrorType$GeometricStdDev,
        na.rm = TRUE
      )
    ) {
      mapping <- structure(
        c(mapping, ggplot2::aes(error_relative = yErrorValues)),
        class = "uneval"
      )
    }
  } else if (any(c("yMin", "yMax") %in% names(plotData))) {
    checkmate::assertNames(names(plotData), must.include = c("yMin", "yMax"))
    mapping <- structure(
      c(mapping, ggplot2::aes(ymin = yMin, ymax = yMax)),
      class = "uneval"
    )
  }
  if (any(names(plotData) %in% "lloq") & dataTypeFilter == 'observed') {
    mapping <- structure(
      c(mapping, ggplot2::aes(lloq = lloq)),
      class = "uneval"
    )
  }

  if (any(names(metaData) %in% "y2")) {
    mapping <- structure(
      c(
        mapping,
        eval(parse(
          text = paste0(
            "ggplot2::aes( y2axis = yUnit == '",
            metaData[["y2"]][["unit"]],
            "')"
          )
        ))
      ),
      class = "uneval"
    )
  }

  # Check if user mapping linetype is applied to observed data or
  # shape is applied to simulated data
  unusualAesthetic <- switch(
    dataTypeFilter,
    'observed' = 'linetype',
    'simulated' = 'shape'
  )
  if (unusualAesthetic %in% names(userMapping)) {
    warning(messages$plotUntypicalAesthtic(
      aesthetic = unusualAesthetic,
      dataType = dataTypeFilter
    ))
  }

  # add user mappings (this overwrites all previous settings)
  if (!is.null(userMapping)) {
    mapping <- structure(
      utils::modifyList(mapping, userMapping),
      class = "uneval"
    )
  }

  return(mapping)
}

#' Creates mapping for residuals in plots.
#'
#' This function generates a mapping for the residuals plot based on the
#' provided user mapping.
#'
#' @param xMapping Mapping for x-axis.
#' @param userMapping Mapping provided by the user; this will update the
#'   internal mapping.
#'
#' @return A mapping object for ggplot2 that includes aesthetics for the x-axis,
#'   predicted values, observed values, and grouping.
#' @keywords internal
#' @noRd
.getMappingForResiduals <- function(xMapping, userMapping) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  mapping <- structure(
    utils::modifyList(
      c(
        xMapping,
        ggplot2::aes(
          predicted = predicted,
          observed = yValues,
          groupby = group
        )
      ),
      userMapping
    ),
    class = "uneval"
  )

  return(mapping)
}

#' Creates mapping for plotData.
#'
#' This function generates a mapping for the plotting based on the provided plot
#' data and metadata.
#'
#' @param plotData Data to map.
#' @param metaData A list with metadata for plotData.
#' @param userMapping Mapping provided by the user; this will update the
#'   internal mapping.
#' @param predictedAxis Which axis to use for predicted values ("x" or "y").
#'
#' @return A mapping object for ggplot2.
#' @keywords internal
#' @noRd
.getMappingForPredictedVsObserved <- function(
  plotData,
  userMapping,
  predictedAxis = "x"
) {
  # initialize variables used as quotes
  predicted <- yMin <- yMax <- lloq <- yValues <- group <- yErrorValues <- NULL

  # Set up mapping based on which axis has predicted values
  if (predictedAxis == "x") {
    mapping <- ggplot2::aes(
      x = predicted,
      y = yValues,
      groupby = group
    )
  } else {
    mapping <- ggplot2::aes(
      x = yValues,
      y = predicted,
      groupby = group
    )
  }

  # delete columns not needed
  plotData <- plotData[,
    which(colSums(is.na(plotData)) != nrow(plotData)),
    with = FALSE
  ]

  if (
    "yErrorType" %in%
      names(plotData) &&
      any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))
  ) {
    if (
      any(
        plotData[["yErrorType"]] == ospsuite::DataErrorType$ArithmeticStdDev,
        na.rm = TRUE
      )
    ) {
      mapping <- structure(
        c(mapping, ggplot2::aes(error = yErrorValues)),
        class = "uneval"
      )
    }

    if (
      any(
        plotData[["yErrorType"]] == ospsuite::DataErrorType$GeometricStdDev,
        na.rm = TRUE
      )
    ) {
      mapping <- structure(
        c(mapping, ggplot2::aes(error_relative = yErrorValues)),
        class = "uneval"
      )
    }
  } else if (any(c("yMin", "yMax") %in% names(plotData))) {
    checkmate::assertNames(names(plotData), must.include = c("yMin", "yMax"))
    mapping <- structure(
      c(mapping, ggplot2::aes(xmin = yMin, xmax = yMax)),
      class = "uneval"
    )
  }

  if ("lloq" %in% names(plotData) && any(!is.na(plotData$lloq))) {
    mapping <- structure(
      utils::modifyList(
        mapping,
        ggplot2::aes(lloq = lloq)
      ),
      class = "uneval"
    )
  }

  mapping <- structure(
    utils::modifyList(
      mapping,
      userMapping
    ),
    class = "uneval"
  )

  return(mapping)
}

#' Aggregates simulated data based on specified methods.
#'
#' This function aggregates simulated data from a `data.table` based on a
#' specified aggregation method (e.g., quantiles, arithmetic mean, geometric
#' mean). It handles both individual and aggregated data, returning a modified
#' `data.table` suitable for plotting.
#'
#' @param plotData A `data.table` that contains the simulated data to be
#'   aggregated. It must include:
#'   - `dataType`: Specifies if the data is `simulated`.
#'   - `yValues`: Numeric values to be aggregated.
#'   - Optional:
#'     - `IndividualId`: Identifier for individual observations, used to
#'       distinguish between individual and population data.
#'     - `group`: Grouping variable(s) for aggregation.
#'     - `name`: Name for the dataset.
#' @param aggregation A character string specifying the aggregation method.
#'   Acceptable values include:
#'   - `quantiles` (default),
#'   - `arithmetic`,
#'   - `geometric`.
#' @param quantiles A numeric vector of quantile values. Default is `NULL`,
#'   which is ignored unless `aggregation` is set to `quantiles`.
#' @param nsd Optional parameter specifying the number of standard deviations to
#'   plot above and below the mean (used for error bars when aggregation is
#'   "arithmetic" or "geometric").
#'
#' @return A modified `data.table` that includes both the observed and
#'   aggregated simulated data. The new data will contain columns for aggregated
#'   values (`yMin`, `yValues`, `yMax`) corresponding to the chosen aggregation
#'   method.
#'
#' @keywords internal
#' @noRd
.aggregateSimulatedData <- function(plotData, aggregation, quantiles, nsd = 1) {
  # initialize variables used in data.table syntax
  IndividualId <- dataType <- NULL # nolint

  checkmate::assertChoice(
    aggregation,
    choices = unlist(DataAggregationMethods),
    null.ok = TRUE
  )
  checkmate::assertNumeric(
    quantiles,
    len = 3,
    any.missing = FALSE,
    sorted = TRUE,
    null.ok = is.null(aggregation) ||
      aggregation != DataAggregationMethods$quantiles
  )

  if ("IndividualId" %in% names(plotData)) {
    if (
      any(
        plotData[
          dataType == "simulated",
          .(N = uniqueN(IndividualId)),
          by = c("group", "name")
        ]$N >
          1
      )
    ) {
      # Extract aggregated simulated data (relevant only for the population plot)
      if (!is.null(aggregation)) {
        aggregationFunction <- switch(
          aggregation,
          "quantiles" = function(x) {
            stats::setNames(
              stats::quantile(
                x,
                probs = quantiles,
                na.rm = TRUE,
                names = FALSE
              ),
              c("yMin", "yValues", "yMax")
            )
          },
          "arithmetic" = function(x) {
            m <- mean(x, na.rm = TRUE)
            s <- stats::sd(x, na.rm = TRUE)
            return(c(
              yMin = m - (s * nsd),
              yValues = m,
              yMax = m + (s * nsd)
            ))
          },
          "geometric" = function(x) {
            gm <- exp(mean(log(x), na.rm = TRUE))
            gsd <- exp(stats::sd(log(x), na.rm = TRUE))

            return(c(
              yMin = exp(log(gm) - (log(gsd) * nsd)),
              yValues = gm,
              yMax = exp(log(gm) + (log(gsd) * nsd))
            ))
          }
        )

        simAggregatedData <-
          plotData[
            dataType == "simulated",
            as.list(aggregationFunction(yValues)),
            by = .(group, name, xValues)
          ]

        # add all descriptor columns
        colsToAdd <- setdiff(
          names(plotData),
          c(
            "IndividualId",
            "yErrorValues",
            "yErrorType",
            names(simAggregatedData)
          )
        )
        dataToAdd <- unique(plotData[
          dataType == "simulated",
          c("group", "name", colsToAdd),
          with = FALSE
        ])
        if (any(duplicated(dataToAdd[, c("group", "name")]))) {
          dataToAdd <- dataToAdd[!duplicated(dataToAdd[, .(group, name)])]
        }
        simAggregatedData <- merge(
          simAggregatedData,
          dataToAdd,
          by = c("group", "name")
        )

        plotData <- rbind(
          plotData[dataType == "observed"],
          simAggregatedData,
          fill = TRUE
        )
      }
    }
  }
  return(plotData)
}
