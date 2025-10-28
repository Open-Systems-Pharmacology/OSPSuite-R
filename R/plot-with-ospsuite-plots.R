##' Creates a time profile plot for given data.
#'
#' This function generates a time profile plot using ggplot2, where the data is grouped by a column named "group".
#'
#' @param plotData An object of class `DataCombined` or a `data.table`.
#' If a `data.table`, it must include the following:
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
#'
#' @param metaData A list containing metadata for the plot. If NULL, a default list is constructed from the data. Expected structure includes information about dimensions and units for both x and y axes.
#' @param mapping A ggplot2 aesthetic mapping object. Default is `ggplot2::aes()`. This is added or replaces the default mapping constructed by the data.
#' @param observedMapping A ggplot2 aesthetic mapping for observed data. Default is the same as `mapping`.
#' @param aggregation The type of the aggregation of simulated data. One of
#'  `quantiles` (Default), `arithmetic` or `geometric` (full list in
#'  `ospsuite::DataAggregationMethods`). Will replace `yValues` by the median,
#'  arithmetic or geometric average and add a set of upper and lower bounds
#'  (`yMin` and `yMax`). It is only applied if the simulated data represents a population.
#' @param quantiles A numerical vector with quantile values (Default: `c(0.05,
#'  0.50, 0.95)`) to be plotted. Ignored if `aggregation` is not `quantiles`.
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
#' }
plotTimeProfile <- function(plotData, # nolint
                            metaData = NULL,
                            mapping = ggplot2::aes(),
                            observedMapping = mapping,
                            aggregation = "quantiles",
                            quantiles = ospsuite.plots::getOspsuite.plots.option(
                              ospsuite.plots::OptionKeys$Percentiles
                            )[c(1, 3, 5)],
                            ...) {
  # initialize variables used for data.table to avoid messages during checks
  yDimension <- yUnit <- dataType <- NULL

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = FALSE,
    aggregation = aggregation,
    quantiles = quantiles
  )
  checkmate::assertNames(names(plotData), must.include = c("xUnit"))

  # Capture additional arguments
  additionalArgs <- list(...)

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData, nYunit = 2)
  }

  if (any(names(metaData) %in% "y2")) {
    if (!("yDimension" %in% names(plotData))) {
      plotData[, yDimension := ""]
      for (yUnitLoop in unique(plotData$yUnit)) {
        plotData[yUnitLoop == yUnit, yDimension := ospsuite::getDimensionForUnit(yUnitLoop)]
      }
    }
  }
  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotTimeProfile,
    args = c(
      list(
        data = plotData[dataType == "simulated"],
        mapping = .getMappingForTimeprofiles(
          plotData = plotData[dataType == "simulated"],
          metaData = metaData,
          userMapping = mapping
        ),
        observedMapping = .getMappingForTimeprofiles(
          plotData = plotData[dataType == "observed"],
          metaData = metaData,
          userMapping = observedMapping
        ),
        metaData = metaData,
        observedData = plotData[dataType == "observed"]
      ),
      additionalArgs
    )
  )

  return(plotObject)
}
#' Plots predicted vs observed data, grouped by "group".
#'
#' This function visualizes the relationship between predicted and observed values, allowing for easy identification of discrepancies.
#'
#' @inheritParams plotTimeProfile
#'
#' @param xyscale A character string specifying the scale for the x and y-axis.
#'          Default is 'log'.
#' @param ... Additional arguments passed to `ospsuite.plots::plotPredVsObs`.
#'
#' @return A `ggplot2` plot object representing predicted vs observed values, including aesthetics for the x and y axes.
#' @export
#'
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a predicted vs observed plot for the provided data
#' plotPredictedVsObserved(myDataCombined)
#' }
plotPredictedVsObserved <- function(plotData, # nolint
                                    metaData = NULL,
                                    mapping = ggplot2::aes(),
                                    xyscale = "log",
                                    comparisonLineVector =
                                      ospsuite.plots::getFoldDistanceList(folds = c(2)),
                                    ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = xyscale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- .getMappingForPredictedVsObserved(
    plotData = plotData,
    userMapping = mapping
  )

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotPredVsObs,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        xyscale = xyscale,
        comparisonLineVector = comparisonLineVector
      ),
      additionalArgs
    )
  )

  return(plotObject)
}
#' Plots residuals vs Time, grouped by "group".
#'
#' This function visualizes the residuals over time, providing insights into the accuracy of the model predictions.
#'
#' @inheritParams plotTimeProfile
#' @param residualScale A character string specifying the scale for the x and y-axis. Default is 'log'.
#' @param ... Additional arguments passed to `ospsuite.plots::plotResVsCov`.
#'
#' @return A `ggplot2` plot object representing residuals vs time.
#' @export
#'
#' @family plot functions based on ospsuite.plots
plotResidualsVsTime <- function(plotData, # nolint
                                metaData = NULL,
                                mapping = ggplot2::aes(),
                                residualScale = "log",
                                ...) {
  # initialize variables used for data.table to avoid messages during checks
  xValues <- predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        x = xValues,
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
  )


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

  return(plotObject)
}

#' Plots residuals vs observed values, grouped by "group".
#'
#' This function visualizes the residuals against observed values, helping to assess model performance.
#'
#' @inheritParams plotTimeProfile
#' @param ... Additional arguments passed to `ospsuite.plots::plotResVsCov`.
#'
#' @return A `ggplot2` plot object representing residuals vs observed values.
#' @export
#'
#' @family plot functions based on ospsuite.plots
#'
#' @examples \dontrun{
#' # Generate a residuals vs observed plot for the provided data
#' plotResidualsVsObserved(convertUnits(
#'   myDataCombined,
#'   xUnit = ospUnits$Time$h,
#'   yUnit = ospUnits$`Concentration [mass]`$`µg/l`
#' ))
#' }
plotResidualsVsObserved <- function(plotData,
                                    metaData = NULL,
                                    mapping = ggplot2::aes(),
                                    residualScale = "log",
                                    ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        x = yValues,
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
  )


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

  return(plotObject)
}

#' Plots residuals as a histogram, grouped by "group".
#'
#' This function generates a histogram of the residuals, providing a visual representation of their distribution.
#'
#' @inheritParams plotTimeProfile
#' @param distribution parameter passed to `ospsuite.plots::plotHistogram`.
#' @param... Additional arguments passed to `ospsuite.plots::plotHistogram`.
#'
#' @return A `ggplot2` plot object representing the histogram of residuals.
#' @export
plotResidualsAsHistogram <- function(plotData,
                                     metaData = NULL,
                                     mapping = ggplot2::aes(),
                                     distribution = "normal",
                                     residualScale = "log",
                                     ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
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
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}


#' Plots a Quantile-Quantile plot, grouped by "group".
#'
#' This function visualizes the distribution of predicted vs observed values using a Q-Q plot.
#'
#' @inheritParams plotTimeProfile
#' @param... Additional arguments passed to `ospsuite.plots::plotQQ`.
#'
#' @return A `ggplot2` plot object representing the Q-Q plot.
#' @export
#'
#' @family plot functions based on ospsuite.plots
plotQuantileQuantilePlot <- function(plotData,
                                     metaData = NULL,
                                     mapping = ggplot2::aes(),
                                     residualScale = "log",
                                     ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(
    plotData = plotData,
    predictedIsNeeded = TRUE,
    scaling = residualScale
  )

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
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
#' This function checks the input data for required columns and formats it for plotting. If predicted values are needed,
#' it calculates them based on the observed and simulated data.
#'
#' @inheritParams plotTimeProfile
#' @param predictedIsNeeded If TRUE, only observed data are returned. If FALSE and the "predicted" column does not exist,
#'        predicted values are calculated.
#' @param scaling A character string specifying the scale for the data. Used in the conversion process.
#'
#' @return A `data.table` with data formatted for plotting.
#' @keywords internal
#' @noRd
.validateAndConvertData <- function(plotData, predictedIsNeeded, scaling, aggregation, quantiles) {
  # initialize variables used for data.table to avoid messages during checks
  dataType <- NULL

  if ("DataCombined" %in% class(plotData)) {
    if (is.null(nrow(plotData$toDataFrame()))) stop(messages$plotNoDataAvailable())

    if (predictedIsNeeded) {
      plotData <- calculateResiduals(
        dataCombined = plotData,
        scaling = scaling
      ) %>%
        data.table::setDT()
      if (nrow(plotData) == 0) stop(messages$plotNoDataAvailable())
      plotData <- plotData %>%
        data.table::setnames(
          old = c("yValuesSimulated", "yValuesObserved"),
          new = c("predicted", "yValues")
        ) %>%
        dplyr::mutate(dataType = "observed")
    } else {
      plotData <- plotData$toDataFrame() %>%
        data.table::setDT()
      # check if we have datatype observed AND simulated and  units differ between dataType
      if (plotData[, uniqueN(dataType)] == 2 &&
        plotData[, .(nUnit = uniqueN(xUnit) * uniqueN(yUnit))]$nUnit > 1) {
        if (!all(plotData[, .(nUnit = uniqueN(xUnit) * uniqueN(yUnit)), by = "dataType"]$nUnit == 1)) {
          stop(messages$plotUnitConsistency())
        }
        plotData <- .unitConverter(
          data = plotData,
          xUnit = plotData[dataType == "observed"]$xUnit[1],
          yUnit = plotData[dataType == "observed"]$yUnit[1]
        )
      }
    }
  }
  checkmate::assertDataFrame(plotData)
  checkmate::assertNames(names(plotData), must.include = c("xValues", "yValues", "group", "dataType"))

  plotData <- .aggregateSimulatedData(plotData = plotData, aggregation = aggregation, quantiles = quantiles)

  if ("yErrorType" %in% names(plotData) &&
    any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))) {
    checkmate::assertNames(names(plotData),
      must.include = c("yErrorValues"),
      .var.name = "columns needed for yErrorValues"
    )

    if (length(unique(plotData[!is.na(yErrorType)][["yErrorType"]])) > 1) {
      stop(messages$plotErrorTypeConsistency())
    }
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

#' Construct Metadata for Time Profile
#'
#' This function generates metadata for time profile plots based on the provided plot data.
#' It extracts and validates the dimensions and units for both x and y values, ensuring
#' that there is no ambiguity in the units.
#'
#' @param plotData A data.table containing the following relevant columns:
#'   - `xUnit`: The unit of the x-axis values.
#'   - `xDimension`: (optional) The dimension of the x values, if already specified.
#'   - `yUnit`: The unit(s) of the y-axis values (can be one or two units).
#'   - `yDimension`: (optional) The dimension of the y values, if already specified.
#' @param nYunit Maximal number of different y-units that the plot can handle.
#'
#' @details
#' The function checks for ambiguities in the x and y units and retrieves the corresponding
#' dimensions. If two y units are provided, it constructs separate metadata for each.
#' The resulting metadata is returned as a list, which includes dimensions and units for
#' both x and y values.
#'
#' @return A list containing metadata with the following structure:
#' - `xValues`: A list with `dimension` and `unit` for the x-axis.
#' - `yValues`: A list with `dimension` and `unit` for the primary y-axis.
#' - `y2`: (optional) A list with `dimension` and `unit` for the secondary y-axis if applicable.
#' @keywords internal
#' @noRd
.constructMetDataForTimeProfile <- function(plotData, nYunit = 1) {
  xUnit <- unique(plotData$xUnit)
  if (length(xUnit) > 1) stop(messages$plotUnitConsistency())
  if ("xDimension" %in% names(plotData)) {
    xDimension <- unique(plotData$xDimension)
  } else {
    xDimension <- ospsuite::getDimensionForUnit(xUnit)
  }
  yUnit <- unique(plotData$yUnit)
  if (length(yUnit) > nYunit) stop(messages$plotUnitConsistency())
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


#' Creates mapping for plotData.
#'
#' This function generates a mapping for the plotting based on the provided plot data and metadata.
#'
#' @param plotData Data to map.
#' @param metaData A list with metadata for plotData.
#' @param userMapping Mapping provided by the user; this will update the internal mapping.
#'
#' @return A mapping object for ggplot2.
#' @keywords internal
#' @noRd
.getMappingForTimeprofiles <- function(plotData, metaData, userMapping) {
  # initialize variables used for data.table to avoid warnings during checks
  xValues <- yValues <- group <- yErrorType <- yErrorValues <- yMin <- yMax <- lloq <- NULL

  mapping <- ggplot2::aes(x = xValues, y = yValues)

  if (any(!is.na(plotData$group))) {
    mapping <- structure(c(mapping, ggplot2::aes(
      groupby = group,
      group = interaction(group, name)
    )), class = "uneval")
  } else {
    mapping <- structure(c(mapping, ggplot2::aes(groupby = name)), class = "uneval")
  }

  # delete columns not needed
  plotData <- plotData[, which(colSums(is.na(plotData)) != nrow(plotData)), with = FALSE]


  if ("yErrorType" %in% names(plotData) &&
    any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))) {
    if (any(plotData[["yErrorType"]] == ospsuite::DataErrorType$ArithmeticStdDev, na.rm = TRUE)) {
      mapping <- structure(c(mapping, ggplot2::aes(error = yErrorValues)), class = "uneval")
    }

    if (any(plotData[["yErrorType"]] == ospsuite::DataErrorType$GeometricStdDev, na.rm = TRUE)) {
      mapping <- structure(c(mapping, ggplot2::aes(error_relative = yErrorValues)), class = "uneval")
    }
  } else if (any(c("yMin", "yMax") %in% names(plotData))) {
    checkmate::assertNames(names(plotData), must.include = c("yMin", "yMax"))
    mapping <- structure(c(mapping, ggplot2::aes(ymin = yMin, ymax = yMax)), class = "uneval")
  }
  if (any(names(plotData) %in% "lloq")) {
    mapping <- structure(c(mapping, ggplot2::aes(lloq = lloq)), class = "uneval")
  }

  if (any(names(metaData) %in% "y2")) {
    mapping <- structure(
      c(
        mapping,
        eval(parse(
          text = paste0(
            "aes( y2axis = yUnit == '",
            metaData[["y2"]][["unit"]], "')"
          )
        ))
      ),
      class = "uneval"
    )
  }

  if (!is.null(userMapping)) {
    mapping <- structure(utils::modifyList(mapping, userMapping), class = "uneval")
  }

  return(mapping)
}
#' Creates mapping for plotData.
#'
#' This function generates a mapping for the plotting based on the provided plot data and metadata.
#'
#' @param plotData Data to map.
#' @param metaData A list with metadata for plotData.
#' @param userMapping Mapping provided by the user; this will update the internal mapping.
#'
#' @return A mapping object for ggplot2.
#' @keywords internal
#' @noRd
.getMappingForPredictedVsObserved <- function(plotData, userMapping) {
  mapping <- ggplot2::aes(
    predicted = predicted,
    observed = yValues,
    groupby = group
  )

  # delete columns not needed
  plotData <- plotData[, which(colSums(is.na(plotData)) != nrow(plotData)), with = FALSE]

  if ("yErrorType" %in% names(plotData) &&
    any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))) {
    if (any(plotData[["yErrorType"]] == ospsuite::DataErrorType$ArithmeticStdDev, na.rm = TRUE)) {
      mapping <- structure(c(mapping, ggplot2::aes(error = yErrorValues)), class = "uneval")
    }

    if (any(plotData[["yErrorType"]] == ospsuite::DataErrorType$GeometricStdDev, na.rm = TRUE)) {
      mapping <- structure(c(mapping, ggplot2::aes(error_relative = yErrorValues)), class = "uneval")
    }
  } else if (any(c("yMin", "yMax") %in% names(plotData))) {
    checkmate::assertNames(names(plotData), must.include = c("yMin", "yMax"))
    mapping <- structure(c(mapping, ggplot2::aes(xmin = yMin, xmax = yMax)), class = "uneval")
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
#' This function aggregates simulated data from a `data.table` based on a specified aggregation method (e.g., quantiles, arithmetic mean, geometric mean).
#' It handles both individual and aggregated data, returning a modified `data.table` suitable for plotting.
#'
#' @param plotData A `data.table` that contains the simulated data to be aggregated. It must include:
#'   - `dataType`: Specifies if the data is `simulated`.
#'   - `yValues`: Numeric values to be aggregated.
#'   - Optional:
#'     - `IndividualId`: Identifier for individual observations, used to distinguish between individual and population data.
#'     - `group`: Grouping variable(s) for aggregation.
#'     - `name`: Name for the dataset.
#' @param aggregation A character string specifying the aggregation method. Acceptable values include:
#'   - `quantiles` (default),
#'   - `arithmetic`,
#'   - `geometric`.
#' @param quantiles A numeric vector of quantile values. Default is `NULL`, which is ignored unless `aggregation` is set to `quantiles`.
#'
#' @return A modified `data.table` that includes both the observed and aggregated simulated data. The new data will contain columns for aggregated values (`yMin`, `yValues`, `yMax`) corresponding to the chosen aggregation method.
#'
#' @keywords internal
#' @noRd
.aggregateSimulatedData <- function(plotData, aggregation, quantiles) {
  checkmate::assertChoice(aggregation, choices = unlist(DataAggregationMethods), null.ok = TRUE)
  checkmate::assertNumeric(quantiles,
    len = 3, any.missing = FALSE, sorted = TRUE,
    null.ok = is.null(aggregation) |
      aggregation != DataAggregationMethods$quantiles
  )

  if ("IndividualId" %in% names(plotData)) {
    if (any(plotData[dataType == "simulated",
      .(N = uniqueN(IndividualId)),
      by = c("group", "name")
    ]$N > 1)) {
      # Extract aggregated simulated data (relevant only for the population plot)
      if (!is.null(aggregation)) {
        aggregationFunction <- switch(aggregation,
          "quantiles" = function(x) {
            stats::setNames(
              stats::quantile(x,
                probs = quantiles,
                na.rm = TRUE,
                names = FALSE
              ),
              c("yMin", "yValues", "yMax")
            )
          },
          "arithmetic" = function(x) {
            mean <- mean(x, na.rm = TRUE, ...)
            sd <- stats::sd(x, na.rm = TRUE)
            return(c(
              yMin = mean - sd,
              yValues = mean,
              yMax = mean + sd
            ))
          },
          "geometric" = function(x, nsd = 1, na.rm = FALSE, ...) {
            gm <- exp(mean(log(x), na.rm = TRUE))
            gsd <- exp(sd(log(x), na.rm = TRUE))

            return(c(
              yMin = exp(log(gm) - log(gsd)),
              yValues = gm,
              yMax = exp(log(gm) + log(gsd))
            ))
          }
        )

        simAggregatedData <-
          plotData[dataType == "simulated",
            as.list(aggregationFunction(yValues)),
            by = .(group, name, xValues)
          ]

        # add all descriptor columns
        colsToAdd <- setdiff(
          names(plotData),
          c(
            "IndividualId", "yErrorValues", "yErrorType",
            names(simAggregatedData)
          )
        )
        dataToAdd <- unique(plotData[dataType == "simulated", c("group", "name", colsToAdd), with = FALSE])
        if (any(duplicated(dataToAdd[, c("group", "name")]))) {
          dataToAdd <- dataToAdd[!duplicated(dataToAdd[, .(group, name)])]
        }
        simAggregatedData <- merge(simAggregatedData, dataToAdd, by = c("group", "name"))

        plotData <- rbind(plotData[dataType == "observed"],
          simAggregatedData,
          fill = TRUE
        )
      }
    }
  }
  return(plotData)
}
