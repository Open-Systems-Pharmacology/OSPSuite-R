#' Make sure entered `DataCombined` object is valid for plotting
#'
#' @family utilities-plotting
#'
#' @keywords internal
#' @noRd
.validateDataCombinedForPlotting <- function(dataCombined) {
  validateIsOfType(dataCombined, "DataCombined")

  # Only single instance is allowed.
  validateIsSameLength(objectCount(dataCombined), 1L)

  # If there are no datasets in the object, inform the user that no plot will
  # be created.
  if (is.null(dataCombined$groupMap)) {
    warning(messages$plottingWithEmptyDataCombined())
  }
}

#' Make sure entered `DefaultPlotConfiguration` object is valid for plotting
#'
#' @family utilities-plotting
#'
#' @keywords internal
#' @noRd
.validateDefaultPlotConfiguration <- function(defaultPlotConfiguration = NULL) {
  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration")
  return(defaultPlotConfiguration)
}

#' Replace missing groupings with dataset names
#'
#' @description
#'
#' Datasets which haven't been assigned to any group will be plotted as a group
#' on its own. That is, the `group` column entries for them will be their names.
#'
#' @param data A data frame returned by `DataCombined$toDataFrame()`.
#'
#' @family utilities-plotting
#'
#' @examples
#'
#' df <- dplyr::tibble(
#'   group = c(
#'     "Stevens 2012 solid total",
#'     "Stevens 2012 solid total",
#'     NA,
#'     NA,
#'     NA
#'   ),
#'   name = c(
#'     "Organism|Lumen|Stomach|Metformin|Gastric retention",
#'     "Stevens_2012_placebo.Placebo_total",
#'     "Stevens_2012_placebo.Sita_dist",
#'     "Stevens_2012_placebo.Sita_proximal",
#'     "Stevens_2012_placebo.Sita_total"
#'   ),
#'   dataType = c(
#'     "simulated",
#'     "observed",
#'     "observed",
#'     "observed",
#'     "observed"
#'   )
#' )
#'
#' # original
#' df
#'
#' # transformed
#' ospsuite:::.addMissingGroupings(df)
#'
#' @keywords internal
.addMissingGroupings <- function(data) {
  data <- dplyr::mutate(
    data,
    group = dplyr::case_when(
      # If grouping is missing, then use dataset name as its own grouping.
      is.na(group) ~ name,
      # Otherwise, no change.
      TRUE ~ group
    )
  )

  return(data)
}

#' Remove unpairable datasets for scatter plot functions
#'
#' @description
#'
#' Scatter plots by their nature require that data should be of paired, i.e. for
#' every simulated dataset in a given group, there should also be a
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
#' @family utilities-plotting
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


#' Extract aggregated simulated data
#'
#' @param simData A data frame with simulated data from
#'   `DataCombined$toDataFrame()`.
#' @inheritParams plotPopulationTimeProfile
#'
#' @details
#'
#' The simulated values will be aggregated across individuals for each time
#' point.
#'
#' @family utilities-plotting
#'
#' @examples
#'
#' # let's create a data frame to test this function
#' df <- dplyr::tibble(
#'   xValues = c(
#'     0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5,
#'     0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2,
#'     3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5
#'   ),
#'   yValues = c(
#'     0,
#'     0.990956723690033, 0.981773018836975, 0.972471475601196, 0.963047087192535,
#'     0.953498184680939, 0, 0.990953505039215, 0.981729507446289, 0.97233647108078,
#'     0.962786376476288, 0.953093528747559, 0, 0.990955889225006, 0.981753170490265,
#'     0.972399413585663, 0.962896287441254, 0.953253626823425, 0, 0.990950107574463,
#'     0.981710314750671, 0.972296476364136, 0.962724387645721, 0.953009009361267,
#'     0, 0.261394888162613, 0.266657412052155, 0.27151620388031, 0.275971591472626,
#'     0.280027687549591, 0, 0.26139160990715, 0.266613900661469, 0.271381109952927,
#'     0.275710910558701, 0.279623001813889, 0, 0.261393994092941, 0.266637593507767,
#'     0.271443992853165, 0.275820910930634, 0.279783099889755, 0, 0.261388212442398,
#'     0.266594797372818, 0.27134120464325, 0.275649011135101, 0.279538512229919
#'   ),
#'   group = c(rep("Stevens 2012 solid total", 24), rep("Stevens 2012 solid distal", 24)),
#'   name = group
#' )
#'
#' # raw data
#' df
#'
#' # aggregated data
#' ospsuite:::.extractAggregatedSimulatedData(df)
#'
#' @keywords internal
.extractAggregatedSimulatedData <- function(simData,
                                            quantiles = c(0.05, 0.5, 0.95)) {
  simAggregatedData <- simData %>%
    # For each dataset, compute quantiles across all individuals for each time point
    #
    # Each group should always a single dataset, so grouping by `group` *and* `name`
    # should produce the same result as grouping by only `group` column.
    #
    # The reason `name` column also needs to be retained in the resulting data
    # is because it is mapped to linetype property in population profile type.
    dplyr::group_by(group, name, xValues) %>% #
    dplyr::summarise(
      yValuesLower = stats::quantile(yValues, quantiles[[1]]),
      yValuesCentral = stats::quantile(yValues, quantiles[[2]]),
      yValuesHigher = stats::quantile(yValues, quantiles[[3]]),
      .groups = "drop" # drop grouping information from the summary data frame
    )

  return(simAggregatedData)
}

#' Create axes labels
#'
#' @details
#'
#' If axes labels haven't been specified, create them using information about
#' dimensions and units present in the data frame produced by
#' `DataCombined$toDataFrame()`.
#'
#' @param data A data frame from `DataCombined$toDataFrame()`, which has
#'   additionally been cleaned using `ospsuite:::.unitConverter()` to have the
#'   same units across datasets.
#' @param specificPlotConfiguration The nature of labels will change depending
#'   on the type of plot. The type of plot can be guessed from the specific
#'   `PlotConfiguration` object used, since each plot has a unique corresponding
#'   class.
#'
#' @family utilities-plotting
#'
#' @examples
#'
#' df <- dplyr::tibble(
#'   dataType = c(rep("simulated", 3), rep("observed", 3)),
#'   xValues = c(0, 14.482, 28.965, 0, 1, 2),
#'   xUnit = "min",
#'   xDimension = "Time",
#'   yValues = c(1, 1, 1, 1, 1, 1),
#'   yUnit = "mol/ml",
#'   yDimension = ospDimensions$`Concentration (mass)`,
#'   yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
#'   molWeight = c(10, 10, 20, 20, 10, 10)
#' )
#'
#' df <- ospsuite:::.unitConverter(df)
#'
#' ospsuite:::.createAxesLabels(df, tlf::TimeProfilePlotConfiguration$new())
#'
#' @keywords internal
.createAxesLabels <- function(data, specificPlotConfiguration) {
  # If empty data frame is entered or plot type is not specified, return early
  if (nrow(data) == 0L || missing(specificPlotConfiguration)) {
    return(NULL)
  }

  # Initialize strings with unique values for units and dimensions.
  #
  # The`.unitConverter()` has already ensured that there is only a single unit
  # for quantities, so we can safely take the unique unit to prepare axes
  # labels.
  xUnitString <- unique(data$xUnit)
  yUnitString <- unique(data$yUnit)

  # There might be multiple dimensions across datasets, select the first one.
  xDimensionString <- unique(data$xDimension)[[1]]
  yDimensionString <- unique(data$yDimension)[[1]]

  # Hard code some concentration dimensions to one dimension: `"Concentration"`
  #
  # For more, see:
  # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/938
  concDimensions <- c(ospDimensions$`Concentration (mass)`, ospDimensions$`Concentration (molar)`)
  xDimensionString <- ifelse(any(xDimensionString %in% concDimensions), "Concentration", xDimensionString)
  yDimensionString <- ifelse(any(yDimensionString %in% concDimensions), "Concentration", yDimensionString)

  # If quantities are unitless, no unit information needs to be displayed.
  # Otherwise, `Dimension [Unit]` pattern is followed.
  xUnitString <- ifelse(xUnitString == "", xUnitString, paste0(" [", xUnitString, "]"))
  xUnitString <- paste0(xDimensionString, xUnitString)
  yUnitString <- ifelse(yUnitString == "", yUnitString, paste0(" [", yUnitString, "]"))
  yUnitString <- paste0(yDimensionString, yUnitString)

  # The exact axis label will depend on the type of the plot, and the type
  # of the plot can be guessed using the specific `PlotConfiguration` object.
  plotType <- class(specificPlotConfiguration)[[1]]

  # If the specific `PlotConfiguration` object is not any of the cases included
  # in the `switch` below, the the labels will remain `NULL`.

  # X-axis label
  xLabel <- switch(plotType,
    "TimeProfilePlotConfiguration" = ,
    "ResVsTimePlotConfiguration" = xUnitString,
    # Note that `yUnitString` here is deliberate.
    #
    # In case of an observed versus simulated plot, `yValues` are plotted on
    # both x- and y-axes, and therefore the units strings are going to be the
    # same for both axes.
    "ObsVsPredPlotConfiguration" = paste0("Observed values (", yUnitString, ")"),
    "ResVsPredPlotConfiguration" = paste0("Simulated values (", yUnitString, ")")
  )

  # Y-axis label
  yLabel <- switch(plotType,
    "TimeProfilePlotConfiguration" = yUnitString,
    "ResVsPredPlotConfiguration" = ,
    "ResVsTimePlotConfiguration" = "Residuals",
    "ObsVsPredPlotConfiguration" = paste0("Simulated values (", yUnitString, ")")
  )

  return(list("xLabel" = xLabel, "yLabel" = yLabel))
}


#' Update axes label fields in `PlotConfiguration` object
#'
#' @family utilities-plotting
#'
#' @keywords internal
#' @noRd
.updatePlotConfigurationAxesLabels <- function(data, plotConfiguration) {
  axesLabels <- .createAxesLabels(data, plotConfiguration)

  # Update only if the user hasn't already specified labels.
  plotConfiguration$labels$xlabel$text <- plotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  plotConfiguration$labels$ylabel$text <- plotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  return(plotConfiguration)
}


#' Created observed versus simulated paired data
#'
#' @param data A data frame from `DataCombined$toDataFrame()`, which has been
#'   further tidied using `.removeUnpairableDatasets()` and then
#'   `.unitConverter()` functions.
#' @param scaling A character specifying scale: either linear (default) or
#'   logarithmic.
#' @param tolerance Tolerance of comparison for observed and simulated time
#'   points. Default is `NULL`, in which case the internal enumerated list
#'   `.thresholdByTimeUnit` will be used to decide on what threshold to use
#'   based on the unit of time measurement.
#'
#' @family utilities-plotting
#'
#' @examples
#'
#' # create an example data frame
#' df <- dplyr::tibble(
#'   dataType = c(rep("observed", 5), rep("simulated", 3)),
#'   xValues = c(1, 3, 3.5, 4, 5, 0, 2, 4),
#'   xUnit = ospUnits$Time$min,
#'   xDimension = ospDimensions$Time,
#'   yValues = c(1.9, 6.1, 7, 8.2, 1, 0, 4, 8),
#'   yErrorValues = rnorm(8),
#'   yUnit = ospUnits$`Concentration [mass]`$`mg/l`,
#'   yDimension = ospDimensions$`Concentration (mass)`
#' )
#'
#' ospsuite:::.calculateResiduals(df)
#'
#' @keywords internal
.calculateResiduals <- function(data,
                                scaling = tlf::Scaling$lin,
                                tolerance = NULL) {
  # Extract time and values to raw vectors. Working with a single data frame is
  # not an option since the dimensions of observed and simulated data frames are
  # different.
  obsTime <- data$xValues[data$dataType == "observed"]
  obsValue <- data$yValues[data$dataType == "observed"]
  simTime <- data$xValues[data$dataType == "simulated"]
  simValue <- data$yValues[data$dataType == "simulated"]

  # If available, error values will be useful for plotting error bars in the
  # scatter plot. Even if not available, add missing values to be consistent.
  if ("yErrorValues" %in% colnames(data)) {
    yErrorValues <- data$yErrorValues[data$dataType == "observed"]
  } else {
    yErrorValues <- rep(NA_real_, length(obsValue))
  }

  # Number of observed and simulated data points
  maxSimPoints <- length(simTime)
  maxObsPoints <- length(obsTime)

  # It is important to initialize this vector to `NA`, and not to `0`.
  predValue <- rep(NA_real_, maxObsPoints)

  # For time points that are not matched, the simulated data needs to be
  # interpolated. This is because simulated data is typically sampled at a
  # higher frequency than the observed data.
  #
  # Interpolation is carried out using the Newton–Raphson method.
  #
  # If index is the same as the length of the vector, then `idx + 1` will be
  # out-of-bounds. So loop only if the index is less than the length of the
  # vector. Thus, `[-maxObsPoints]`.
  #
  # Note that this does *not* mean that the value at the last index
  # in `predValue` vector is always going to be `NA`. It is also possible
  # that there is an exact match at this time point.
  for (idx in seq_along(obsTime)[-maxObsPoints]) {
    currentObsTime <- obsTime[idx]
    currentSimTime <- simTime[idx]
    nextSimTime <- simTime[idx + 1L]
    currentSimValue <- simValue[idx]
    nextSimValue <- simValue[idx + 1L]

    # If the next simulated time point is already OOB but the last simulated
    # time point is still within the bounds of observed time points,
    # interpolation can still be carried out.
    if (idx >= maxSimPoints) {
      if (simTime[maxSimPoints] < obsTime[maxObsPoints]) {
        currentSimTime <- simTime[maxSimPoints - 1L]
        nextSimTime <- simTime[maxSimPoints]
        currentSimValue <- simValue[maxSimPoints - 1L]
        nextSimValue <- simValue[maxSimPoints]
      }
    }

    # f(x) =
    predValue[idx] <-
      # f0 * ((x1 - x) / (x1 - x0)) +
      currentSimValue * ((nextSimTime - currentObsTime) / (nextSimTime - currentSimTime)) +
      # f1 * ((x - x0) / (x1 - x0))
      nextSimValue * ((currentObsTime - currentSimTime) / (nextSimTime - currentSimTime))
  }

  # The exact tolerance used to decide when observed and simulated time points
  # match will depend on the unit used for time measurement.
  #
  # Given that this function will always be called after `.unitConverter()`,
  # there will only be a single unit across datasets.
  timeUnit <- unique(data$xUnit)
  tolerance <- tolerance %||% .thresholdByTimeUnit[[timeUnit]]

  # Figure out time points where both observed and simulated data were sampled.
  obsExactMatchIndices <- .extractMatchingIndices(obsTime, simTime, tolerance)
  simExactMatchIndices <- .extractMatchingIndices(simTime, obsTime, tolerance)

  # For exactly matched time points, there is no need for interpolation.
  predValue[obsExactMatchIndices] <- simValue[simExactMatchIndices]

  # Link observed and interpolated predicted for each observed time point using
  # a data frame.
  pairedData <- dplyr::tibble(
    "obsTime" = obsTime,
    "xUnit" = timeUnit,
    "xDimension" = unique(data$xDimension),
    "obsValue" = obsValue,
    "yErrorValues" = yErrorValues,
    "predValue" = predValue,
    "yUnit" = unique(data$yUnit),
    "yDimension" = unique(data$yDimension)
  )

  # The linear scaling is represented either of the following:
  #
  # - `"lin"` (in `DefaultPlotConfiguration`)
  # - `"identity"` (in `tlf::PlotConfiguration`, because of `{ggplot2}`)
  if (scaling %in% c("lin", "identity")) {
    pairedData <- dplyr::mutate(pairedData, resValue = obsValue - predValue)
  } else {
    pairedData <- dplyr::mutate(pairedData, resValue = log(obsValue) - log(predValue))
  }

  # Add minimum and maximum values for observed data to plot error bars
  pairedData <- dplyr::mutate(
    pairedData,
    yValuesLower = obsValue - yErrorValues,
    yValuesHigher = obsValue + yErrorValues
  )

  return(pairedData)
}

#' Compute error bar bounds from error type
#'
#' @details
#'
#' There are only three possibilities:
#'
#' - The error type is arithmetic (`DataErrorType$ArithmeticStdDev`).
#' - The error type is geometric (`DataErrorType$GeometricStdDev`).
#' - If the errors are none of these, then add `NA`s (of type `double`), since
#'   these are the only error types supported in `DataErrorType`.
#'
#' @keywords internal
#' @noRd
.computeBoundsFromErrorType <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }

  if (!all(is.na(data$yErrorValues)) && !all(is.na(data$yErrorType))) {
    data <- dplyr::mutate(data,
      yValuesLower = dplyr::case_when(
        yErrorType == DataErrorType$ArithmeticStdDev ~ yValues - yErrorValues,
        yErrorType == DataErrorType$GeometricStdDev ~ yValues / yErrorValues,
        TRUE ~ NA_real_
      ),
      yValuesHigher = dplyr::case_when(
        yErrorType == DataErrorType$ArithmeticStdDev ~ yValues + yErrorValues,
        yErrorType == DataErrorType$GeometricStdDev ~ yValues * yErrorValues,
        TRUE ~ NA_real_
      )
    )
  } else {
    # These columns should always be present in the data frame because they are
    # part of `{tlf}` mapping.
    data <- dplyr::mutate(data, yValuesLower = NA_real_, yValuesHigher = NA_real_)
  }

  return(data)
}


#' Extract data frame for scatter plot functions
#'
#' @family utilities-plotting
#'
#' @keywords internal
#' @noRd
.dataCombinedToPairedData <- function(dataCombined, defaultPlotConfiguration, scaling) {
  combinedData <- dataCombined$toDataFrame()

  # Remove the observed and simulated datasets which can't be paired.
  combinedData <- .removeUnpairableDatasets(combinedData)

  # Return early if there are no pair-able datasets present
  if (nrow(combinedData) == 0L) {
    warning(messages$plottingWithNoPairedDatasets())
    return(NULL)
  }

  # Getting all datasets to have the same units.
  combinedData <- .unitConverter(combinedData, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # Create observed versus simulated paired data using interpolation for each
  # grouping level and combine the resulting data frames in a row-wise manner.
  #
  # Both of these routines will be carried out by `dplyr::group_modify()`.
  pairedData <- combinedData %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .calculateResiduals(.x, scaling)) %>%
    dplyr::ungroup()

  return(pairedData)
}

#' Threshold to match time points
#'
#' @description
#' A named list with a unique threshold for each measurement unit for time.
#'
#' @family utilities-plotting
#'
#' @keywords internal
#' @noRd
.thresholdByTimeUnit <- list(
  s = 10,
  min = 1,
  h = 0.1,
  `day(s)` = 0.01,
  `week(s)` = 0.001,
  `month(s)` = 0.0001,
  `year(s)` = 0.00001,
  ks = 0.01
)

#' Custom function to extract matching indices
#'
#' @description
#'
#' None of the base equality/match operators (`%in%`, `==`, `all.equal`) allow
#' tolerance for comparing two numeric values. Therefore, `dplyr::near()` is
#' used.
#'
#' But even `dplyr::near()` is not up to the task because it carries out vector
#' comparison element-wise, whereas what is needed is `match()`-like behavior,
#' where each element in the first vector is compared against all values in the
#' second vector for equality.
#'
#' This custom function does exactly this.
#'
#' @inheritParams dplyr::near
#' @inheritParams .calculateResiduals
#'
#' @family utilities-plotting
#'
#' @examples
#'
#' ospsuite:::.extractMatchingIndices(c(1, 2), c(1.001, 3, 4))
#' ospsuite:::.extractMatchingIndices(c(1, 2), c(1.001, 3, 4), tolerance = 0.00001)
#' ospsuite:::.extractMatchingIndices(c(1, 2), c(3, 4))
#'
#' @keywords internal
.extractMatchingIndices <- function(x, y, tolerance = 0.001) {
  # Vectorize `dplyr::near()` function only over the `y` argument.
  # Note that that `Vectorize()` is a function operator and will return a function.
  customNear <- Vectorize(dplyr::near, vectorize.args = c("y"), SIMPLIFY = FALSE)

  # Apply the vectorized function to the two vectors and then check where the
  # comparisons are equal (i.e. `TRUE`) using `which()`.
  #
  # Use `compact()` to remove empty elements from the resulting list.
  index_list <- purrr::compact(purrr::map(customNear(x, y, tol = tolerance), which))

  # If there are any matches, return the indices as an atomic vector of integers.
  if (length(index_list) > 0L) {
    index_vector <- purrr::simplify(index_list, "integer")
    return(index_vector)
  }

  # If there are no matches, return an empty vector of `integer` type.
  return(integer(0L))
}


#' Create plot-specific `tlf::PlotConfiguration` object
#'
#' @details
#'
#' The default plot configuration and the labels needs to vary from plot-to-plot
#' because each plot has its specific (default) aesthetic needs that need to be
#' met.
#'
#' For example, although the axes labels for profile plots will be (e.g.) "Time
#' vs Fraction", they will be (e.g.) "Observed vs simulated values" for scatter
#' plots. Additionally, mapping group to line colors might be desirable for a
#' profile plot, it is not so for scatter plots.
#'
#' This function generates object of specific subclass of
#' `tlf::PlotConfiguration` needed for the given plot but with suitable defaults
#' taken from the `DefaultPlotConfiguration` object.
#'
#' @param specificPlotConfiguration A specific subclass of
#'   `tlf::PlotConfiguration` needed for the given plot.
#' @param generalPlotConfiguration A `DefaultPlotConfiguration` object.
#'
#' @family utilities-plotting
#'
#' @examples
#'
#' ospsuite:::.convertGeneralToSpecificPlotConfiguration(
#'   tlf::TimeProfilePlotConfiguration$new(),
#'   ospsuite::DefaultPlotConfiguration$new()
#' )
#'
#' @keywords internal
.convertGeneralToSpecificPlotConfiguration <- function(specificPlotConfiguration,
                                                       generalPlotConfiguration) {
  # Plot-specific configuration defaults -----------------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class.
  plotType <- class(specificPlotConfiguration)[[1]]

  # For `plotIndividualTimeProfile()` and `plotPopulationTimeProfile()`
  if (plotType == "TimeProfilePlotConfiguration") {
    generalPlotConfiguration$linesColor <- generalPlotConfiguration$linesColor %||% tlf::ColorMaps$ospDefault
    generalPlotConfiguration$linesLinetype <- generalPlotConfiguration$linesLinetype %||% tlf::Linetypes$solid
    generalPlotConfiguration$legendPosition <- generalPlotConfiguration$legendPosition %||% tlf::LegendPositions$insideTopRight
    generalPlotConfiguration$xAxisScale <- generalPlotConfiguration$xAxisScale %||% tlf::Scaling$lin
    generalPlotConfiguration$yAxisScale <- generalPlotConfiguration$yAxisScale %||% tlf::Scaling$lin
  }

  # For `plotObservedVsSimulated()`
  if (plotType == "ObsVsPredPlotConfiguration") {
    generalPlotConfiguration$linesColor <- generalPlotConfiguration$linesColor %||% "black"
    generalPlotConfiguration$legendPosition <- generalPlotConfiguration$legendPosition %||% tlf::LegendPositions$insideBottomRight
    generalPlotConfiguration$xAxisScale <- generalPlotConfiguration$xAxisScale %||% tlf::Scaling$log
    generalPlotConfiguration$yAxisScale <- generalPlotConfiguration$yAxisScale %||% tlf::Scaling$log
    # every fold distance line should get a unique type of line
    generalPlotConfiguration$linesLinetype <- generalPlotConfiguration$linesLinetype %||% names(tlf::Linetypes)
  }

  # For `plotResidualsVsTime()` and `plotResidualsVsSimulated()`
  if (plotType %in% c("ResVsTimePlotConfiguration", "ResVsPredPlotConfiguration")) {
    generalPlotConfiguration$linesColor <- generalPlotConfiguration$linesColor %||% "black"
    generalPlotConfiguration$linesLinetype <- generalPlotConfiguration$linesLinetype %||% tlf::Linetypes$dashed
    generalPlotConfiguration$xAxisScale <- generalPlotConfiguration$xAxisScale %||% tlf::Scaling$lin
    generalPlotConfiguration$yAxisScale <- generalPlotConfiguration$yAxisScale %||% tlf::Scaling$lin
  }

  # labels object ---------------------------------------

  labelTitle <- tlf::Label$new(
    text = generalPlotConfiguration$title,
    color = generalPlotConfiguration$titleColor,
    size = generalPlotConfiguration$titleSize,
    fontFace = generalPlotConfiguration$titleFontFace,
    fontFamily = generalPlotConfiguration$titleFontFamily,
    angle = generalPlotConfiguration$titleAngle,
    align = generalPlotConfiguration$titleAlign
  )

  labelSubtitle <- tlf::Label$new(
    text = generalPlotConfiguration$subtitle,
    color = generalPlotConfiguration$subtitleColor,
    size = generalPlotConfiguration$subtitleSize,
    fontFace = generalPlotConfiguration$subtitleFontFace,
    fontFamily = generalPlotConfiguration$subtitleFontFamily,
    angle = generalPlotConfiguration$subtitleAngle,
    align = generalPlotConfiguration$subtitleAlign
  )

  labelCaption <- tlf::Label$new(
    text = generalPlotConfiguration$caption,
    color = generalPlotConfiguration$captionColor,
    size = generalPlotConfiguration$captionSize,
    fontFace = generalPlotConfiguration$captionFontFace,
    fontFamily = generalPlotConfiguration$captionFontFamily,
    angle = generalPlotConfiguration$captionAngle,
    align = generalPlotConfiguration$captionAlign
  )

  labelXLabel <- tlf::Label$new(
    text = generalPlotConfiguration$xLabel,
    color = generalPlotConfiguration$xLabelColor,
    size = generalPlotConfiguration$xLabelSize,
    fontFace = generalPlotConfiguration$xLabelFontFace,
    fontFamily = generalPlotConfiguration$xLabelFontFamily,
    angle = generalPlotConfiguration$xLabelAngle,
    align = generalPlotConfiguration$xLabelAlign
  )

  labelYLabel <- tlf::Label$new(
    text = generalPlotConfiguration$yLabel,
    color = generalPlotConfiguration$yLabelColor,
    size = generalPlotConfiguration$yLabelSize,
    fontFace = generalPlotConfiguration$yLabelFontFace,
    fontFamily = generalPlotConfiguration$yLabelFontFamily,
    angle = generalPlotConfiguration$yLabelAngle,
    align = generalPlotConfiguration$yLabelAlign
  )

  labelConfiguration <- tlf::LabelConfiguration$new(
    title = labelTitle,
    subtitle = labelSubtitle,
    caption = labelCaption,
    xlabel = labelXLabel,
    ylabel = labelYLabel
  )

  # legend object ---------------------------------------

  legendTitleFont <- tlf::Font$new(
    size = generalPlotConfiguration$legendTitleSize,
    color = generalPlotConfiguration$legendTitleColor,
    fontFamily = generalPlotConfiguration$legendTitleFontFamily,
    fontFace = generalPlotConfiguration$legendTitleFontFace,
    angle = generalPlotConfiguration$legendTitleAngle,
    align = generalPlotConfiguration$legendTitleAlign
  )

  legendTitleLabel <- tlf::Label$new(
    text = generalPlotConfiguration$legendTitle,
    font = legendTitleFont
  )

  legendKeysFont <- tlf::Font$new(
    size = generalPlotConfiguration$legendKeysSize,
    color = generalPlotConfiguration$legendKeysColor,
    fontFamily = generalPlotConfiguration$legendKeysFontFamily,
    fontFace = generalPlotConfiguration$legendKeysFontFace,
    angle = generalPlotConfiguration$legendKeysAngle,
    align = generalPlotConfiguration$legendKeysAlign
  )

  legendConfiguration <- tlf::LegendConfiguration$new(
    position = generalPlotConfiguration$legendPosition,
    caption = NULL,
    title = legendTitleLabel, # for legend title aesthetics
    font = legendKeysFont, # for legend keys aesthetics
    background = NULL
  )

  # background objects -----------------------------------

  labelWatermark <- tlf::Label$new(
    text = generalPlotConfiguration$watermark,
    color = generalPlotConfiguration$watermarkColor,
    size = generalPlotConfiguration$watermarkSize,
    fontFace = generalPlotConfiguration$watermarkFontFace,
    fontFamily = generalPlotConfiguration$watermarkFontFamily,
    angle = generalPlotConfiguration$watermarkAngle,
    align = generalPlotConfiguration$watermarkAlign
  )

  plotBackground <- tlf::BackgroundElement$new(
    fill = generalPlotConfiguration$plotBackgroundFill,
    color = generalPlotConfiguration$plotBackgroundColor,
    size = generalPlotConfiguration$plotBackgroundSize,
    linetype = generalPlotConfiguration$plotBackgroundLinetype
  )

  plotPanelBackground <- tlf::BackgroundElement$new(
    fill = generalPlotConfiguration$plotPanelBackgroundFill,
    color = generalPlotConfiguration$plotPanelBackgroundColor,
    size = generalPlotConfiguration$plotPanelBackgroundSize,
    linetype = generalPlotConfiguration$plotPanelBackgroundLinetype
  )

  xAxis <- tlf::LineElement$new(
    color = generalPlotConfiguration$xAxisColor,
    size = generalPlotConfiguration$xAxisSize,
    linetype = generalPlotConfiguration$xAxisLinetype
  )

  yAxis <- tlf::LineElement$new(
    color = generalPlotConfiguration$yAxisColor,
    size = generalPlotConfiguration$yAxisSize,
    linetype = generalPlotConfiguration$yAxisLinetype
  )

  xGrid <- tlf::LineElement$new(
    color = generalPlotConfiguration$xGridColor,
    size = generalPlotConfiguration$xGridSize,
    linetype = generalPlotConfiguration$xGridLinetype
  )

  yGrid <- tlf::LineElement$new(
    color = generalPlotConfiguration$yGridColor,
    size = generalPlotConfiguration$yGridSize,
    linetype = generalPlotConfiguration$yGridLinetype
  )

  backgroundConfiguration <- tlf::BackgroundConfiguration$new(
    watermark = generalPlotConfiguration$labelWatermark,
    plot = generalPlotConfiguration$plotBackground,
    panel = generalPlotConfiguration$plotPanelBackground,
    xAxis = generalPlotConfiguration$xAxis,
    yAxis = generalPlotConfiguration$yAxis,
    xGrid = generalPlotConfiguration$xGrid,
    yGrid = generalPlotConfiguration$yGrid
  )

  # xAxis objects -----------------------------------

  xAxisFont <- tlf::Font$new(
    size = generalPlotConfiguration$xAxisLabelTicksSize,
    color = generalPlotConfiguration$xAxisLabelTicksColor,
    fontFamily = generalPlotConfiguration$xAxisLabelTicksFontFamily,
    fontFace = generalPlotConfiguration$xAxisLabelTicksFontFace,
    angle = generalPlotConfiguration$xAxisLabelTicksAngle,
    align = generalPlotConfiguration$xAxisLabelTicksAlign
  )

  xAxisConfiguration <- tlf::XAxisConfiguration$new(
    limits = generalPlotConfiguration$xAxisLimits,
    scale = generalPlotConfiguration$xAxisScale,
    ticks = generalPlotConfiguration$xAxisTicks,
    ticklabels = generalPlotConfiguration$xAxisTicksLabels,
    font = generalPlotConfiguration$xAxisFont,
    expand = generalPlotConfiguration$xAxisExpand
  )

  # yAxis objects -----------------------------------

  yAxisFont <- tlf::Font$new(
    size = generalPlotConfiguration$yAxisLabelTicksSize,
    color = generalPlotConfiguration$yAxisLabelTicksColor,
    fontFamily = generalPlotConfiguration$yAxisLabelTicksFontFamily,
    fontFace = generalPlotConfiguration$yAxisLabelTicksFontFace,
    angle = generalPlotConfiguration$yAxisLabelTicksAngle,
    align = generalPlotConfiguration$yAxisLabelTicksAlign
  )

  yAxisConfiguration <- tlf::YAxisConfiguration$new(
    limits = generalPlotConfiguration$yAxisLimits,
    scale = generalPlotConfiguration$yAxisScale,
    ticks = generalPlotConfiguration$yAxisTicks,
    ticklabels = generalPlotConfiguration$yAxisTicksLabels,
    font = generalPlotConfiguration$yAxisFont,
    expand = generalPlotConfiguration$yAxisExpand
  )

  # lines -------------------------------------------------------

  linesConfiguration <- tlf::ThemeAestheticSelections$new(
    color = generalPlotConfiguration$linesColor,
    shape = generalPlotConfiguration$linesShape,
    size = generalPlotConfiguration$linesSize,
    linetype = generalPlotConfiguration$linesLinetype,
    alpha = generalPlotConfiguration$linesAlpha
  )

  # points -------------------------------------------------------

  pointsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = generalPlotConfiguration$pointsColor,
    shape = generalPlotConfiguration$pointsShape,
    size = generalPlotConfiguration$pointsSize,
    alpha = generalPlotConfiguration$pointsAlpha
  )

  # ribbons -------------------------------------------------------

  ribbonsConfiguration <- tlf::ThemeAestheticSelections$new(
    fill = generalPlotConfiguration$ribbonsFill,
    shape = generalPlotConfiguration$ribbonsShape,
    size = generalPlotConfiguration$ribbonsSize,
    linetype = generalPlotConfiguration$ribbonsLinetype,
    alpha = generalPlotConfiguration$ribbonsAlpha
  )

  # errorbars -------------------------------------------------------

  errorbarsConfiguration <- tlf::ThemeAestheticSelections$new(
    size = generalPlotConfiguration$errorbarsSize,
    # TODO: https://github.com/Open-Systems-Pharmacology/TLF-Library/issues/347
    # capSize = generalPlotConfiguration$errorbarsCapSize,
    linetype = generalPlotConfiguration$errorbarsLinetype,
    alpha = generalPlotConfiguration$errorbarsAlpha
  )

  # Update specific plot configuration object ----------------------

  # Do one-to-one mappings of public fields
  specificPlotConfiguration$labels <- labelConfiguration
  specificPlotConfiguration$legend <- legendConfiguration
  specificPlotConfiguration$xAxis <- xAxisConfiguration
  specificPlotConfiguration$yAxis <- yAxisConfiguration
  specificPlotConfiguration$background <- backgroundConfiguration
  specificPlotConfiguration$lines <- linesConfiguration
  specificPlotConfiguration$points <- pointsConfiguration
  specificPlotConfiguration$ribbons <- ribbonsConfiguration
  specificPlotConfiguration$errorbars <- errorbarsConfiguration

  return(specificPlotConfiguration)
}
