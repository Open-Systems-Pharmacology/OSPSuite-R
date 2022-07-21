#' Observed versus predicted/simulated scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @param foldDistance A vector for plotting lines at required fold distances
#'   The vector can include only fold distance values different from `1`. Even
#'   if it is not specified, it will **always** be included.
#'
#' @import tlf
#'
#' @family plotting
#'
#' @examples
#'
#' # TODO: add example
#'
#' @export
plotObservedVsSimulated <- function(dataCombined,
                                    defaultPlotConfiguration = NULL,
                                    foldDistance = 2) {
  # validation -----------------------------

  .validateDataCombinedForPlotting(dataCombined)
  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(defaultPlotConfiguration)

  if (is.null(dataCombined$groupMap)) {
    return(NULL)
  }

  # `ObsVsPredPlotConfiguration` object -----------------------------

  # Create an instance of `ObsVsPredPlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  obsVsPredPlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    specificPlotConfiguration = tlf::ObsVsPredPlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # Linear scaling is stored as identity scaling in `{tlf}`
  is_any_scale_linear <- (
    obsVsPredPlotConfiguration$xAxis$scale == "identity" ||
      obsVsPredPlotConfiguration$yAxis$scale == "identity"
  )

  # The argument `foldDistance` should only include fold values different from
  # the default value, which must always be present.
  #
  # The default value depends on the scale:
  #
  # - For linear scale: `1`
  # - For logarithmic scale: `0`
  defaultFoldDistance <- ifelse(is_any_scale_linear, 0, 1)

  if (!any(dplyr::near(defaultFoldDistance, foldDistance))) {
    foldDistance <- c(defaultFoldDistance, foldDistance)
  }

  if (is_any_scale_linear && !is.null(foldDistance)) {
    warning(messages$linearScaleWithFoldDistance())
    foldDistance <- 0
  }

  # data frames -----------------------------

  combinedData <- dataCombined$toDataFrame()

  # Remove the observed and simulated datasets which can't be paired.
  combinedData <- .removeUnpairableDatasets(combinedData)

  # Return early if there are no pair-able datasets present
  if (nrow(combinedData) == 0L) {
    warning(messages$plottingWithNoPairedDatasets())
    return(NULL)
  }

  # Getting all units on the same scale
  combinedData <- .unitConverter(combinedData, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # paired data frame -----------------------------

  # Create observed versus simulated paired data using interpolation for each
  # grouping level and combine the resulting data frames in a row-wise manner.
  #
  # Both of these routines will be carried out by `dplyr::group_modify()`.
  pairedData <- combinedData %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .calculateResiduals(.x, scaling = obsVsPredPlotConfiguration$yAxis$scale)) %>%
    dplyr::ungroup()

  # Add min and max values for horizontal error bars
  pairedData <- dplyr::mutate(
    pairedData,
    obsValueLower = obsValue - obsErrorValue,
    obsValueHigher = obsValue + obsErrorValue
  )

  # Time points at which predicted values can't be interpolated, and need to be
  # extrapolated.
  #
  # This will happen in rare case scenarios where simulated data is sampled at a
  # lower frequency than observed data.
  predValueMissingIndices <- which(is.na(pairedData$predValue))

  # Warn the user about failure to interpolate.
  if (length(predValueMissingIndices) > 0) {
    warning(
      messages$printMultipleEntries(
        header = messages$valuesNotInterpolated(),
        entries = pairedData$obsTime[predValueMissingIndices]
      )
    )
  }

  # axes labels -----------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class. The labels can then
  # be prepared accordingly.
  axesLabels <- .createAxesLabels(combinedData, obsVsPredPlotConfiguration)
  obsVsPredPlotConfiguration$labels$xlabel$text <- obsVsPredPlotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  obsVsPredPlotConfiguration$labels$ylabel$text <- obsVsPredPlotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  # plot -----------------------------

  tlf::plotObsVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ObsVsPredDataMapping$new(
      x = "obsValue",
      y = "predValue",
      group = "group",
      xmin = "obsValueLower",
      xmax = "obsValueHigher",
      lines = NULL
    ),
    foldDistance = foldDistance,
    smoother = NULL,
    plotConfiguration = obsVsPredPlotConfiguration
  )
}
