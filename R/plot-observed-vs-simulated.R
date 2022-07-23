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

  # Create an instance of plot-specific class object
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

  # Create a paired data frame (observed versus simulated) from `DataCombined` object.
  #
  # `DefaultPlotConfiguration` provides units for conversion.
  # `PlotConfiguration` provides scaling details needed while computing residuals.
  pairedData <- .dataCombinedToPairedData(dataCombined, defaultPlotConfiguration, obsVsPredPlotConfiguration$yAxis$scale)

  # Quit early if there is no data to visualize.
  if (is.null(pairedData)) {
    return(NULL)
  }

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

  obsVsPredPlotConfiguration <- .updatePlotConfigurationAxesLabels(pairedData, obsVsPredPlotConfiguration)

  # plot -----------------------------

  tlf::setDefaultErrorbarCapWidth(defaultPlotConfiguration$errorbarsCapWidth)

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
    plotConfiguration = obsVsPredPlotConfiguration
  )
}
