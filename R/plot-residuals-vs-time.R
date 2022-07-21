#' Residuals versus time scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams tlf::plotResVsTime
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
plotResidualsVsTime <- function(dataCombined,
                                defaultPlotConfiguration = NULL) {
  # validation -----------------------------

  .validateDataCombinedForPlotting(dataCombined)
  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(defaultPlotConfiguration)

  if (is.null(dataCombined$groupMap)) {
    return(NULL)
  }

  # `ResVsTimePlotConfiguration` object -----------------------------

  # Create an instance of `ResVsTimePlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  resVsTimePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    specificPlotConfiguration = tlf::ResVsTimePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # This should never be the case as the residuals should be centered around 0.
  is_y_scale_logarithmic <- resVsTimePlotConfiguration$yAxis$scale == "log"
  if (is_y_scale_logarithmic) {
    stop(messages$logScaleNotAllowed())
  }

  # data frames -----------------------------

  # Create a paired data frame (observed versus simulated) from `DataCombined` object.
  #
  # `DefaultPlotConfiguration` provides units for conversion.
  # `PlotConfiguration` provides scaling details needed while computing residuals.
  pairedData <- .dataCombinedToPairedData(dataCombined, defaultPlotConfiguration, resVsTimePlotConfiguration$yAxis$scale)

  # Quit early if there is no data to visualize.
  if (is.null(pairedData)) {
    return(NULL)
  }

  # axes labels -----------------------------

  resVsTimePlotConfiguration <- .updatePlotConfigurationAxesLabels(pairedData, resVsTimePlotConfiguration)

  # plot -----------------------------

  tlf::plotResVsTime(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ResVsTimeDataMapping$new(
      x = "obsValue",
      y = "resValue",
      group = "group"
    ),
    plotConfiguration = resVsTimePlotConfiguration
  )
}
