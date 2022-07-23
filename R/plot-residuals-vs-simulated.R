#' Residuals versus time scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams tlf::plotResVsPred
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
plotResidualsVsSimulated <- function(dataCombined,
                                     defaultPlotConfiguration = NULL) {
  # validation -----------------------------

  .validateDataCombinedForPlotting(dataCombined)
  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(defaultPlotConfiguration)

  if (is.null(dataCombined$groupMap)) {
    return(NULL)
  }

  # `ResVsPredPlotConfiguration` object -----------------------------

  # Create an instance of plot-specific class object
  resVsPredPlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    specificPlotConfiguration = tlf::ResVsPredPlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # This should never be the case as the residuals should be centered around 0.
  is_y_scale_logarithmic <- resVsPredPlotConfiguration$yAxis$scale == "log"
  if (is_y_scale_logarithmic) {
    stop(messages$logScaleNotAllowed())
  }

  # data frames -----------------------------

  # Create a paired data frame (observed versus simulated) from `DataCombined` object.
  #
  # `DefaultPlotConfiguration` provides units for conversion.
  # `PlotConfiguration` provides scaling details needed while computing residuals.
  pairedData <- .dataCombinedToPairedData(dataCombined, defaultPlotConfiguration, resVsPredPlotConfiguration$yAxis$scale)

  # Quit early if there is no data to visualize.
  if (is.null(pairedData)) {
    return(NULL)
  }

  # axes labels -----------------------------

  resVsPredPlotConfiguration <- .updatePlotConfigurationAxesLabels(pairedData, resVsPredPlotConfiguration)

  # plot -----------------------------

  tlf::setDefaultErrorbarCapWidth(defaultPlotConfiguration$errorbarsCapWidth)

  tlf::plotResVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ResVsPredDataMapping$new(
      x = "predValue",
      y = "resValue",
      group = "group"
    ),
    plotConfiguration = resVsPredPlotConfiguration
  )
}
