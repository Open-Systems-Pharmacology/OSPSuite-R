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
                                defaultPlotConfiguration = NULL,
                                smoother = NULL) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L) # only single instance is allowed
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

  # data frames -----------------------------

  combinedData <- dataCombined$toDataFrame()

  # Remove the observed and simulated datasets which can't be paired.
  combinedData <- .removeUnpairableDatasets(combinedData)

  # Getting all units on the same scale
  combinedData <- .unitConverter(combinedData, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # FIXME: placeholder
  pairedData <- .calculateResiduals(combinedData)

  # `ResVsTimePlotConfiguration` object -----------------------------

  # Create an instance of `ResVsTimePlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  resVsTimePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    data = combinedData,
    specificPlotConfiguration = tlf::ResVsTimePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # axes labels -----------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class. The labels can then
  # be prepared accordingly.
  axesLabels <- .createAxesLabels(combinedData, resVsTimePlotConfiguration)
  resVsTimePlotConfiguration$labels$xlabel$text <- resVsTimePlotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  resVsTimePlotConfiguration$labels$ylabel$text <- resVsTimePlotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  # plot -----------------------------

  tlf::plotObsVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ResVsTimeDataMapping$new(
      x = "xValues",
      y = "resValue",
      group = "group",
      lines = NULL
    ),
    smoother = smoother,
    plotConfiguration = resVsTimePlotConfiguration
  )
}
