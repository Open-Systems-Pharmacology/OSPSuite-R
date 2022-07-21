#' Time-profile plot of individual data
#'
#' @param dataCombined A `DataCombined` object.
#' @param defaultPlotConfiguration A `DefaultPlotConfiguration` object, which is
#'   an `R6` class object that defines plot properties.
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
plotIndividualTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL) {
  .plotTimeProfile(dataCombined, defaultPlotConfiguration)
}


#' Common plotting function for creating time-profile plot
#'
#' @keywords internal
#' @noRd
.plotTimeProfile <- function(dataCombined,
                             defaultPlotConfiguration = NULL,
                             quantiles = NULL) {
  # validation -----------------------------

  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = TRUE)
  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L) # only single instance is allowed

  if (is.null(dataCombined$groupMap)) {
    warning(messages$plottingWithEmptyDataCombined())
    return(NULL)
  }

  # data frames -----------------------------

  combinedData <- dataCombined$toDataFrame()

  # Getting all units on the same scale
  combinedData <- .unitConverter(combinedData, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # Datasets which haven't been assigned to any group will be plotted as a group
  # on its own. That is, the `group` column entries for them will be their names.
  combinedData <- .addMissingGroupings(combinedData)

  # `TimeProfilePlotConfiguration` object -----------------------------

  # Create an instance of `TimeProfilePlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  timeProfilePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    specificPlotConfiguration = tlf::TimeProfilePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # axes labels -----------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class. The labels can then
  # be prepared accordingly.
  axesLabels <- .createAxesLabels(combinedData, timeProfilePlotConfiguration)
  timeProfilePlotConfiguration$labels$xlabel$text <- timeProfilePlotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  timeProfilePlotConfiguration$labels$ylabel$text <- timeProfilePlotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  # plot -----------------------------

  obsData <- as.data.frame(dplyr::filter(combinedData, dataType == "observed"))

  if (nrow(obsData) == 0) {
    obsData <- NULL
  }

  simData <- as.data.frame(dplyr::filter(combinedData, dataType == "simulated"))

  if (nrow(simData) == 0) {
    simData <- NULL
  }

  # Extract aggregated simulated data (relevant only for the population plot)
  if (!is.null(quantiles) && !is.null(simData)) {
    simData <- as.data.frame(.extractAggregatedSimulatedData(simData, quantiles))
  }

  if (!is.null(quantiles)) {
    dataMapping <- tlf::TimeProfileDataMapping$new(
      x = "xValues",
      y = "yValuesCentral",
      ymin = "yValuesLower",
      ymax = "yValuesHigher",
      group = "group"
    )
  } else {
    dataMapping <- tlf::TimeProfileDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group"
    )
  }

  profilePlot <- tlf::plotTimeProfile(
    data = simData,
    dataMapping = dataMapping,
    observedData = obsData,
    observedDataMapping = tlf::ObservedDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group",
      error = "yErrorValues"
    ),
    plotConfiguration = timeProfilePlotConfiguration
  )

  return(profilePlot)
}
