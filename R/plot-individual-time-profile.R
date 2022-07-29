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

  .validateDataCombinedForPlotting(dataCombined)
  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(defaultPlotConfiguration)

  if (is.null(dataCombined$groupMap)) {
    return(NULL)
  }

  # `TimeProfilePlotConfiguration` object -----------------------------

  # Create an instance of plot-specific class object
  timeProfilePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    specificPlotConfiguration = tlf::TimeProfilePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # data frames -----------------------------

  combinedData <- dataCombined$toDataFrame()

  # Getting all units on the same scale
  combinedData <- .unitConverter(combinedData, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # Datasets which haven't been assigned to any group will be plotted as a group
  # on its own. That is, the `group` column entries for them will be their names.
  combinedData <- .addMissingGroupings(combinedData)

  # axes labels -----------------------------

  timeProfilePlotConfiguration <- .updatePlotConfigurationAxesLabels(combinedData, timeProfilePlotConfiguration)

  # plot -----------------------------

  obsData <- as.data.frame(dplyr::filter(combinedData, dataType == "observed"))

  if (nrow(obsData) == 0) {
    obsData <- NULL
    hasMultipleObsDatasetsPerGroup <- FALSE
  } else {
    hasMultipleObsDatasetsPerGroup <- .hasMultipleDatasetsPerGroup(obsData)
  }


  simData <- as.data.frame(dplyr::filter(combinedData, dataType == "simulated"))

  if (nrow(simData) == 0) {
    simData <- NULL
    hasMultipleSimDatasetsPerGroup <- FALSE
  } else {
    hasMultipleSimDatasetsPerGroup <- .hasMultipleDatasetsPerGroup(simData)
  }

  # Extract aggregated simulated data (relevant only for the population plot)
  if (!is.null(quantiles) && !is.null(simData)) {
    simData <- as.data.frame(.extractAggregatedSimulatedData(simData, quantiles))
  }

  if (!is.null(quantiles)) {
    if (hasMultipleSimDatasetsPerGroup) {
      dataMapping <- tlf::TimeProfileDataMapping$new(
        x = "xValues",
        y = "yValuesCentral",
        ymin = "yValuesLower",
        ymax = "yValuesHigher",
        color = "group",
        linetype = "name"
      )
    } else {
      dataMapping <- tlf::TimeProfileDataMapping$new(
        x = "xValues",
        y = "yValuesCentral",
        ymin = "yValuesLower",
        ymax = "yValuesHigher",
        group = "group"
      )
    }

    if (hasMultipleObsDatasetsPerGroup) {
      observedDataMapping <- tlf::ObservedDataMapping$new(
        x = "xValues",
        y = "yValues",
        shape = "name",
        color = "group"
      )
    } else {
      observedDataMapping <- tlf::ObservedDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group"
      )
    }
  } else {
    obsData <- .computeBoundsFromErrorType(obsData)

    if (hasMultipleSimDatasetsPerGroup) {
      dataMapping <- tlf::TimeProfileDataMapping$new(
        x = "xValues",
        y = "yValues",
        color = "group",
        linetype = "name"
      )
    } else {
      dataMapping <- tlf::TimeProfileDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group"
      )
    }

    if (hasMultipleObsDatasetsPerGroup) {
      observedDataMapping <- tlf::ObservedDataMapping$new(
        x = "xValues",
        y = "yValues",
        ymin = "yValuesLower",
        ymax = "yValuesHigher",
        shape = "name",
        color = "group"
      )
    } else {
      observedDataMapping <- tlf::ObservedDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group",
        ymin = "yValuesLower",
        ymax = "yValuesHigher"
      )
    }
  }

  tlf::setDefaultErrorbarCapSize(defaultPlotConfiguration$errorbarsCapSize)

  profilePlot <- tlf::plotTimeProfile(
    data = simData,
    dataMapping = dataMapping,
    observedData = obsData,
    observedDataMapping = observedDataMapping,
    plotConfiguration = timeProfilePlotConfiguration
  )

  if (hasMultipleSimDatasetsPerGroup) {
    profilePlot <- profilePlot + ggplot2::guides(linetype = "none")
  }

  if (hasMultipleObsDatasetsPerGroup) {
    profilePlot <- profilePlot + ggplot2::guides(shape = "none")
  }

  return(profilePlot)
}
