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
#' # simulated data
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simResults <- runSimulation(sim)
#' outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#'
#' # observed data
#' obsData <- lapply(
#'   c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
#'   function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
#' )
#' names(obsData) <- lapply(obsData, function(x) x$name)
#'
#'
#' # Create a new instance of `DataCombined` class
#' myDataCombined <- DataCombined$new()
#'
#' # Add simulated results
#' myDataCombined$addSimulationResults(
#'   simulationResults = simResults,
#'   quantitiesOrPaths = outputPath,
#'   groups = "Aciclovir PVB"
#' )
#'
#' # Add observed data set
#' myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")
#'
#' # Create a new instance of `DefaultPlotConfiguration` class
#' myPlotConfiguration <- DefaultPlotConfiguration$new()
#' myPlotConfiguration$title <- "My Plot Title"
#' myPlotConfiguration$subtitle <- "My Plot Subtitle"
#' myPlotConfiguration$caption <- "My Sources"
#'
#' # plot
#' plotIndividualTimeProfile(myDataCombined, myPlotConfiguration)
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
    obsData <- .computeBoundsFromErrorType(obsData)
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

  # To avoid repetition, assign column names to variables and use them instead
  x <- "xValues"
  y <- "yValues"
  ymin <- "yValuesLower"
  ymax <- "yValuesHigher"
  group <- color <- "group"
  linetype <- shape <- "name"

  # population time profile mappings ------------------------------

  # The exact mappings chosen will depend on whether there are multiple datasets
  # of a given type present per group
  if (!is.null(quantiles)) {
    if (hasMultipleSimDatasetsPerGroup) {
      simulatedDataMapping <- tlf::TimeProfileDataMapping$new(x, y, ymin, ymax,
        color = color,
        linetype = linetype
      )
    } else {
      simulatedDataMapping <- tlf::TimeProfileDataMapping$new(x, y, ymin, ymax,
        group = group
      )
    }

    if (hasMultipleObsDatasetsPerGroup) {
      observedDataMapping <- tlf::ObservedDataMapping$new(x, y, ymin, ymax,
        shape = shape,
        color = color
      )
    } else {
      observedDataMapping <- tlf::ObservedDataMapping$new(x, y, ymin, ymax,
        group = group
      )
    }
  }

  # individual time profile mappings ------------------------------

  if (is.null(quantiles)) {
    if (hasMultipleSimDatasetsPerGroup) {
      simulatedDataMapping <- tlf::TimeProfileDataMapping$new(x, y,
        color = color,
        linetype = linetype
      )
    } else {
      simulatedDataMapping <- tlf::TimeProfileDataMapping$new(x, y,
        group = group
      )
    }

    if (hasMultipleObsDatasetsPerGroup) {
      observedDataMapping <- tlf::ObservedDataMapping$new(x, y, ymin, ymax,
        shape = shape,
        color = color
      )
    } else {
      observedDataMapping <- tlf::ObservedDataMapping$new(x, y, ymin, ymax,
        group = group
      )
    }
  }

  tlf::setDefaultErrorbarCapSize(defaultPlotConfiguration$errorbarsCapSize)

  profilePlot <- tlf::plotTimeProfile(
    data = simData,
    dataMapping = simulatedDataMapping,
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
