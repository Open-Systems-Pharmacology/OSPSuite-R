#' Time-profile plot of individual data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams calculateResiduals
#' @param defaultPlotConfiguration A `DefaultPlotConfiguration` object, which is
#'   an `R6` class object that defines plot properties.
#' @param showLegendPerDataset Logical flag to display separate legend entries
#' for observed and simulated datasets, if available. This is experimental and
#' may not work reliably when both observed and simulated datasets > 1. Defaults
#' to `FALSE`.
#'
#' @import tlf
#'
#' @family plotting
#'
#' @examples
#' # simulated data
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simResults <- runSimulations(sim)[[1]]
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
plotIndividualTimeProfile <- function(
  dataCombined,
  defaultPlotConfiguration = NULL,
  showLegendPerDataset = FALSE
) {
  lifecycle::deprecate_soft(
    when = "12.5",
    what = "plotIndividualTimeProfile()",
    with = "plotTimeProfile()",
    details = "It will be removed in version 14.0."
  )
  .plotTimeProfile(dataCombined, defaultPlotConfiguration, showLegendPerDataset)
}


#' Common plotting function for creating time-profile plot
#'
#' @keywords internal
#' @noRd
.plotTimeProfile <- function(
  dataCombined,
  defaultPlotConfiguration = NULL,
  showLegendPerDataset,
  aggregation = NULL,
  ...
) {
  # validation -----------------------------

  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(
    defaultPlotConfiguration
  )

  validateIsLogical(showLegendPerDataset)
  .validateDataCombinedForPlotting(dataCombined)
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

  # Getting all units on the same scale
  combinedData <- convertUnits(
    dataCombined,
    defaultPlotConfiguration$xUnit,
    defaultPlotConfiguration$yUnit
  )

  # Datasets which haven't been assigned to any group will be plotted as a group
  # on its own. That is, the `group` column entries for them will be their names.
  combinedData <- .addMissingGroupings(combinedData)

  # axes labels -----------------------------

  timeProfilePlotConfiguration <- .updatePlotConfigurationAxesLabels(
    combinedData,
    timeProfilePlotConfiguration
  )

  # plot -----------------------------

  obsData <- as.data.frame(dplyr::filter(combinedData, dataType == "observed"))

  if (nrow(obsData) == 0) {
    obsData <- NULL
  } else {
    obsData <- .computeBoundsFromErrorType(obsData)
  }

  simData <- as.data.frame(dplyr::filter(combinedData, dataType == "simulated"))

  if (nrow(simData) == 0) {
    simData <- NULL
  }

  # Extract aggregated simulated data (relevant only for the population plot)
  if (!is.null(aggregation) && !is.null(simData)) {
    simData <- as.data.frame(.extractAggregatedSimulatedData(
      simData,
      aggregation,
      ...
    ))
  }

  # To avoid repetition, assign column names to variables and use them instead
  x <- "xValues"
  y <- "yValues"
  ymin <- "yValuesLower"
  ymax <- "yValuesHigher"
  color <- fill <- "group"
  linetype <- shape <- "name"

  # LLOQ is not mapped by default
  lloq <- NULL
  # Map LLOQ if defaultPlotConfiguration$displayLLOQ is set to TRUE and lloq
  # column contains at least one non NA value.
  if (
    defaultPlotConfiguration$displayLLOQ & !all(is.na(unique(obsData$lloq)))
  ) {
    lloq <- "lloq"
  }

  # population time profile mappings with ribbon ------------------------------

  # The exact mappings chosen will depend on whether there are multiple datasets
  # of a given type present per group
  if (!is.null(aggregation)) {
    simulatedDataMapping <- tlf::TimeProfileDataMapping$new(
      x,
      y,
      ymin,
      ymax,
      color = color,
      linetype = linetype,
      fill = fill
    )
    # individual time profile mappings ------------------------------------------
  } else {
    simulatedDataMapping <- tlf::TimeProfileDataMapping$new(
      x,
      y,
      color = color,
      linetype = linetype
    )
  }

  observedDataMapping <- tlf::ObservedDataMapping$new(
    x,
    y,
    ymin,
    ymax,
    shape = shape,
    color = color,
    lloq = lloq
  )

  tlf::setDefaultErrorbarCapSize(defaultPlotConfiguration$errorbarsCapSize)

  profilePlot <- tlf::plotTimeProfile(
    data = simData,
    dataMapping = simulatedDataMapping,
    observedData = obsData,
    observedDataMapping = observedDataMapping,
    plotConfiguration = timeProfilePlotConfiguration
  )

  # Suppress certain mappings in the legend
  if (!showLegendPerDataset) {
    profilePlot <- profilePlot +
      ggplot2::guides(linetype = "none", shape = "none")
  } else {
    # Build guides list conditionally based on whether fill is used
    guidesList <- list(
      linetype = ggplot2::guide_legend(title = NULL, order = 0),
      shape = ggplot2::guide_legend(title = NULL, order = 0),
      color = ggplot2::guide_legend(title = NULL, order = 1)
    )
    # Only include fill guide if aggregation is used (population plots)
    if (!is.null(aggregation)) {
      guidesList$fill <- ggplot2::guide_legend(title = NULL, order = 1)
    }
    profilePlot <- profilePlot +
      ggplot2::guides(!!!guidesList)
  }

  return(profilePlot)
}
