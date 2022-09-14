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
#' plotResidualsVsSimulated(myDataCombined, myPlotConfiguration)
#'
#' @export
plotResidualsVsSimulated <- function(dataCombined,
                                     defaultPlotConfiguration = NULL) {
  # validation -----------------------------

  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(defaultPlotConfiguration)

  .validateDataCombinedForPlotting(dataCombined)
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
  pairedData <- calculateResiduals(dataCombined,
    scaling = resVsPredPlotConfiguration$yAxis$scale,
    xUnit = defaultPlotConfiguration$xUnit,
    yUnit = defaultPlotConfiguration$yUnit
  )

  # Quit early if there is no data to visualize.
  if (is.null(pairedData)) {
    return(NULL)
  }

  # axes labels -----------------------------

  resVsPredPlotConfiguration <- .updatePlotConfigurationAxesLabels(pairedData, resVsPredPlotConfiguration)

  # plot -----------------------------

  tlf::setDefaultErrorbarCapSize(defaultPlotConfiguration$errorbarsCapSize)

  tlf::plotResVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ResVsPredDataMapping$new(
      x = "yValuesSimulated",
      y = "residualValues",
      group = "group",
      shape = "name"
    ),
    plotConfiguration = resVsPredPlotConfiguration
  ) + ggplot2::guides(shape = "none") # Suppress certain mappings in the legend
}
