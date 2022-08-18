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
#' plotObservedVsSimulated(myDataCombined, myPlotConfiguration)
#' @export
plotObservedVsSimulated <- function(dataCombined,
                                    defaultPlotConfiguration = NULL,
                                    foldDistance = 2) {
  # validation -----------------------------

  defaultPlotConfiguration <- .validateDefaultPlotConfiguration(defaultPlotConfiguration)

  .validateDataCombinedForPlotting(dataCombined)
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
    obsVsPredPlotConfiguration$xAxis$scale == tlf::Scaling$identity ||
      obsVsPredPlotConfiguration$yAxis$scale == tlf::Scaling$identity
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
  pairedData <- calculateResiduals(dataCombined,
    scaling = obsVsPredPlotConfiguration$yAxis$scale,
    xUnit = defaultPlotConfiguration$xUnit,
    yUnit = defaultPlotConfiguration$yUnit
  )

  # Quit early if there is no data to visualize.
  if (is.null(pairedData)) {
    return(NULL)
  }

  # Add minimum and maximum values for observed data to plot error bars
  pairedData <- dplyr::mutate(
    pairedData,
    yValuesObservedLower = yValuesObserved - yErrorValues,
    yValuesObservedHigher = yValuesObserved + yErrorValues,
    .after = yValuesObserved # Create new columns after `yValuesObserved` column
  )

  # Time points at which predicted values can't be interpolated, and need to be
  # extrapolated.
  #
  # This will happen in rare case scenarios where simulated data is sampled at a
  # lower frequency than observed data.
  predictedValuesMissingIndices <- which(is.na(pairedData$yValuesSimulated))

  # Warn the user about failure to interpolate.
  if (length(predictedValuesMissingIndices) > 0) {
    warning(
      messages$printMultipleEntries(
        header = messages$valuesNotInterpolated(),
        entries = pairedData$xValues[predictedValuesMissingIndices]
      )
    )
  }

  # axes labels -----------------------------

  obsVsPredPlotConfiguration <- .updatePlotConfigurationAxesLabels(pairedData, obsVsPredPlotConfiguration)

  # plot -----------------------------

  tlf::setDefaultErrorbarCapSize(defaultPlotConfiguration$errorbarsCapSize)

  tlf::plotObsVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ObsVsPredDataMapping$new(
      x     = "yValuesObserved",
      y     = "yValuesSimulated",
      group = "group",
      xmin  = "yValuesObservedLower",
      xmax  = "yValuesObservedHigher"
    ),
    foldDistance = foldDistance,
    plotConfiguration = obsVsPredPlotConfiguration
  )
}
