#' Observed versus predicted/simulated scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @param foldDistance A vector for plotting lines at required fold distances
#'   The vector can include only fold distance values `>1`. An
#'   `x`-fold distance is defined as all simulated values within the range
#'   between `x`-fold (depicted by the upper fold range line) and `1/x`-fold
#'   (depicted by the lower fold range line) of observed values. The identity
#'   line can be interpreted as the `1`-fold range.
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

  # foldDistance should be above 1
  if (any(foldDistance <= 1)) {
    stop(messages$plotObservedVsSimulatedWrongFoldDistance("foldDistance", foldDistance))
  }

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

  # In logarithmic scale, if any of the values are `0`, plotting will fail.
  #
  # To avoid this, just remove rows where any of the quantities are `0`s.
  if (obsVsPredPlotConfiguration$yAxis$scale %in% c(tlf::Scaling$log, tlf::Scaling$ln)) {
    pairedData <- dplyr::filter(
      pairedData,
      yValuesObserved != 0, yValuesSimulated != 0
    )
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

  # Since groups might include more than one observed dataset (indicated by shape)
  # in a group (indicated by color), we have to override the default shape legend
  # and assign a manual shape to each legend entry
  # The shapes follow the settings in the user-provided plot configuration
  overrideShapeAssignment <- pairedData %>%
    dplyr::select(name, group) %>%
    dplyr::distinct() %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(shapeAssn = obsVsPredPlotConfiguration$points$shape[1:nrow(.)]) %>%
    dplyr::filter(!duplicated(group))

  plotObject <- tlf::plotObsVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ObsVsPredDataMapping$new(
      x     = "yValuesObserved",
      y     = "yValuesSimulated",
      group = "group",
      xmin  = "yValuesObservedLower",
      xmax  = "yValuesObservedHigher",
      shape = "name"
    ),
    foldDistance = foldDistance,
    plotConfiguration = obsVsPredPlotConfiguration
  )

  return(plotObject + ggplot2::guides(
    shape = "none",
    col = ggplot2::guide_legend(
      title = obsVsPredPlotConfiguration$legend$title$text,
      title.theme = obsVsPredPlotConfiguration$legend$title$createPlotFont(),
      override.aes = list(shape = overrideShapeAssignment$shapeAssn)
    )
  ))
}
