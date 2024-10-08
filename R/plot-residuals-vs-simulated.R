#' Residuals versus time scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams tlf::plotResVsPred
#' @param scaling A character of length one specifying the scale type for residual. can be lin or log.
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
#' plotResidualsVsSimulated(myDataCombined,
#'   scaling = "log",
#'   defaultPlotConfiguration = myPlotConfiguration
#' )
#'
#' @export
plotResidualsVsSimulated <- function(dataCombined,
                                     defaultPlotConfiguration = NULL,
                                     scaling = "lin") {
  # validation -----------------------------

  rlang::arg_match(scaling, values = c("lin", "log"))

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
    scaling = scaling,
    xUnit = defaultPlotConfiguration$xUnit,
    yUnit = defaultPlotConfiguration$yUnit
  )

  # Quit early if there is no data to visualize.
  if (is.null(pairedData)) {
    return(NULL)
  }

  # Since groups might include more than one observed dataset (indicated by shape)
  # in a group (indicated by color), we have to override the default shape legend
  # and assign a manual shape to each legend entry
  # The shapes follow the settings in the user-provided plot configuration
  overrideShapeAssignment <- pairedData %>%
    dplyr::select(name, group) %>%
    dplyr::distinct() %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(shapeAssn = unlist(tlf::Shapes[resVsPredPlotConfiguration$points$shape[1:nrow(.)]])) %>%
    dplyr::filter(!duplicated(group))

  # axes labels -----------------------------

  resVsPredPlotConfiguration <- .updatePlotConfigurationAxesLabels(pairedData, resVsPredPlotConfiguration)

  if (scaling == "log") {
    resVsPredPlotConfiguration$labels$ylabel$text <- paste(resVsPredPlotConfiguration$labels$ylabel$text, "(log)")
  }

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
  ) + ggplot2::guides(
    shape = "none",
    col = ggplot2::guide_legend(
      title = resVsPredPlotConfiguration$legend$title$text,
      title.theme = resVsPredPlotConfiguration$legend$title$createPlotTextFont(),
      override.aes = list(shape = overrideShapeAssignment$shapeAssn)
    )
  )
}
