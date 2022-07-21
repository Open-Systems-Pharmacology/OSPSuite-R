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
                                defaultPlotConfiguration = NULL) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L) # only single instance is allowed
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

  if (is.null(dataCombined$groupMap)) {
    warning(messages$plottingWithEmptyDataCombined())
    return(NULL)
  }

  # data frames -----------------------------

  combinedData <- dataCombined$toDataFrame()

  # Remove the observed and simulated datasets which can't be paired.
  combinedData <- .removeUnpairableDatasets(combinedData)

  # Return early if there are no pair-able datasets present
  if (nrow(combinedData) == 0L) {
    warning(messages$plottingWithNoPairedDatasets())
    return(NULL)
  }

  # Getting all units on the same scale
  combinedData <- .unitConverter(combinedData, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # `ResVsTimePlotConfiguration` object -----------------------------

  # Create an instance of `ResVsTimePlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  resVsTimePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    specificPlotConfiguration = tlf::ResVsTimePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # paired data frame -----------------------------

  # Create observed versus simulated paired data using interpolation for each
  # grouping level and combine the resulting data frames in a row-wise manner.
  #
  # Both of these routines will be carried out by `dplyr::group_modify()`.
  pairedData <- combinedData %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .calculateResiduals(.x, scaling = resVsTimePlotConfiguration$yAxis$scale)) %>%
    dplyr::ungroup()

  # axes labels -----------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class. The labels can then
  # be prepared accordingly.
  axesLabels <- .createAxesLabels(combinedData, resVsTimePlotConfiguration)
  resVsTimePlotConfiguration$labels$xlabel$text <- resVsTimePlotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  resVsTimePlotConfiguration$labels$ylabel$text <- resVsTimePlotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  # plot -----------------------------

  tlf::plotResVsTime(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ResVsTimeDataMapping$new(
      x = "obsValue",
      y = "resValue",
      group = "group",
      lines = NULL
    ),
    plotConfiguration = resVsTimePlotConfiguration
  )
}
