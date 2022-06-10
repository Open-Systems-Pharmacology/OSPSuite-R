#' Observed versus predicted/simulated scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams tlf::plotObsVsPred
#' @param foldDistance A vector for plotting lines at required fold differences.
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
plotObservedVsSimulated <- function(dataCombined,
                                    defaultPlotConfiguration = NULL,
                                    smoother = NULL,
                                    foldDistance = c(0, 1.5, 2)) {
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

  # Create observed versus simulated paired data using interpolation for each
  # grouping level and combine the resulting data frames row-wise. The last
  # step will be automatically carried by `dplyr::group_modify()`.
  pairedData <- combinedData %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .createObsVsPredData(.x)) %>%
    dplyr::ungroup()

  # Time points at which predicted values can't be interpolated, and need to be
  # extrapolated.
  #
  # This will happen in rare case scenarios where simulated data is sampled at a
  # lower frequency than observed data.
  predValueMissingIndices <- which(is.na(pairedData$predValue))

  # Warn the user about failure to interpolate.
  if (length(predValueMissingIndices) > 0) {
    warning(
      messages$printMultipleEntries(
        header = messages$valuesNotInterpolated(),
        entries = pairedData$obsTime[predValueMissingIndices]
      ),
      call. = FALSE
    )
  }

  # `ObsVsPredPlotConfiguration` object -----------------------------

  # Create an instance of `ObsVsPredPlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  obsVsPredPlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    data = combinedData,
    specificPlotConfiguration = tlf::ObsVsPredPlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # axes labels -----------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class. The labels can then
  # be prepared accordingly.
  axesLabels <- .createAxesLabels(combinedData, obsVsPredPlotConfiguration)
  obsVsPredPlotConfiguration$labels$xlabel$text <- obsVsPredPlotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  obsVsPredPlotConfiguration$labels$ylabel$text <- obsVsPredPlotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  # plot -----------------------------

  tlf::plotObsVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ObsVsPredDataMapping$new(
      x = "obsValue",
      y = "predValue",
      group = "group",
      lines = NULL
    ),
    foldDistance = foldDistance,
    smoother = smoother,
    plotConfiguration = obsVsPredPlotConfiguration
  )
}

