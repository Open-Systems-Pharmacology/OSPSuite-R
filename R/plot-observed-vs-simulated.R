#' Observed versus predicted/simulated scatter plot
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams tlf::plotObsVsPred
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
                                    lines = c(-0.2, 0, 0.2)) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L) # only single instance is allowed
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

  # data frames -----------------------------

  df <- dataCombined$toDataFrame()

  # Remove the observed and simulated datasets which can't be paired.
  df <- .removeUnpairableDatasets(df)

  # Getting all units on the same scale
  df <- .unitConverter(df, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # Create observed versus simulated paired data using interpolation for each
  # grouping level and combine the resulting data frames row-wise. The last
  # step will be automatically carried by `dplyr::group_modify()`.
  pairedData <- df %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(.f = ~ .createPairedData(.x)) %>%
    dplyr::ungroup()

  # `ObsVsPredPlotConfiguration` object -----------------------------

  # Create an instance of `ObsVsPredPlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  obsVsPredPlotConfiguration <- ospsuite:::.convertGeneralToSpecificPlotConfiguration(
    data = df,
    specificPlotConfiguration = tlf::ObsVsPredPlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # axes labels -----------------------------

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class. The labels can then
  # be prepared accordingly.
  axesLabels <- .createAxesLabels(df, obsVsPredPlotConfiguration)
  obsVsPredPlotConfiguration$labels$xlabel$text <- obsVsPredPlotConfiguration$labels$xlabel$text %||% axesLabels$xLabel
  obsVsPredPlotConfiguration$labels$ylabel$text <- obsVsPredPlotConfiguration$labels$ylabel$text %||% axesLabels$yLabel

  # plot -----------------------------

  tlf::plotObsVsPred(
    data = as.data.frame(pairedData),
    dataMapping = tlf::ObsVsPredDataMapping$new(
      x = "obsValue",
      y = "predValue",
      group = "group",
      lines = lines
    ),
    smoother = smoother,
    plotConfiguration = obsVsPredPlotConfiguration
  )
}

