#' Time-values profile plot for population simulations
#'
#' @inheritParams plotIndividualTimeProfile
#' @param quantiles A numerical vector with quantile values (Default: `c(0.05,
#'   0.50, 0.95)`), with the quantile values defining the aggregation of
#'   individual data. In the profile plot, the middle value will be used to draw
#'   a line, while the lower and upper values will be used to create a ribbon.
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
plotPopulationTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL,
                                      quantiles = c(0.05, 0.5, 0.95)) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L) # only single instance is allowed
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)
  validateIsNumeric(quantiles, nullAllowed = FALSE)
  validateIsOfLength(quantiles, 3L)

  # data frames -----------------------------

  df <- dataCombined$toDataFrame()

  # Getting all units on the same scale
  df <- .unitConverter(df, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # Datasets which haven't been assigned to any group will be plotted as a group
  # on its own. That is, the `group` column entries for them will be their names.
  df <- .addMissingGroupings(df)

  # `TimeProfilePlotConfiguration` object -----------------------------

  # Create an instance of `TimeProfilePlotConfiguration` class by doing a
  # one-to-one mapping of internal plot configuration object's public fields
  populationTimeProfilePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    data = df,
    specificPlotConfiguration = tlf::TimeProfilePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # plot -----------------------------

  obsData <- as.data.frame(dplyr::filter(df, dataType == "observed"))

  if (nrow(obsData) == 0) {
    obsData <- NULL
  }

  simData <- as.data.frame(dplyr::filter(df, dataType == "simulated"))

  if (nrow(simData) == 0) {
    simData <- NULL
  }

  profilePlot <- tlf::plotTimeProfile(
    data = simData,
    dataMapping = tlf::TimeProfileDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group"
    ),
    observedData = obsData,
    observedDataMapping = tlf::ObservedDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group",
      error = "yErrorValues"
    ),
    plotConfiguration = populationTimeProfilePlotConfiguration
  )

  # Extract current mappings in the legend (which are going to be incorrect).
  legendCaptionData <- tlf::getLegendCaption(profilePlot)

  # Update the legend data frame to have the correct mappings.
  newLegendCaptionData <- .updateLegendCaptionData(legendCaptionData, populationTimeProfilePlotConfiguration)

  # Update plot legend using this new data frame.
  profilePlot <- tlf::updateTimeProfileLegend(profilePlot, caption = newLegendCaptionData)

  return(profilePlot)
}
