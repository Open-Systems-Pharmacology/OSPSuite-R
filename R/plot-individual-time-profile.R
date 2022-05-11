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
#'
#' # TODO: add example
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsSameLength(objectCount(dataCombined), 1L) # only single instance is allowed
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

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
  individualTimeProfilePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
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
    plotConfiguration = individualTimeProfilePlotConfiguration
  )

  # Extract current mappings in the legend (which are going to be incorrect).
  legendCaptionData <- tlf::getLegendCaption(profilePlot)

  # Update the legend data frame to have the correct mappings.
  newLegendCaptionData <- .updateLegendCaptionData(legendCaptionData, individualTimeProfilePlotConfiguration)

  # Update plot legend using this new data frame.
  profilePlot <- tlf::updateTimeProfileLegend(profilePlot, caption = newLegendCaptionData)

  return(profilePlot)
}
