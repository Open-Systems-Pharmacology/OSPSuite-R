#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @param defaultPlotConfiguration A `DefaultPlotConfiguration` object, which is
#'   an `R6` class object that defines plot properties.
#'
#' @import tlf
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = DefaultPlotConfiguration$new()) {

  # validation -----------------------------

  validateIsOfType(dataCombined, "DataCombined")
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

  # data frames -----------------------------

  df <- dataCombined$toDataFrame()
  df <- .unitConverter(df, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)
  obsData <- dplyr::filter(df, dataType == "observed")
  simData <- dplyr::filter(df, dataType == "simulated")

  # TimeProfilePlotConfiguration object -----------------------------

  # Create an instance of `defaultInternalPlotConfiguration` class
  defaultInternalPlotConfiguration <- .createDefaultInternalPlotConfiguration(defaultPlotConfiguration)

  # Create an instance of `TimeProfilePlotConfiguration` class
  individualTimeProfilePlotConfiguration <- tlf::TimeProfilePlotConfiguration$new()

  individualTimeProfilePlotConfiguration$labels <- defaultInternalPlotConfiguration$labels
  individualTimeProfilePlotConfiguration$legend <- defaultInternalPlotConfiguration$legend
  individualTimeProfilePlotConfiguration$xAxis <- defaultInternalPlotConfiguration$xAxis
  individualTimeProfilePlotConfiguration$yAxis <- defaultInternalPlotConfiguration$yAxis
  individualTimeProfilePlotConfiguration$background <- defaultInternalPlotConfiguration$background
  individualTimeProfilePlotConfiguration$lines <- defaultInternalPlotConfiguration$lines
  individualTimeProfilePlotConfiguration$points <- defaultInternalPlotConfiguration$points
  individualTimeProfilePlotConfiguration$ribbons <- defaultInternalPlotConfiguration$ribbons
  individualTimeProfilePlotConfiguration$errorbars <- defaultInternalPlotConfiguration$errorbars
  individualTimeProfilePlotConfiguration$export <- defaultInternalPlotConfiguration$export

  # plot -----------------------------

  plotTimeProfile(
    data = as.data.frame(simData),
    dataMapping = TimeProfileDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group"
    ),
    observedData = as.data.frame(obsData),
    observedDataMapping = ObservedDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group",
      error = "yErrorValues"
    ),
    plotConfiguration = individualTimeProfilePlotConfiguration
  )
}
