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
                                      defaultPlotConfiguration) {

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

  # Annotations
  individualTimeProfilePlotConfiguration$labels <- defaultInternalPlotConfiguration$labels

  # Legend Configuration
  individualTimeProfilePlotConfiguration$legend <- defaultInternalPlotConfiguration$legend

  # X-Axis configuration
  individualTimeProfilePlotConfiguration$xAxis <- defaultInternalPlotConfiguration$xAxis

  # Y-Axis configuration
  individualTimeProfilePlotConfiguration$yAxis <- defaultInternalPlotConfiguration$yAxis

  # Background configuration
  individualTimeProfilePlotConfiguration$background <- defaultInternalPlotConfiguration$background

  # Configurations for aesthetics
  individualTimeProfilePlotConfiguration$lines <- defaultInternalPlotConfiguration$lines
  individualTimeProfilePlotConfiguration$points <- defaultInternalPlotConfiguration$points
  individualTimeProfilePlotConfiguration$ribbons <- defaultInternalPlotConfiguration$ribbons
  individualTimeProfilePlotConfiguration$errorbars <- defaultInternalPlotConfiguration$errorbars

  # Export configuration
  individualTimeProfilePlotConfiguration$export <- defaultInternalPlotConfiguration$export

  # plot -----------------------------
  #
  #   tlfTheme <- tlfTheme %||% system.file("themes", "ospsuiteTLFTheme.json", package = "ospsuite")
  #   useTheme(loadThemeFromJson(tlfTheme))

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
