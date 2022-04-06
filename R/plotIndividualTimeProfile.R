#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @param tlfTheme A path to JSON file containing
#'   [`Theme`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Theme.html)
#'    object for `{tlf}` library.
#' @param ospPlotConfiguration A `ospPlotConfiguration` object, which is an `R6`
#'   class object that defines plot properties (like labels, axes scaling and
#'   limits, legend position, etc.).
#' @param xUnit,yUnit Units for x- and y-axes, respectively.
#'
#' @import tlf
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      ospPlotConfiguration,
                                      xUnit = NULL,
                                      yUnit = NULL,
                                      tlfTheme = NULL) {

  # validation -----------------------------

  validateIsOfType(dataCombined, "DataCombined")
  validateIsOfType(ospPlotConfiguration, "ospPlotConfiguration")

  # data frames -----------------------------

  df <- dataCombined$toDataFrame()
  df <- ospsuite:::.unitConverter(df, xUnit, yUnit)
  obsData <- dplyr::filter(df, dataType == "observed")
  simData <- dplyr::filter(df, dataType == "simulated")

  # TimeProfilePlotConfiguration object -----------------------------

  # Create an instance of `ospInternalPlotConfiguration` class
  ospInternalPlotConfiguration <- .createOSPInternalPlotConfiguration(ospPlotConfiguration)

  # Create an instance of `TimeProfilePlotConfiguration` class
  individualTimeProfilePlotConfiguration <- tlf::TimeProfilePlotConfiguration$new()

  # Annotations
  individualTimeProfilePlotConfiguration$labels <- ospInternalPlotConfiguration$labels

  # Legend Configuration
  individualTimeProfilePlotConfiguration$legend <- ospInternalPlotConfiguration$legend

  # X-Axis configuration
  individualTimeProfilePlotConfiguration$xAxis <- ospInternalPlotConfiguration$xAxis

  # Y-Axis configuration
  individualTimeProfilePlotConfiguration$yAxis <- ospInternalPlotConfiguration$yAxis

  # Background configuration
  individualTimeProfilePlotConfiguration$background <- ospInternalPlotConfiguration$background

  # Configurations for aesthetics
  individualTimeProfilePlotConfiguration$lines <- ospInternalPlotConfiguration$lines
  individualTimeProfilePlotConfiguration$points <- ospInternalPlotConfiguration$points
  individualTimeProfilePlotConfiguration$ribbons <- ospInternalPlotConfiguration$ribbons
  individualTimeProfilePlotConfiguration$errorbars <- ospInternalPlotConfiguration$errorbars

  # Export configuration
  individualTimeProfilePlotConfiguration$export <- ospInternalPlotConfiguration$export

  # plot -----------------------------

  tlfTheme <- tlfTheme %||% system.file("themes", "ospsuiteTLFTheme.json", package = "ospsuite")
  useTheme(loadThemeFromJson(tlfTheme))

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
