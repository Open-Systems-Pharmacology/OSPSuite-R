#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @param tlfTheme A path to JSON file containing
#'   [`Theme`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Theme.html)
#'    object for `{tlf}` library.
#' @param ospPlotConfiguration A `ospPlotConfiguration` object,
#'   which is an `R6` class object that defines plot properties (like labels,
#'   axes scaling and limits, legend position, etc.). You can create instance of this object using `createPlotConfiguration()` function.
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

  # Create an instance of `TimeProfilePlotConfiguration` object
  individualTimeProfilePlotConfiguration <- tlf::TimeProfilePlotConfiguration$new()

  # Annotations
  individualTimeProfilePlotConfiguration$labels$title$text <- ospPlotConfiguration$title
  individualTimeProfilePlotConfiguration$labels$subtitle$text <- ospPlotConfiguration$subtitle
  individualTimeProfilePlotConfiguration$labels$xlabel$text <- ospPlotConfiguration$xlabel
  individualTimeProfilePlotConfiguration$labels$ylabel$text <- ospPlotConfiguration$ylabel

  # Legend Configuration
  individualTimeProfilePlotConfiguration$legend <- ospPlotConfiguration$legend

  # X-Axis configuration
  individualTimeProfilePlotConfiguration$xAxis <- ospPlotConfiguration$xAxis

  # Y-Axis configuration
  individualTimeProfilePlotConfiguration$yAxis <- ospPlotConfiguration$yAxis

  # Background configuration
  individualTimeProfilePlotConfiguration$background <- ospPlotConfiguration$background

  # Configurations for aesthetics
  individualTimeProfilePlotConfiguration$lines <- ospPlotConfiguration$lines
  individualTimeProfilePlotConfiguration$points <- ospPlotConfiguration$points
  individualTimeProfilePlotConfiguration$ribbons <- ospPlotConfiguration$ribbons
  individualTimeProfilePlotConfiguration$errorbars <- ospPlotConfiguration$errorbars

  # Export configuration
  individualTimeProfilePlotConfiguration$export <- ospPlotConfiguration$export

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
