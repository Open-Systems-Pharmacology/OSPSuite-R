#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @param tlfTheme A path to JSON file containing
#'   [`Theme`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Theme.html)
#'    object for `{tlf}` library.
#' @param individualTimeProfilePlotConfiguration A `PlotConfiguration` object.
#'   For more, see:
#'   <https://www.open-systems-pharmacology.org/TLF-Library/articles/plot-configuration.html>.
#'
#' @import tlf
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      individualTimeProfilePlotConfiguration = TimeProfilePlotConfiguration$new(),
                                      tlfTheme = NULL) {
  validateIsOfType(dataCombined, "DataCombined")

  df <- dataCombined$toDataFrame()

  obsData <- dplyr::filter(df, dataType == "observed")
  simData <- dplyr::filter(df, dataType == "simulated")

  # TODO: remove once `unitConverter()` function is available
  if (unique(obsData$yUnit) == "%" && unique(simData$yUnit) == "") {
    simData <- simData %>% dplyr::mutate(yValues = yValues * 100)
  }

  tlfTheme <- tlfTheme %||% system.file("themes", "ospsuiteTLFTheme.json", package = "ospsuite")

  useTheme(loadThemeFromJson(tlfTheme))

  # ospsuiteTimeProfilePlotConfiguration <- TimeProfilePlotConfiguration$new(
  #   title = title,
  #   subtitle = subtitle,
  #   xlabel = xlabel,
  #   ylabel = ylabel,
  #   legendTitle = legendTitle
  # )
  #
  # ospsuiteTimeProfilePlotConfiguration$xAxis$scale <- xAxisScale
  # ospsuiteTimeProfilePlotConfiguration$yAxis$scale <- yAxisScale

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
