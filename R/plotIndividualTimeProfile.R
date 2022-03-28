#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @param tlfTheme A path to JSON file containing
#'   [`Theme`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Theme.html)
#'    object for `{tlf}` library.
#' @param individualTimeProfilePlotConfiguration A `PlotConfiguration` object,
#'   which is an `R6` class object that defines plot properties (like labels,
#'   axes scaling and limits, legend position, etc.). All available options for
#'   constructor method of this object can be found by running
#'   `?tlf::PlotConfiguration`. To learn more about this object, see:
#'   <https://www.open-systems-pharmacology.org/TLF-Library/articles/plot-configuration.html>.
#'
#' @import tlf
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      tlfTheme = NULL,
                                      individualTimeProfilePlotConfiguration = tlf::PlotConfiguration$new(
                                        xlabel = "xValues",
                                        ylabel = "yValues",
                                        legendTitle = "group"
                                      )) {
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
