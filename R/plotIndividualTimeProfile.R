#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @param tlfTheme A path to JSON file containing
#'   [`Theme`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Theme.html)
#'    object for `{tlf}` library.
#' @param xAxisScale,yAxisScale A character/string that decides transformations
#'   for axes (Default: `"lin"` (for continuous linear scale)).
#'   Other available options are:
#'   - `"log"` (for continuous log10 scale (i.e., with base 10))
#'   - `"ln"` (for continuous natural logarithm (ln) scale (i.e., with base *e*))
#'   - `"sqrt"` (for continusous square root scale)
#' @param title,subtitle,xlabel,ylabel,legendTitle A character/string defining
#'   plot title, subtitle, axes labels, legend title, respectively.
#'
#' @import tlf
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      xAxisScale = "lin",
                                      yAxisScale = "lin",
                                      title = NULL,
                                      subtitle = NULL,
                                      xlabel = NULL,
                                      ylabel = NULL,
                                      legendTitle = NULL,
                                      tlfTheme = NULL) {
  validateIsOfType(dataCombined, "DataCombined")

  df <- dataCombined$toDataFrame()

  obsData <- dplyr::filter(df, dataType == "observed")
  simData <- dplyr::filter(df, dataType == "simulated")

  # TODO: feels a bit hacky
  #
  # Can this be fixed in `DataCombined$toDataFrame()` itself?
  #
  # "Doing unit conversion by hand is very suspicious. Both simulated and
  # observed should be converted to the same data so that they can be compared"
  # - Michael
  if (unique(obsData$yUnit) == "%" && unique(simData$yUnit) == "") {
    simData <- simData %>% dplyr::mutate(yValues = yValues * 100)
  }


  tlfTheme <- tlfTheme %||% system.file("themes", "ospsuiteTLFTheme.json", package = "ospsuite")

  useTheme(loadThemeFromJson(tlfTheme))

  ospsuiteTimeProfilePlotConfiguration <- TimeProfilePlotConfiguration$new(
    title = title,
    subtitle = subtitle,
    xlabel = xlabel,
    ylabel = ylabel,
    legendTitle = legendTitle
  )

  ospsuiteTimeProfilePlotConfiguration$xAxis$scale <- xAxisScale
  ospsuiteTimeProfilePlotConfiguration$yAxis$scale <- yAxisScale

  plotTimeProfile(
    data = as.data.frame(simData),
    dataMapping = TimeProfileDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "name"
    ),
    observedData = as.data.frame(obsData),
    observedDataMapping = ObservedDataMapping$new(
      x = "xValues",
      y = "yValues",
      group = "group",
      uncertainty = "yErrorValues"
    ),
    plotConfiguration = ospsuiteTimeProfilePlotConfiguration
  )
}
