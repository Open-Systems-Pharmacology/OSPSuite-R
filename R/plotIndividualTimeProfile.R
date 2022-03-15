#' Concentration time profile plot
#'
#' @param dataCombined A `DataCombined` object.
#' @inheritParams tlf::useTheme
#' @param title,subtitle,xlabel,ylabel,legendTitle A character/string defining
#'   plot title, subtitle, axes labels, legend title, respectively.
#'
#' @import tlf
#'
#' @export
plotIndividualTimeProfile <- function(dataCombined,
                                      theme = NULL,
                                      title = NULL,
                                      subtitle = NULL,
                                      xlabel = NULL,
                                      ylabel = NULL,
                                      legendTitle = NULL) {
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

  useTheme(loadThemeFromJson(system.file("themes", "ospsuiteTheme.json", package = "ospsuite")))

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
    plotConfiguration = TimeProfilePlotConfiguration$new(
      title = title,
      subtitle = subtitle,
      xlabel = xlabel,
      ylabel = ylabel,
      legendTitle = legendTitle
    )
  )
}
