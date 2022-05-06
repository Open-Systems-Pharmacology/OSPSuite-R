#' Concentration time profile plot for individual
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
  populationTimeProfilePlotConfiguration <- .convertGeneralToSpecificPlotConfiguration(
    data = df,
    specificPlotConfiguration = tlf::TimeProfilePlotConfiguration$new(),
    generalPlotConfiguration = defaultPlotConfiguration
  )

  # plot -----------------------------

  # Which dataset types are present?
  datasetTypePresent <- .extractPresentDatasetTypes(dataCombined)

  # both observed and simulated
  if (datasetTypePresent == .presentDataTypes$Both) {
    obsData <- dplyr::filter(df, dataType == "observed")
    simData <- dplyr::filter(df, dataType == "simulated")

    profilePlot <- tlf::plotTimeProfile(
      data = as.data.frame(simData),
      dataMapping = tlf::TimeProfileDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group"
      ),
      observedData = as.data.frame(obsData),
      observedDataMapping = tlf::ObservedDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group",
        error = "yErrorValues"
      ),
      plotConfiguration = populationTimeProfilePlotConfiguration
    )

    # Extract color and shape mappings
    legendCaptionData <- tlf::getLegendCaption(profilePlot)

    # Extract as many colors as there are datasets from the specified color palette.
    colorPalette <- defaultPlotConfiguration$pointsColor[1:nrow(legendCaptionData)]

    # New version of legend mappings.
    newLegendCaptionData <- dplyr::mutate(legendCaptionData, color = colorPalette)

    # Update plot with these colors.
    profilePlot <- tlf::updateTimeProfileLegend(profilePlot, caption = newLegendCaptionData)
  }

  # only observed
  if (datasetTypePresent == .presentDataTypes$Observed) {
    obsData <- dplyr::filter(df, dataType == "observed")

    profilePlot <- tlf::plotTimeProfile(
      observedData = as.data.frame(obsData),
      observedDataMapping = tlf::ObservedDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group",
        error = "yErrorValues"
      ),
      plotConfiguration = populationTimeProfilePlotConfiguration
    )
  }

  # only simulated
  if (datasetTypePresent == .presentDataTypes$Simulated) {
    simData <- dplyr::filter(df, dataType == "simulated")

    profilePlot <- tlf::plotTimeProfile(
      data = as.data.frame(simData),
      dataMapping = tlf::TimeProfileDataMapping$new(
        x = "xValues",
        y = "yValues",
        group = "group"
      ),
      plotConfiguration = populationTimeProfilePlotConfiguration
    )
  }

  return(profilePlot)
}
