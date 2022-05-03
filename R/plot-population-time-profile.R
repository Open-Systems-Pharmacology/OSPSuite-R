#' Concentration time profile plot for population
#'
#' @inheritParams plotIndividualTimeProfile
#' @param quantiles A numerical vector with quantile values (Default: `c(0.05,
#'   0.50, 0.95)`). In the profile plot, the middle value will be used to draw a
#'   line, while the lower and upper values will be used to create a ribbon.
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
plotPopulationTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL,
                                      quantiles = c(0.05, 0.5, 0.95)) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)
  validateIsNumeric(quantiles, nullAllowed = FALSE)
  validateIsOfLength(quantiles, 3L)

  # data frames -----------------------------

  df <- dataCombined$toDataFrame()

  # Getting all units on the same scale
  df <- .unitConverter(df, defaultPlotConfiguration$xUnit, defaultPlotConfiguration$yUnit)

  # Datasets which haven't been assigned to any group will be plotted as a group
  # on its own. That is, the `group` column entries for them will be their names.
  df <- .addMissingGroupings(df)

  # Extracting observed vs simulated datasets to their own data frames for convenience
  obsData <- dplyr::filter(df, dataType == "observed")
  simData <- dplyr::filter(df, dataType == "simulated")

  # Compute quantiles
  simData <- simData %>%
    # Compute across all individuals for each time point
    dplyr::group_by(xValues) %>% #
    dplyr::mutate(
      yValuesLower   = stats::quantile(yValues, quantiles[[1]]),
      yValuesCentral = stats::quantile(yValues, quantiles[[2]]),
      yValuesHigher  = stats::quantile(yValues, quantiles[[3]]),
      .after = yValues # place the newly created columns after this column
    ) %>%
    dplyr::ungroup()

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

  # If axes labels haven't been specified, create them using dimensions and units.
  individualTimeProfilePlotConfiguration$labels$xlabel$text <-
    individualTimeProfilePlotConfiguration$labels$xlabel$text %||%
    paste0(unique(df$xDimension), " [", unique(df$xUnit), "]")

  individualTimeProfilePlotConfiguration$labels$ylabel$text <-
    individualTimeProfilePlotConfiguration$labels$ylabel$text %||%
    paste0(unique(df$yDimension), " [", unique(df$yUnit), "]")

  # plot -----------------------------

  profilePlot <- tlf::plotTimeProfile(
    data = as.data.frame(simData),
    dataMapping = tlf::TimeProfileDataMapping$new(
      x = "xValues",
      y = "yValuesCentral",
      ymin = "yValuesLower",
      ymax = "yValuesHigher",
      group = "group"
    ),
    plotConfiguration = individualTimeProfilePlotConfiguration
  )

  # # Extract color and shape mappings
  # legendCaptionData <- tlf::getLegendCaption(profilePlot)
  #
  # # Extract as many colors as there are datasets from the specified color palette.
  # colorPalette <- defaultPlotConfiguration$pointsColor[1:nrow(legendCaptionData)]
  #
  # # New version of legend mappings.
  # newLegendCaptionData <- dplyr::mutate(legendCaptionData, color = colorPalette, fill = colorPalette)
  #
  # # Update plot with these colors.
  # profilePlot <- tlf::updateTimeProfileLegend(profilePlot, caption = newLegendCaptionData)

  return(profilePlot)
}
