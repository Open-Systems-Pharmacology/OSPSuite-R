#' Concentration time profile plot
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
plotIndividualTimeProfile <- function(dataCombined, defaultPlotConfiguration = NULL) {
  # validation -----------------------------

  defaultPlotConfiguration <- defaultPlotConfiguration %||% DefaultPlotConfiguration$new()
  validateIsOfType(dataCombined, "DataCombined")
  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

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
    plotConfiguration = individualTimeProfilePlotConfiguration
  )

  # Extract color and shape mappings
  legendCaptionData <- tlf::getLegendCaption(profilePlot)

  # Update plot to have as many colors as there are datasets.
  tlf::updateTimeProfileLegend(
    plotObject = profilePlot,
    caption = dplyr::mutate(
      legendCaptionData,
      color = defaultPlotConfiguration$pointsColor[1:nrow(legendCaptionData)]
    )
  )
}


#' Replace missing groupings with dataset names
#'
#' @description
#'
#' Datasets which haven't been assigned to any group will be plotted as a group
#' on its own. That is, the `group` column entries for them will be their names.
#'
#' @param data A data frame returned by `DataCombined$toDataFrame()`.
#'
#' @examples
#'
#' df <- dplyr::tibble(
#'   group = c(
#'     "Stevens 2012 solid total",
#'     "Stevens 2012 solid total",
#'     NA,
#'     NA,
#'     NA
#'   ),
#'   name = c(
#'     "Organism|Lumen|Stomach|Metformin|Gastric retention",
#'     "Stevens_2012_placebo.Placebo_total",
#'     "Stevens_2012_placebo.Sita_dist",
#'     "Stevens_2012_placebo.Sita_proximal",
#'     "Stevens_2012_placebo.Sita_total"
#'   ),
#'   dataType = c(
#'     "simulated",
#'     "observed",
#'     "observed",
#'     "observed",
#'     "observed"
#'   )
#' )
#'
#' # original
#' df
#'
#' # transformed
#' ospsuite:::.addMissingGroupings(df)
#'
#' @keywords internal
.addMissingGroupings <- function(data) {
  data <- dplyr::mutate(
    data,
    group = dplyr::case_when(
      # If grouping is missing, then use dataset name as its own grouping.
      is.na(group) ~ name,
      # Otherwise, no change.
      TRUE ~ group
    )
  )

  return(data)
}
