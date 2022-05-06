#' An enum representing dataset types present in `DataCombined`
#' @keywords internal
.presentDataTypes <- enum(c(
  Observed = "only observed",
  Simulated = "only simulated",
  Both = "both observed and simulated"
))

#' Extracting dataset types present in `DataCombined`
#'
#' @description
#'
#' While creating plots, different code needs to be used depending on whether
#' only simulated, only observed, or both dataset types are present in the
#' `DataCombined`.
#'
#' @keywords internal
.extractPresentDatasetTypes <- function(dataCombined) {
  validateIsOfType(dataCombined, "DataCombined")

  dataTypeUnique <- unique(dataCombined$groupMap$dataType)

  if (length(dataTypeUnique) == 2L && all(dataTypeUnique %in% c("observed", "simulated"))) {
    datasetTypePresent <- .presentDataTypes$Both
  } else if (length(dataTypeUnique) == 1L && dataTypeUnique == "observed") {
    datasetTypePresent <- .presentDataTypes$Observed
  } else if (length(dataTypeUnique) == 1L && dataTypeUnique == "simulated") {
    datasetTypePresent <- .presentDataTypes$Simulated
  }

  return(datasetTypePresent)
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

#' Extract aggregated simulated data
#'
#' @keywords internal
#' @noRd
.extractAggregatedSimulatedData <- function(simData, quantiles) {
  # Compute quantiles
  simAggregatedData <- simData %>%
    # For each dataset, compute across all individuals for each time point
    dplyr::group_by(group, xValues) %>% #
    dplyr::summarise(
      yValuesLower   = stats::quantile(yValues, quantiles[[1]]),
      yValuesCentral = stats::quantile(yValues, quantiles[[2]]),
      yValuesHigher  = stats::quantile(yValues, quantiles[[3]]),
      .groups = "drop" # drop grouping information from the summary data frame
    )

  return(simAggregatedData)
}

#' Create plot-specific `tlf::PlotConfiguration` object
#'
#' @keywords internal
#' @noRd
.convertGeneralToSpecificPlotConfiguration <- function(data,
                                                       specificPlotConfiguration,
                                                       generalPlotConfiguration) {
  # Do one-to-one mappings of public fields
  specificPlotConfiguration$labels <- generalPlotConfiguration$labels
  specificPlotConfiguration$legend <- generalPlotConfiguration$legend
  specificPlotConfiguration$xAxis <- generalPlotConfiguration$xAxis
  specificPlotConfiguration$yAxis <- generalPlotConfiguration$yAxis
  specificPlotConfiguration$background <- generalPlotConfiguration$background
  specificPlotConfiguration$lines <- generalPlotConfiguration$lines
  specificPlotConfiguration$points <- generalPlotConfiguration$points
  specificPlotConfiguration$ribbons <- generalPlotConfiguration$ribbons
  specificPlotConfiguration$errorbars <- generalPlotConfiguration$errorbars
  specificPlotConfiguration$export <- generalPlotConfiguration$export

  # In the code below, `.unitConverter()` has already ensured that there is only
  # a single unit for x and y quantities, so we can safely take the unique unit
  # to prepare axes labels.

  # If axes labels haven't been specified, create them using dimensions and units.
  # If quantities are unitless, no unit information will be displayed, otherwise
  # `Dimension [Unit]` pattern will be followed.
  xUnitString <- unique(data$xUnit)
  yUnitString <- unique(data$yUnit)
  xUnitString <- ifelse(xUnitString == "", xUnitString, paste0(" [", xUnitString, "]"))
  yUnitString <- ifelse(yUnitString == "", yUnitString, paste0(" [", yUnitString, "]"))

  specificPlotConfiguration$labels$xlabel$text <-
    specificPlotConfiguration$labels$xlabel$text %||%
    paste0(unique(data$xDimension), xUnitString)

  specificPlotConfiguration$labels$ylabel$text <-
    specificPlotConfiguration$labels$ylabel$text %||%
    paste0(unique(data$yDimension), yUnitString)

  return(specificPlotConfiguration)
}
