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
      yValuesLower = stats::quantile(yValues, quantiles[[1]]),
      yValuesCentral = stats::quantile(yValues, quantiles[[2]]),
      yValuesHigher = stats::quantile(yValues, quantiles[[3]]),
      .groups = "drop" # drop grouping information from the summary data frame
    )

  return(simAggregatedData)
}

#' Create plot-specific `tlf::PlotConfiguration` object
#'
#' @param data A data frame containing information about dimensions and units
#'   for the x-and y-axes quantities.
#' @param specificPlotConfiguration A specific subclass of
#'   `tlf::PlotConfiguration` needed for the given plot.
#' @param generalPlotConfiguration A `DefaultPlotConfiguration` object.
#'
#' @keywords internal
#' @noRd
.convertGeneralToSpecificPlotConfiguration <- function(data,
                                                       specificPlotConfiguration,
                                                       generalPlotConfiguration) {
  validateIsOfType(generalPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

  # labels object ---------------------------------------

  labelTitle <- tlf::Label$new(
    text = generalPlotConfiguration$title,
    color = generalPlotConfiguration$titleColor,
    size = generalPlotConfiguration$titleSize,
    fontFace = generalPlotConfiguration$titleFontFace,
    fontFamily = generalPlotConfiguration$titleFontFamily,
    angle = generalPlotConfiguration$titleAngle,
    align = generalPlotConfiguration$titleAlign
  )

  labelSubtitle <- tlf::Label$new(
    text = generalPlotConfiguration$subtitle,
    color = generalPlotConfiguration$subtitleColor,
    size = generalPlotConfiguration$subtitleSize,
    fontFace = generalPlotConfiguration$subtitleFontFace,
    fontFamily = generalPlotConfiguration$subtitleFontFamily,
    angle = generalPlotConfiguration$subtitleAngle,
    align = generalPlotConfiguration$subtitleAlign
  )

  labelCaption <- tlf::Label$new(
    text = generalPlotConfiguration$caption,
    color = generalPlotConfiguration$captionColor,
    size = generalPlotConfiguration$captionSize,
    fontFace = generalPlotConfiguration$captionFontFace,
    fontFamily = generalPlotConfiguration$captionFontFamily,
    angle = generalPlotConfiguration$captionAngle,
    align = generalPlotConfiguration$captionAlign
  )

  labelXLabel <- tlf::Label$new(
    text = generalPlotConfiguration$xLabel,
    color = generalPlotConfiguration$xLabelColor,
    size = generalPlotConfiguration$xLabelSize,
    fontFace = generalPlotConfiguration$xLabelFontFace,
    fontFamily = generalPlotConfiguration$xLabelFontFamily,
    angle = generalPlotConfiguration$xLabelAngle,
    align = generalPlotConfiguration$xLabelAlign
  )

  labelYLabel <- tlf::Label$new(
    text = generalPlotConfiguration$yLabel,
    color = generalPlotConfiguration$yLabelColor,
    size = generalPlotConfiguration$yLabelSize,
    fontFace = generalPlotConfiguration$yLabelFontFace,
    fontFamily = generalPlotConfiguration$yLabelFontFamily,
    angle = generalPlotConfiguration$yLabelAngle,
    align = generalPlotConfiguration$yLabelAlign
  )

  labelConfiguration <- tlf::LabelConfiguration$new(
    title = labelTitle,
    subtitle = labelSubtitle,
    caption = labelCaption,
    xlabel = labelXLabel,
    ylabel = labelYLabel
  )

  # legend object ---------------------------------------

  legendTitleFont <- tlf::Font$new(
    size = generalPlotConfiguration$legendTitleSize,
    color = generalPlotConfiguration$legendTitleColor,
    fontFamily = generalPlotConfiguration$legendTitleFontFamily,
    fontFace = generalPlotConfiguration$legendTitleFontFace,
    angle = generalPlotConfiguration$legendTitleAngle,
    align = generalPlotConfiguration$legendTitleAlign
  )

  legendCaptionFont <- tlf::Font$new(
    size = generalPlotConfiguration$legendCaptionSize,
    color = generalPlotConfiguration$legendCaptionColor,
    fontFamily = generalPlotConfiguration$legendCaptionFontFamily,
    fontFace = generalPlotConfiguration$legendCaptionFontFace,
    angle = generalPlotConfiguration$legendCaptionAngle,
    align = generalPlotConfiguration$legendCaptionAlign
  )

  legendConfiguration <- tlf::LegendConfiguration$new(
    position = generalPlotConfiguration$legendPosition,
    caption = NULL,
    title = generalPlotConfiguration$legendTitle,
    font = generalPlotConfiguration$legendCaptionFont,
    background = NULL
  )

  # background objects -----------------------------------

  labelWatermark <- tlf::Label$new(
    text = generalPlotConfiguration$watermark,
    color = generalPlotConfiguration$watermarkColor,
    size = generalPlotConfiguration$watermarkSize,
    fontFace = generalPlotConfiguration$watermarkFontFace,
    fontFamily = generalPlotConfiguration$watermarkFontFamily,
    angle = generalPlotConfiguration$watermarkAngle,
    align = generalPlotConfiguration$watermarkAlign
  )

  plotBackground <- tlf::BackgroundElement$new(
    fill = generalPlotConfiguration$plotBackgroundFill,
    color = generalPlotConfiguration$plotBackgroundColor,
    size = generalPlotConfiguration$plotBackgroundSize,
    linetype = generalPlotConfiguration$plotBackgroundLinetype
  )

  plotPanelBackground <- tlf::BackgroundElement$new(
    fill = generalPlotConfiguration$plotPanelBackgroundFill,
    color = generalPlotConfiguration$plotPanelBackgroundColor,
    size = generalPlotConfiguration$plotPanelBackgroundSize,
    linetype = generalPlotConfiguration$plotPanelBackgroundLinetype
  )

  xAxis <- tlf::LineElement$new(
    color = generalPlotConfiguration$xAxisColor,
    size = generalPlotConfiguration$xAxisSize,
    linetype = generalPlotConfiguration$xAxisLinetype
  )

  yAxis <- tlf::LineElement$new(
    color = generalPlotConfiguration$yAxisColor,
    size = generalPlotConfiguration$yAxisSize,
    linetype = generalPlotConfiguration$yAxisLinetype
  )

  xGrid <- tlf::LineElement$new(
    color = generalPlotConfiguration$xGridColor,
    size = generalPlotConfiguration$xGridSize,
    linetype = generalPlotConfiguration$xGridLinetype
  )

  yGrid <- tlf::LineElement$new(
    color = generalPlotConfiguration$yGridColor,
    size = generalPlotConfiguration$yGridSize,
    linetype = generalPlotConfiguration$yGridLinetype
  )

  backgroundConfiguration <- tlf::BackgroundConfiguration$new(
    watermark = generalPlotConfiguration$labelWatermark,
    plot = generalPlotConfiguration$plotBackground,
    panel = generalPlotConfiguration$plotPanelBackground,
    xAxis = generalPlotConfiguration$xAxis,
    yAxis = generalPlotConfiguration$yAxis,
    xGrid = generalPlotConfiguration$xGrid,
    yGrid = generalPlotConfiguration$yGrid
  )

  # xAxis objects -----------------------------------

  xAxisFont <- tlf::Font$new(
    size = generalPlotConfiguration$xAxisLabelTicksSize,
    color = generalPlotConfiguration$xAxisLabelTicksColor,
    fontFamily = generalPlotConfiguration$xAxisLabelTicksFontFamily,
    fontFace = generalPlotConfiguration$xAxisLabelTicksFontFace,
    angle = generalPlotConfiguration$xAxisLabelTicksAngle,
    align = generalPlotConfiguration$xAxisLabelTicksAlign
  )

  xAxisConfiguration <- tlf::XAxisConfiguration$new(
    limits = generalPlotConfiguration$xAxisLimits,
    scale = generalPlotConfiguration$xAxisScale,
    ticks = generalPlotConfiguration$xAxisTicks,
    ticklabels = generalPlotConfiguration$xAxisTicksLabels,
    font = generalPlotConfiguration$xAxisFont
  )

  # yAxis objects -----------------------------------

  yAxisFont <- tlf::Font$new(
    size = generalPlotConfiguration$yAxisLabelTicksSize,
    color = generalPlotConfiguration$yAxisLabelTicksColor,
    fontFamily = generalPlotConfiguration$yAxisLabelTicksFontFamily,
    fontFace = generalPlotConfiguration$yAxisLabelTicksFontFace,
    angle = generalPlotConfiguration$yAxisLabelTicksAngle,
    align = generalPlotConfiguration$yAxisLabelTicksAlign
  )

  yAxisConfiguration <- tlf::YAxisConfiguration$new(
    limits = generalPlotConfiguration$yAxisLimits,
    scale = generalPlotConfiguration$yAxisScale,
    ticks = generalPlotConfiguration$yAxisTicks,
    ticklabels = generalPlotConfiguration$yAxisTicksLabels,
    font = generalPlotConfiguration$yAxisFont
  )

  # lines -------------------------------------------------------

  linesConfiguration <- tlf::ThemeAestheticSelections$new(
    color = generalPlotConfiguration$linesColor,
    shape = generalPlotConfiguration$linesShape,
    size = generalPlotConfiguration$linesSize,
    linetype = generalPlotConfiguration$linesLinetype,
    alpha = generalPlotConfiguration$linesAlpha
  )

  # points -------------------------------------------------------

  pointsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = generalPlotConfiguration$pointsColor,
    shape = generalPlotConfiguration$pointsShape,
    size = generalPlotConfiguration$pointsSize,
    linetype = generalPlotConfiguration$pointsLinetype,
    alpha = generalPlotConfiguration$pointsAlpha
  )

  # ribbons -------------------------------------------------------

  ribbonsConfiguration <- tlf::ThemeAestheticSelections$new(
    fill = generalPlotConfiguration$ribbonsFill,
    shape = generalPlotConfiguration$ribbonsShape,
    size = generalPlotConfiguration$ribbonsSize,
    linetype = generalPlotConfiguration$ribbonsLinetype,
    alpha = generalPlotConfiguration$ribbonsAlpha
  )

  # errorbars -------------------------------------------------------

  errorbarsConfiguration <- tlf::ThemeAestheticSelections$new(
    shape = generalPlotConfiguration$errorbarsShape,
    size = generalPlotConfiguration$errorbarsSize,
    linetype = generalPlotConfiguration$errorbarsLinetype,
    alpha = generalPlotConfiguration$errorbarsAlpha
  )

  # export -------------------------------------------------------

  exportConfiguration <- tlf::ExportConfiguration$new(
    name = generalPlotConfiguration$plotSaveFileName,
    format = generalPlotConfiguration$plotSaveFileFormat,
    width = generalPlotConfiguration$plotSaveFileWidth,
    height = generalPlotConfiguration$plotSaveFileHeight,
    units = generalPlotConfiguration$plotSaveFileDimensionUnits,
    dpi = generalPlotConfiguration$plotSaveFileDpi
  )

  # Update specific plot configuration object ----------------------

  # Do one-to-one mappings of public fields
  specificPlotConfiguration$labels <- labelConfiguration
  specificPlotConfiguration$legend <- legendConfiguration
  specificPlotConfiguration$xAxis <- xAxisConfiguration
  specificPlotConfiguration$yAxis <- yAxisConfiguration
  specificPlotConfiguration$background <- backgroundConfiguration
  specificPlotConfiguration$lines <- linesConfiguration
  specificPlotConfiguration$points <- pointsConfiguration
  specificPlotConfiguration$ribbons <- ribbonsConfiguration
  specificPlotConfiguration$errorbars <- errorbarsConfiguration
  specificPlotConfiguration$export <- exportConfiguration

  # Axes labels -----------------------------------

  # If axes labels haven't been specified, create them using dimensions and units.

  # In the code below, `.unitConverter()` has already ensured that there is only
  # a single unit for x and y quantities, so we can safely take the unique unit
  # to prepare axes labels.
  xUnitString <- unique(data$xUnit)
  yUnitString <- unique(data$yUnit)

  # If quantities are unitless, no unit information will be displayed.
  # Otherwise, `Dimension [Unit]` pattern will be followed.
  xUnitString <- ifelse(xUnitString == "", xUnitString, paste0(" [", xUnitString, "]"))
  yUnitString <- ifelse(yUnitString == "", yUnitString, paste0(" [", yUnitString, "]"))


  # The exact axis label will depend on the type of the plot, and the type
  # of the plot can be guessed using the specific `PlotConfiguration` object
  # entered in this function.
  #
  # If the specific `PlotConfiguration` object is not any of the cases included
  # in the `switch` below, the result will be no change; i.e., the labels will
  # continue to be `NULL`.

  # x-axis label
  if (is.null(specificPlotConfiguration$labels$xlabel$text)) {
    xUnitString <- paste0(unique(data$xDimension), xUnitString)

    xUnitString <- switch(class(specificPlotConfiguration)[[1]],
      "TimeProfilePlotConfiguration" = xUnitString,
      "ResVsPredPlotConfiguration" = xUnitString,
      "ObsVsPredPlotConfiguration" = paste0("Observed values (", xUnitString, ")")
    )

    specificPlotConfiguration$labels$xlabel$text <- xUnitString
  }

  # y-axis label
  if (is.null(specificPlotConfiguration$labels$ylabel$text)) {
    yUnitString <- paste0(unique(data$yDimension), yUnitString)

    yUnitString <- switch(class(specificPlotConfiguration)[[1]],
      "TimeProfilePlotConfiguration" = yUnitString,
      "ResVsPredPlotConfiguration" = "Residuals",
      "ObsVsPredPlotConfiguration" = paste0("Simulated values (", yUnitString, ")")
    )

    specificPlotConfiguration$labels$ylabel$text <- yUnitString
  }

  return(specificPlotConfiguration)
}
