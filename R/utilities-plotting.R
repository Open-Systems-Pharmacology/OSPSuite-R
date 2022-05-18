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

#' Remove unpairable datasets for scatter plots
#'
#' @description
#'
#' Datasets which haven't been assigned to any group will be removed from the
#' combined data frame.
#'
#' @param data A data frame returned by `DataCombined$toDataFrame()`.
#'
#' @examples
#'
#' df <- dplyr::tribble(
#'   ~name, ~dataType, ~group,
#'   "Sim1", "Simulated", "GroupA",
#'   "Sim2", "Simulated", "GroupA",
#'   "Obs1", "Observed", "GroupB",
#'   "Obs2", "Observed", "GroupB",
#'   "Sim3", "Simulated", "GroupC",
#'   "Obs3", "Observed", "GroupC",
#'   "Sim4", "Simulated", "GroupD",
#'   "Obs4", "Observed", "GroupD",
#'   "Obs5", "Observed", "GroupD",
#'   "Sim5", "Simulated", "GroupE",
#'   "Sim6", "Simulated", "GroupE",
#'   "Obs7", "Observed", "GroupE",
#'   "Sim7", "Simulated", "GroupF",
#'   "Sim8", "Simulated", "GroupF",
#'   "Obs8", "Observed", "GroupF",
#'   "Obs9", "Observed", "GroupF",
#'   "Sim9", "Simulated", NA,
#'   "Obs10", "Observed", NA
#' )
#'
#' # original
#' df
#'
#' # transformed
#' ospsuite:::.removeUnpairableDatasets(df)
#'
#' @keywords internal
.removeUnpairableDatasets <- function(data) {
  # How many rows were originally present
  originalDatasets <- unique(data$name)

  # Remove datasets that don't belong to any group.
  data <- dplyr::filter(data, !is.na(group))

  # Remove groups (and the datasets therein) with only one type (either only
  # observed or only simulated) of dataset.
  data <- data %>%
    dplyr::group_by(group) %>%
    dplyr::filter(length(unique(dataType)) > 1L) %>%
    dplyr::ungroup()

  # How many rows are present after filtering
  finalDatasets <- unique(data$name)

  # Warn the user about the filtering if it took place
  if (length(finalDatasets) < length(originalDatasets)) {
    missingDatasets <- originalDatasets[!originalDatasets %in% finalDatasets]

    message(messages$printMultipleEntries(
      header = messages$datasetsToGroupNotFound(),
      entries = missingDatasets
    ))
  }

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

#' Create axes labels
#'
#' @param data A data frame from `DataCombined$toDataFrame()`, which has
#'   additionally been cleaned using `.unitConverter()` to have the same units
#'   across datasets.
#' @param specificPlotConfiguration The nature of labels will change depending
#'   on the type of plot, which can be guessed from the specific
#'   `PlotConfiguration` object used, since each plot has a unique corresponding
#'   class.
#'
#' @examples
#'
#' df <- dplyr::tibble(
#'   dataType = c(rep("simulated", 3), rep("observed", 3)),
#'   xValues = c(0, 14.482, 28.965, 0, 1, 2),
#'   xUnit = "min",
#'   xDimension = "Time",
#'   yValues = c(1, 1, 1, 1, 1, 1),
#'   yUnit = "mol/ml",
#'   yDimension = ospDimensions$`Concentration (mass)`,
#'   yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
#'   molWeight = c(10, 10, 20, 20, 10, 10)
#' )
#'
#' df <- ospsuite:::.unitConverter(df)
#'
#' ospsuite:::.createAxesLabels(df, tlf::TimeProfilePlotConfiguration$new())
#'
#' @details
#'
#' If axes labels haven't been specified, create them using dimensions and units.
#'
#' @keywords internal
.createAxesLabels <- function(data, specificPlotConfiguration) {
  # If empty data frame is entered or plot type is not specified, return early
  if (nrow(data) == 0L || missing(specificPlotConfiguration)) {
    return(NULL)
  }

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class.
  plotType <- class(specificPlotConfiguration)[[1]]

  # Initialize strings with unique values for units and dimensions.
  #
  # The`.unitConverter()` has already ensured that there is only a single unit
  # for x and y quantities, so we can safely take the unique unit to prepare
  # axes labels.
  xUnitString <- unique(data$xUnit)
  yUnitString <- unique(data$yUnit)
  xDimensionString <- unique(data$xDimension)
  yDimensionString <- unique(data$yDimension)

  # Currently, hard code any of the different concentration dimensions to just
  # one dimension: "Concentration"
  #
  # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/938
  concDimensions <- c(
    ospDimensions$`Concentration (mass)`,
    ospDimensions$`Concentration (molar)`
  )

  if (xDimensionString %in% concDimensions) {
    xDimensionString <- "Concentration"
  }

  if (yDimensionString %in% concDimensions) {
    yDimensionString <- "Concentration"
  }

  # If quantities are unitless, no unit information will be displayed.
  # Otherwise, `Dimension [Unit]` pattern will be followed.
  xUnitString <- ifelse(xUnitString == "", xUnitString, paste0(" [", xUnitString, "]"))
  xUnitString <- paste0(xDimensionString, xUnitString)
  yUnitString <- ifelse(yUnitString == "", yUnitString, paste0(" [", yUnitString, "]"))
  yUnitString <- paste0(yDimensionString, yUnitString)

  # The exact axis label will depend on the type of the plot, and the type
  # of the plot can be guessed using the specific `PlotConfiguration` object
  # entered in this function.
  #
  # If the specific `PlotConfiguration` object is not any of the cases included
  # in the `switch` below, the result will be no change; i.e., the labels will
  # continue to be `NULL`.

  # x-axis label
  xLabel <- switch(plotType,
    "TimeProfilePlotConfiguration" = xUnitString,
    "ResVsPredPlotConfiguration" = xUnitString,
    # Note that `yUnitString` here is deliberate.
    #
    # In case of an observed versus simulated plot, `yValues` are plotted on
    # both x- and y-axes, and therefore the units strings are going to be the
    # same for both axes.
    "ObsVsPredPlotConfiguration" = paste0("Observed values (", yUnitString, ")")
  )

  # y-axis label
  yLabel <- switch(plotType,
    "TimeProfilePlotConfiguration" = yUnitString,
    "ResVsPredPlotConfiguration" = "Residuals",
    "ObsVsPredPlotConfiguration" = paste0("Simulated values (", yUnitString, ")")
  )

  return(list("xLabel" = xLabel, "yLabel" = yLabel))
}


#' Created observed versus simulated paired data
#'
#' @param data A data frame from `DataCombined$toDataFrame()`, which has been
#'   further tidied using `.removeUnpairableDatasets()` and then
#'   `.unitConverter()` functions.
#'
#' @keywords internal
.createObsVsPredData <- function(data) {
  # Extract time and values to raw vectors. Working with a single data frame is
  # not an option since the dimensions of observed and simulated data frames are
  # different.
  obsTime <- data$xValues[data$dataType == "observed"]
  obsValue <- data$yValues[data$dataType == "observed"]
  simTime <- data$xValues[data$dataType == "simulated"]
  simValue <- data$yValues[data$dataType == "simulated"]

  # Number of observed and simulated data points
  maxSimPoints <- length(simTime)
  maxObsPoints <- length(obsTime)

  # It is important to initialize this vector to `NA`, and not to `0`.
  predValue <- rep(NA_real_, maxObsPoints)

  # For time points that are not matched, the simulated data needs to be
  # interpolated. This is because simulated data is typically sampled at a
  # higher frequency than the observed data.
  #
  # Interpolation is carried out using the Newton–Raphson method.
  #
  # If index is the same as the length of the vector, then `idx + 1` will be
  # out-of-bounds. So loop only if the index is less than the length of the
  # vector. Thus, `[-maxObsPoints]`.
  #
  # Note that this does *not* mean that the value at the last index
  # in `predValue` vector is always going to be `NA`. It is also possible
  # that there is an exact match at this time point.
  for (idx in seq_along(obsTime)[-maxObsPoints]) {
    currentObsTime <- obsTime[idx]
    currentSimTime <- simTime[idx]
    nextSimTime <- simTime[idx + 1L]
    currentSimValue <- simValue[idx]
    nextSimValue <- simValue[idx + 1L]

    # If the next simulated time point is already OOB but the last simulated
    # time point is still within the bounds of observed time points,
    # interpolation can still be carried out.
    if (idx >= maxSimPoints) {
      if (simTime[maxSimPoints] < obsTime[maxObsPoints]) {
        currentSimTime <- simTime[maxSimPoints - 1L]
        nextSimTime <- simTime[maxSimPoints]
        currentSimValue <- simValue[maxSimPoints - 1L]
        nextSimValue <- simValue[maxSimPoints]
      }
    }

    # f(x) =
    predValue[idx] <-
      # f0 * ((x1 - x) / (x1 - x0)) +
      currentSimValue * ((nextSimTime - currentObsTime) / (nextSimTime - currentSimTime)) +
      # f1 * ((x - x0) / (x1 - x0))
      nextSimValue * ((currentObsTime - currentSimTime) / (nextSimTime - currentSimTime))
  }

  # Figure out time points where both observed and simulated data were sampled.
  obsExactMatchIndices <- which(obsTime %in% simTime)
  simExactMatchIndices <- which(simTime %in% obsTime)

  # For exactly matched time points, there is no need for interpolation.
  predValue[obsExactMatchIndices] <- simValue[simExactMatchIndices]

  # Link observed and interpolated predicted for each observed time point using
  # a data frame.
  pairedData <- dplyr::tibble(
    "obsTime" = obsTime,
    "obsValue" = obsValue,
    "predValue" = predValue
  )

  return(pairedData)
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

  # Plot-specific configuration defaults -----------------------------------

  # The default plot configuration and the labels will vary from plot-to-plot.
  #
  # For example, although the axes labels for profile plots will be (e.g.) "Time
  # vs Fraction", it will be "observed vs simulated values" with the same unit
  # for scatter plot. Additionally, mapping group to line colors might be
  # desirable for a profile plot, it is not so for scatter plots.

  # The type of plot can be guessed from the specific `PlotConfiguration` object
  # used, since each plot has a unique corresponding class.
  plotType <- class(specificPlotConfiguration)[[1]]

  # For `plotIndividualTimeProfile()` and `plotPopulationTimeProfile()`
  if (plotType == "TimeProfilePlotConfiguration") {
    generalPlotConfiguration$linesColor <- generalPlotConfiguration$linesColor %||% tlf::ColorMaps$ospDefault
    generalPlotConfiguration$linesLinetype <- generalPlotConfiguration$linesLinetype %||% tlf::Linetypes$solid
  }

  # For `plotObservedVsSimulated()`
  if (plotType == "ObsVsPredPlotConfiguration") {
    generalPlotConfiguration$linesColor <- generalPlotConfiguration$linesColor %||% "black"
    generalPlotConfiguration$linesLinetype <- generalPlotConfiguration$linesLinetype %||% tlf::Linetypes$dashed
  }

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

  return(specificPlotConfiguration)
}


#' Update legend with specified mappings
#'
#' @details
#'
#' Relevant only in the context of profile plots.
#'
#' By default, every grouping won't get a unique aesthetic mapping. Therefore,
#' the current function uses brute force method to update them afterwards.
#'
#' @param legendCaptionData Data frame extracted using `tlf::getLegendCaption()`.
#' @param timeProfilePlotConfiguration An instance of
#'   `TimeProfilePlotConfiguration` class.
#'
#' @keywords internal
#' @noRd
.updateLegendCaptionData <- function(legendCaptionData, timeProfilePlotConfiguration) {
  # For convenience, assign object properties to vectors
  #
  # Note that the exact shapes need to be saved in the data frame, and not the
  # name for that shape in `tlf::Shapes` list.
  pointsColor <- timeProfilePlotConfiguration$points$color
  pointsShape <- unname(unlist(tlf::Shapes)[timeProfilePlotConfiguration$points$shape])
  lineTypes <- timeProfilePlotConfiguration$lines$linetype

  # As many unique colors and shapes will be needed as there are groupings
  numberOfColorsNeeded <- numberOfShapesNeeded <- nrow(legendCaptionData)

  # Number of needed line types would be equal to only non-blank line types.
  numberOfLinetypesNeeded <- nrow(legendCaptionData[legendCaptionData$linetype != tlf::Linetypes$blank, ])

  # Extract as many color, shape, and line type values as there are datasets
  # from the specified plot configuration object.
  #
  # This is necessary only if a vector of values is provided.
  if (numberOfColorsNeeded > 0L && length(pointsColor) > 1L) {
    pointsColor <- pointsColor[1:numberOfColorsNeeded]
  }

  if (numberOfShapesNeeded > 0L && length(pointsShape) > 1L) {
    pointsShape <- pointsShape[1:numberOfShapesNeeded]
  }

  if (numberOfLinetypesNeeded > 0L && length(lineTypes) > 1L) {
    lineTypes <- lineTypes[1:numberOfLinetypesNeeded]
  }

  # New version of legend mappings for shape and color: all rows are replaced.
  newLegendCaptionData <- dplyr::mutate(legendCaptionData,
    color = pointsColor,
    shape = pointsShape
  )

  # New version of legend mappings for line type: only non-blank line rows are replaced.
  # Relevant only if there were any blank lines in the original legend data frame.
  if (numberOfLinetypesNeeded > 0L) {
    newLegendCaptionData[newLegendCaptionData$linetype != tlf::Linetypes$blank, ]$linetype <- lineTypes
  }

  return(newLegendCaptionData)
}
