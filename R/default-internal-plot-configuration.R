# class definition ---------------------------------

#' @title Convenience class for plot configuration for OSP plots
#'
#' @description
#'
#' An intermediary R6 class that provides a single portal to prepare the
#' `PlotConfiguration` object needed by `{tlf}` plotting functions. The specific
#' type of `PlotConfiguration` objects can access all necessary objects formed
#' from user inputs using this internal object.
#'
#' @field labels `tlf::LabelConfiguration` object defining properties of labels.
#' @field legend `tlf::LegendConfiguration` object defining legend properties.
#' @field xAxis `tlf::XAxisConfiguration` object defining `x`-axis properties.
#' @field yAxis `tlf::YAxisConfiguration` object defining `y`-axis properties.
#' @field background `tlf::BackgroundConfiguration` object defining the
#'   configuration of background.
#' @field lines `tlf::ThemeAestheticSelections` object or list defining how
#'   lines are plotted.
#' @field points `tlf::ThemeAestheticSelections` object or list defining how
#'   points are plotted.
#' @field ribbons `tlf::ThemeAestheticSelections` object or list defining
#'   how ribbons are plotted.
#' @field errorbars `tlf::ThemeAestheticSelections` object or list defining
#'   how errorbars are plotted.
#' @field export `tlf::SaveConfiguration` object defining saving properties.
#'
#' @keywords internal
DefaultInternalPlotConfiguration <- R6::R6Class(
  "DefaultInternalPlotConfiguration",
  public = list(
    labels = NULL,
    legend = NULL,
    xAxis = NULL,
    yAxis = NULL,
    background = NULL,
    lines = NULL,
    points = NULL,
    ribbons = NULL,
    errorbars = NULL,
    export = NULL
  )
)


# function to create instance ---------------------------------

#' Create an instance of `defaultInternalPlotConfiguration` class
#'
#' @examples
#'
#' ospsuite:::.createDefaultInternalPlotConfiguration(DefaultPlotConfiguration$new())
#'
#' @keywords internal
#' @noRd
.createDefaultInternalPlotConfiguration <- function(defaultPlotConfiguration) {
  # check if `{tlf}` is installed; will produce an error if the package is not installed
  requireNamespace("tlf", quietly = TRUE)

  validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration", nullAllowed = FALSE)

  # labels object ---------------------------------------

  labelTitle <- tlf::Label$new(
    text = defaultPlotConfiguration$title,
    font = NULL,
    color = defaultPlotConfiguration$titleColor,
    size = defaultPlotConfiguration$titleSize,
    fontFace = defaultPlotConfiguration$titleFontFace,
    fontFamily = defaultPlotConfiguration$titleFontFamily,
    angle = defaultPlotConfiguration$titleAngle,
    align = defaultPlotConfiguration$titleAlign
  )

  labelSubtitle <- tlf::Label$new(
    text = defaultPlotConfiguration$subtitle,
    font = NULL,
    color = defaultPlotConfiguration$subtitleColor,
    size = defaultPlotConfiguration$subtitleSize,
    fontFace = defaultPlotConfiguration$subtitleFontFace,
    fontFamily = defaultPlotConfiguration$subtitleFontFamily,
    angle = defaultPlotConfiguration$subtitleAngle,
    align = defaultPlotConfiguration$subtitleAlign
  )

  labelCaption <- tlf::Label$new(
    text = defaultPlotConfiguration$caption,
    font = NULL,
    color = defaultPlotConfiguration$captionColor,
    size = defaultPlotConfiguration$captionSize,
    fontFace = defaultPlotConfiguration$captionFontFace,
    fontFamily = defaultPlotConfiguration$captionFontFamily,
    angle = defaultPlotConfiguration$captionAngle,
    align = defaultPlotConfiguration$captionAlign
  )

  labelXLabel <- tlf::Label$new(
    text = defaultPlotConfiguration$xLabel,
    font = NULL,
    color = defaultPlotConfiguration$xLabelColor,
    size = defaultPlotConfiguration$xLabelSize,
    fontFace = defaultPlotConfiguration$xLabelFontFace,
    fontFamily = defaultPlotConfiguration$xLabelFontFamily,
    angle = defaultPlotConfiguration$xLabelAngle,
    align = defaultPlotConfiguration$xLabelAlign
  )

  labelYLabel <- tlf::Label$new(
    text = defaultPlotConfiguration$yLabel,
    font = NULL,
    color = defaultPlotConfiguration$yLabelColor,
    size = defaultPlotConfiguration$yLabelSize,
    fontFace = defaultPlotConfiguration$yLabelFontFace,
    fontFamily = defaultPlotConfiguration$yLabelFontFamily,
    angle = defaultPlotConfiguration$yLabelAngle,
    align = defaultPlotConfiguration$yLabelAlign
  )

  labels <- tlf::LabelConfiguration$new(
    title = labelTitle,
    subtitle = labelSubtitle,
    caption = labelCaption,
    xlabel = labelXLabel,
    ylabel = labelYLabel
  )

  # legend object ---------------------------------------

  legendTitleFont <- tlf::Font$new(
    size = defaultPlotConfiguration$legendTitleSize,
    color = defaultPlotConfiguration$legendTitleColor,
    fontFamily = defaultPlotConfiguration$legendTitleFontFamily,
    fontFace = defaultPlotConfiguration$legendTitleFontFace,
    angle = defaultPlotConfiguration$legendTitleAngle,
    align = defaultPlotConfiguration$legendTitleAlign
  )

  legendCaptionFont <- tlf::Font$new(
    size = defaultPlotConfiguration$legendCaptionSize,
    color = defaultPlotConfiguration$legendCaptionColor,
    fontFamily = defaultPlotConfiguration$legendCaptionFontFamily,
    fontFace = defaultPlotConfiguration$legendCaptionFontFace,
    angle = defaultPlotConfiguration$legendCaptionAngle,
    align = defaultPlotConfiguration$legendCaptionAlign
  )

  legendConfiguration <- tlf::LegendConfiguration$new(
    position = defaultPlotConfiguration$legendPosition,
    caption = NULL,
    title = defaultPlotConfiguration$legendTitle,
    font = defaultPlotConfiguration$legendCaptionFont,
    background = NULL
  )

  # background objects -----------------------------------

  labelWatermark <- tlf::Label$new(
    text = defaultPlotConfiguration$watermark,
    font = NULL,
    color = defaultPlotConfiguration$watermarkColor,
    size = defaultPlotConfiguration$watermarkSize,
    fontFace = defaultPlotConfiguration$watermarkFontFace,
    fontFamily = defaultPlotConfiguration$watermarkFontFamily,
    angle = defaultPlotConfiguration$watermarkAngle,
    align = defaultPlotConfiguration$watermarkAlign
  )

  plotBackground <- tlf::BackgroundElement$new(
    fill = defaultPlotConfiguration$plotBackgroundFill,
    color = defaultPlotConfiguration$plotBackgroundColor,
    size = defaultPlotConfiguration$plotBackgroundSize,
    linetype = defaultPlotConfiguration$plotBackgroundLinetype
  )

  plotPanelBackground <- tlf::BackgroundElement$new(
    fill = defaultPlotConfiguration$plotPanelBackgroundFill,
    color = defaultPlotConfiguration$plotPanelBackgroundColor,
    size = defaultPlotConfiguration$plotPanelBackgroundSize,
    linetype = defaultPlotConfiguration$plotPanelBackgroundLinetype
  )

  xAxis <- tlf::LineElement$new(
    color = defaultPlotConfiguration$xAxisColor,
    size = defaultPlotConfiguration$xAxisSize,
    linetype = defaultPlotConfiguration$xAxisLinetype
  )

  yAxis <- tlf::LineElement$new(
    color = defaultPlotConfiguration$yAxisColor,
    size = defaultPlotConfiguration$yAxisSize,
    linetype = defaultPlotConfiguration$yAxisLinetype
  )

  xGrid <- tlf::LineElement$new(
    color = defaultPlotConfiguration$xGridColor,
    size = defaultPlotConfiguration$xGridSize,
    linetype = defaultPlotConfiguration$xGridLinetype
  )

  yGrid <- tlf::LineElement$new(
    color = defaultPlotConfiguration$yGridColor,
    size = defaultPlotConfiguration$yGridSize,
    linetype = defaultPlotConfiguration$yGridLinetype
  )

  background <- tlf::BackgroundConfiguration$new(
    watermark = defaultPlotConfiguration$labelWatermark,
    plot = defaultPlotConfiguration$plotBackground,
    panel = defaultPlotConfiguration$plotPanelBackground,
    xAxis = defaultPlotConfiguration$xAxis,
    yAxis = defaultPlotConfiguration$yAxis,
    xGrid = defaultPlotConfiguration$xGrid,
    yGrid = defaultPlotConfiguration$yGrid
  )

  # xAxis objects -----------------------------------

  xAxisFont <- tlf::Font$new(
    size = defaultPlotConfiguration$xAxisLabelTicksSize,
    color = defaultPlotConfiguration$xAxisLabelTicksColor,
    fontFamily = defaultPlotConfiguration$xAxisLabelTicksFontFamily,
    fontFace = defaultPlotConfiguration$xAxisLabelTicksFontFace,
    angle = defaultPlotConfiguration$xAxisLabelTicksAngle,
    align = defaultPlotConfiguration$xAxisLabelTicksAlign
  )

  xAxisConfiguration <- tlf::XAxisConfiguration$new(
    limits = defaultPlotConfiguration$xAxisLimits,
    scale = defaultPlotConfiguration$xAxisScale,
    ticks = defaultPlotConfiguration$xAxisTicks,
    ticklabels = defaultPlotConfiguration$xAxisTicksLabels,
    font = defaultPlotConfiguration$xAxisFont
  )

  # yAxis objects -----------------------------------

  yAxisFont <- tlf::Font$new(
    size = defaultPlotConfiguration$yAxisLabelTicksSize,
    color = defaultPlotConfiguration$yAxisLabelTicksColor,
    fontFamily = defaultPlotConfiguration$yAxisLabelTicksFontFamily,
    fontFace = defaultPlotConfiguration$yAxisLabelTicksFontFace,
    angle = defaultPlotConfiguration$yAxisLabelTicksAngle,
    align = defaultPlotConfiguration$yAxisLabelTicksAlign
  )

  yAxisConfiguration <- tlf::YAxisConfiguration$new(
    limits = defaultPlotConfiguration$yAxisLimits,
    scale = defaultPlotConfiguration$yAxisScale,
    ticks = defaultPlotConfiguration$yAxisTicks,
    ticklabels = defaultPlotConfiguration$yAxisTicksLabels,
    font = defaultPlotConfiguration$yAxisFont
  )

  # lines -------------------------------------------------------

  linesConfiguration <- tlf::ThemeAestheticSelections$new(
    color = defaultPlotConfiguration$linesColor,
    shape = defaultPlotConfiguration$linesShape,
    size = defaultPlotConfiguration$linesSize,
    linetype = defaultPlotConfiguration$linesLinetype,
    alpha = defaultPlotConfiguration$linesAlpha
  )

  # points -------------------------------------------------------

  pointsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = defaultPlotConfiguration$pointsColor,
    fill = defaultPlotConfiguration$pointsFill,
    shape = defaultPlotConfiguration$pointsShape,
    size = defaultPlotConfiguration$pointsSize,
    linetype = defaultPlotConfiguration$pointsLinetype,
    alpha = defaultPlotConfiguration$pointsAlpha
  )

  # ribbons -------------------------------------------------------

  ribbonsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = defaultPlotConfiguration$ribbonsColor,
    fill = defaultPlotConfiguration$ribbonsFill,
    shape = defaultPlotConfiguration$ribbonsShape,
    size = defaultPlotConfiguration$ribbonsSize,
    linetype = defaultPlotConfiguration$ribbonsLinetype,
    alpha = defaultPlotConfiguration$ribbonsAlpha
  )

  # errorbars -------------------------------------------------------

  errorbarsConfiguration <- tlf::ThemeAestheticSelections$new(
    shape = defaultPlotConfiguration$errorbarsShape,
    size = defaultPlotConfiguration$errorbarsSize,
    linetype = defaultPlotConfiguration$errorbarsLinetype,
    alpha = defaultPlotConfiguration$errorbarsAlpha
  )

  # export -------------------------------------------------------

  exportConfiguration <- tlf::ExportConfiguration$new(
    name = defaultPlotConfiguration$plotSaveFileName,
    format = defaultPlotConfiguration$plotSaveFileFormat,
    width = defaultPlotConfiguration$plotSaveFileWidth,
    height = defaultPlotConfiguration$plotSaveFileHeight,
    units = defaultPlotConfiguration$plotSaveFileDimensionUnits,
    dpi = defaultPlotConfiguration$plotSaveFileDpi
  )

  # defaultInternalPlotConfiguration object -----------------------------------

  defaultInternalPlotConfiguration <- DefaultInternalPlotConfiguration$new()

  defaultInternalPlotConfiguration$labels <- labels
  defaultInternalPlotConfiguration$legend <- legendConfiguration
  defaultInternalPlotConfiguration$xAxis <- xAxisConfiguration
  defaultInternalPlotConfiguration$yAxis <- yAxisConfiguration
  defaultInternalPlotConfiguration$background <- background
  defaultInternalPlotConfiguration$lines <- linesConfiguration
  defaultInternalPlotConfiguration$points <- pointsConfiguration
  defaultInternalPlotConfiguration$ribbons <- ribbonsConfiguration
  defaultInternalPlotConfiguration$errorbars <- errorbarsConfiguration
  defaultInternalPlotConfiguration$export <- exportConfiguration

  return(defaultInternalPlotConfiguration)
}
