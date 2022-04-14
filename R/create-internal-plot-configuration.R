#' Create an instance of `ospInternalPlotConfiguration` class
#' @keywords internal
#' @noRd
.createOSPInternalPlotConfiguration <- function(ospPlotConfiguration) {

  # labels object ---------------------------------------

  labelTitle <- tlf::Label$new(
    text = ospPlotConfiguration$title,
    font = NULL,
    color = ospPlotConfiguration$titleColor,
    size = ospPlotConfiguration$titleSize,
    fontFace = ospPlotConfiguration$titleFontFace,
    fontFamily = ospPlotConfiguration$titleFontFamily,
    angle = ospPlotConfiguration$titleAngle
  )

  labelSubtitle <- tlf::Label$new(
    text = ospPlotConfiguration$subtitle,
    font = NULL,
    color = ospPlotConfiguration$subtitleColor,
    size = ospPlotConfiguration$subtitleSize,
    fontFace = ospPlotConfiguration$subtitleFontFace,
    fontFamily = ospPlotConfiguration$subtitleFontFamily,
    angle = ospPlotConfiguration$subtitleAngle
  )

  labelXLabel <- tlf::Label$new(
    text = ospPlotConfiguration$xLabel,
    font = NULL,
    color = ospPlotConfiguration$xLabelColor,
    size = ospPlotConfiguration$xLabelSize,
    fontFace = ospPlotConfiguration$xLabelFontFace,
    fontFamily = ospPlotConfiguration$xLabelFontFamily,
    angle = ospPlotConfiguration$xLabelAngle
  )

  labelYLabel <- tlf::Label$new(
    text = ospPlotConfiguration$yLabel,
    font = NULL,
    color = ospPlotConfiguration$yLabelColor,
    size = ospPlotConfiguration$yLabelSize,
    fontFace = ospPlotConfiguration$yLabelFontFace,
    fontFamily = ospPlotConfiguration$yLabelFontFamily,
    angle = ospPlotConfiguration$yLabelAngle
  )

  labels <- tlf::LabelConfiguration$new(
    title = labelTitle,
    subtitle = labelSubtitle,
    xlabel = labelXLabel,
    ylabel = labelYLabel
  )

  # legend object ---------------------------------------

  legendTitleFont <- tlf::Font$new(
    size = ospPlotConfiguration$legendTitleSize,
    color = ospPlotConfiguration$legendTitleColor,
    fontFamily = ospPlotConfiguration$legendTitleFontFamily,
    fontFace = ospPlotConfiguration$legendTitleFontFace,
    angle = ospPlotConfiguration$legendTitleAngle
  )

  legendCaptionFont <- tlf::Font$new(
    size = ospPlotConfiguration$legendCaptionSize,
    color = ospPlotConfiguration$legendCaptionColor,
    fontFamily = ospPlotConfiguration$legendCaptionFontFamily,
    fontFace = ospPlotConfiguration$legendCaptionFontFace,
    angle = ospPlotConfiguration$legendCaptionAngle
  )

  legendConfig <- tlf::LegendConfiguration$new(
    position = ospPlotConfiguration$legendPosition,
    caption = NULL,
    title = ospPlotConfiguration$legendTitle,
    titleFont = ospPlotConfiguration$legendTitleFont,
    font = ospPlotConfiguration$legendCaptionFont,
    background = NULL
  )

  # background objects -----------------------------------

  labelWatermark <- tlf::Label$new(
    text = ospPlotConfiguration$watermark,
    font = NULL,
    color = ospPlotConfiguration$watermarkColor,
    size = ospPlotConfiguration$watermarkSize,
    fontFace = ospPlotConfiguration$watermarkFontFace,
    fontFamily = ospPlotConfiguration$watermarkFontFamily,
    angle = ospPlotConfiguration$watermarkAngle
  )

  plotBackground <- tlf::BackgroundElement$new(
    fill = ospPlotConfiguration$plotBackgroundFill,
    color = ospPlotConfiguration$plotBackgroundColor,
    size = ospPlotConfiguration$plotBackgroundSize,
    linetype = ospPlotConfiguration$plotBackgroundLinetype
  )

  plotPanelBackground <- tlf::BackgroundElement$new(
    fill = ospPlotConfiguration$plotPanelBackgroundFill,
    color = ospPlotConfiguration$plotPanelBackgroundColor,
    size = ospPlotConfiguration$plotPanelBackgroundSize,
    linetype = ospPlotConfiguration$plotPanelBackgroundLinetype
  )

  xAxis <- tlf::LineElement$new(
    color = ospPlotConfiguration$xAxisColor,
    size = ospPlotConfiguration$xAxisSize,
    linetype = ospPlotConfiguration$xAxisLinetype
  )

  yAxis <- tlf::LineElement$new(
    color = ospPlotConfiguration$yAxisColor,
    size = ospPlotConfiguration$yAxisSize,
    linetype = ospPlotConfiguration$yAxisLinetype
  )

  xGrid <- tlf::LineElement$new(
    color = ospPlotConfiguration$xGridColor,
    size = ospPlotConfiguration$xGridSize,
    linetype = ospPlotConfiguration$xGridLinetype
  )

  yGrid <- tlf::LineElement$new(
    color = ospPlotConfiguration$yGridColor,
    size = ospPlotConfiguration$yGridSize,
    linetype = ospPlotConfiguration$yGridLinetype
  )

  background <- tlf::BackgroundConfiguration$new(
    watermark = ospPlotConfiguration$labelWatermark,
    plot = ospPlotConfiguration$plotBackground,
    panel = ospPlotConfiguration$plotPanelBackground,
    xAxis = ospPlotConfiguration$xAxis,
    yAxis = ospPlotConfiguration$yAxis,
    xGrid = ospPlotConfiguration$xGrid,
    yGrid = ospPlotConfiguration$yGrid
  )

  # xAxis objects -----------------------------------

  xAxisFont <- tlf::Font$new(
    size = ospPlotConfiguration$xAxisLabelSize,
    color = ospPlotConfiguration$xAxisLabelColor,
    fontFamily = ospPlotConfiguration$xAxisLabelFontFamily,
    fontFace = ospPlotConfiguration$xAxisLabelFontFace,
    angle = ospPlotConfiguration$xAxisLabelAngle
  )

  xAxisConfiguration <- tlf::XAxisConfiguration$new(
    limits = ospPlotConfiguration$xAxisLimits,
    scale = ospPlotConfiguration$xAxisScale,
    ticks = ospPlotConfiguration$xAxisTicks,
    ticklabels = ospPlotConfiguration$xAxisTickLabels,
    font = ospPlotConfiguration$xAxisFont
  )

  # yAxis objects -----------------------------------

  yAxisFont <- tlf::Font$new(
    size = ospPlotConfiguration$yAxisLabelSize,
    color = ospPlotConfiguration$yAxisLabelColor,
    fontFamily = ospPlotConfiguration$yAxisLabelFontFamily,
    fontFace = ospPlotConfiguration$yAxisLabelFontFace,
    angle = ospPlotConfiguration$yAxisLabelAngle
  )

  yAxisConfiguration <- tlf::YAxisConfiguration$new(
    limits = ospPlotConfiguration$yAxisLimits,
    scale = ospPlotConfiguration$yAxisScale,
    ticks = ospPlotConfiguration$yAxisTicks,
    ticklabels = ospPlotConfiguration$yAxisTickLabels,
    font = ospPlotConfiguration$yAxisFont
  )

  # lines -------------------------------------------------------

  linesConfiguration <- tlf::ThemeAestheticSelections$new(
    color = ospPlotConfiguration$linesColor,
    fill = ospPlotConfiguration$linesFill,
    shape = ospPlotConfiguration$linesShape,
    size = ospPlotConfiguration$linesSize,
    linetype = ospPlotConfiguration$linesLinetype,
    alpha = ospPlotConfiguration$linesAlpha
  )

  # points -------------------------------------------------------

  pointsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = ospPlotConfiguration$pointsColor,
    fill = ospPlotConfiguration$pointsFill,
    shape = ospPlotConfiguration$pointsShape,
    size = ospPlotConfiguration$pointsSize,
    linetype = ospPlotConfiguration$pointsLinetype,
    alpha = ospPlotConfiguration$pointsAlpha
  )

  # ribbons -------------------------------------------------------

  ribbonsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = ospPlotConfiguration$ribbonsColor,
    fill = ospPlotConfiguration$ribbonsFill,
    shape = ospPlotConfiguration$ribbonsShape,
    size = ospPlotConfiguration$ribbonsSize,
    linetype = ospPlotConfiguration$ribbonsLinetype,
    alpha = ospPlotConfiguration$ribbonsAlpha
  )

  # errorbars -------------------------------------------------------

  errorbarsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = ospPlotConfiguration$errorbarsColor,
    fill = ospPlotConfiguration$errorbarsFill,
    shape = ospPlotConfiguration$errorbarsShape,
    size = ospPlotConfiguration$errorbarsSize,
    linetype = ospPlotConfiguration$errorbarsLinetype,
    alpha = ospPlotConfiguration$errorbarsAlpha
  )

  # export -------------------------------------------------------

  exportConfiguration <- tlf::ExportConfiguration$new(
    format = ospPlotConfiguration$plotSaveFileFormat,
    width = ospPlotConfiguration$plotSaveFileWidth,
    height = ospPlotConfiguration$plotSaveFileHeight,
    units = ospPlotConfiguration$plotSaveFileDimensionUnits
  )

  # ospInternalPlotConfiguration object -----------------------------------

  ospInternalPlotConfiguration$new(
    labels = labels,
    legend = legendConfig,
    xAxis = xAxisConfiguration,
    yAxis = yAxisConfiguration,
    background = background,
    lines = linesConfiguration,
    points = pointsConfiguration,
    ribbons = ribbonsConfiguration,
    errorbars = errorbarsConfiguration,
    export = exportConfiguration
  )
}
