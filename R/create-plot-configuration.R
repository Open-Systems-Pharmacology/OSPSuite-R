#' Create an instance of `ospPlotConfiguration` class
#'
#' @param title,subtitle,xlabel,ylabel,legendTitle,watermark A character string
#'   providing plot annotations for plot title, subtitle, x-axis label, y-axis
#'   label, plot legend, watermark, respectively.
#' @param legendPosition A character string defining the legend position
#'   (default: `"outsideRight"`). Available options
#'   are:`"none"`,`"insideTop"`,`"insideTopLeft"`,
#'   `"insideLeft"`,`"insideBottomLeft"`,`"insideBottom"`,`"insideBottomRight"`,
#'   `"insideRight"`,`"insideTopRight"`,`"outsideTop"`,`"outsideTopLeft"`,
#'   `"outsideLeft"`,`"outsideBottomLeft"`,`"outsideBottom"`,`"outsideBottomRight"`,
#'    `"outsideRight"`,`"outsideTopRight"`.
#' @param legendTitleSize,legendTitleColor,legendTitleFontFamily,legendTitleFontFace,legendTitleAngle,legendCaptionSize Aesthetic properties for the legend title.
#' @param legendCaptionColor,legendCaptionFontFamily,legendCaptionFontFace,legendCaptionAngle Aesthetic properties for the legend caption.
#' @param xAxisTickLabels,xAxisLabelSize,xAxisLabelColor,xAxisLabelFontFamily,xAxisLabelFontFace,xAxisLabelAngle Aesthetic properties for the x-axis label.
#' @param yAxisTickLabels,yAxisLabelSize,yAxisLabelColor,yAxisLabelFontFamily,yAxisLabelFontFace,yAxisLabelAngle Aesthetic properties for the y-axis label.
#' @param xAxisLimits,yAxisLimits A numeric vector of axis limits for the x-and
#'   y-axis, respectively.
#' @param xAxisTicks,yAxisTicks A numeric vector or a function defining where to
#'   position x-and y-axis ticks, respectively.
#' @param xAxisScale,yAxisScale A character string defining axis scale (default:
#'   `"lin"`). Available options are: `"lin"`, `"log"`, `"ln"`, `"discrete"`,
#'   `"reverse"`, `"sqrt"`, `"time"`, `"date"`.
#' @param watermarkSize,watermarkColor,watermarkFontFamily,watermarkFontFace,watermarkAngle Aesthetic properties for the watermark.
#' @param linesColor,linesFill,linesShape,linesSize,linesLinetype,linesAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for lines.
#' @param pointsColor,pointsFill,pointsShape,pointsSize,pointsLinetype,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @param ribbonsColor,ribbonsFill,ribbonsShape,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @param errorbarsColor,errorbarsFill,errorbarsShape,errorbarsSize,errorbarsLinetype,errorbarsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for errorbars.
#' @param plotSaveFileFormat,plotSaveFileWidth,plotSaveFileHeight,plotSaveFileDimensionUnits File format to which the plot needs to be saved, and the units and dimensions for saving the plot.
#'
#' @export
createPlotConfiguration <- function(title = NULL,
                                    subtitle = NULL,
                                    xlabel = NULL,
                                    ylabel = NULL,
                                    # legend
                                    legendPosition = "outsideRight",
                                    legendTitle = NULL,
                                    legendTitleSize = NULL,
                                    legendTitleColor = NULL,
                                    legendTitleFontFamily = NULL,
                                    legendTitleFontFace = NULL,
                                    legendTitleAngle = NULL,
                                    legendCaptionSize = NULL,
                                    legendCaptionColor = NULL,
                                    legendCaptionFontFamily = NULL,
                                    legendCaptionFontFace = NULL,
                                    legendCaptionAngle = NULL,
                                    # XAxisConfiguration
                                    xAxisLimits = NULL,
                                    xAxisScale = "lin",
                                    xAxisTicks = NULL,
                                    xAxisTickLabels = NULL,
                                    xAxisLabelSize = NULL,
                                    xAxisLabelColor = NULL,
                                    xAxisLabelFontFamily = NULL,
                                    xAxisLabelFontFace = NULL,
                                    xAxisLabelAngle = NULL,
                                    # YAxisConfiguration
                                    yAxisLimits = NULL,
                                    yAxisScale = "lin",
                                    yAxisTicks = NULL,
                                    yAxisTickLabels = NULL,
                                    yAxisLabelSize = NULL,
                                    yAxisLabelColor = NULL,
                                    yAxisLabelFontFamily = NULL,
                                    yAxisLabelFontFace = NULL,
                                    yAxisLabelAngle = NULL,
                                    # watermark
                                    watermark = NULL,
                                    watermarkSize = NULL,
                                    watermarkColor = NULL,
                                    watermarkFontFamily = NULL,
                                    watermarkFontFace = NULL,
                                    watermarkAngle = NULL,
                                    # lines
                                    linesColor = NULL,
                                    linesFill = NULL,
                                    linesShape = NULL,
                                    linesSize = NULL,
                                    linesLinetype = NULL,
                                    linesAlpha = NULL,
                                    # points
                                    pointsColor = NULL,
                                    pointsFill = NULL,
                                    pointsShape = NULL,
                                    pointsSize = NULL,
                                    pointsLinetype = NULL,
                                    pointsAlpha = NULL,
                                    # ribbons
                                    ribbonsColor = NULL,
                                    ribbonsFill = NULL,
                                    ribbonsShape = NULL,
                                    ribbonsSize = NULL,
                                    ribbonsLinetype = NULL,
                                    ribbonsAlpha = NULL,
                                    # errorbars
                                    errorbarsColor = NULL,
                                    errorbarsFill = NULL,
                                    errorbarsShape = NULL,
                                    errorbarsSize = NULL,
                                    errorbarsLinetype = NULL,
                                    errorbarsAlpha = NULL,
                                    # export
                                    plotSaveFileFormat = "png",
                                    plotSaveFileWidth = 16,
                                    plotSaveFileHeight = 9,
                                    plotSaveFileDimensionUnits = "cm") {

  # legend objects ---------------------------------------

  legendTitleFont <- tlf::Font$new(
    size = legendTitleSize,
    color = legendTitleColor,
    fontFamily = legendTitleFontFamily,
    fontFace = legendTitleFontFace,
    angle = legendTitleAngle
  )

  legendCaptionFont <- tlf::Font$new(
    size = legendCaptionSize,
    color = legendCaptionColor,
    fontFamily = legendCaptionFontFamily,
    fontFace = legendCaptionFontFace,
    angle = legendCaptionAngle
  )

  legendConfig <- tlf::LegendConfiguration$new(
    position = legendPosition,
    caption = NULL,
    title = legendTitle,
    titleFont = legendTitleFont,
    font = legendCaptionFont,
    background = NULL
  )

  # watermark objects -----------------------------------

  labelWatermark <- tlf::Label$new(
    text = watermark,
    font = NULL,
    color = watermarkColor,
    size = watermarkSize,
    fontFace = watermarkFontFace,
    fontFamily = watermarkFontFamily,
    angle = watermarkAngle
  )

  # xAxis objects -----------------------------------

  xAxisFont <- tlf::Font$new(
    size = xAxisLabelSize,
    color = xAxisLabelColor,
    fontFamily = xAxisLabelFontFamily,
    fontFace = xAxisLabelFontFace,
    angle = xAxisLabelAngle
  )

  xAxisConfiguration <- tlf::XAxisConfiguration$new(
    limits = xAxisLimits,
    scale = xAxisScale,
    ticks = xAxisTicks,
    ticklabels = xAxisTickLabels,
    font = xAxisFont
  )

  # yAxis objects -----------------------------------

  yAxisFont <- tlf::Font$new(
    size = yAxisLabelSize,
    color = yAxisLabelColor,
    fontFamily = yAxisLabelFontFamily,
    fontFace = yAxisLabelFontFace,
    angle = yAxisLabelAngle
  )

  yAxisConfiguration <- tlf::YAxisConfiguration$new(
    limits = yAxisLimits,
    scale = yAxisScale,
    ticks = yAxisTicks,
    ticklabels = yAxisTickLabels,
    font = yAxisFont
  )

  # lines -------------------------------------------------------

  linesConfiguration <- tlf::ThemeAestheticSelections$new(
    color = linesColor,
    fill = linesFill,
    shape = linesShape,
    size = linesSize,
    linetype = linesLinetype,
    alpha = linesAlpha
  )

  # points -------------------------------------------------------

  pointsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = pointsColor,
    fill = pointsFill,
    shape = pointsShape,
    size = pointsSize,
    linetype = pointsLinetype,
    alpha = pointsAlpha
  )

  # ribbons -------------------------------------------------------

  ribbonsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = ribbonsColor,
    fill = ribbonsFill,
    shape = ribbonsShape,
    size = ribbonsSize,
    linetype = ribbonsLinetype,
    alpha = ribbonsAlpha
  )

  # errorbars -------------------------------------------------------

  errorbarsConfiguration <- tlf::ThemeAestheticSelections$new(
    color = errorbarsColor,
    fill = errorbarsFill,
    shape = errorbarsShape,
    size = errorbarsSize,
    linetype = errorbarsLinetype,
    alpha = errorbarsAlpha
  )

  # export -------------------------------------------------------

  exportConfiguration <- tlf::ExportConfiguration$new(
    format = plotSaveFileFormat,
    width = plotSaveFileWidth,
    height = plotSaveFileHeight,
    units = plotSaveFileDimensionUnits
  )

  # ospPlotConfiguration object -----------------------------------

  ospPlotConfiguration$new(
    title = title,
    subtitle = subtitle,
    xlabel = xlabel,
    ylabel = ylabel,
    legend = legendConfig,
    xAxis = xAxisConfiguration,
    yAxis = yAxisConfiguration,
    watermark = labelWatermark,
    lines = linesConfiguration,
    points = pointsConfiguration,
    ribbons = ribbonsConfiguration,
    errorbars = errorbarsConfiguration,
    export = exportConfiguration
  )
}
