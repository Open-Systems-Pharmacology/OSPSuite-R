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
                                    background = NULL,
                                    plotArea = NULL,
                                    panelArea = NULL,
                                    xGrid = NULL,
                                    yGrid = NULL,
                                    # watermark
                                    watermark = NULL,
                                    watermarkSize = NULL,
                                    watermarkColor = NULL,
                                    watermarkFontFamily = NULL,
                                    watermarkFontFace = NULL,
                                    watermarkAngle = NULL,
                                    # lines
                                    lines = NULL,
                                    # points
                                    points = NULL,
                                    # ribbons
                                    ribbons = NULL,
                                    # errorbars
                                    errorbars = NULL,
                                    export = NULL,
                                    format = NULL,
                                    width = NULL,
                                    height = NULL,
                                    units = NULL) {

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

  xAxisConfiguration <- AxisConfiguration$new(
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

  yAxisConfiguration <- AxisConfiguration$new(
    limits = yAxisLimits,
    scale = yAxisScale,
    ticks = yAxisTicks,
    ticklabels = yAxisTickLabels,
    font = yAxisFont
  )

  # ospPlotConfiguration object -----------------------------------

  ospPlotConfiguration$new(
    title          = title,
    subtitle       = subtitle,
    xlabel         = xlabel,
    ylabel         = ylabel,
    legend         = legendConfig,
    xAxis          = xAxisConfiguration,
    yAxis          = yAxisConfiguration,
    background     = background,
    plotArea       = plotArea,
    panelArea      = panelArea,
    xGrid          = xGrid,
    yGrid          = yGrid,
    watermark      = labelWatermark,
    lines          = lines,
    points         = points,
    ribbons        = ribbons,
    errorbars      = errorbars,
    export         = export,
    format         = format,
    width          = width,
    height         = height,
    units          = units
  )
}
