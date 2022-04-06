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
#' @param watermarkSize,watermarkColor,watermarkFontFamily,watermarkFontFace,watermarkAngle A character string specifying the aesthetic properties for the watermark.
#' @param plotBackgroundFill,plotBackgroundColor,plotBackgroundSize,plotBackgroundLinetype A character string specifying the aesthetic properties for the plot background.
#' @param plotPanelBackgroundFill,plotPanelBackgroundColor,plotPanelBackgroundSize,plotPanelBackgroundLinetype A character string specifying the aesthetic properties for the plot panel (inside of plot) background.
#' @param xAxisColor,xAxisSize,xAxisLinetype A character string specifying the aesthetic properties for the x-axis.
#' @param yAxisColor,yAxisSize,yAxisLinetype A character string specifying the aesthetic properties for the y-axis.
#' @param xGridColor,xGridSize,xGridLinetype A character string specifying the aesthetic properties for the x-axis grid.
#' @param yGridColor,yGridSize,yGridLinetype A character string specifying the aesthetic properties for the y-axis grid.
#' @param linesColor,linesFill,linesShape,linesSize,linesLinetype,linesAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for lines.
#' @param pointsColor,pointsFill,pointsShape,pointsSize,pointsLinetype,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @param ribbonsColor,ribbonsFill,ribbonsShape,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @param errorbarsColor,errorbarsFill,errorbarsShape,errorbarsSize,errorbarsLinetype,errorbarsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for errorbars.
#' @param plotSaveFileFormat,plotSaveFileWidth,plotSaveFileHeight,plotSaveFileDimensionUnits File format to which the plot needs to be saved, and the units and dimensions for saving the plot.
#'
#' @export
createPlotConfiguration <- function( # labels
                                    title = NULL,
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
                                    # background configuration
                                    watermark = NULL,
                                    watermarkSize = NULL,
                                    watermarkColor = NULL,
                                    watermarkFontFamily = NULL,
                                    watermarkFontFace = NULL,
                                    watermarkAngle = NULL,
                                    plotBackgroundFill = NULL,
                                    plotBackgroundColor = NULL,
                                    plotBackgroundSize = NULL,
                                    plotBackgroundLinetype = NULL,
                                    plotPanelBackgroundFill = NULL,
                                    plotPanelBackgroundColor = NULL,
                                    plotPanelBackgroundSize = NULL,
                                    plotPanelBackgroundLinetype = NULL,
                                    xAxisColor = NULL,
                                    xAxisSize = NULL,
                                    xAxisLinetype = NULL,
                                    yAxisColor = NULL,
                                    yAxisSize = NULL,
                                    yAxisLinetype = NULL,
                                    xGridColor = NULL,
                                    xGridSize = NULL,
                                    xGridLinetype = NULL,
                                    yGridColor = NULL,
                                    yGridSize = NULL,
                                    yGridLinetype = NULL,
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

  ospPlotConfiguration$new(
    title = title,
    subtitle = subtitle,
    xlabel = xlabel,
    ylabel = ylabel,
    legendPosition = legendPosition,
    legendTitle = legendTitle,
    legendTitleSize = legendTitleSize,
    legendTitleColor = legendTitleColor,
    legendTitleFontFamily = legendTitleFontFamily,
    legendTitleFontFace = legendTitleFontFace,
    legendTitleAngle = legendTitleAngle,
    legendCaptionSize = legendCaptionSize,
    legendCaptionColor = legendCaptionColor,
    legendCaptionFontFamily = legendCaptionFontFamily,
    legendCaptionFontFace = legendCaptionFontFace,
    legendCaptionAngle = legendCaptionAngle,
    xAxisLimits = xAxisLimits,
    xAxisScale = xAxisScale,
    xAxisTicks = xAxisTicks,
    xAxisTickLabels = xAxisTickLabels,
    xAxisLabelSize = xAxisLabelSize,
    xAxisLabelColor = xAxisLabelColor,
    xAxisLabelFontFamily = xAxisLabelFontFamily,
    xAxisLabelFontFace = xAxisLabelFontFace,
    xAxisLabelAngle = xAxisLabelAngle,
    yAxisLimits = yAxisLimits,
    yAxisScale = yAxisScale,
    yAxisTicks = yAxisTicks,
    yAxisTickLabels = yAxisTickLabels,
    yAxisLabelSize = yAxisLabelSize,
    yAxisLabelColor = yAxisLabelColor,
    yAxisLabelFontFamily = yAxisLabelFontFamily,
    yAxisLabelFontFace = yAxisLabelFontFace,
    yAxisLabelAngle = yAxisLabelAngle,
    watermark = watermark,
    watermarkSize = watermarkSize,
    watermarkColor = watermarkColor,
    watermarkFontFamily = watermarkFontFamily,
    watermarkFontFace = watermarkFontFace,
    watermarkAngle = watermarkAngle,
    plotBackgroundFill = plotBackgroundFill,
    plotBackgroundColor = plotBackgroundColor,
    plotBackgroundSize = plotBackgroundSize,
    plotBackgroundLinetype = plotBackgroundLinetype,
    plotPanelBackgroundFill = plotPanelBackgroundFill,
    plotPanelBackgroundColor = plotPanelBackgroundColor,
    plotPanelBackgroundSize = plotPanelBackgroundSize,
    plotPanelBackgroundLinetype = plotPanelBackgroundLinetype,
    xAxisColor = xAxisColor,
    xAxisSize = xAxisSize,
    xAxisLinetype = xAxisLinetype,
    yAxisColor = yAxisColor,
    yAxisSize = yAxisSize,
    yAxisLinetype = yAxisLinetype,
    xGridColor = xGridColor,
    xGridSize = xGridSize,
    xGridLinetype = xGridLinetype,
    yGridColor = yGridColor,
    yGridSize = yGridSize,
    yGridLinetype = yGridLinetype,
    linesColor = linesColor,
    linesFill = linesFill,
    linesShape = linesShape,
    linesSize = linesSize,
    linesLinetype = linesLinetype,
    linesAlpha = linesAlpha,
    pointsColor = pointsColor,
    pointsFill = pointsFill,
    pointsShape = pointsShape,
    pointsSize = pointsSize,
    pointsLinetype = pointsLinetype,
    pointsAlpha = pointsAlpha,
    ribbonsColor = ribbonsColor,
    ribbonsFill = ribbonsFill,
    ribbonsShape = ribbonsShape,
    ribbonsSize = ribbonsSize,
    ribbonsLinetype = ribbonsLinetype,
    ribbonsAlpha = ribbonsAlpha,
    errorbarsColor = errorbarsColor,
    errorbarsFill = errorbarsFill,
    errorbarsShape = errorbarsShape,
    errorbarsSize = errorbarsSize,
    errorbarsLinetype = errorbarsLinetype,
    errorbarsAlpha = errorbarsAlpha,
    plotSaveFileFormat = plotSaveFileFormat,
    plotSaveFileWidth = plotSaveFileWidth,
    plotSaveFileHeight = plotSaveFileHeight,
    plotSaveFileDimensionUnits = plotSaveFileDimensionUnits
  )
}
