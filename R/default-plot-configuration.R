#' @title Convenience class for plot configuration for OSP plots
#'
#' @description
#'
#' R6 class defining the configuration for `{ospsuite}` plots. It holds values
#' for all relevant plot properties.
#'
#' Note that the values this objects contains are of general-purpose utility. In
#' other words, the public members of this class instance can be used to specify
#' arguments for base plots, ggplot plots, or any other plotting framework.
#'
#' @field xUnit,yUnit Units for quantities plotted on x- and y-axes, respectively.
#' @field title,subtitle,xLabel,yLabel,legendTitle,watermark A character string
#'   providing plot annotations for plot title, subtitle, x-axis label, y-axis
#'   label, plot legend, watermark, respectively.
#' @field titleColor,titleSize,titleFontFace,titleFontFamily,titleAngle Aesthetic properties for the plot title.
#' @field subtitleColor,subtitleSize,subtitleFontFace,subtitleFontFamily,subtitleAngle Aesthetic properties for the plot subtitle.
#' @field xLabelColor,xLabelSize,xLabelFontFace,xLabelFontFamily,xLabelAngle Aesthetic properties for the plot xLabel.
#' @field yLabelColor,yLabelSize,yLabelFontFace,yLabelFontFamily,yLabelAngle Aesthetic properties for the plot yLabel.
#' @field legendPosition A character string defining the legend position.
#'   Available options can be seen using `tlf::LegendPositions` list.
#' @field legendTitleSize,legendTitleColor,legendTitleFontFamily,legendTitleFontFace,legendTitleAngle,legendCaptionSize Aesthetic properties for the legend title.
#' @field legendCaptionColor,legendCaptionFontFamily,legendCaptionFontFace,legendCaptionAngle Aesthetic properties for the legend caption.
#' @field xAxisTickLabels,xAxisLabelSize,xAxisLabelColor,xAxisLabelFontFamily,xAxisLabelFontFace,xAxisLabelAngle Aesthetic properties for the x-axis label.
#' @field yAxisTickLabels,yAxisLabelSize,yAxisLabelColor,yAxisLabelFontFamily,yAxisLabelFontFace,yAxisLabelAngle Aesthetic properties for the y-axis label.
#' @field xAxisLimits,yAxisLimits A numeric vector of axis limits for the x-and
#'   y-axis, respectively.
#' @field xAxisTicks,yAxisTicks A numeric vector or a function defining where to
#'   position x-and y-axis ticks, respectively.
#' @field xAxisScale,yAxisScale A character string defining axis scale.
#'   Available options can be seen using `tlf::Scaling` list.
#' @field watermarkSize,watermarkColor,watermarkFontFamily,watermarkFontFace,watermarkAngle A character string specifying the aesthetic properties for the watermark.
#' @field plotBackgroundFill,plotBackgroundColor,plotBackgroundSize,plotBackgroundLinetype A character string specifying the aesthetic properties for the plot background.
#' @field plotPanelBackgroundFill,plotPanelBackgroundColor,plotPanelBackgroundSize,plotPanelBackgroundLinetype A character string specifying the aesthetic properties for the plot panel (inside of plot) background.
#' @field xAxisColor,xAxisSize,xAxisLinetype A character string specifying the aesthetic properties for the x-axis.
#' @field yAxisColor,yAxisSize,yAxisLinetype A character string specifying the aesthetic properties for the y-axis.
#' @field xGridColor,xGridSize,xGridLinetype A character string specifying the aesthetic properties for the x-axis grid.
#' @field yGridColor,yGridSize,yGridLinetype A character string specifying the aesthetic properties for the y-axis grid.
#' @field linesColor,linesFill,linesShape,linesSize,linesLinetype,linesAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for lines.
#' @field pointsColor,pointsFill,pointsShape,pointsSize,pointsLinetype,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @field ribbonsColor,ribbonsFill,ribbonsShape,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @field errorbarsColor,errorbarsFill,errorbarsShape,errorbarsSize,errorbarsLinetype,errorbarsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for errorbars.
#' @field plotSaveFileFormat,plotSaveFileWidth,plotSaveFileHeight,plotSaveFileDimensionUnits File format to which the plot needs to be saved, and the units and dimensions for saving the plot.
#'
#' @export
DefaultPlotConfiguration <- R6::R6Class(
  "DefaultPlotConfiguration",
  public = list(
    # units
    xUnit = NULL,
    yUnit = NULL,

    # title
    title = NULL,
    titleColor = NULL,
    titleSize = NULL,
    titleFontFace = NULL,
    titleFontFamily = NULL,
    titleAngle = NULL,

    # subtitle
    subtitle = NULL,
    subtitleColor = NULL,
    subtitleSize = NULL,
    subtitleFontFace = NULL,
    subtitleFontFamily = NULL,
    subtitleAngle = NULL,

    # xLabel
    xLabel = "xValues",
    xLabelColor = NULL,
    xLabelSize = NULL,
    xLabelFontFace = NULL,
    xLabelFontFamily = NULL,
    xLabelAngle = NULL,

    # yLabel
    yLabel = "yValues",
    yLabelColor = NULL,
    yLabelSize = NULL,
    yLabelFontFace = NULL,
    yLabelFontFamily = NULL,
    yLabelAngle = NULL,

    # legend
    legendPosition = "insideTopRight",
    legendTitle = NULL,
    legendTitleSize = 10,
    legendTitleColor = "black",
    legendTitleFontFamily = "",
    legendTitleFontFace = "plain",
    legendTitleAngle = 0,

    # legend caption
    legendCaptionSize = NULL,
    legendCaptionColor = NULL,
    legendCaptionFontFamily = NULL,
    legendCaptionFontFace = NULL,
    legendCaptionAngle = NULL,

    # XAxisConfiguration
    xAxisLimits = NULL,
    xAxisScale = NULL,
    xAxisTicks = NULL,
    xAxisTickLabels = NULL,
    xAxisLabelSize = NULL,
    xAxisLabelColor = NULL,
    xAxisLabelFontFamily = NULL,
    xAxisLabelFontFace = NULL,
    xAxisLabelAngle = NULL,

    # YAxisConfiguration
    yAxisLimits = NULL,
    yAxisScale = NULL,
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

    # plot background
    plotBackgroundFill = NULL,
    plotBackgroundColor = NULL,
    plotBackgroundSize = NULL,
    plotBackgroundLinetype = NULL,

    # plot panel background
    plotPanelBackgroundFill = NULL,
    plotPanelBackgroundColor = NULL,
    plotPanelBackgroundSize = NULL,
    plotPanelBackgroundLinetype = NULL,

    # xAxis
    xAxisColor = "black",
    xAxisSize = 0.5,
    xAxisLinetype = NULL,

    # yAxis
    yAxisColor = NULL,
    yAxisSize = NULL,
    yAxisLinetype = NULL,

    # xGrid
    xGridColor = NULL,
    xGridSize = NULL,
    xGridLinetype = NULL,

    # yGrid
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
    plotSaveFileDimensionUnits = "cm"
  )
)
