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
#' @field titleColor,titleSize,titleFontFace,titleFontFamily,titleAngle,titleAlign Aesthetic properties for the plot title.
#' @field subtitleColor,subtitleSize,subtitleFontFace,subtitleFontFamily,subtitleAngle,subtitleAlign Aesthetic properties for the plot subtitle.
#' @field xLabelColor,xLabelSize,xLabelFontFace,xLabelFontFamily,xLabelAngle,xLabelAlign Aesthetic properties for the plot xLabel.
#' @field yLabelColor,yLabelSize,yLabelFontFace,yLabelFontFamily,yLabelAngle,yLabelAlign Aesthetic properties for the plot yLabel.
#' @field legendPosition A character string defining the legend position.
#'   Available options can be seen using `tlf::LegendPositions` list.
#' @field legendTitleSize,legendTitleColor,legendTitleFontFamily,legendTitleFontFace,legendTitleAngle,legendTitleAlign Aesthetic properties for the legend title.
#' @field legendCaptionSize,legendCaptionColor,legendCaptionFontFamily,legendCaptionFontFace,legendCaptionAngle,legendCaptionAlign Aesthetic properties for the legend caption.
#' @field xAxisTicksLabels,xAxisLabelTicksSize,xAxisLabelTicksColor,xAxisLabelTicksFontFamily,xAxisLabelTicksFontFace,xAxisLabelTicksAngle,xAxisLabelTicksAlign Aesthetic properties for the x-axis label.
#' @field yAxisTicksLabels,yAxisLabelTicksSize,yAxisLabelTicksColor,yAxisLabelTicksFontFamily,yAxisLabelTicksFontFace,yAxisLabelTicksAngle,yAxisLabelTicksAlign Aesthetic properties for the y-axis label.
#' @field xAxisLimits,yAxisLimits A numeric vector of axis limits for the x-and
#'   y-axis, respectively.
#' @field xAxisTicks,yAxisTicks A numeric vector or a function defining where to
#'   position x-and y-axis ticks, respectively.
#' @field xAxisScale,yAxisScale A character string defining axis scale.
#'   Available options can be seen using `tlf::Scaling` list.
#' @field watermarkSize,watermarkColor,watermarkFontFamily,watermarkFontFace,watermarkAngle,watermarkAlign A character string specifying the aesthetic properties for the watermark.
#' @field plotBackgroundFill,plotBackgroundColor,plotBackgroundSize,plotBackgroundLinetype A character string specifying the aesthetic properties for the plot background.
#' @field plotPanelBackgroundFill,plotPanelBackgroundColor,plotPanelBackgroundSize,plotPanelBackgroundLinetype A character string specifying the aesthetic properties for the plot panel (inside of plot) background.
#' @field xAxisColor,xAxisSize,xAxisLinetype A character string specifying the aesthetic properties for the x-axis.
#' @field yAxisColor,yAxisSize,yAxisLinetype A character string specifying the aesthetic properties for the y-axis.
#' @field xGridColor,xGridSize,xGridLinetype A character string specifying the aesthetic properties for the x-axis grid.
#' @field yGridColor,yGridSize,yGridLinetype A character string specifying the aesthetic properties for the y-axis grid.
#' @field linesColor,linesFill,linesSize,linesLinetype,linesAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for lines.
#' @field pointsColor,pointsFill,pointsShape,pointsSize,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @field ribbonsColor,ribbonsFill,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @field errorbarsSize,errorbarsLinetype,errorbarsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for errorbars.
#' @field plotSaveFileName,plotSaveFileFormat,plotSaveFileWidth,plotSaveFileHeight,plotSaveFileDimensionUnits,plotSaveFileDpi File name (without extension) format to which the plot needs to be saved, and the specifications for saving the plot.
#'
#' @examples
#'
#' if (requireNamespace("tlf")) {
#'
#'   # Create a new instance of this class
#'   myPlotConfiguration <- DefaultPlotConfiguration$new()
#'
#'   # Change defaults
#'   myPlotConfiguration$title <- "My Plot Title"
#'   myPlotConfiguration$pointsSize <- 2.5
#'   myPlotConfiguration$legendTitle <- "My Legend Title"
#'
#'   # Checking new values
#'   myPlotConfiguration$pointsSize
#'
#'   # To check all default values, you can print the object
#'   myPlotConfiguration
#' }
#'
#' @family plotting
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
    titleColor = "black",
    titleSize = 12,
    titleFontFace = "plain",
    titleFontFamily = "",
    titleAngle = 0,
    titleAlign = "left",

    # subtitle
    subtitle = NULL,
    subtitleColor = "black",
    subtitleSize = 10,
    subtitleFontFace = "plain",
    subtitleFontFamily = "",
    subtitleAngle = 0,
    subtitleAlign = "left",

    # xLabel
    xLabel = NULL,
    xLabelColor = "black",
    xLabelSize = 10,
    xLabelFontFace = "plain",
    xLabelFontFamily = "",
    xLabelAngle = 0,
    xLabelAlign = "center",

    # yLabel
    yLabel = NULL,
    yLabelColor = "black",
    yLabelSize = 10,
    yLabelFontFace = "plain",
    yLabelFontFamily = "",
    yLabelAngle = 90,
    yLabelAlign = "center",

    # legend
    legendPosition = "insideTopRight",
    legendTitle = NULL,
    legendTitleSize = 10,
    legendTitleColor = "black",
    legendTitleFontFamily = "",
    legendTitleFontFace = "plain",
    legendTitleAngle = 0,
    legendTitleAlign = "center",

    # legend caption
    legendCaptionSize = 10,
    legendCaptionColor = "black",
    legendCaptionFontFamily = "",
    legendCaptionFontFace = "plain",
    legendCaptionAngle = 0,
    legendCaptionAlign = "center",

    # XAxisConfiguration
    xAxisLimits = NULL,
    xAxisScale = tlf::Scaling$lin,
    xAxisTicks = NULL,
    xAxisTicksLabels = NULL,
    xAxisLabelTicksSize = NULL,
    xAxisLabelTicksColor = "black",
    xAxisLabelTicksFontFamily = "",
    xAxisLabelTicksFontFace = "plain",
    xAxisLabelTicksAngle = 0,
    xAxisLabelTicksAlign = "center",

    # YAxisConfiguration
    yAxisLimits = NULL,
    yAxisScale = tlf::Scaling$lin,
    yAxisTicks = NULL,
    yAxisTicksLabels = NULL,
    yAxisLabelTicksSize = NULL,
    yAxisLabelTicksColor = "black",
    yAxisLabelTicksFontFamily = "",
    yAxisLabelTicksFontFace = "plain",
    yAxisLabelTicksAngle = 90,
    yAxisLabelTicksAlign = "center",

    # watermark
    watermark = NULL,
    watermarkSize = 20,
    watermarkColor = "grey40",
    watermarkFontFamily = "",
    watermarkFontFace = "plain",
    watermarkAngle = 30,
    watermarkAlign = "center",

    # plot background
    plotBackgroundFill = "white",
    plotBackgroundColor = "black",
    plotBackgroundSize = 0.5,
    plotBackgroundLinetype = "blank",

    # plot panel background
    plotPanelBackgroundFill = "white",
    plotPanelBackgroundColor = "black",
    plotPanelBackgroundSize = 0.5,
    plotPanelBackgroundLinetype = "solid",

    # xAxis
    xAxisColor = "black",
    xAxisSize = 0.5,
    xAxisLinetype = "solid",

    # yAxis
    yAxisColor = "black",
    yAxisSize = 0.5,
    yAxisLinetype = "solid",

    # xGrid
    xGridColor = "grey",
    xGridSize = 0.25,
    xGridLinetype = "blank",

    # yGrid
    yGridColor = "grey",
    yGridSize = 0.25,
    yGridLinetype = "blank",

    # lines
    linesColor = tlf::ColorMaps$ospDefault,
    linesFill = tlf::ColorMaps$ospDefault,
    linesSize = 1,
    linesLinetype = "dashed",
    linesAlpha = 0.75,

    # points
    pointsColor = tlf::ColorMaps$ospDefault,
    pointsFill = tlf::ColorMaps$ospDefault,
    pointsShape = names(tlf::Shapes),
    pointsSize = 3,
    pointsAlpha = 0.75,

    # ribbons
    ribbonsColor = tlf::ColorMaps$ospDefault,
    ribbonsFill = tlf::ColorMaps$ospDefault,
    ribbonsSize = 1,
    ribbonsLinetype = "solid",
    ribbonsAlpha = 0.75,

    # errorbars
    # Color and fill are taken from point mapping, therefore no
    # `errorbarsColor`, `errorbarsFill` parameters
    errorbarsSize = 1,
    errorbarsLinetype = "solid",
    errorbarsAlpha = 0.75,

    # export
    plotSaveFileName = "figure",
    plotSaveFileFormat = "png",
    plotSaveFileWidth = 16,
    plotSaveFileHeight = 9,
    plotSaveFileDimensionUnits = "cm",
    plotSaveFileDpi = 300
  )
)
