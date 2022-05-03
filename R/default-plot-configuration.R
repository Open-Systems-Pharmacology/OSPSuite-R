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
#' @details
#'
#' Aesthetic mappings describe how variables are mapped to visual properties of
#' geometric layers. In the current class, the default values for these
#' properties are derived from `{tlf}` lists:
#'
#' - color, fill: `tlf::ColorMaps$ospDefault`
#' - shape: `tlf::Shapes`
#' - scaling: `tlf::Scaling`
#' - legend position: `tlf::LegendPositions`
#' - alignments: `tlf::Alignments`
#' - font face: `tlf::FontFaces`
#' - linetype: `tlf::Linetypes`
#'
#' The available units for `x`-and `y`-axes variables depend on the dimensions
#' of these quantities (`ospsuite::ospDimensions`). Available options can be
#' seen with `ospsuite::ospUnits`.
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
#' @field linesColor,linesSize,linesLinetype,linesAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for lines.
#' @field pointsColor,pointsFill,pointsShape,pointsSize,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @field ribbonsColor,ribbonsFill,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @field errorbarsSize,errorbarsLinetype,errorbarsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for errorbars.
#' @field plotSaveFileName,plotSaveFileFormat,plotSaveFileWidth,plotSaveFileHeight,plotSaveFileDimensionUnits,plotSaveFileDpi File name (without extension) format to which the plot needs to be saved, and the specifications for saving the plot.
#'
#' @examples
#'
#' # Create a new instance of this class
#' myPlotConfiguration <- DefaultPlotConfiguration$new()
#'
#' # Change defaults
#' myPlotConfiguration$title <- "My Plot Title"
#' myPlotConfiguration$pointsSize <- 2.5
#' myPlotConfiguration$legendTitle <- "My Legend Title"
#'
#' # Checking new values
#' myPlotConfiguration$pointsSize
#'
#' # To check all default values, you can print the object
#' myPlotConfiguration
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
    titleFontFace = tlf::FontFaces$plain,
    titleFontFamily = "",
    titleAngle = 0,
    titleAlign = tlf::Alignments$left,

    # subtitle
    subtitle = NULL,
    subtitleColor = "black",
    subtitleSize = 10,
    subtitleFontFace = tlf::FontFaces$plain,
    subtitleFontFamily = "",
    subtitleAngle = 0,
    subtitleAlign = tlf::Alignments$left,

    # xLabel
    xLabel = NULL,
    xLabelColor = "black",
    xLabelSize = 10,
    xLabelFontFace = tlf::FontFaces$plain,
    xLabelFontFamily = "",
    xLabelAngle = 0,
    xLabelAlign = tlf::Alignments$center,

    # yLabel
    yLabel = NULL,
    yLabelColor = "black",
    yLabelSize = 10,
    yLabelFontFace = tlf::FontFaces$plain,
    yLabelFontFamily = "",
    yLabelAngle = 90,
    yLabelAlign = tlf::Alignments$center,

    # legend
    legendPosition = tlf::LegendPositions$insideTopRight,
    legendTitle = NULL,
    legendTitleSize = 10,
    legendTitleColor = "black",
    legendTitleFontFamily = "",
    legendTitleFontFace = tlf::FontFaces$plain,
    legendTitleAngle = 0,
    legendTitleAlign = tlf::Alignments$center,

    # legend caption
    legendCaptionSize = 10,
    legendCaptionColor = "black",
    legendCaptionFontFamily = "",
    legendCaptionFontFace = tlf::FontFaces$plain,
    legendCaptionAngle = 0,
    legendCaptionAlign = tlf::Alignments$center,

    # XAxisConfiguration
    xAxisLimits = NULL,
    xAxisScale = tlf::Scaling$lin,
    xAxisTicks = NULL,
    xAxisTicksLabels = NULL,
    xAxisLabelTicksSize = NULL,
    xAxisLabelTicksColor = "black",
    xAxisLabelTicksFontFamily = "",
    xAxisLabelTicksFontFace = tlf::FontFaces$plain,
    xAxisLabelTicksAngle = 0,
    xAxisLabelTicksAlign = tlf::Alignments$center,

    # YAxisConfiguration
    yAxisLimits = NULL,
    yAxisScale = tlf::Scaling$lin,
    yAxisTicks = NULL,
    yAxisTicksLabels = NULL,
    yAxisLabelTicksSize = NULL,
    yAxisLabelTicksColor = "black",
    yAxisLabelTicksFontFamily = "",
    yAxisLabelTicksFontFace = tlf::FontFaces$plain,
    yAxisLabelTicksAngle = 90,
    yAxisLabelTicksAlign = tlf::Alignments$center,

    # watermark
    watermark = NULL,
    watermarkSize = 20,
    watermarkColor = "grey40",
    watermarkFontFamily = "",
    watermarkFontFace = tlf::FontFaces$plain,
    watermarkAngle = 30,
    watermarkAlign = tlf::Alignments$center,

    # plot background
    plotBackgroundFill = "white",
    plotBackgroundColor = "black",
    plotBackgroundSize = 0.5,
    plotBackgroundLinetype = tlf::Linetypes$blank,

    # plot panel background
    plotPanelBackgroundFill = "white",
    plotPanelBackgroundColor = "black",
    plotPanelBackgroundSize = 0.5,
    plotPanelBackgroundLinetype = tlf::Linetypes$solid,

    # xAxis
    xAxisColor = "black",
    xAxisSize = 0.5,
    xAxisLinetype = tlf::Linetypes$solid,

    # yAxis
    yAxisColor = "black",
    yAxisSize = 0.5,
    yAxisLinetype = tlf::Linetypes$solid,

    # xGrid
    xGridColor = "grey",
    xGridSize = 0.25,
    xGridLinetype = tlf::Linetypes$blank,

    # yGrid
    yGridColor = "grey",
    yGridSize = 0.25,
    yGridLinetype = tlf::Linetypes$blank,

    # lines
    # There is no `linesFill` because it doesn't make sense to "fill" a line
    # with color. There is already `linesColor` for that.
    linesColor = tlf::ColorMaps$ospDefault,
    linesSize = 1,
    linesLinetype = tlf::Linetypes$dashed,
    linesAlpha = 0.75,

    # points
    pointsColor = tlf::ColorMaps$ospDefault,
    pointsFill = tlf::ColorMaps$ospDefault,
    pointsShape = names(tlf::Shapes),
    pointsSize = 3,
    pointsAlpha = 0.75,

    # ribbons
    ribbonsColor = tlf::ColorMaps$ospDefault, # colors for edgelines of a ribbon
    ribbonsFill = tlf::ColorMaps$ospDefault,
    ribbonsSize = 1,
    ribbonsLinetype = tlf::Linetypes$solid,
    ribbonsAlpha = 0.75,

    # errorbars
    # Color and fill are taken from point mapping, therefore no
    # `errorbarsColor`, `errorbarsFill` parameters
    errorbarsSize = 1,
    errorbarsLinetype = tlf::Linetypes$solid,
    errorbarsAlpha = 0.5,

    # export
    plotSaveFileName = "figure",
    plotSaveFileFormat = "png",
    plotSaveFileWidth = 16,
    plotSaveFileHeight = 9,
    plotSaveFileDimensionUnits = "cm",
    plotSaveFileDpi = 300
  )
)
