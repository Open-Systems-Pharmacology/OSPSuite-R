#' @title Plot configuration for OSP plots
#'
#' @description
#'
#' R6 configuration class defining properties of plots that can be created with
#' `plotIndividualTimeProfile()`, `plotPopulationTimeProfile()`,
#' `plotObservedVsSimulated()`, and `plotResidualsVsTime()`.
#'
#' @details
#'
#' # Specifying aesthetic properties
#'
#' Aesthetic mappings describe how groups are mapped to visual properties
#' (color, shape, size, etc.) of the geometries included in the plot (e.g.
#' point, line, ribbon, etc.).
#'
#' The supported values for each property can be seen using `{tlf}` lists:
#'
#' - color, fill: `tlf::ColorMaps$ospDefault`
#' - shape: `tlf::Shapes`
#' - scaling: `tlf::Scaling`
#' - legend position: `tlf::LegendPositions`
#' - alignments: `tlf::Alignments`
#' - linetype: `tlf::Linetypes`
#'
#' For example, all parameters related to color (`titleColor`,
#' `yAxisLabelTicksColor`, etc.) accept any of the palettes available in
#' ``tlf::ColorMaps` (e.g. `tlf::ColorMaps$ospDefault`).
#'
#' # Specifying units
#'
#' The available units for `x`-and `y`-axes depend on the dimensions of these
#' quantities (`ospsuite::ospDimensions`). Supported units can be seen with
#' `ospsuite::ospUnits`.
#'
#' # Specifying fonts
#'
#' A font is a particular set of glyphs (character shapes), differentiated from
#' other fonts in the same family by additional properties such as stroke
#' weight, slant, relative width, etc.
#'
#' A font face (aka typeface) is the design of lettering, characterized by
#' variations in size, weight (e.g. bold), slope (e.g. italic), width (e.g.
#' condensed), and so on. The available font faces can seen using
#' `tlf::FontFaces` list.
#'
#' A font family is a grouping of fonts defined by shared design styles.
#'
#' The available font families will depend on which fonts have been installed on
#' your computer. This information can be extracted by running the following
#' code:
#'
#' ```
#' library(systemfonts)
#' system_fonts()
#' ```
#'
#' # Saving plot
#'
#' The plots can be saved using `tlf::exportPlot()` function. Once you pass a
#' plot object to this function, it will use the `save*` public members
#' specified in the plot configuration object to save the plot.
#'
#' ```
#' # save plot to an object
#' myPlot <- plotIndividualTimeProfile(myDataComb, myPC)
#' tlf::exportPlot(myPlot)
#' ```
#' The plot will be saved in the current working directory. The default
#' dimensions with which the plot will be saved are 16 cm x 9 cm.
#'
#' The available file formats to save the plot are: "eps", "ps", "tex" (pictex),
#' "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf".
#'
#' @field xUnit,yUnit Units for quantities plotted on x- and y-axes, respectively.
#' @field title,subtitle,caption,xLabel,yLabel,legendTitle,watermark A character
#'   string providing plot annotations for plot title, subtitle, caption, x-axis
#'   label, y-axis label, plot legend, watermark, respectively.
#' @field titleColor,titleSize,titleFontFace,titleFontFamily,titleAngle,titleAlign Aesthetic properties for the plot title.
#' @field subtitleColor,subtitleSize,subtitleFontFace,subtitleFontFamily,subtitleAngle,subtitleAlign Aesthetic properties for the plot subtitle.
#' @field captionColor,captionSize,captionFontFace,captionFontFamily,captionAngle,captionAlign Aesthetic properties for the plot caption.
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
#' @field pointsColor,pointsShape,pointsSize,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @field ribbonsFill,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @field errorbarsSize,errorbarsLinetype,errorbarsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for errorbars.
#' @field saveFileName,saveFileFormat,saveFileWidth,saveFileHeight,saveFileDimensionUnits,saveFileDpi File name (without extension) format to which the plot needs to be saved, and the specifications for saving the plot.
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
    # units ------------------------------------

    xUnit = NULL,
    yUnit = NULL,

    # title ------------------------------------

    title = NULL,
    titleColor = "black",
    titleSize = 12,
    titleFontFace = tlf::FontFaces$plain,
    titleFontFamily = "",
    titleAngle = 0,
    titleAlign = tlf::Alignments$left,

    # subtitle ------------------------------------

    subtitle = NULL,
    subtitleColor = "black",
    subtitleSize = 10,
    subtitleFontFace = tlf::FontFaces$plain,
    subtitleFontFamily = "",
    subtitleAngle = 0,
    subtitleAlign = tlf::Alignments$left,

    # caption ------------------------------------

    caption = NULL,
    captionColor = "black",
    captionSize = 8,
    captionFontFace = tlf::FontFaces$plain,
    captionFontFamily = "",
    captionAngle = 0,
    captionAlign = tlf::Alignments$right,

    # xLabel ------------------------------------

    xLabel = NULL,
    xLabelColor = "black",
    xLabelSize = 10,
    xLabelFontFace = tlf::FontFaces$plain,
    xLabelFontFamily = "",
    xLabelAngle = 0,
    xLabelAlign = tlf::Alignments$center,

    # yLabel ------------------------------------

    yLabel = NULL,
    yLabelColor = "black",
    yLabelSize = 10,
    yLabelFontFace = tlf::FontFaces$plain,
    yLabelFontFamily = "",
    yLabelAngle = 90,
    yLabelAlign = tlf::Alignments$center,

    # legend ------------------------------------

    legendPosition = NULL,
    legendTitle = NULL,
    legendTitleSize = 10,
    legendTitleColor = "black",
    legendTitleFontFamily = "",
    legendTitleFontFace = tlf::FontFaces$plain,
    legendTitleAngle = 0,
    legendTitleAlign = tlf::Alignments$center,

    # legendCaption ------------------------------------

    legendCaptionSize = 10,
    legendCaptionColor = "black",
    legendCaptionFontFamily = "",
    legendCaptionFontFace = tlf::FontFaces$plain,
    legendCaptionAngle = 0,
    legendCaptionAlign = tlf::Alignments$center,

    # XAxisConfiguration ------------------------------------

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

    # YAxisConfiguration ------------------------------------

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

    # watermark ------------------------------------

    watermark = NULL,
    watermarkSize = 20,
    watermarkColor = "grey40",
    watermarkFontFamily = "",
    watermarkFontFace = tlf::FontFaces$plain,
    watermarkAngle = 30,
    watermarkAlign = tlf::Alignments$center,

    # plotBackground ------------------------------------

    plotBackgroundFill = "white",
    plotBackgroundColor = "black",
    plotBackgroundSize = 0.5,
    plotBackgroundLinetype = tlf::Linetypes$blank,

    # plotPanelBackground ------------------------------------

    plotPanelBackgroundFill = "white",
    plotPanelBackgroundColor = "black",
    plotPanelBackgroundSize = 0.5,
    plotPanelBackgroundLinetype = tlf::Linetypes$solid,

    # xAxis ------------------------------------

    xAxisColor = "black",
    xAxisSize = 0.5,
    xAxisLinetype = tlf::Linetypes$solid,

    # yAxis ------------------------------------

    yAxisColor = "black",
    yAxisSize = 0.5,
    yAxisLinetype = tlf::Linetypes$solid,

    # xGrid ------------------------------------

    xGridColor = "grey",
    xGridSize = 0.25,
    xGridLinetype = tlf::Linetypes$blank,

    # yGrid ------------------------------------

    yGridColor = "grey",
    yGridSize = 0.25,
    yGridLinetype = tlf::Linetypes$blank,

    # lines ------------------------------------

    # There is no `linesFill` because it doesn't make sense to "fill" a line
    # with color. There is already `linesColor` for that.
    linesColor = NULL,
    linesSize = 1,
    linesLinetype = NULL,
    linesAlpha = 0.75,

    # points ------------------------------------

    # There is no `pointsFill` because it doesn't make sense to "fill" a line
    # with color. There is already `pointsColor` for that.
    pointsColor = NULL,
    pointsShape = NULL,
    pointsSize = 3,
    pointsAlpha = 0.75,

    # ribbons ------------------------------------

    # There is no `ribbonsColor` because it doesn't make sense to color a
    # ribbon. There is already `ribbonsFill` for that.
    ribbonsFill = tlf::ColorMaps$ospDefault,
    ribbonsSize = 1,
    ribbonsLinetype = names(tlf::Linetypes),
    ribbonsAlpha = 0.50,

    # errorbars ------------------------------------

    # Color and fill are taken from point mapping, therefore no
    # `errorbarsColor`, `errorbarsFill` parameters
    errorbarsSize = 1,
    errorbarsLinetype = tlf::Linetypes$solid,
    errorbarsAlpha = 0.75,

    # export ------------------------------------

    saveFileName = "figure",
    saveFileFormat = "png",
    saveFileWidth = 16,
    saveFileHeight = 9,
    saveFileDimensionUnits = "cm",
    saveFileDpi = 300
  )
)
