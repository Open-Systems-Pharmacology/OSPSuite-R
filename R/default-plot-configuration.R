#' @title Plot configuration for OSP plots
#'
#' @description
#'
#' R6 configuration class defining properties of plots that can be created with
#' `plotIndividualTimeProfile()`, `plotPopulationTimeProfile()`,
#' `plotObservedVsSimulated()`, and `plotResidualsVsTime()`.
#'
#' The following sections provide more details on how to customize it further.
#'
#' # Specifying aesthetic properties
#'
#' Aesthetic mappings describe how groups are mapped to visual properties
#' (color, shape, size, etc.) of the geometries included in the plot (e.g.
#' point, line, ribbon, etc.).
#'
#' The supported values for each property can be seen using `{tlf}` lists:
#'
#' - color, fill: `tlf::ColorMaps`
#' - shape: `tlf::Shapes`
#' - legend position: `tlf::LegendPositions`
#' - alignments: `tlf::Alignments`
#' - linetype: `tlf::Linetypes`
#'
#' For example, all parameters related to color (`titleColor`,
#' `yAxisLabelTicksColor`, etc.) accept any of the palettes available in
#' `tlf::ColorMaps` (e.g. `tlf::ColorMaps$ospDefault`).
#'
#' Note that these are named lists, and, therefore, if you want to assign a
#' specific element from a list to an object's public field, you will have to
#' extract that element first.
#'
#' For example, if you want to specify that the
#' legend position should be outside the plot to the left and at bottom, you
#' will have to do the following:
#'
#' ```r
#' myPlotConfiguration <- DefaultPlotConfiguration$new()
#' myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideBottomLeft
#' ```
#'
#' Of course, the extracted element doesn't have to be a single value, and can
#' also be an atomic vector. For example, if you want to assign a different line
#' type to each group in a profile plot, you will have to assign a vector of
#' line types.
#'
#' ```r
#' myPlotConfiguration <- DefaultPlotConfiguration$new()
#' myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)
#' ```
#'
#' If there are more number of elements in the vector than the number of groups,
#' the additional elements will be ignored.
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
#' ```r
#' # install.packages("systemfonts")
#' library(systemfonts)
#' system_fonts()
#' ```
#'
#' # Specifying scaling
#'
#' Transformations for both x- and y-axes can be (independently) specified. The
#' default is linear for both axes.
#'
#' The available transformations can be seen in the `tlf::Scaling` list.
#'
#' # Saving plot
#'
#' By default, the plots will be shown in plot pane of your IDE, but the plots
#' can also be saved to a file using the `ggplot2::ggsave()` function.
#'
#' ```r
#' myPlot <- plotIndividualTimeProfile(myDataComb, myPC)
#' ggplot2::ggsave(filename = "plot_1.png", plot = myPlot)
#' ```
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
    titleSize = PlotAnnotationTextSize$plotTitleSize,
    titleFontFace = tlf::FontFaces$plain,
    titleFontFamily = "",
    titleAngle = 0,
    titleAlign = tlf::Alignments$left,

    # subtitle ------------------------------------

    subtitle = NULL,
    subtitleColor = "black",
    subtitleSize = PlotAnnotationTextSize$plotSubtitleSize,
    subtitleFontFace = tlf::FontFaces$plain,
    subtitleFontFamily = "",
    subtitleAngle = 0,
    subtitleAlign = tlf::Alignments$left,

    # caption ------------------------------------

    caption = NULL,
    captionColor = "black",
    captionSize = PlotAnnotationTextSize$plotCaptionSize,
    captionFontFace = tlf::FontFaces$plain,
    captionFontFamily = "",
    captionAngle = 0,
    captionAlign = tlf::Alignments$right,

    # xLabel ------------------------------------

    xLabel = NULL,
    xLabelColor = "black",
    xLabelSize = PlotAnnotationTextSize$plotXLabelSize,
    xLabelFontFace = tlf::FontFaces$plain,
    xLabelFontFamily = "",
    xLabelAngle = 0,
    xLabelAlign = tlf::Alignments$center,

    # yLabel ------------------------------------

    yLabel = NULL,
    yLabelColor = "black",
    yLabelSize = PlotAnnotationTextSize$plotYLabelSize,
    yLabelFontFace = tlf::FontFaces$plain,
    yLabelFontFamily = "",
    yLabelAngle = 90,
    yLabelAlign = tlf::Alignments$center,

    # legend ------------------------------------

    legendPosition = NULL,
    legendTitle = NULL,
    legendTitleSize = PlotAnnotationTextSize$plotLegendTitleSize,
    legendTitleColor = "black",
    legendTitleFontFamily = "",
    legendTitleFontFace = tlf::FontFaces$plain,
    legendTitleAngle = 0,
    legendTitleAlign = tlf::Alignments$center,

    # legendCaption ------------------------------------

    legendCaptionSize = PlotAnnotationTextSize$plotLegendCaptionSize,
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
    errorbarsAlpha = 0.75
  )
)
