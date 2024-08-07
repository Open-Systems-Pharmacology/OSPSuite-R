#' @title Plot configuration for OSP plots
#'
#' @description
#'
#' R6 configuration class defining aesthetic properties of plots that can be
#' created with `plotIndividualTimeProfile()`, `plotPopulationTimeProfile()`,
#' `plotObservedVsSimulated()`, and `plotResidualsVsTime()`.
#'
#' To interactively explore various aesthetic properties and appearance of plots
#' with these properties, you can use the [Shiny app](https://www.open-systems-pharmacology.org/TLF-Library/articles/theme-maker.html) from `{tlf}` package.
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
#' # Specifying tick labels
#'
#' `tlf::TickLabelTransforms` lists of all available tick label transformations.
#' For example, selecting `tlf::TickLabelTransforms$identity` will display tick
#' labels as they are, while selecting `tlf::TickLabelTransforms$log` will
#' display tick labels in logarithmic scale format.
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
#' @field titleColor,titleSize,titleFontFace,titleFontFamily,titleAngle,titleAlign,titleMargin Aesthetic properties for the plot title.
#' @field subtitleColor,subtitleSize,subtitleFontFace,subtitleFontFamily,subtitleAngle,subtitleAlign,subtitleMargin Aesthetic properties for the plot subtitle.
#' @field captionColor,captionSize,captionFontFace,captionFontFamily,captionAngle,captionAlign,captionMargin Aesthetic properties for the plot caption.
#' @field xLabelColor,xLabelSize,xLabelFontFace,xLabelFontFamily,xLabelAngle,xLabelAlign,xLabelMargin Aesthetic properties for the plot xLabel.
#' @field yLabelColor,yLabelSize,yLabelFontFace,yLabelFontFamily,yLabelAngle,yLabelAlign,yLabelMargin Aesthetic properties for the plot yLabel.
#' @field legendPosition A character string defining the legend position.
#'   Available options can be seen using `tlf::LegendPositions` list.
#' @field legendTitleSize,legendTitleColor,legendTitleFontFamily,legendTitleFontFace,legendTitleAngle,legendTitleAlign,legendTitleMargin Aesthetic properties for the legend title.
#' @field legendKeysSize,legendKeysColor,legendKeysFontFamily,legendKeysFontFace,legendKeysAngle,legendKeysAlign,legendKeysMargin Aesthetic properties for the legend caption.
#' @field legendBackgroundColor,legendBackgroundAlpha,legendBorderColor,legendBorderType,legendBorderSize Aesthetic properties for the legend box
#' @field xAxisTicksLabels,xAxisLabelTicksSize,xAxisLabelTicksColor,xAxisLabelTicksFontFamily,xAxisLabelTicksFontFace,xAxisLabelTicksAngle,xAxisLabelTicksAlign,xAxisLabelTicksMargin,xAxisExpand Aesthetic properties for the x-axis label.
#' @field yAxisTicksLabels,yAxisLabelTicksSize,yAxisLabelTicksColor,yAxisLabelTicksFontFamily,yAxisLabelTicksFontFace,yAxisLabelTicksAngle,yAxisLabelTicksAlign,yAxisLabelTicksMargin,yAxisExpand Aesthetic properties for the y-axis label.
#' @field xAxisLimits,yAxisLimits A numeric vector of axis limits for the x-and
#'   y-axis, respectively. This will preserve all data points but zoom in the plot.
#' @field xValuesLimits,yValuesLimits A numeric vector of values limits for the x-and
#'   y-axis, respectively. This will filter out the data points outside the specified ranges before plotting.
#' @field xAxisTicks,yAxisTicks A numeric vector or a function defining where to
#'   position x-and y-axis ticks, respectively.
#' @field xAxisScale,yAxisScale A character string defining axis scale.
#'   Available options can be seen using `tlf::Scaling` list.
#' @field watermarkSize,watermarkColor,watermarkFontFamily,watermarkFontFace,watermarkAngle,watermarkAlign,watermarkMargin A character string specifying the aesthetic properties for the watermark.
#' @field plotBackgroundFill,plotBackgroundColor,plotBackgroundSize,plotBackgroundLinetype A character string specifying the aesthetic properties for the plot background.
#' @field plotPanelBackgroundFill,plotPanelBackgroundColor,plotPanelBackgroundSize,plotPanelBackgroundLinetype A character string specifying the aesthetic properties for the plot panel (inside of plot) background.
#' @field xAxisColor,xAxisSize,xAxisLinetype A character string specifying the aesthetic properties for the x-axis.
#' @field yAxisColor,yAxisSize,yAxisLinetype A character string specifying the aesthetic properties for the y-axis.
#' @field xGridColor,xGridSize,xGridLinetype A character string specifying the aesthetic properties for the x-axis grid.
#' @field yGridColor,yGridSize,yGridLinetype A character string specifying the aesthetic properties for the y-axis grid.
#' @field linesColor,linesSize,linesLinetype,linesAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for lines.
#' @field pointsColor,pointsShape,pointsSize,pointsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for points.
#' @field ribbonsFill,ribbonsSize,ribbonsLinetype,ribbonsAlpha A selection key or values for choice of color, fill, shape, size, linetype, alpha, respectively, for ribbons.
#' @field errorbarsSize,errorbarsLinetype,errorbarsAlpha,errorbarsCapSize A selection key or values for choice of color, fill, shape, size, linetype, alpha, cap width/height, respectively, for error bars.
#' @field displayLLOQ A Boolean controlling display Lower Limit of Quantification lines. Default to True.
#' @field lloqDirection A string controlling how the LLOQ lines are plotted. Can be "vertical", "horizontal" or "both". Default to NULL to respect specific plot configurations.
#' @field foldLinesLegend A Boolean controlling the drawing of the fold lines in the legend. Default to False.
#' @field foldLinesLegendDiagonal A Boolean controlling whether the fold lines legend should be horizontal or diagonal lines.
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
    titleSize = tlf::PlotAnnotationTextSize$plotTitleSize,
    titleFontFace = tlf::FontFaces$plain,
    titleFontFamily = "",
    titleAngle = 0,
    titleAlign = tlf::Alignments$left,
    titleMargin = c(20, 2, 10, 2), # top, right, bottom, left

    # subtitle ------------------------------------

    subtitle = NULL,
    subtitleColor = "black",
    subtitleSize = tlf::PlotAnnotationTextSize$plotSubtitleSize,
    subtitleFontFace = tlf::FontFaces$plain,
    subtitleFontFamily = "",
    subtitleAngle = 0,
    subtitleAlign = tlf::Alignments$left,
    subtitleMargin = c(0, 2, 10, 2),

    # caption ------------------------------------

    caption = NULL,
    captionColor = "black",
    captionSize = tlf::PlotAnnotationTextSize$plotCaptionSize,
    captionFontFace = tlf::FontFaces$plain,
    captionFontFamily = "",
    captionAngle = 0,
    captionAlign = tlf::Alignments$right,
    captionMargin = c(2, 2, 5, 2),

    # xLabel ------------------------------------

    xLabel = NULL,
    xLabelColor = "black",
    xLabelSize = tlf::PlotAnnotationTextSize$plotXLabelSize,
    xLabelFontFace = tlf::FontFaces$plain,
    xLabelFontFamily = "",
    xLabelAngle = 0,
    xLabelAlign = tlf::Alignments$center,
    xLabelMargin = c(10, 2, 5, 2),

    # yLabel ------------------------------------

    yLabel = NULL,
    yLabelColor = "black",
    yLabelSize = tlf::PlotAnnotationTextSize$plotYLabelSize,
    yLabelFontFace = tlf::FontFaces$plain,
    yLabelFontFamily = "",
    yLabelAngle = 90,
    yLabelAlign = tlf::Alignments$center,
    yLabelMargin = c(5, 2, 10, 2),

    # legendTitle ------------------------------------

    legendPosition = NULL,
    legendTitle = NULL,
    legendTitleSize = tlf::PlotAnnotationTextSize$plotLegendTitleSize,
    legendTitleColor = "black",
    legendTitleFontFamily = "",
    legendTitleFontFace = tlf::FontFaces$plain,
    legendTitleAngle = 0,
    legendTitleAlign = tlf::Alignments$left,
    legendTitleMargin = c(2, 2, 2, 2),

    # legendBox ------------------------------------

    legendBackgroundColor = "white",
    legendBackgroundAlpha = 0,
    legendBorderColor = NULL,
    legendBorderType = 1,
    legendBorderSize = NULL,

    # legendKeys ------------------------------------

    legendKeysSize = tlf::PlotAnnotationTextSize$plotLegendCaptionSize,
    legendKeysColor = "black",
    legendKeysFontFamily = "",
    legendKeysFontFace = tlf::FontFaces$plain,
    legendKeysAngle = 0,
    legendKeysAlign = tlf::Alignments$left,
    legendKeysMargin = c(2, 0, 2, 0),

    # XAxisConfiguration ------------------------------------

    xAxisLimits = NULL,
    xValuesLimits = NULL,
    xAxisScale = NULL,
    xAxisTicks = NULL,
    xAxisTicksLabels = tlf::TickLabelTransforms$identity,
    xAxisLabelTicksSize = 8,
    xAxisLabelTicksColor = "black",
    xAxisLabelTicksFontFamily = "",
    xAxisLabelTicksFontFace = tlf::FontFaces$plain,
    xAxisLabelTicksAngle = 0,
    xAxisLabelTicksAlign = tlf::Alignments$center,
    xAxisLabelTicksMargin = c(2, 2, 2, 2),

    # YAxisConfiguration ------------------------------------

    yAxisLimits = NULL,
    yValuesLimits = NULL,
    yAxisScale = NULL,
    yAxisTicks = NULL,
    yAxisTicksLabels = tlf::TickLabelTransforms$identity,
    yAxisLabelTicksSize = 8,
    yAxisLabelTicksColor = "black",
    yAxisLabelTicksFontFamily = "",
    yAxisLabelTicksFontFace = tlf::FontFaces$plain,
    yAxisLabelTicksAngle = 90,
    yAxisLabelTicksAlign = tlf::Alignments$center,
    yAxisLabelTicksMargin = c(2, 2, 2, 2),

    # watermark ------------------------------------

    watermark = NULL,
    watermarkSize = tlf::PlotAnnotationTextSize$plotWatermarkSize,
    watermarkColor = "grey40",
    watermarkFontFamily = "",
    watermarkFontFace = tlf::FontFaces$plain,
    watermarkAngle = 30,
    watermarkAlign = tlf::Alignments$center,
    watermarkMargin = c(1, 1, 1, 1),

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
    xAxisExpand = FALSE,

    # yAxis ------------------------------------

    yAxisColor = "black",
    yAxisSize = 0.5,
    yAxisLinetype = tlf::Linetypes$solid,
    yAxisExpand = FALSE,

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
    pointsColor = tlf::ColorMaps$ospDefault,
    pointsShape = names(tlf::Shapes),
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
    errorbarsCapSize = 4,
    errorbarsLinetype = tlf::Linetypes$solid,
    errorbarsAlpha = 0.75,

    # LLOQ -----------------------------------------
    displayLLOQ = TRUE,
    lloqDirection = NULL,

    # Fold Distance Lines --------------------------
    foldLinesLegend = FALSE,
    foldLinesLegendDiagonal = FALSE
  )
)
