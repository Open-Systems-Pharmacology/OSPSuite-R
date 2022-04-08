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
#' @field title,subtitle,xlabel,ylabel,legendTitle,watermark A character string
#'   providing plot annotations for plot title, subtitle, x-axis label, y-axis
#'   label, plot legend, watermark, respectively.
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
ospPlotConfiguration <- R6::R6Class(
  "ospPlotConfiguration",
  public = list(
    # labels
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,

    # legend
    legendPosition = NULL,
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
    plotSaveFileFormat = NULL,
    plotSaveFileWidth = NULL,
    plotSaveFileHeight = NULL,
    plotSaveFileDimensionUnits = NULL,

    #' @description Create a new `ospInternalPlotConfiguration` object
    #'
    #' @param title,subtitle,xlabel,ylabel,legendTitle,watermark A character string
    #'   providing plot annotations for plot title, subtitle, x-axis label, y-axis
    #'   label, plot legend, watermark, respectively.
    #' @param legendPosition A character string defining the legend position.
    #'   Available options can be seen using `tlf::LegendPositions` list.
    #' @param legendTitleSize,legendTitleColor,legendTitleFontFamily,legendTitleFontFace,legendTitleAngle,legendCaptionSize Aesthetic properties for the legend title.
    #' @param legendCaptionColor,legendCaptionFontFamily,legendCaptionFontFace,legendCaptionAngle Aesthetic properties for the legend caption.
    #' @param xAxisTickLabels,xAxisLabelSize,xAxisLabelColor,xAxisLabelFontFamily,xAxisLabelFontFace,xAxisLabelAngle Aesthetic properties for the x-axis label.
    #' @param yAxisTickLabels,yAxisLabelSize,yAxisLabelColor,yAxisLabelFontFamily,yAxisLabelFontFace,yAxisLabelAngle Aesthetic properties for the y-axis label.
    #' @param xAxisLimits,yAxisLimits A numeric vector of axis limits for the x-and
    #'   y-axis, respectively.
    #' @param xAxisTicks,yAxisTicks A numeric vector or a function defining where to
    #'   position x-and y-axis ticks, respectively.
    #' @param xAxisScale,yAxisScale A character string defining axis scale.
    #'   Available options can be seen using `tlf::Scaling` list.
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
    #' @return A new `ospPlotConfiguration` object
    initialize = function( # labels
                          title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL,
                          # legend
                          legendPosition = NULL,
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

      # labels
      self$title <- title
      self$subtitle <- subtitle
      self$xlabel <- xlabel
      self$ylabel <- ylabel

      # legend
      self$legendPosition <- legendPosition
      self$legendTitle <- legendTitle
      self$legendTitleSize <- legendTitleSize
      self$legendTitleColor <- legendTitleColor
      self$legendTitleFontFamily <- legendTitleFontFamily
      self$legendTitleFontFace <- legendTitleFontFace
      self$legendTitleAngle <- legendTitleAngle
      self$legendCaptionSize <- legendCaptionSize
      self$legendCaptionColor <- legendCaptionColor
      self$legendCaptionFontFamily <- legendCaptionFontFamily
      self$legendCaptionFontFace <- legendCaptionFontFace
      self$legendCaptionAngle <- legendCaptionAngle

      # XAxisConfiguration
      self$xAxisLimits <- xAxisLimits
      self$xAxisScale <- xAxisScale
      self$xAxisTicks <- xAxisTicks
      self$xAxisTickLabels <- xAxisTickLabels
      self$xAxisLabelSize <- xAxisLabelSize
      self$xAxisLabelColor <- xAxisLabelColor
      self$xAxisLabelFontFamily <- xAxisLabelFontFamily
      self$xAxisLabelFontFace <- xAxisLabelFontFace
      self$xAxisLabelAngle <- xAxisLabelAngle

      # YAxisConfiguration
      self$yAxisLimits <- yAxisLimits
      self$yAxisScale <- yAxisScale
      self$yAxisTicks <- yAxisTicks
      self$yAxisTickLabels <- yAxisTickLabels
      self$yAxisLabelSize <- yAxisLabelSize
      self$yAxisLabelColor <- yAxisLabelColor
      self$yAxisLabelFontFamily <- yAxisLabelFontFamily
      self$yAxisLabelFontFace <- yAxisLabelFontFace
      self$yAxisLabelAngle <- yAxisLabelAngle

      # background configuration
      self$watermark <- watermark
      self$watermarkSize <- watermarkSize
      self$watermarkColor <- watermarkColor
      self$watermarkFontFamily <- watermarkFontFamily
      self$watermarkFontFace <- watermarkFontFace
      self$watermarkAngle <- watermarkAngle
      self$plotBackgroundFill <- plotBackgroundFill
      self$plotBackgroundColor <- plotBackgroundColor
      self$plotBackgroundSize <- plotBackgroundSize
      self$plotBackgroundLinetype <- plotBackgroundLinetype
      self$plotPanelBackgroundFill <- plotPanelBackgroundFill
      self$plotPanelBackgroundColor <- plotPanelBackgroundColor
      self$plotPanelBackgroundSize <- plotPanelBackgroundSize
      self$plotPanelBackgroundLinetype <- plotPanelBackgroundLinetype
      self$xAxisColor <- xAxisColor
      self$xAxisSize <- xAxisSize
      self$xAxisLinetype <- xAxisLinetype
      self$yAxisColor <- yAxisColor
      self$yAxisSize <- yAxisSize
      self$yAxisLinetype <- yAxisLinetype
      self$xGridColor <- xGridColor
      self$xGridSize <- xGridSize
      self$xGridLinetype <- xGridLinetype
      self$yGridColor <- yGridColor
      self$yGridSize <- yGridSize
      self$yGridLinetype <- yGridLinetype

      # lines
      self$linesColor <- linesColor
      self$linesFill <- linesFill
      self$linesShape <- linesShape
      self$linesSize <- linesSize
      self$linesLinetype <- linesLinetype
      self$linesAlpha <- linesAlpha

      # points
      self$pointsColor <- pointsColor
      self$pointsFill <- pointsFill
      self$pointsShape <- pointsShape
      self$pointsSize <- pointsSize
      self$pointsLinetype <- pointsLinetype
      self$pointsAlpha <- pointsAlpha

      # ribbons
      self$ribbonsColor <- ribbonsColor
      self$ribbonsFill <- ribbonsFill
      self$ribbonsShape <- ribbonsShape
      self$ribbonsSize <- ribbonsSize
      self$ribbonsLinetype <- ribbonsLinetype
      self$ribbonsAlpha <- ribbonsAlpha

      # errorbars
      self$errorbarsColor <- errorbarsColor
      self$errorbarsFill <- errorbarsFill
      self$errorbarsShape <- errorbarsShape
      self$errorbarsSize <- errorbarsSize
      self$errorbarsLinetype <- errorbarsLinetype
      self$errorbarsAlpha <- errorbarsAlpha

      # export
      self$plotSaveFileFormat <- plotSaveFileFormat
      self$plotSaveFileWidth <- plotSaveFileWidth
      self$plotSaveFileHeight <- plotSaveFileHeight
      self$plotSaveFileDimensionUnits <- plotSaveFileDimensionUnits
    }
  )
)
