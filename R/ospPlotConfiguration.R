#' @title Plot configuration for OSP plots
#' @description R6 class defining the configuration of a `{ospsuite}` plots.
#'
#' @param title character or `Label` object defining plot title
#' @param subtitle character or `Label` object defining plot subtitle
#' @param xlabel character or `Label` object defining plot xlabel
#' @param ylabel character or `Label` object defining plot ylabel
#' @param legend `LegendConfiguration` object defining legend properties
#' @param legendTitle character defining legend title
#' @param legendPosition character defining legend position. Use Enum
#'   `LegendPositions` to get a list of available to legend positions.
#' @param xAxis `XAxisConfiguration` object defining x-axis properties
#' @param yAxis `YAxisConfiguration` object defining y-axis properties
#' @param background `BackgroundConfiguration` object defining background properties
#' @param plotArea `BackgroundElement` object defining properties of plot area
#' @param panelArea `BackgroundElement` object defining properties of panel area
#' @param xGrid `LineElement` object defining properties of x-grid background
#' @param yGrid `LineElement` object defining properties of y-grid background
#' @param watermark `Label` object defining watermark
#' @param export R6 class `SaveConfiguration` defining saving properties
#' @param format character defining the format of the file to be saved
#' @param width numeric values defining the width in `units` of the plot dimensions after saving
#' @param height numeric values defining the height in `units` of the plot dimensions after saving
#' @param units character defining the unit of the saving dimension
#' @param lines `ThemeAestheticSelections` object or list defining how lines are plotted
#' @param points `ThemeAestheticSelections` object or list defining how points are plotted
#' @param ribbons `ThemeAestheticSelections` object or list defining how ribbons are plotted
#' @param errorbars `ThemeAestheticSelections` object or list defining how errorbars are plotted
#'
#' @export
ospPlotConfiguration <- R6::R6Class(
  "ospPlotConfiguration",
  public = list(
    # annotations
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,

    # Legend Configuration
    legend = NULL,

    # X-Axis configuration
    xAxis = NULL,

    # Y-Axis configuration
    yAxis = NULL,

    # Background configuration
    # background = NULL,
    # plotArea = NULL,
    # panelArea = NULL,
    # xGrid = NULL,
    # yGrid = NULL,
    # watermark = NULL,

    # configuration of how objects are plotted
    lines = NULL,
    points = NULL,
    ribbons = NULL,
    errorbars = NULL,

    # Export configuration
    export = NULL,

    #' @description Create a new `ospPlotConfiguration` object
    #'
    #' @return A new `PlotConfiguration` object
    initialize = function(title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL,
                          legend = NULL,
                          xAxis = NULL,
                          yAxis = NULL,
                          # background = NULL,
                          # plotArea = NULL,
                          # panelArea = NULL,
                          # xGrid = NULL,
                          # yGrid = NULL,
                          watermark = NULL,
                          lines = NULL,
                          points = NULL,
                          ribbons = NULL,
                          errorbars = NULL,
                          export = NULL) {
      # Annotations
      self$title <- title
      self$subtitle <- subtitle
      self$xlabel <- xlabel
      self$ylabel <- ylabel

      # Legend Configuration
      self$legend <- legend

      # X-Axis configuration
      self$xAxis <- xAxis

      # Y-Axis configuration
      self$yAxis <- yAxis

      # Background configuration
      # self$background <- background
      # self$plotArea <- plotArea
      # self$panelArea <- panelArea
      # self$xGrid <- xGrid
      # self$yGrid <- yGrid
      # self$watermark <- watermark

      # Configurations for aesthetics
      self$lines <- lines
      self$points <- points
      self$ribbons <- ribbons
      self$errorbars <- errorbars

      # Export configuration
      self$export <- export
    }
  )
)
