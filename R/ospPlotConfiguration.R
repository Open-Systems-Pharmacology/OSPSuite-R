#' @title Plot configuration for OSP plots
#' @description R6 class defining the configuration of a `{ospsuite}` plots.
#'
#' @export
ospPlotConfiguration <- R6::R6Class(
  "ospPlotConfiguration",
  public = list(
    #' @field title A character string providing plot title.
    title = NULL,

    #' @field subtitle A character string providing plot subtitle.
    subtitle = NULL,

    #' @field xlabel A character string providing plot x-axis label.
    xlabel = NULL,

    #' @field ylabel A character string providing plot y-axis label.
    ylabel = NULL,

    #' @field legend `tlf::LegendConfiguration` object defining legend properties.
    legend = NULL,

    #' @field xAxis `tlf::XAxisConfiguration` object defining x-axis properties.
    xAxis = NULL,

    #' @field yAxis `tlf::YAxisConfiguration` object defining y-axis properties.
    yAxis = NULL,

    #' @field watermark `tlf::Label` object defining watermark.
    watermark = NULL,

    #' @field lines `tlf::ThemeAestheticSelections` object or list defining how
    #'   lines are plotted.
    lines = NULL,

    #' @field points `tlf::ThemeAestheticSelections` object or list defining how
    #'   points are plotted.
    points = NULL,

    #' @field ribbons `tlf::ThemeAestheticSelections` object or list defining
    #'   how ribbons are plotted.
    ribbons = NULL,

    #' @field errorbars `tlf::ThemeAestheticSelections` object or list defining
    #'   how errorbars are plotted.
    errorbars = NULL,

    #' @field export R6 class `tlf::SaveConfiguration` defining saving properties.
    export = NULL,

    #' @description Create a new `ospPlotConfiguration` object
    #'
    #' @param title,subtitle,xlabel,ylabel A character string providing plot
    #'   annotations for plot title, subtitle, x-axis label, y-axis label, plot
    #'   legend, watermark, respectively.
    #' @param legend `tlf::LegendConfiguration` object defining legend properties.
    #' @param xAxis `tlf::XAxisConfiguration` object defining x-axis properties.
    #' @param yAxis `tlf::YAxisConfiguration` object defining y-axis properties.
    #' @param watermark `tlf::Label` object defining watermark.
    #' @param lines `tlf::ThemeAestheticSelections` object or list defining how
    #'   lines are plotted.
    #' @param points `tlf::ThemeAestheticSelections` object or list defining how
    #'   points are plotted.
    #' @param ribbons `tlf::ThemeAestheticSelections` object or list defining
    #'   how ribbons are plotted.
    #' @param errorbars `tlf::ThemeAestheticSelections` object or list defining
    #'   how errorbars are plotted.
    #' @param export R6 class `tlf::SaveConfiguration` defining saving
    #'   properties.
    #'
    #' @return A new `PlotConfiguration` object
    initialize = function(title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL,
                          legend = NULL,
                          xAxis = NULL,
                          yAxis = NULL,
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

      # Watermark
      self$watermark <- watermark

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
