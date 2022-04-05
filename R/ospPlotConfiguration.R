#' @title Plot configuration for OSP plots
#' @description R6 class defining the configuration of a `{ospsuite}` plots.
#'
#' @export
ospPlotConfiguration <- R6::R6Class(
  "ospPlotConfiguration",
  public = list(
    #' @field labels `tlf::LabelConfiguration` object defining properties of labels.
    labels = NULL,

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
    #' @param labels `tlf::LabelConfiguration` object defining properties of labels.
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
    initialize = function(labels = NULL,
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
      self$labels <- labels

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
