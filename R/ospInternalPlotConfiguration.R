#' @title Convenience class for plot configuration for OSP plots
#'
#' @description
#'
#' An intermediary R6 class that provides a single portal to prepare the
#' `PlotConfiguration` object needed by `{tlf}` plotting functions. The specific
#' type of `PlotConfiguration` objects can access all necessary objects formed
#' from user inputs using this internal object.
#'
#' @field labels `tlf::LabelConfiguration` object defining properties of labels.
#' @field legend `tlf::LegendConfiguration` object defining legend properties.
#' @field xAxis `tlf::XAxisConfiguration` object defining x-axis properties.
#' @field yAxis `tlf::YAxisConfiguration` object defining y-axis properties.
#' @field background `tlf::BackgroundConfiguration` object defining the
#'   configuration of background.
#' @field lines `tlf::ThemeAestheticSelections` object or list defining how
#'   lines are plotted.
#' @field points `tlf::ThemeAestheticSelections` object or list defining how
#'   points are plotted.
#' @field ribbons `tlf::ThemeAestheticSelections` object or list defining
#'   how ribbons are plotted.
#' @field errorbars `tlf::ThemeAestheticSelections` object or list defining
#'   how errorbars are plotted.
#' @field export R6 class `tlf::SaveConfiguration` defining saving properties.
#'
#' @keywords internal
#' @noRd
ospInternalPlotConfiguration <- R6::R6Class(
  "ospInternalPlotConfiguration",
  public = list(
    labels = NULL,
    legend = NULL,
    xAxis = NULL,
    yAxis = NULL,
    background = NULL,
    lines = NULL,
    points = NULL,
    ribbons = NULL,
    errorbars = NULL,
    export = NULL,

    #' @description Create a new `ospInternalPlotConfiguration` object
    #'
    #' @param labels `tlf::LabelConfiguration` object defining properties of labels.
    #' @param legend `tlf::LegendConfiguration` object defining legend properties.
    #' @param xAxis `tlf::XAxisConfiguration` object defining x-axis properties.
    #' @param yAxis `tlf::YAxisConfiguration` object defining y-axis properties.
    #' @param background `tlf::BackgroundConfiguration` object defining the
    #'   configuration of background.
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
                          background = NULL,
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

      # Background configuration
      self$background <- background

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
