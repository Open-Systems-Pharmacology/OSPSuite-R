#' @title Convenience class for plot configuration for OSP plots
#'
#' @description
#'
#' R6 class defining the labels for `{ospsuite}` plots.
#'
#' Note that the values this objects contains are of general-purpose utility. In
#' other words, the public members of this class instance can be used to specify
#' arguments for base plots, ggplot plots, or any other plotting framework.
#'
#' @field title,subtitle,xlabel,ylabel,legendTitle A character string
#'   providing plot annotations for plot title, subtitle, x-axis label, y-axis
#'   label, plot legend, respectively.
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
    legendTitle = NULL,

    #' @description Create a new `ospInternalPlotConfiguration` object
    #'
    #' @param title,subtitle,xlabel,ylabel,legendTitle A character string
    #'   providing plot annotations for plot title, subtitle, x-axis label,
    #'   y-axis label, plot legend, respectively.
    #'
    #' @return A new `ospPlotConfiguration` object
    initialize = function(title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL,
                          legendTitle = NULL) {

      # labels
      self$title <- title
      self$subtitle <- subtitle
      self$xlabel <- xlabel
      self$ylabel <- ylabel

      # legend
      self$legendTitle <- legendTitle
    }
  )
)
