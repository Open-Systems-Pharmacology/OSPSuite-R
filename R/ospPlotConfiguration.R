#' @title Convenience class storing plot labels
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
#' @field xUnit,yUnit Units for x- and y-axes, respectively.
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

    # units
    xUnit = NULL,
    yUnit = NULL
  )
)
