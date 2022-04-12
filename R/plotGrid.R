#' Create a plot grid
#'
#' @param plotGridConfiguration  A `PlotGridConfiguration` object, which
#'   is an `R6` class object that defines properties of a plot grid (like number
#'   of rows, columns, labels, etc.).
#'
#' @description
#'
#' Create a plot grid using the `patchwork::wrap_plots()` function. The required
#' arguments are supplied through the `PlotGridConfiguration` object.
#'
#' @examples
#'
#' library(ospsuite)
#' library(ggplot2)
#' library(tlf)
#'
#' # plots to be arranged in a grid
#' set.seed(123)
#' ls_plots <- list(
#'   tlf::plotHistogram(x = rnorm(100)),
#'   tlf::plotHistogram(x = rnorm(100, mean = 3)),
#'   tlf::plotHistogram(x = rnorm(100, mean = 10))
#' )
#'
#' # create an instance of plot configuration class
#' plotGridObj <- PlotGridConfiguration$new(plotList = ls_plots)
#'
#' # specify further customizations for the plot grid
#' plotGridObj$title <- "my combined plot"
#' plotGridObj$subtitle <- "something clever"
#' plotGridObj$caption <- "my sources"
#' plotGridObj$nColumns <- 2L
#' plotGridObj$tagLevels <- "A"
#' plotGridObj$tagPrefix <- "Plot ("
#' plotGridObj$tagSuffix <- ")"
#'
#' # plot the grid
#' plotGrid(plotGridObj)
#' @references
#' For more, see: <https://patchwork.data-imaginist.com/articles/patchwork.html>
#'
#' @export
plotGrid <- function(plotGridConfiguration) {
  validateIsOfType(plotGridConfiguration, "PlotGridConfiguration")

  patchwork::wrap_plots(
    plotGridConfiguration$plotList,
    ncol = plotGridConfiguration$nColumns,
    nrow = plotGridConfiguration$nRows,
    byrow = plotGridConfiguration$byRow,
    widths = plotGridConfiguration$widths,
    heights = plotGridConfiguration$heights,
    guides = plotGridConfiguration$guides,
    design = plotGridConfiguration$design
  ) +
    patchwork::plot_annotation(
      title = plotGridConfiguration$title,
      subtitle = plotGridConfiguration$subtitle,
      caption = plotGridConfiguration$caption,
      tag_levels = plotGridConfiguration$tagLevels,
      tag_prefix = plotGridConfiguration$tagPrefix,
      tag_suffix = plotGridConfiguration$tagSuffix,
      tag_sep = plotGridConfiguration$tagSeparator,
      theme = plotGridConfiguration$theme
    )
}
