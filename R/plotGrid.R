#' Create a plot grid
#'
#' @param PlotGridConfiguration  A `PlotGridConfiguration` object, which
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
#'
#' @references
#' For more, see: <https://patchwork.data-imaginist.com/articles/patchwork.html>
#'
#' @export
plotGrid <- function(PlotGridConfiguration) {
  patchwork::wrap_plots(
    PlotGridConfiguration$plotList,
    ncol = PlotGridConfiguration$nColumns,
    nrow = PlotGridConfiguration$nRows,
    byrow = PlotGridConfiguration$byRow,
    widths = PlotGridConfiguration$widths,
    heights = PlotGridConfiguration$heights,
    guides = PlotGridConfiguration$guides,
    design = PlotGridConfiguration$design
  ) +
    patchwork::plot_annotation(
      title = PlotGridConfiguration$title,
      subtitle = PlotGridConfiguration$subtitle,
      caption = PlotGridConfiguration$caption,
      tag_levels = PlotGridConfiguration$tagLevels,
      tag_prefix = PlotGridConfiguration$tagPrefix,
      tag_suffix = PlotGridConfiguration$tagSuffix,
      tag_sep = PlotGridConfiguration$tagSeparator,
      theme = PlotGridConfiguration$theme
    )
}
