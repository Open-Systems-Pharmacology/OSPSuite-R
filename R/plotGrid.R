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
#' library(tlf)
#' library(ospsuite)
#'
#' ls_plots <- list(
#'   plotHistogram(x = rnorm(100)),
#'   plotHistogram(x = rnorm(100, mean = 3)),
#'   plotHistogram(x = rnorm(100, mean = 10))
#' )
#'
#' plotGridObj <- createPlotGridConfiguration(
#'   plotList = ls_plots,
#'   title = "my combined plot",
#'   subtitle = "something clever",
#'   caption = "something dumb"
#' )
#'
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
