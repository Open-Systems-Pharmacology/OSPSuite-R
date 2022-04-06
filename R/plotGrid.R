#' Create a plot grid
#'
#' @param ospPlotGridConfiguration  A `ospPlotGridConfiguration` object, which
#'   is an `R6` class object that defines properties of a plot grid (like number
#'   of rows, columns, labels, etc.). You can create instance of this object
#'   using `createPlotGridConfiguration()` function.
#'
#' @description
#'
#' Create a plot grid using the `patchwork::wrap_plots()` function. The required
#' arguments are supplied through the `ospPlotGridConfiguration` object.
#'
#' @export
plotGrid <- function(ospPlotGridConfiguration) {
  patchwork::wrap_plots(
    ospPlotGridConfiguration$plotList,
    ncol = ospPlotGridConfiguration$nColumns,
    nrow = ospPlotGridConfiguration$nRows,
    byrow = ospPlotGridConfiguration$byRow,
    widths = ospPlotGridConfiguration$widths,
    heights = ospPlotGridConfiguration$heights,
    guides = ospPlotGridConfiguration$guides,
    design = ospPlotGridConfiguration$design
  ) +
    patchwork::plot_annotation(
      title = ospPlotGridConfiguration$title,
      subtitle = ospPlotGridConfiguration$subtitle,
      caption = ospPlotGridConfiguration$caption,
      tag_levels = ospPlotGridConfiguration$tagLevels,
      tag_prefix = ospPlotGridConfiguration$tagPrefix,
      tag_suffix = ospPlotGridConfiguration$tagSuffix,
      tag_sep = ospPlotGridConfiguration$tagSeparator,
      theme = ospPlotGridConfiguration$theme
    )
}
