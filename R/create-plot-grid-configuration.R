#' @title Create `ospPlotGridConfiguration` class object
#'
#' @param plotList A list containing `ggplot` objects.
#' @param title,subtitle,caption Text strings to use for the various plot
#' annotations, where plot refers to the grid of plots as a whole.
#' @param tagLevels A character vector defining the enumeration format to use
#' at each level. Possible values are `'a'` for lowercase letters, `'A'` for
#' uppercase letters, `'1'` for numbers, `'i'` for lowercase Roman numerals, and
#' `'I'` for uppercase Roman numerals. It can also be a list containing
#' character vectors defining arbitrary tag sequences. If any element in the
#' list is a scalar and one of `'a'`, `'A'`, `'1'`, `'i`, or `'I'`, this level
#' will be expanded to the expected sequence.
#' @param tagPrefix,tagSuffix Strings that should appear before or after the
#' tag.
#' @param tagSeparator A separator between different tag levels
#' @param theme A ggplot theme specification to use for the plot. Only elements
#' related to the titles as well as plot margin and background will be used.
#' @param nColumns,nRows The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byRow Analogous to `byrow` in [matrix()][base::matrix]. If `FALSE` the
#' plots will be filled in in column-major order.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid.
#' @param guides A string specifying how guides should be treated in the layout.
#' `'collect'` will collect guides below to the given nesting level, removing
#' duplicates. `'keep'` will stop collection at this level and let guides be
#' placed alongside their plot. `auto` will allow guides to be collected if a
#' upper level tries, but place them alongside the plot if not. If you modify
#' default guide "position" with [theme(legend.position=...)][ggplot2::theme]
#' while also collecting guides you must apply that change to the overall
#' patchwork.
#' @param design Specification of the location of areas in the layout. Can either
#' be specified as a text string or by concatenating calls to [area()] together.
#' See the examples for further information on use.
#'
#' @export
createPlotGridConfiguration <- function(plotList,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        tagLevels = NULL,
                                        tagPrefix = NULL,
                                        tagSuffix = NULL,
                                        tagSeparator = NULL,
                                        theme = NULL,
                                        nColumns = NULL,
                                        nRows = NULL,
                                        byRow = NULL,
                                        widths = NULL,
                                        heights = NULL,
                                        guides = NULL,
                                        design = NULL) {
  ospPlotGridConfiguration$new(
    plotList = plotList,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tagLevels = tagLevels,
    tagPrefix = tagPrefix,
    tagSuffix = tagSuffix,
    tagSeparator = tagSeparator,
    theme = theme,
    nColumns = nColumns,
    nRows = nRows,
    byRow = byRow,
    widths = widths,
    heights = heights,
    guides = guides,
    design = design
  )
}
