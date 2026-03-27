# Plot Residuals Histogram

Plots residuals as a histogram, grouped by "group".

This function generates a histogram of the residuals, providing a visual
representation of their distribution.

## Usage

``` r
plotResidualsAsHistogram(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  yUnit = NULL,
  distribution = "normal",
  residualScale = "log",
  ...
)
```

## Arguments

- plotData:

  An object of class `DataCombined` or a `data.table`. If a
  `data.table`, it must include the following:

  - `xValues`: Numeric time points.

  - `yValues`: Observed or simulated values (numeric).

  - `group`: Grouping variable (factor or character).

  - `name`: Name for the dataset (factor or character).

  - `xUnit`: Unit of the x-axis values (character).

  - `yUnit`: Unit of the y-axis values (character).

  - `dataType`: Specifies data type—either `observed` or `simulated`.

  - Optional:

    - `yErrorType`: Type of y error, relative to
      [`ospsuite::DataErrorType`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataErrorType.md).

    - `yErrorValues`: Numeric error values.

    - `yMin`, `yMax`: Custom ranges for y-axis instead of error types.

    - `IndividualId`: Used for aggregation of simulated population data.

- metaData:

  A list containing metadata for the plot. If NULL, a default list is
  constructed from the data. Expected structure includes information
  about dimensions and units for both x and y axes.

- mapping:

  A ggplot2 aesthetic mapping object. Default is
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  This is added or replaces the default mapping constructed by the data.

- yUnit:

  A character string specifying the target unit for the simulated and
  observed y-values used for residual calculation. If `NULL` (default),
  the most frequent unit in the data is used. For available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospUnits.md).

- distribution:

  parameter passed to
  [`ospsuite.plots::plotHistogram`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotHistogram.html).

- residualScale:

  Either "linear", "log", or "ratio" method for computing residuals.
  Default is `log`.

- ...:

  Arguments passed on to
  [`ospsuite.plots::plotHistogram`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotHistogram.html)

  `asBarPlot`

  :   A `logical` indicating if `geom_histogram` should be used (for
      continuous data) or `geom_bar` (for categorical data). If TRUE,
      the variables `distribution`, `meanFunction`, `xScale`, and
      `xScaleArgs` are ignored.

  `geomHistAttributes`

  :   A `list` of arguments passed to
      [`ggplot2::geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
      (or `geom_bar` if `asBarPlot` = TRUE).

  `plotAsFrequency`

  :   A `logical` indicating if the histogram displays frequency on the
      y-axis.

  `xScale`

  :   either 'linear' then
      [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      or 'log' then
      [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      is used

  `xScaleArgs`

  :   list of arguments passed to
      [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      or
      [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

  `yScale`

  :   either 'linear' then
      [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      or 'log' then
      [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      is used

  `meanFunction`

  :   Function selection for the display of a vertical line. Options:
      `'none'`, `'mean'`, `'geomean'`, `'median'`, `'auto'` (default).
      `'auto'` selects `'mean'` for normal distribution, `'geomean'` for
      lognormal, `'median'` for other distributions, and `'none'` when
      no distribution fit.

## Value

A `ggplot2` plot object representing the histogram of residuals.

## See also

Other plot functions based on ospsuite.plots:
[`plotPredictedVsObserved()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotPredictedVsObserved.md),
[`plotQuantileQuantilePlot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotQuantileQuantilePlot.md),
[`plotResidualsVsCovariate()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotResidualsVsCovariate.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotTimeProfile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a histogram of residuals with default settings
plotResidualsAsHistogram(myDataCombined)

# Generate a histogram with linear scale
plotResidualsAsHistogram(myDataCombined, residualScale = "linear")
} # }
```
