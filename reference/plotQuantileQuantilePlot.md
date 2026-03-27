# Plot Quantile-Quantile Plot

Plots a Quantile-Quantile plot, grouped by "group".

This function visualizes the distribution of predicted vs observed
values using a Q-Q plot.

## Usage

``` r
plotQuantileQuantilePlot(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  xUnit = NULL,
  yUnit = NULL,
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

- xUnit:

  A character string specifying the target unit for the x-axis. If
  `NULL` (default), the most frequent unit in the data is used. For
  available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospUnits.md).

- yUnit:

  A character string specifying the target unit for the simulated and
  observed y-values used for residual calculation. If `NULL` (default),
  the most frequent unit in the data is used. For available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospUnits.md).

- residualScale:

  Either "linear", "log", or "ratio" method for computing residuals.
  Default is `log`.

- ...:

  Arguments passed on to
  [`ospsuite.plots::plotQQ`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotQQ.html)

  `xScaleArgs`

  :   list of arguments passed to
      [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      or
      [`ggplot2::scale_x_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

  `yScaleArgs`

  :   list of arguments passed to
      [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      or
      [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

  `geomQQAttributes`

  :   A list of arguments passed to
      [`ggplot2::stat_qq()`](https://ggplot2.tidyverse.org/reference/geom_qq.html).

  `geomQQLineAttributes`

  :   A list of arguments passed to
      [`ggplot2::stat_qq_line()`](https://ggplot2.tidyverse.org/reference/geom_qq.html).

## Value

A `ggplot2` plot object representing the Q-Q plot.

## See also

Other plot functions based on ospsuite.plots:
[`plotPredictedVsObserved()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotPredictedVsObserved.md),
[`plotResidualsAsHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotResidualsAsHistogram.md),
[`plotResidualsVsCovariate()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotResidualsVsCovariate.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotTimeProfile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a Q-Q plot with default settings
plotQuantileQuantilePlot(myDataCombined)

# Generate a Q-Q plot with linear scale
plotQuantileQuantilePlot(myDataCombined, residualScale = "linear")
} # }
```
