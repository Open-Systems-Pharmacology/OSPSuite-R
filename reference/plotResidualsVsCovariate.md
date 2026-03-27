# Plot Residuals vs Covariate

Plots residuals vs a covariate (time, observed, or predicted values),
grouped by "group".

This function visualizes the residuals against time, observed, or
predicted (simulated) values, helping to assess model performance.

## Usage

``` r
plotResidualsVsCovariate(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  xUnit = NULL,
  yUnit = NULL,
  residualScale = "log",
  xAxis = "observed",
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

  A character string specifying the target unit for the time values.
  (only relevant if `xAxis = "time"`) If `NULL` (default), the most
  frequent unit in the data is used. For available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospUnits.md).

- yUnit:

  A character string specifying the target unit for the simulated and
  observed y-values used for residual calculation and (if
  `xAxis != "time"`) displayed on the x-Axis. If `NULL` (default), the
  most frequent unit in the data is used. For available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospUnits.md).

- residualScale:

  Either "linear", "log", or "ratio" method for computing residuals.
  Default is `log`.

- xAxis:

  A character string specifying what to display on the x-axis. Options
  are `"time"` (time points from xValues), `"observed"` (observed
  values, default), or `"predicted"` (predicted/simulated values).

- ...:

  Arguments passed on to
  [`ospsuite.plots::plotYVsX`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotYVsX.html),
  [`ospsuite.plots::plotResVsCov`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotResVsCov.html)

  `geomPointAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)

  `geomComparisonLineAttributes`

  :   A `list` of arguments passed to `ggplot2::hline` or
      `ggplot2::abline` to display comparison lines.

  `geomLLOQAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)

  `groupAesthetics`

  :   A character vector of aesthetic names used for grouping data
      points when calculating comparison statistics. Data will be
      grouped by combinations of these aesthetics before computing
      counts and proportions within comparison lines. Common grouping
      aesthetics include `"colour"`, `"fill"`, `"shape"`.

  `addRegression`

  :   A boolean that activates the insertion of a regression line.

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

  `comparisonLineVector`

  :   A vector defining the comparison lines.

## Value

A `ggplot2` plot object representing residuals vs time, observed, or
predicted values.

## Details

### Residual Calculation

Residuals are calculated by pairing observed and simulated data at
matching time points within the same group. Only datasets that can be
paired (i.e., have corresponding observed and simulated values) are
included in the residual plot. The function automatically removes
unpaired datasets with a warning, converts units to ensure consistent
comparisons, and computes residuals as the difference between observed
and predicted values.

### Residual Scales

The `residualScale` parameter controls how residuals are displayed:

- `linear`: Absolute residuals (Observed - Predicted). Values centered
  around zero indicate good model fit. Useful for normally distributed
  errors.

- `log`: Log-transformed residuals, calculated as log(Observed /
  Predicted). Values centered around zero indicate good fit. Preferred
  for log-normally distributed data or when errors are proportional to
  magnitude.

- `ratio`: Ratio of observed to predicted (Observed / Predicted). Values
  centered around 1.0 indicate good fit. Useful for understanding
  relative prediction error.

## See also

Other plot functions based on ospsuite.plots:
[`plotPredictedVsObserved()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotPredictedVsObserved.md),
[`plotQuantileQuantilePlot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotQuantileQuantilePlot.md),
[`plotResidualsAsHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotResidualsAsHistogram.md),
[`plotTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotTimeProfile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a residuals vs observed plot for the provided data
plotResidualsVsCovariate(
  myDataCombined,
  xUnit = ospUnits$Time$h,
  yUnit = ospUnits$`Concentration [mass]`$`µg/l`,
  xAxis = "time",
  residualScale = 'linear'
)

# Generate a residuals vs predicted plot
plotResidualsVsCovariate(myDataCombined, xAxis = "predicted")

# Generate a residuals vs time plot
plotResidualsVsCovariate(myDataCombined, xAxis = "time")
} # }
```
