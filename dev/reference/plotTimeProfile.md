# Create Time Profile Plot

Creates a time profile plot for given data.

This function generates a time profile plot using ggplot2, where the
data is grouped by a column named "group".

## Usage

``` r
plotTimeProfile(
  plotData,
  metaData = NULL,
  mapping = ggplot2::aes(),
  observedMapping = NULL,
  xUnit = NULL,
  yUnit = NULL,
  y2Unit = NULL,
  aggregation = "quantiles",
  quantiles =
    ospsuite.plots::getOspsuite.plots.option(ospsuite.plots::OptionKeys$defaultPercentiles),
  nsd = 1,
  showLegendPerDataset = "all",
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
      [`ospsuite::DataErrorType`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DataErrorType.md).

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

- observedMapping:

  A ggplot2 aesthetic mapping for observed data. Default is NULL. Then a
  copy of mapping without line typical aesthetics like linetype and
  linewidth is used.

- xUnit:

  A character string specifying the target unit for the x-axis. If
  `NULL` (default), the most frequent unit in the data is used. For
  available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospUnits.md).

- yUnit:

  A character string specifying the target unit for the primary y-axis.
  If `NULL` (default), the most frequent unit in the data is used. For
  available units, see
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospUnits.md).

- y2Unit:

  A character string specifying the target unit for the secondary y-axis
  (only applicable when data contains two y-dimensions). If `NULL`
  (default), the most frequent unit in the data is used.

- aggregation:

  The type of the aggregation of simulated data. One of `quantiles`
  (Default), `arithmetic` or `geometric` (full list in
  [`ospsuite::DataAggregationMethods`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DataAggregationMethods.md)).
  Will replace `yValues` by the median, arithmetic or geometric average
  and add a set of upper and lower bounds (`yMin` and `yMax`). It is
  only applied if the simulated data represents a population.

- quantiles:

  A numerical vector with quantile values (Default:
  `c(0.05, 0.50, 0.95)`) to be plotted. Ignored if `aggregation` is not
  `quantiles`.

- nsd:

  Optional parameter specifying the number of standard deviations to
  plot above and below the mean (used for error bars when aggregation is
  "arithmetic" or "geometric"). Ignored if `aggregation` is `quantiles`.

- showLegendPerDataset:

  Controls display of separate legend entries for individual datasets.
  One of:

  - `"none"`: No per-dataset differentiation. Only group-level legend.

  - `"all"` (default): Differentiate both observed (via `shape`) and
    simulated (via `linetype`).

  - `"observed"`: Differentiate only observed data via different shapes.

  - `"simulated"`: Differentiate only simulated data via different line
    types.

  User-provided `mapping` and `observedMapping` will override internal
  settings. A warning is issued if the override removes per-dataset
  differentiation.

- ...:

  Arguments passed on to
  [`ospsuite.plots::plotTimeProfile`](https://www.open-systems-pharmacology.org/OSPSuite.Plots/reference/plotTimeProfile.html)

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

  `yScaleArgs`

  :   list of arguments passed to
      [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
      or
      [`ggplot2::scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)

  `y2Scale`

  :   either 'linear' the secondary axis is displayed linear, or 'log'
      secondary axis is displayed with log scale

  `y2ScaleArgs`

  :   list of arguments passed to
      [`ggplot2::sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html),
      trans, break are set by code

  `plotObject`

  :   An optional `ggplot` object on which to add the plot layers

  `geomLineAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)

  `geomRibbonAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)

  `geomPointAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)

  `geomErrorbarAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_errorbar`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)

  `geomLLOQAttributes`

  :   A `list` with arguments which are passed on to the call
      [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)

  `groupAesthetics`

  :   vector of aesthetics, which are used for columns mapped with
      `groupby`,

## Value

A `ggplot2` plot object representing the time profile, or `NULL` if the
data contains no plottable entries.

## Details

### Automatic Unit Conversion

When using a `DataCombined` object, the function automatically converts
mixed units to a common unit. The target unit is determined by the most
frequently occurring unit in the observed data (or simulated data if no
observed data exists). Concentration dimensions (`Concentration (mass)`
and `Concentration (molar)`) are treated as compatible and can be
converted between each other if molecular weight is available.

### Mixed Error Types

The function automatically handles data containing different error type
specifications:

- If all data uses the same error type (`ArithmeticStdDev` or
  `GeometricStdDev`), it is passed directly to the plotting function.

- If data contains **mixed error types**, they are automatically
  converted to `yMin`/`yMax` bounds:

  - `ArithmeticStdDev`: `yMin = yValues - yErrorValues`,
    `yMax = yValues + yErrorValues`

  - `GeometricStdDev`: `yMin = yValues / yErrorValues`,
    `yMax = yValues * yErrorValues`

- For custom error types (not `ArithmeticStdDev` or `GeometricStdDev`),
  provide error bounds directly in `yMin` and `yMax` columns.

## See also

Other plot functions based on ospsuite.plots:
[`plotPredictedVsObserved()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotPredictedVsObserved.md),
[`plotQuantileQuantilePlot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotQuantileQuantilePlot.md),
[`plotResidualsAsHistogram()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsAsHistogram.md),
[`plotResidualsVsCovariate()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsCovariate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a time profile plot for the provided data
plotTimeProfile(myDataCombined,
  xUnit = ospUnits$Time$h,
  yUnit = ospUnits$`Concentration [mass]`$`mg/l`)

# Show individual dataset names for observed data only
plotTimeProfile(manyObsDC, showLegendPerDataset = "observed")

# Show individual dataset names for simulated data only
plotTimeProfile(manySimDC, showLegendPerDataset = "simulated")

# Show individual dataset names for both observed and simulated
plotTimeProfile(manyObsSimDC, showLegendPerDataset = "all")
} # }
```
