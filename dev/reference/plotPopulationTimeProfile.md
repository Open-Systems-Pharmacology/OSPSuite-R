# Time-values profile plot for population simulations

Time-values profile plot for population simulations

## Usage

``` r
plotPopulationTimeProfile(
  dataCombined,
  defaultPlotConfiguration = NULL,
  aggregation = "quantiles",
  quantiles = c(0.05, 0.5, 0.95),
  showLegendPerDataset = FALSE,
  ...
)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class containing both observed and
  simulated datasets to be compared.

- defaultPlotConfiguration:

  A `DefaultPlotConfiguration` object, which is an `R6` class object
  that defines plot properties.

- aggregation:

  The type of the aggregation of individual data. One of `quantiles`
  (Default), `arithmetic` or `geometric` (full list in
  [`ospsuite::DataAggregationMethods`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DataAggregationMethods.md)).
  Will replace `yValues` by the median, arithmetic or geometric average
  and add a set of upper and lower bounds (`yValuesLower` and
  `yValuesHigher`).

- quantiles:

  A numerical vector with quantile values (Default:
  `c(0.05, 0.50, 0.95)`) to be plotted. Ignored if `aggregation` is not
  `quantiles`.

- showLegendPerDataset:

  Logical flag to display separate legend entries for observed and
  simulated datasets, if available. This is experimental and may not
  work reliably when both observed and simulated datasets \> 1. Defaults
  to `FALSE`.

- ...:

  additionnal arguments to pass to
  [`.extractAggregatedSimulatedData()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-extractAggregatedSimulatedData.md)

## Details

The simulated values will be aggregated across individuals for each time
point.

For `aggregation = quantiles` (default), the quantile values defined in
the argument `quantiles` will be used. In the profile plot, the middle
value will be used to draw a line, while the lower and upper values will
be used as the lower und upper ranges. For `aggregation = arithmetic`,
arithmetic mean with arithmetic standard deviation (SD) will be plotted.
Use the optional parameter `nsd` to change the number of SD to plot
above and below the mean. For `aggregation = geometric`, geometric mean
with geometric standard deviation (SD) will be plotted. Use the optional
parameter `nsd` to change the number of SD to plot above and below the
mean.

## See also

Other plotting:
[`DefaultPlotConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DefaultPlotConfiguration.md),
[`plotIndividualTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotIndividualTimeProfile.md),
[`plotObservedVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotObservedVsSimulated.md),
[`plotResidualsVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsSimulated.md),
[`plotResidualsVsTime()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsTime.md)

## Examples

``` r
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

populationResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file("extdata", "SimResults_pop.csv", package = "ospsuite")
)

# Create a new instance of `DataCombined` class
myDataComb <- DataCombined$new()
myDataComb$addSimulationResults(populationResults)


# plot
plotPopulationTimeProfile(myDataComb)


# plot with other quantiles
plotPopulationTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))


# plot with arithmetic mean
plotPopulationTimeProfile(myDataComb,
  aggregation = "arithmetic"
)

```
