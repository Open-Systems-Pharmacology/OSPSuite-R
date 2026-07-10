# Time-profile plot of individual data

**\[deprecated\]**

## Usage

``` r
plotIndividualTimeProfile(
  dataCombined,
  defaultPlotConfiguration = NULL,
  showLegendPerDataset = FALSE
)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class containing both observed and
  simulated datasets to be compared.

- defaultPlotConfiguration:

  A `DefaultPlotConfiguration` object, which is an `R6` class object
  that defines plot properties.

- showLegendPerDataset:

  Logical flag to display separate legend entries for observed and
  simulated datasets, if available. This is experimental and may not
  work reliably when both observed and simulated datasets \> 1. Defaults
  to `FALSE`.

## See also

Other plotting:
[`DefaultPlotConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DefaultPlotConfiguration.md),
[`plotObservedVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotObservedVsSimulated.md),
[`plotPopulationTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotPopulationTimeProfile.md),
[`plotResidualsVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsSimulated.md),
[`plotResidualsVsTime()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsTime.md)

## Examples

``` r
# Create a new instance of `DefaultPlotConfiguration` class
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"

# plot
plotIndividualTimeProfile(dataCombinedAciclovir, myPlotConfiguration)
#> Warning: `plotIndividualTimeProfile()` was deprecated in ospsuite 12.4.2.
#> ℹ Please use `plotTimeProfile()` instead.
#> ℹ It will be removed in version 14.0.

```
