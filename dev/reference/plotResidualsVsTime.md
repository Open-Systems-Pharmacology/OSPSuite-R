# Residuals versus time scatter plot

**\[deprecated\]**

## Usage

``` r
plotResidualsVsTime(
  dataCombined,
  defaultPlotConfiguration = NULL,
  scaling = "lin"
)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class containing both observed and
  simulated datasets to be compared.

- defaultPlotConfiguration:

  A `DefaultPlotConfiguration` object, which is an `R6` class object
  that defines plot properties.

- scaling:

  A character of length one specifying the scale type for residual. can
  be lin or log.

## See also

Other plotting:
[`DefaultPlotConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DefaultPlotConfiguration.md),
[`plotIndividualTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotIndividualTimeProfile.md),
[`plotObservedVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotObservedVsSimulated.md),
[`plotPopulationTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotPopulationTimeProfile.md),
[`plotResidualsVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsSimulated.md)

## Examples

``` r
# Create a new instance of `DefaultPlotConfiguration` class
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"

# plot
plotResidualsVsTime(
dataCombinedAciclovir,
scaling = "lin",
defaultPlotConfiguration = myPlotConfiguration
)
#> Warning: `plotResidualsVsTime()` was deprecated in ospsuite 12.4.2.
#> ℹ Please use `plotResidualsVsCovariate()` instead.
#> ℹ It will be removed in version 14.0.

```
