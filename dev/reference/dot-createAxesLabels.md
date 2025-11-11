# Create axes labels

Create axes labels

## Usage

``` r
.createAxesLabels(data, specificPlotConfiguration)
```

## Arguments

- data:

  A data frame from `DataCombined$toDataFrame()`, which has additionally
  been cleaned using `ospsuite:::.unitConverter()` to have the same
  units across datasets.

- specificPlotConfiguration:

  The nature of labels will change depending on the type of plot. The
  type of plot can be guessed from the specific `PlotConfiguration`
  object used, since each plot has a unique corresponding class.

## Details

If axes labels haven't been specified, create them using information
about dimensions and units present in the data frame produced by
`DataCombined$toDataFrame()`.

## See also

Other utilities-plotting:
[`.addMissingGroupings()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-addMissingGroupings.md),
[`.convertGeneralToSpecificPlotConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-convertGeneralToSpecificPlotConfiguration.md),
[`.extractAggregatedSimulatedData()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-extractAggregatedSimulatedData.md)

## Examples

``` r
df <- dplyr::tibble(
  dataType = c(rep("simulated", 3), rep("observed", 3)),
  xValues = c(0, 14.482, 28.965, 0, 1, 2),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(1, 1, 1, 1, 1, 1),
  yUnit = "mol/ml",
  yDimension = ospDimensions$`Concentration (mass)`,
  yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
  molWeight = c(10, 10, 20, 20, 10, 10)
)

df <- ospsuite:::.unitConverter(df)

ospsuite:::.createAxesLabels(df, tlf::TimeProfilePlotConfiguration$new())
#> $xLabel
#> [1] "Time [min]"
#> 
#> $yLabel
#> [1] "Concentration [mol/ml]"
#> 
```
