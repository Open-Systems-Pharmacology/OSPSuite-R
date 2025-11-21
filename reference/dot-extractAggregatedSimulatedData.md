# Extract aggregated simulated data

Extract aggregated simulated data

## Usage

``` r
.extractAggregatedSimulatedData(simData, aggregation = "quantiles", ...)
```

## Arguments

- simData:

  A data frame with simulated data from `DataCombined$toDataFrame()`.

- aggregation:

  The type of the aggregation of individual data. One of `quantiles`
  (Default), `arithmetic` or `geometric` (full list in
  [`ospsuite::DataAggregationMethods`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataAggregationMethods.md)).
  Will replace `yValues` by the median, arithmetic or geometric average
  and add a set of upper and lower bounds (`yValuesLower` and
  `yValuesHigher`).

- ...:

  Arguments passed on to
  [`.normRange`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-normRange.md)

  `nsd`

  :   optional argument defining the number of standard deviation to add
      and substract to the mean

- quantiles:

  A numerical vector with quantile values (Default:
  `c(0.05, 0.50, 0.95)`) to be plotted. Ignored if `aggregation` is not
  `quantiles`.

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

Other utilities-plotting:
[`.addMissingGroupings()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-addMissingGroupings.md),
[`.convertGeneralToSpecificPlotConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-convertGeneralToSpecificPlotConfiguration.md),
[`.createAxesLabels()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-createAxesLabels.md)

## Examples

``` r
# let's create a data frame to test this function
df <- dplyr::tibble(
  xValues = c(
    0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5,
    0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2,
    3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5
  ),
  yValues = c(
    0,
    0.990956723690033, 0.981773018836975, 0.972471475601196, 0.963047087192535,
    0.953498184680939, 0, 0.990953505039215, 0.981729507446289, 0.97233647108078,
    0.962786376476288, 0.953093528747559, 0, 0.990955889225006, 0.981753170490265,
    0.972399413585663, 0.962896287441254, 0.953253626823425, 0, 0.990950107574463,
    0.981710314750671, 0.972296476364136, 0.962724387645721, 0.953009009361267,
    0, 0.261394888162613, 0.266657412052155, 0.27151620388031, 0.275971591472626,
    0.280027687549591, 0, 0.26139160990715, 0.266613900661469, 0.271381109952927,
    0.275710910558701, 0.279623001813889, 0, 0.261393994092941, 0.266637593507767,
    0.271443992853165, 0.275820910930634, 0.279783099889755, 0, 0.261388212442398,
    0.266594797372818, 0.27134120464325, 0.275649011135101, 0.279538512229919
  ),
  group = c(rep("Stevens 2012 solid total", 24), rep("Stevens 2012 solid distal", 24)),
  name = group
)

# raw data
df
#> # A tibble: 48 × 4
#>    xValues yValues group                    name                    
#>      <dbl>   <dbl> <chr>                    <chr>                   
#>  1       0   0     Stevens 2012 solid total Stevens 2012 solid total
#>  2       1   0.991 Stevens 2012 solid total Stevens 2012 solid total
#>  3       2   0.982 Stevens 2012 solid total Stevens 2012 solid total
#>  4       3   0.972 Stevens 2012 solid total Stevens 2012 solid total
#>  5       4   0.963 Stevens 2012 solid total Stevens 2012 solid total
#>  6       5   0.953 Stevens 2012 solid total Stevens 2012 solid total
#>  7       0   0     Stevens 2012 solid total Stevens 2012 solid total
#>  8       1   0.991 Stevens 2012 solid total Stevens 2012 solid total
#>  9       2   0.982 Stevens 2012 solid total Stevens 2012 solid total
#> 10       3   0.972 Stevens 2012 solid total Stevens 2012 solid total
#> # ℹ 38 more rows

# aggregated data
ospsuite:::.extractAggregatedSimulatedData(df)
#>                         group                      name xValues yValuesLower
#>                        <char>                    <char>   <num>        <num>
#>  1:  Stevens 2012 solid total  Stevens 2012 solid total       0    0.0000000
#>  2:  Stevens 2012 solid total  Stevens 2012 solid total       1    0.9909506
#>  3:  Stevens 2012 solid total  Stevens 2012 solid total       2    0.9817132
#>  4:  Stevens 2012 solid total  Stevens 2012 solid total       3    0.9723025
#>  5:  Stevens 2012 solid total  Stevens 2012 solid total       4    0.9627337
#>  6:  Stevens 2012 solid total  Stevens 2012 solid total       5    0.9530217
#>  7: Stevens 2012 solid distal Stevens 2012 solid distal       0    0.0000000
#>  8: Stevens 2012 solid distal Stevens 2012 solid distal       1    0.2613887
#>  9: Stevens 2012 solid distal Stevens 2012 solid distal       2    0.2665977
#> 10: Stevens 2012 solid distal Stevens 2012 solid distal       3    0.2713472
#> 11: Stevens 2012 solid distal Stevens 2012 solid distal       4    0.2756583
#> 12: Stevens 2012 solid distal Stevens 2012 solid distal       5    0.2795512
#>       yValues yValuesHigher
#>         <num>         <num>
#>  1: 0.0000000     0.0000000
#>  2: 0.9909547     0.9909566
#>  3: 0.9817413     0.9817700
#>  4: 0.9723679     0.9724607
#>  5: 0.9628413     0.9630245
#>  6: 0.9531736     0.9534615
#>  7: 0.0000000     0.0000000
#>  8: 0.2613928     0.2613948
#>  9: 0.2666257     0.2666544
#> 10: 0.2714126     0.2715054
#> 11: 0.2757659     0.2759490
#> 12: 0.2797031     0.2799910
```
