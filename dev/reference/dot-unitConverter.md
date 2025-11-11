# Convert a data frame to common units

Convert a data frame to common units

## Usage

``` r
.unitConverter(data, xUnit = NULL, yUnit = NULL)
```

## Arguments

- data:

  A data frame (or a tibble) from `DataCombined$toDataFrame()`.

- xUnit, yUnit:

  Target units for `xValues` and `yValues`, respectively. If not
  specified (`NULL`), first of the existing units in the respective
  columns (`xUnit` and `yUnit`) will be selected as the common unit. For
  available dimensions and units, see
  [`ospsuite::ospDimensions`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospDimensions.md)
  and
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospUnits.md),
  respectively.

## See also

toUnit

## Examples

``` r
# small dataframe to illustrate the conversion
(df <- dplyr::tibble(
  dataType = c(rep("simulated", 3), rep("observed", 3)),
  xValues = c(0, 14.482, 28.965, 0, 1, 2),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(1, 1, 1, 1, 1, 1),
  yUnit = c("mol", "mol", "mol", "g", "g", "g"),
  yDimension = c("Amount", "Amount", "Amount", "Mass", "Mass", "Mass"),
  yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
  yErrorUnit = c("mol", "mol", "mol", "g", "g", "g"),
  molWeight = c(10, 10, 20, 20, 20, 10)
))
#> # A tibble: 6 × 10
#>   dataType  xValues xUnit xDimension yValues yUnit yDimension yErrorValues
#>   <chr>       <dbl> <chr> <chr>        <dbl> <chr> <chr>             <dbl>
#> 1 simulated     0   min   Time             1 mol   Amount             2.75
#> 2 simulated    14.5 min   Time             1 mol   Amount             2.92
#> 3 simulated    29.0 min   Time             1 mol   Amount             2.75
#> 4 observed      0   min   Time             1 g     Mass              NA   
#> 5 observed      1   min   Time             1 g     Mass              NA   
#> 6 observed      2   min   Time             1 g     Mass              NA   
#> # ℹ 2 more variables: yErrorUnit <chr>, molWeight <dbl>

# default conversion
ospsuite:::.unitConverter(df)
#> # A tibble: 6 × 10
#>   dataType  xValues xUnit xDimension yValues yUnit yDimension yErrorValues
#>   <chr>       <dbl> <chr> <chr>        <dbl> <chr> <chr>             <dbl>
#> 1 simulated     0   min   Time            10 g     Amount             27.5
#> 2 simulated    14.5 min   Time            10 g     Amount             29.2
#> 3 simulated    29.0 min   Time            20 g     Amount             54.9
#> 4 observed      0   min   Time             1 g     Mass               NA  
#> 5 observed      1   min   Time             1 g     Mass               NA  
#> 6 observed      2   min   Time             1 g     Mass               NA  
#> # ℹ 2 more variables: yErrorUnit <chr>, molWeight <dbl>

# customizing conversion with specified unit(s)
ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$h)
#> # A tibble: 6 × 10
#>   dataType  xValues xUnit xDimension yValues yUnit yDimension yErrorValues
#>   <chr>       <dbl> <chr> <chr>        <dbl> <chr> <chr>             <dbl>
#> 1 simulated  0      h     Time            10 g     Amount             27.5
#> 2 simulated  0.241  h     Time            10 g     Amount             29.2
#> 3 simulated  0.483  h     Time            20 g     Amount             54.9
#> 4 observed   0      h     Time             1 g     Mass               NA  
#> 5 observed   0.0167 h     Time             1 g     Mass               NA  
#> 6 observed   0.0333 h     Time             1 g     Mass               NA  
#> # ℹ 2 more variables: yErrorUnit <chr>, molWeight <dbl>
ospsuite:::.unitConverter(df, yUnit = ospUnits$Mass$kg)
#> # A tibble: 6 × 10
#>   dataType  xValues xUnit xDimension yValues yUnit yDimension yErrorValues
#>   <chr>       <dbl> <chr> <chr>        <dbl> <chr> <chr>             <dbl>
#> 1 simulated     0   min   Time         0.01  kg    Amount           0.0275
#> 2 simulated    14.5 min   Time         0.01  kg    Amount           0.0292
#> 3 simulated    29.0 min   Time         0.02  kg    Amount           0.0549
#> 4 observed      0   min   Time         0.001 kg    Mass            NA     
#> 5 observed      1   min   Time         0.001 kg    Mass            NA     
#> 6 observed      2   min   Time         0.001 kg    Mass            NA     
#> # ℹ 2 more variables: yErrorUnit <chr>, molWeight <dbl>
ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$s, yUnit = ospUnits$Amount$mmol)
#> # A tibble: 6 × 10
#>   dataType  xValues xUnit xDimension yValues yUnit yDimension yErrorValues
#>   <chr>       <dbl> <chr> <chr>        <dbl> <chr> <chr>             <dbl>
#> 1 simulated      0  s     Time          1000 mmol  Amount             2747
#> 2 simulated    869. s     Time          1000 mmol  Amount             2918
#> 3 simulated   1738. s     Time          1000 mmol  Amount             2746
#> 4 observed       0  s     Time            50 mmol  Mass                 NA
#> 5 observed      60  s     Time            50 mmol  Mass                 NA
#> 6 observed     120  s     Time           100 mmol  Mass                 NA
#> # ℹ 2 more variables: yErrorUnit <chr>, molWeight <dbl>
```
