# Find the most common units

Find the most common units

## Usage

``` r
.extractMostFrequentUnit(data, unitColumn)
```

## Arguments

- data:

  A data frame (or a tibble) from `DataCombined$toDataFrame()`.

- unitColumn:

  The name of the column containing units (e.g. `xUnit`).

## Examples

``` r
df <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(0.25, 45, 78),
  yUnit = c("", "%", "%"),
  yErrorUnit = c("", "%", "%"),
  yDimension = "Fraction",
  molWeight = 10
)

ospsuite:::.extractMostFrequentUnit(df, unitColumn = "xUnit")
#> [1] "min"
ospsuite:::.extractMostFrequentUnit(df, unitColumn = "yUnit")
#> [1] "%"
```
