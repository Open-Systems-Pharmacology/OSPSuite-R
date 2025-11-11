# Validate that single instance of `DataCombined`

Validate that single instance of `DataCombined`

## Usage

``` r
.validateScalarDataCombined(dataCombined)
```

## Examples

``` r
ospsuite:::.validateScalarDataCombined(DataCombined$new()) # okay
#> NULL
# ospsuite:::.validateScalarDataCombined(list(DataCombined$new(), DataCombined$new())) # error
```
