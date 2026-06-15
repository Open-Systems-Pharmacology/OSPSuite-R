# Check if unit is supported in the OSPSuite platform

Check if unit is supported in the OSPSuite platform

## Usage

``` r
isSupportedUnit(unit)
```

## Arguments

- unit:

  String name of the unit

## Value

Logical: `TRUE` if the unit is supported, `FALSE` otherwise.

## Details

Returns `TRUE` if the provided unit is supported in the OSPSuite
platform, otherwise `FALSE`

## Examples

``` r
isSupportedUnit("kg")
#> [1] TRUE
isSupportedUnit("invalid_unit")
#> [1] FALSE
```
