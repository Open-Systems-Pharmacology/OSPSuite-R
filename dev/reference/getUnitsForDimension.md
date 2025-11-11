# Get units for a given dimension

Get units for a given dimension

## Usage

``` r
getUnitsForDimension(dimension)
```

## Arguments

- dimension:

  Name of dimension for which units should be returned

## Value

Returns a vector containing all units defined in the dimension

## Examples

``` r
getUnitsForDimension("Mass")
#> [1] "kg" "g"  "mg" "µg" "ng" "pg"
```
