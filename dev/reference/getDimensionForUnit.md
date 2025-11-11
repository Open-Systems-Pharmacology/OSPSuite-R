# Get dimension for a given unit

Get dimension for a given unit

## Usage

``` r
getDimensionForUnit(unit)
```

## Arguments

- unit:

  Unit used to find the corresponding dimension.

## Value

Returns the name of dimension that can be used to support the given unit
or `NULL` if the dimension cannot be found.

## Examples

``` r
getDimensionForUnit("mg")
#> [1] "Mass"
```
