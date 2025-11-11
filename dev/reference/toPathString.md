# Convert a path array to a path as string with entries separated by '\|'

Convert a path array to a path as string with entries separated by '\|'

## Usage

``` r
toPathString(...)
```

## Arguments

- ...:

  Path entries to concatenate into a path string.

## Value

A string built using each entry of the pathArray.

## Examples

``` r
toPathString(c("Organism", "Organ", "Liver"))
#> [1] "Organism|Organ|Liver"
```
