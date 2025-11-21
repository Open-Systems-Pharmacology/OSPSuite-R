# Convert a path defined as string to a path array

Convert a path defined as string to a path array

## Usage

``` r
toPathArray(path)
```

## Arguments

- path:

  A string representation of a path, with path entries separated by '\|'

## Value

An array containing one element for each path entry.

## Examples

``` r
toPathArray("Organism|Organ|Liver")
#> [1] "Organism" "Organ"    "Liver"   
```
