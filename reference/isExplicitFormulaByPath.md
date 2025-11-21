# Is the value defined by an explicit formula

Is the value defined by an explicit formula

## Usage

``` r
isExplicitFormulaByPath(path, simulation, stopIfNotFound = TRUE)
```

## Arguments

- path:

  Path to the quantity

- simulation:

  A `Simulation` object that contains the quantity

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no quantity exists for the given
  path, an error is thrown. If `FALSE`, `FALSE` is returned.

## Value

`TRUE` if the value is an explicit formula, `FALSE` otherwise. Also
returns `FALSE` if no quantity with the given path is found and
`stopInfNotFound` is set to `FALSE`.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
isExplicitFormulaByPath("Organism|Liver|Volume", sim) # FALSE
#> [1] FALSE
```
