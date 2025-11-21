# Get the values of quantities in the simulation by path

Get the values of quantities in the simulation by path

## Usage

``` r
getQuantityValuesByPath(
  quantityPaths,
  simulation,
  units = NULL,
  stopIfNotFound = TRUE
)
```

## Arguments

- quantityPaths:

  A single or a list of absolute quantity paths

- simulation:

  Simulation containing the quantities

- units:

  A string or a list of strings defining the units of returned values.
  If `NULL` (default), values are returned in base units. If not `NULL`,
  must have the same length as `quantityPaths`. Single entries may be
  `NULL`.

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no quantity exists for the given
  path, an error is thrown. If `FALSE`, a warning is shown to the user.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
getQuantityValuesByPath(
  list("Organism|Liver|Volume", "Organism|Liver|A"),
  sim, list("ml", NULL)
)
#> [1] 10000    30
```
