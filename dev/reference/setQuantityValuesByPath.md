# Set the values of quantities in the simulation by path

Set the values of quantities in the simulation by path

## Usage

``` r
setQuantityValuesByPath(
  quantityPaths,
  values,
  simulation,
  units = NULL,
  stopIfNotFound = TRUE
)
```

## Arguments

- quantityPaths:

  A single or a list of absolute quantity paths

- values:

  A numeric value that should be assigned to the quantities or a vector
  of numeric values, if the value of more than one quantity should be
  changed. Must have the same length as 'quantityPaths'.

- simulation:

  Simulation containing the quantities

- units:

  A string or a list of strings defining the units of the `values`. If
  `NULL` (default), values are assumed to be in base units. If not
  `NULL`, must have the same length as `quantityPaths`.

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no quantity exists for the given
  path, an error is thrown. If `FALSE`, a warning is shown to the user.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
setQuantityValuesByPath("Organism|Liver|Volume", 1, sim)

setParameterValuesByPath(list("Organism|Liver|Volume", "Organism|Liver|A"), c(2, 3), sim)
```
