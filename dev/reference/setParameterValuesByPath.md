# Set the values of parameters in the simulation by path

Set the values of parameters in the simulation by path

## Usage

``` r
setParameterValuesByPath(
  parameterPaths,
  values,
  simulation,
  units = NULL,
  stopIfNotFound = TRUE
)
```

## Arguments

- parameterPaths:

  A single or a list of parameter path

- values:

  A numeric value that should be assigned to the parameters or a vector
  of numeric values, if the value of more than one parameter should be
  changed. Must have the same length as 'parameterPaths'

- simulation:

  Simulation uses to retrieve parameter instances from given paths.

- units:

  A string or a list of strings defining the units of the `values`. If
  `NULL` (default), values are assumed to be in base units. If not
  `NULL`, must have the same length as `quantityPaths`.

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no parameter exists for the given
  path, an error is thrown. If `FALSE`, a warning is shown to the user

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
setParameterValuesByPath("Organism|Liver|Volume", 1, sim)

setParameterValuesByPath(c("Organism|Liver|Volume", "Organism|Volume"), c(2, 3), sim)
```
