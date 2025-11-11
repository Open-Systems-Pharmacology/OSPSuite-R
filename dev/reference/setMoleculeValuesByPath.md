# Set molecule start values in the simulation by path

Set molecule start values in the simulation by path

## Usage

``` r
setMoleculeValuesByPath(
  moleculePaths,
  values,
  simulation,
  units = NULL,
  stopIfNotFound = TRUE
)
```

## Arguments

- moleculePaths:

  A single or a list of molecule paths

- values:

  A numeric value that should be assigned to the molecule start value or
  a vector of numeric values, if the start value of more than one
  molecule should be changed. Must have the same length as
  `moleculePaths`

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
setMoleculeValuesByPath("Organism|Liver|A", 1, sim)

setMoleculeValuesByPath(
  c("Organism|Liver|A", "Organism|Liver|B"),
  c(2, 3),
  sim,
  units = c("µmol", "mmol")
)
```
