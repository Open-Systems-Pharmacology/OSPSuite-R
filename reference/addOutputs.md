# Adds the quantities as output into the `simulation`. The quantities can either be specified using explicit instances or using paths.

Adds the quantities as output into the `simulation`. The quantities can
either be specified using explicit instances or using paths.

## Usage

``` r
addOutputs(quantitiesOrPaths, simulation, stopIfNotFound = TRUE)
```

## Arguments

- quantitiesOrPaths:

  Quantity instances (element or vector) (typically retrieved using
  `getAllQuantitiesMatching`) or quantity path (element or vector) to
  add.

- simulation:

  Instance of a simulation for which output selection should be updated.

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no quantity exists for the given
  path, an error is thrown. If `FALSE`, `NULL` is returned.

## Examples

``` r
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

paths <- c("Organism|VenousBlood|Plasma|Aciclovir", "Organism|ArterialBlood|**|Aciclovir")
addOutputs(paths, sim)

parameter <- getParameter("Organism|Liver|Volume", sim)
addOutputs(parameter, sim)
```
