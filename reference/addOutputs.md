# Adds the quantities as output into the `simulation`. The quantities can either be specified using explicit instances or using paths.

Adds the quantities as output into the `simulation`. The quantities can
either be specified using explicit instances or using paths.

## Usage

``` r
addOutputs(quantitiesOrPaths, simulation)
```

## Arguments

- quantitiesOrPaths:

  Quantity instances (element or vector) (typically retrieved using
  `getAllQuantitiesMatching`) or quantity path (element or vector) to
  add.

- simulation:

  Instance of a simulation for which output selection should be updated.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

paths <- c("Organism|VenousBlood|Plasma|Caffeine", "Organism|ArterialBlood|**|Caffeine")
addOutputs(paths, sim)

parameter <- getParameter("Organism|Liver|Volume", sim)
addOutputs(parameter, sim)
```
