# Retrieves the display path of the parameters defined by paths in the simulation

Retrieves the display path of the parameters defined by paths in the
simulation

## Usage

``` r
getParameterDisplayPaths(paths, simulation)
```

## Arguments

- paths:

  A single string or array of paths path relative to the `container`

- simulation:

  A simulation used to find the entities

## Value

a display path for each parameter in paths

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
displayPath <- getParameterDisplayPaths("Organism|Liver|Volume", sim)
```
