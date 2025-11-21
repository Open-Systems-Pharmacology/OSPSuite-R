# Retrieve all parameters of the given simulation matching the given path criteria and also potential candidate for sensitivity variation

Retrieve all parameters of the given simulation matching the given path
criteria and also potential candidate for sensitivity variation

## Usage

``` r
getAllParametersForSensitivityAnalysisMatching(paths, simulation)
```

## Arguments

- paths:

  A vector of strings representing the path of the parameters
  (potentially using wildcards)

- simulation:

  Simulation used to find the parameters

## Value

A list of parameters matching the path criteria and also candidates for
a sensitivity analysis. The list is empty if no parameters matching were
found.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Return all `Volume` parameters defined in all direct containers of the organism
params <- getAllParametersForSensitivityAnalysisMatching("Organism|*|Volume", sim)
```
