# Calculates the pkAnalyses for all output values available in `results`.

Calculates the pkAnalyses for all output values available in `results`.

## Usage

``` r
calculatePKAnalyses(results)
```

## Arguments

- results:

  Results of simulation. Typically the `results` are calculated using
  `runSimulations` or imported from csv file via `importResults`.

## Value

An instance of `SimulationPKAnalyses` class.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

addOutputs("Organism|VenousBlood|*|Caffeine", sim)
results <- runSimulations(sim)[[1]]
pkAnalyses <- calculatePKAnalyses(results)
```
