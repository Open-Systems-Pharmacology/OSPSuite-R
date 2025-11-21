# Loads a population from a csv file and returns the population.

Loads a population from a csv file and returns the population.

## Usage

``` r
loadPopulation(csvPopulationFile)
```

## Arguments

- csvPopulationFile:

  Full path of csv population file to load.

## Examples

``` r
csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")

population <- loadPopulation(csvPath)
```
