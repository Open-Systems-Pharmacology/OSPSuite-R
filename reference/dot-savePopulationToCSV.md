# Saves the population to csv file

Saves the population to csv file

## Usage

``` r
.savePopulationToCSV(population, filePath)
```

## Arguments

- population:

  Population to export to csv (typically imported from file using
  `loadPopulation`)

- filePath:

  Full path where the population will be saved.

## Examples

``` r
csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")

# Load the population
population <- loadPopulation(csvPath)

# Exports the population
exportPopulationToCSV(population, tempfile())
```
