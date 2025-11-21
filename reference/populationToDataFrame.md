# Creates a data.frame containing one column for each parameter defined in the population

Creates a data.frame containing one column for each parameter defined in
the population

## Usage

``` r
populationToDataFrame(population)

populationToTibble(population)
```

## Arguments

- population:

  Population to convert to data frame (typically imported from file
  using `loadPopulation`)

## Examples

``` r
csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")

population <- loadPopulation(csvPath)
df <- populationToDataFrame(population)
```
