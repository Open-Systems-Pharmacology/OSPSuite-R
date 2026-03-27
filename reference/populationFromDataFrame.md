# Creates a `Population` object from a data.frame

Creates a `Population` object from a data.frame

## Usage

``` r
populationFromDataFrame(dataFrame)
```

## Arguments

- dataFrame:

  A data.frame containing population data. Each column represents a
  parameter path or covariate, with one row per individual. If no
  `IndividualId` column is present, one will be automatically generated
  with 0-based sequential IDs.

## Value

A `Population` object

## Examples

``` r
csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")

population <- loadPopulation(csvPath)
df <- populationToDataFrame(population)
populationFromDf <- populationFromDataFrame(df)
```
