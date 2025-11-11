# Loads aging data (typically generated from PK-Sim) i

Loads aging data (typically generated from PK-Sim) i

## Usage

``` r
loadAgingDataFromCSV(filePath)
```

## Arguments

- filePath:

  Full path containing an aging data table.

## Examples

``` r
csvPath <- system.file("extdata", "aging_data.csv", package = "ospsuite")

agingData <- loadAgingDataFromCSV(csvPath)
```
