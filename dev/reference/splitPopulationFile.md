# Loads a population from the `csvPopulationFile` and split the loaded population according to `numberOfCores`.

Loads a population from the `csvPopulationFile` and split the loaded
population according to `numberOfCores`.

## Usage

``` r
splitPopulationFile(
  csvPopulationFile,
  numberOfCores,
  outputFolder,
  outputFileName
)
```

## Arguments

- csvPopulationFile:

  Full path of csv population file to split.

- numberOfCores:

  Number of cores used for parallelization computing. The population
  will be split across all cores.

- outputFolder:

  Folder where all split files will be created

- outputFileName:

  File names will be constructed using this parameter concatenated with
  the core index.

## Value

A string vector containing the full path of the population files
created. Note that there might be less files than cores

## Examples

``` r
csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")

# Split the population in up to 3 files, saved in the temp folder
splitFiles <- splitPopulationFile(csvPath, 3, tempdir(), "PopFile")
```
