# Imports the simulation results from one or more csv files

Imports the simulation results from one or more csv files

## Usage

``` r
importResultsFromCSV(simulation, filePaths)
```

## Arguments

- simulation:

  Instance of a simulation used to calculate the results

- filePaths:

  Full path of result files to import. Typically only one file is
  provided but a list of files is sometimes available when the
  simulation was parallelized and computed on different machines

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
resultPath <- system.file("extdata", "res.csv", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath)

# Run the simulation
results <- importResultsFromCSV(sim, resultPath)
```
