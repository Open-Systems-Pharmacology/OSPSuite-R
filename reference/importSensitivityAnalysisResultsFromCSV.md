# Imports the simulation analysis results from one or more csv files

Imports the simulation analysis results from one or more csv files

## Usage

``` r
importSensitivityAnalysisResultsFromCSV(simulation, filePaths)
```

## Arguments

- simulation:

  Instance of a simulation for which the sensitivity analysis was
  performed

- filePaths:

  Full path of sensitivity analysis result files to import. Typically
  only one file is provided but a list of files is sometimes available
  when the sensitivity analysis run was parallelized and computed on
  different machines

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
resultPath <- system.file("extdata", "sa.csv", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath)

# Run the simulation
results <- importSensitivityAnalysisResultsFromCSV(sim, resultPath)
```
