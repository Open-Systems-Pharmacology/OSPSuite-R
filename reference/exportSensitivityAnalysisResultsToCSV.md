# Saves the simulation analysis results to csv file

Saves the simulation analysis results to csv file

## Usage

``` r
exportSensitivityAnalysisResultsToCSV(results, filePath)
```

## Arguments

- results:

  Results to export (typically calculated using `runSensitivityAnalysis`
  or imported from file)

- filePath:

  Full path where the results will be saved.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath)

# Create a new sensitivity object for the simulation
sensitivity <- SensitivityAnalysis$new(sim)

# Runs the sensitivity analysis
results <- runSensitivityAnalysis(sensitivity)

# Export the results to csv file
exportSensitivityAnalysisResultsToCSV(results, tempfile())
```
