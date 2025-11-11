# Saves the simulation results to csv file

Saves the simulation results to csv file

## Usage

``` r
.saveResultsToCSV(results, filePath)
```

## Arguments

- results:

  Results to export (typically calculated using `runSimulations` or
  imported from file).

- filePath:

  Full path where the results will be saved.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath)

# Add some outputs to the simulation
addOutputs("Organism|**|*", sim)

# Run the simulation
results <- runSimulations(sim)[[1]]

# Export the results to csv file
exportResultsToCSV(results, tempfile())
```
