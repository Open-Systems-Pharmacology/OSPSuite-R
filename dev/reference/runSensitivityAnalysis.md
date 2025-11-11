# Runs a sensitivity analysis

Runs a sensitivity analysis

## Usage

``` r
runSensitivityAnalysis(
  sensitivityAnalysis,
  sensitivityAnalysisRunOptions = NULL
)
```

## Arguments

- sensitivityAnalysis:

  Instance of a `SensitivityAnalysis` to run

- sensitivityAnalysisRunOptions:

  Optional instance of a `SensitivityAnalysisRunOptions` used during the
  sensitivity analysis run

## Value

SimulationResults (one entry per Individual)

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Create a new sensitivity object for the simulation
sensitivity <- SensitivityAnalysis$new(sim)

# Runs the sensitivity analysis
results <- runSensitivityAnalysis(sensitivity)
```
