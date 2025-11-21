# Returns an array of parameter path with one entry for each parameter that is used in the simulation and can potentially be used for sensitivity analysis

Returns an array of parameter path with one entry for each parameter
that is used in the simulation and can potentially be used for
sensitivity analysis

## Usage

``` r
potentialVariableParameterPathsFor(simulation)
```

## Arguments

- simulation:

  Instance of a simulation for which variable parameters should be
  retrieved

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath)

parameterPaths <- potentialVariableParameterPathsFor(sim)
```
