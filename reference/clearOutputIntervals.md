# Removes all intervals as well as all single time points from the output schema defined in `simulation`

Removes all intervals as well as all single time points from the output
schema defined in `simulation`

## Usage

``` r
clearOutputIntervals(simulation)
```

## Arguments

- simulation:

  Instance of a simulation for which output intervals should be cleared

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Make sure we create a new simulation so that we do not impact other examples
sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)

clearOutputIntervals(sim)
```
