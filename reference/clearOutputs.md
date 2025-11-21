# Removes all selected output from the given `simulation`

Removes all selected output from the given `simulation`

## Usage

``` r
clearOutputs(simulation)
```

## Arguments

- simulation:

  Instance of a simulation for which output selection should be cleared.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath, )

clearOutputs(sim)
```
