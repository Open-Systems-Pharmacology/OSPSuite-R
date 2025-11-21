# Removes a simulation from simulations cache.

Removes a simulation from simulations cache.

## Usage

``` r
removeSimulationFromCache(simulation)
```

## Arguments

- simulation:

  Simulation to be removed from the cache

## Value

TRUE if the simulation was cached and could be removed from cache. FALSE
otherwise, usually indicating that the specific simulation was not
cached.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim1 <- loadSimulation(simPath)
sim2 <- loadSimulation(simPath, loadFromCache = FALSE, addToCache = FALSE)

removeSimulationFromCache(sim1) # returns TRUE
#> [1] TRUE
removeSimulationFromCache(sim2) # returns FALSE
#> [1] FALSE
```
