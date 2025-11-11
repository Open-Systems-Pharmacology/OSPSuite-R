# Retrieves the path of all observers defined in the container and all its children

Retrieves the path of all observers defined in the container and all its
children

## Usage

``` r
getAllObserverPathsIn(container)
```

## Arguments

- container:

  A Container or Simulation used to find the observers

## Value

An array with one entry per observer defined in the container

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulation.md),
[`getContainer()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getContainer.md)
and
[`getAllContainersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getAllContainersMatching.md)
to retrieve objects of type Container or Simulation

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Returns the path of all quantities defined in the simulation
observerPaths <- getAllObserverPathsIn(sim)
```
