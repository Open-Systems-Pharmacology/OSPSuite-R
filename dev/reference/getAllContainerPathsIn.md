# Retrieves the path of all containers defined in the container and all its children

Retrieves the path of all containers defined in the container and all
its children

## Usage

``` r
getAllContainerPathsIn(container)
```

## Arguments

- container:

  A Container or Simulation used to find the parameters

## Value

An array with one entry per container defined in the container

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

# Returns the path of all molecules defined in the simulation
moleculePaths <- getAllContainerPathsIn(sim)
```
