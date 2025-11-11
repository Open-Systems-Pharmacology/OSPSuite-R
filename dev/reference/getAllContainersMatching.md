# Retrieve all sub containers of a parent container (simulation or container instance) matching the given path criteria

Retrieve all sub containers of a parent container (simulation or
container instance) matching the given path criteria

## Usage

``` r
getAllContainersMatching(paths, container)
```

## Arguments

- paths:

  A vector of strings representing the paths relative to the `container`

- container:

  A Container or Simulation used to find the containers

## Value

A list of containers matching the path criteria. The list is empty if no
containers matching were found.

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulation.md)
and
[`getContainer()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getContainer.md)
to create objects of type Container or Simulation

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Return all `Intracellular` containers defined in all direct containers of the organism
containers <- getAllContainersMatching("Organism|*|Intracellular", sim)

# Return all `Intracellular` containers defined in all direct containers of the organism
# and the container "Interstitial" under 'Organism|Brain'
paths <- c("Organism|*|Intracellular", "Organism|Brain|Interstitial")
containers <- getAllContainersMatching(paths, sim)

# Returns all `Intracellular` containers defined in `Organism` and all its subcontainers
containers <- getAllContainersMatching("Organism|**|Intracellular", sim)
```
