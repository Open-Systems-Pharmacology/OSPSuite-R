# Retrieve all quantities of a container (simulation or container instance) matching the given path criteria

Retrieve all quantities of a container (simulation or container
instance) matching the given path criteria

## Usage

``` r
getAllQuantitiesMatching(paths, container)
```

## Arguments

- paths:

  A vector of strings relative to the `container`

- container:

  A Container or Simulation used to find the parameters

## Value

A list of quantities matching the path criteria. The list is empty if no
quantity matching were found.

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/loadSimulation.md),
[`getContainer()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getContainer.md)
and
[`getAllContainersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getAllContainersMatching.md)
to retrieve objects of type Container or Simulation

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Return all `Volume` quantities defined in all direct containers of the organism
quantities <- getAllQuantitiesMatching("Organism|*|Volume", sim)

# Return all `Volume` quantities defined in all direct containers of the organism
# and the parameter 'Weight (tissue)' of the container 'Liver'
paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
quantities <- getAllQuantitiesMatching(paths, sim)

# Returns all `Volume` quantities defined in `Organism` and all its subcontainers
quantities <- getAllQuantitiesMatching("Organism|**|Volume", sim)
```
