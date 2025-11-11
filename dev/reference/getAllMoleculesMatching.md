# Retrieve all molecules of a container (simulation or container instance) matching the given path criteria

Retrieve all molecules of a container (simulation or container instance)
matching the given path criteria

## Usage

``` r
getAllMoleculesMatching(paths, container)
```

## Arguments

- paths:

  A vector of strings representing the paths relative to the `container`

- container:

  A Container or Simulation used to find the molecules

## Value

A list of molecules matching the path criteria. The list is empty if no
molecules matching were found.

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

# Return all `A` molecules defined in all direct containers of the organism
molecules <- getAllMoleculesMatching("Organism|*|A", sim)

# Return all `A` molecules defined in all direct containers of the organism
# and the molecule `B`` of the container 'Liver'
paths <- c("Organism|*|A", "Organism|Liver|B")
molecules <- getAllMoleculesMatching(paths, sim)

# Returns all `A` molecules defined in `Organism` and all its subcontainers
molecules <- getAllMoleculesMatching("Organism|**|A", sim)
```
