# Retrieve all parameters of a container (simulation or container instance) matching the given path criteria

Retrieve all parameters of a container (simulation or container
instance) matching the given path criteria

## Usage

``` r
getAllParametersMatching(paths, container)
```

## Arguments

- paths:

  A vector of strings representing the paths relative to the `container`

- container:

  A Container or Simulation used to find the parameters

## Value

A list of parameters matching the path criteria. The list is empty if no
parameters matching were found.

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

# Return all `Volume` parameters defined in all direct containers of the organism
params <- getAllParametersMatching("Organism|*|Volume", sim)

# Return all `Volume` parameters defined in all direct containers of the organism
# and the parameter 'Weight (tissue)' of the container 'Liver'
paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
params <- getAllParametersMatching(paths, sim)

# Returns all `Volume` parameters defined in `Organism` and all its subcontainers
params <- getAllParametersMatching("Organism|**|Volume", sim)
```
