# Retrieves all path of entities defined within the container (simulation or container instance)

Retrieves all path of entities defined within the container (simulation
or container instance)

## Usage

``` r
.getAllEntityPathsIn(container, entityType, method = NULL)
```

## Arguments

- container:

  A Container or Simulation used to find the entities

- entityType:

  Type of entity for which the path should be returned.

- method:

  Method to call in the underlying .NET class. (optional). If
  unspecified, the method will be estimated from entity type.

## Value

An array of paths (one for each entity found under the container and its
sub containers) The list is empty if no entities matching were found.

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulation.md),
[`getContainer()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getContainer.md)
and
[`getAllContainersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getAllContainersMatching.md)
to create objects of type Container or Simulation
