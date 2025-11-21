# Retrieve all entities of a container (simulation or container instance) matching the given path criteria.

Retrieve all entities of a container (simulation or container instance)
matching the given path criteria.

## Usage

``` r
.getAllEntitiesMatching(paths, container, entityType, method = NULL)
```

## Arguments

- paths:

  A vector of strings representing the paths relative to the `container`

- container:

  A Container or Simulation used to find the entities

- entityType:

  Class of the type that should be returned.

- method:

  Method to call in the underlying .NET class. (optional). If
  unspecified, the method will be estimated from entity type

## Value

A list of entities matching the path criteria coerced to the
`entityType`. The list is empty if no entities matching were found.

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/loadSimulation.md),
[`getContainer()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getContainer.md)
and
[`getAllContainersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getAllContainersMatching.md)
to create objects of type Container or Simulation
