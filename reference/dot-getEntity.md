# Retrieve a single entity by path in the given container

Retrieve a single entity by path in the given container

## Usage

``` r
.getEntity(path, container, entityType, stopIfNotFound = TRUE)
```

## Arguments

- path:

  A string representing the path relative to the `container`

- container:

  A Container or Simulation used to find the entities

- entityType:

  Class of the type that should be returned. Supported types are
  Container, Quantity, and Parameter.

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no entity exists for the given path,
  an error is thrown. If `FALSE`, `NULL` is returned.

## Value

The `Entity` with the given path coerced to the `entityType`. If the
entity for the path does not exist, an error is thrown in case of
`stopIfNotFound` is TRUE (default), otherwise `NULL`

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/loadSimulation.md),
[`getContainer()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getContainer.md)
and
[`getAllContainersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getAllContainersMatching.md)
to create objects of type Container or Simulation
