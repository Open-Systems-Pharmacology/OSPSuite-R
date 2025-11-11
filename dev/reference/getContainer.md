# Retrieve a single container by path under the given container

Retrieve a single container by path under the given container

## Usage

``` r
getContainer(path, container, stopIfNotFound = TRUE)
```

## Arguments

- path:

  A string representing the path relative to the `container`

- container:

  A Container or Simulation used to find the containers

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no container exists for the given
  path, an error is thrown. If `FALSE`, `NULL` is returned.

## Value

The `Container` with the given path. If the container for the path does
not exist, an error is thrown if `stopIfNotFound` is TRUE (default),
otherwise `NULL`

## See also

[`loadSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulation.md)
and `getContainer()` to create objects of type Container or Simulation

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
container <- getContainer("Organism|Liver", sim)
```
