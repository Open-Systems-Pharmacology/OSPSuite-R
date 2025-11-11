# Retrieve a single quantity by path in the given container

Retrieve a single quantity by path in the given container

## Usage

``` r
getQuantity(path, container, stopIfNotFound = TRUE)
```

## Arguments

- path:

  A string representing the path relative to the `container`

- container:

  A Container or Simulation used to find the parameters

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no quantity exists for the given
  path, an error is thrown. If `FALSE`, `NULL` is returned.

## Value

The `Quantity` with the given path. If the quantity for the path does
not exist, an error is thrown if `stopIfNotFound` is `TRUE` (default),
otherwise `NULL`

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
quantity <- getQuantity("Organism|Liver|Volume", sim)
```
