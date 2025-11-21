# Retrieve a single parameter by path in the given container

Retrieve a single parameter by path in the given container

## Usage

``` r
getParameter(path, container, stopIfNotFound = TRUE)
```

## Arguments

- path:

  A string representing the path relative to the `container`

- container:

  A Container or Simulation used to find the parameters

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no parameter exist for the given
  path, an error is thrown. If `FALSE`, `NULL` is returned.

## Value

The `Parameter` with the given path. If the parameter for the path does
not exist, an error is thrown if `stopIfNotFound` is TRUE (default),
otherwise `NULL`

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
param <- getParameter("Organism|Liver|Volume", sim)
```
