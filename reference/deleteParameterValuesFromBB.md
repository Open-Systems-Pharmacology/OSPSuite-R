# Delete entries from a Parameter Values Building Block

Delete entries from a Parameter Values Building Block

## Usage

``` r
deleteParameterValuesFromBB(parameterValuesBuildingBlock, quantityPaths)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`.

- quantityPaths:

  A list of full paths of the quantities (usually parameters) that will
  be deleted. Should contain all path elements and the parameter name,
  separated by `|`. Entries not present in the provided BB are ignored.

## Value

Invisibly returns the updated `parameterValuesBuildingBlock` object.

## Examples

``` r
sim <- loadSimulation(system.file(
  "extdata",
  "simple.pkml",
  package = "ospsuite"
))
module <- sim$configuration$modules[[1]]
pvBB <- module$getParameterValuesBBs()[[1]]
deleteParameterValuesFromBB(pvBB, quantityPaths = "Organism|Volume")
```
