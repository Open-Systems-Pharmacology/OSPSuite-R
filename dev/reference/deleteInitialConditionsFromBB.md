# Delete entries from an Initial Conditions Building Block

Delete entries from an Initial Conditions Building Block

## Usage

``` r
deleteInitialConditionsFromBB(initialConditionsBuildingBlock, quantityPaths)
```

## Arguments

- initialConditionsBuildingBlock:

  A `BuildingBlock` object of type `Initial Conditions`.

- quantityPaths:

  A list of full paths of the quantities (usually molecules) that will
  be deleted. Entries not present in the provided BB are ignored. Should
  contain all path elements and the molecule name, separated by `|`.

## Value

Invisibly returns the updated `initialConditionsBuildingBlock` object.

## Examples

``` r
sim <- loadSimulation(system.file(
  "extdata",
  "simple.pkml",
  package = "ospsuite"
))
module <- sim$configuration$modules[[1]]
icBB <- module$getInitialConditionsBBs()[[1]]
deleteInitialConditionsFromBB(
  icBB,
  quantityPaths = c("Organism|Liver|A", "Organism|Liver|B")
)
```
