# Delete entries from an Initial Conditions Building Block

Delete entries from an Initial Conditions Building Block

## Usage

``` r
deleteInitialConditions(initialConditionsBuildingBlock, quantityPaths)
```

## Arguments

- initialConditionsBuildingBlock:

  A `BuildingBlock` object of type `Initial Conditions`.

- quantityPaths:

  A list of full paths of the quantities (usually molecules) that will
  be deleted. Entries not present in the provided BB are ignored. Should
  contain all path elements and the molecule name, separated by `|`.
