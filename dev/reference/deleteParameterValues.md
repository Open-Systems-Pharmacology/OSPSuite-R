# Delete entries from a Parameter Values Building Block

Delete entries from a Parameter Values Building Block

## Usage

``` r
deleteParameterValues(parameterValuesBuildingBlock, quantityPaths)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`.

- quantityPaths:

  A list of full paths of the quantities (usually parameters) that will
  be deleted. Should contain all path elements and the parameter name,
  separated by `|`. Entries not present in the provided BB are ignored.
