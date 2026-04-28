# Set or add parameter values to an existing Parameter Values building block.

This function allows adding or modifying parameter value entries. Only
constant values are allowed. Values are expected in the provided `units`
and will be converted to base units internally.

## Usage

``` r
setParameterValuesInBB(
  parameterValuesBuildingBlock,
  quantityPaths,
  quantityValues,
  units,
  dimensions = NULL
)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`. The entries will
  be added to or set in this building block.

- quantityPaths:

  A list of full paths of the quantities (usually parameters). Should
  contain all path elements and the parameter name, separated by `|`.

- quantityValues:

  A list of values for the quantities in the provided `units`. The
  length of this list should be equal to the length of `quantityPaths`.

- units:

  A single unit string or a list of unit strings for the quantity
  values. If a single string is provided, it will be used for all
  quantities.

- dimensions:

  A single dimension or a list of dimensions (string names) of parameter
  values. Supported dimensions are listed in `ospDimensions`. If `NULL`
  (default), dimensions are derived from the provided `units` using
  [`getDimensionForUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getDimensionForUnit.md).

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
setParameterValuesInBB(
  pvBB,
  quantityPaths = "Organism|Organ|Parameter",
  quantityValues = 1.33,
  units = "ml/min/kg"
)
```
