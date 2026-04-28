# Convert a Parameter Values Building Block to a data frame.

Convert a Parameter Values Building Block to a data frame.

## Usage

``` r
parameterValuesBBToDataFrame(parameterValuesBuildingBlock)
```

## Arguments

- parameterValuesBuildingBlock:

  A `Parameter Values` Building Block.

## Value

A data frame with the following columns:

- `Container Path`: Full path to the container where the parameter is
  located.

- `Parameter Name`: Name of the parameter.

- `Value`: Value of the parameter. For values that are defined by a
  formula, the return value can be `NaN`.

- `Unit`: Unit of the parameter value.

- `Value Origin`: Origin of the parameter value.

## Examples

``` r
sim <- loadSimulation(system.file(
  "extdata",
  "simple.pkml",
  package = "ospsuite"
))
module <- sim$configuration$modules[[1]]
pvBB <- module$getParameterValuesBBs()[[1]]
df <- parameterValuesBBToDataFrame(pvBB)
```
