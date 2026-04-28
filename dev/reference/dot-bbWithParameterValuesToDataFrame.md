# Convert a building block containing parameter values to a data frame.

Works with any building block that contains parameter values, including
`Parameter Values`, `Individual`, and `Expression Profile` building
blocks.

## Usage

``` r
.bbWithParameterValuesToDataFrame(buildingBlock)
```

## Arguments

- buildingBlock:

  A `BuildingBlock` object of type `Parameter Values`, `Individual`, or
  `Expression Profile`.

## Value

A data frame with the following columns:

- `Container Path`: Full path to the container where the parameter is
  located.

- `Parameter Name`: Name of the parameter.

- `Value`: Value of the parameter. For values that are defined by a
  formula, the return value can be `NaN`.

- `Unit`: Unit of the parameter value.

- `Value Origin`: Origin of the parameter value.
