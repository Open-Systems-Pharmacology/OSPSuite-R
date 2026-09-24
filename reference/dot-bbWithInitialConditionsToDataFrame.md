# Convert a building block containing initial conditions to a data frame.

Works with any building block that contains initial conditions,
including `Initial Conditions` and `Expression Profile` building blocks.

## Usage

``` r
.bbWithInitialConditionsToDataFrame(buildingBlock)
```

## Arguments

- buildingBlock:

  A `BuildingBlock` object of type `Initial Conditions` or
  `Expression Profile`.

## Value

A data frame with the following columns:

- `Container Path`: Full path to the container where the molecule is
  located.

- `Molecule Name`: Name of the molecule.

- `Is Present`: Boolean indicating if the molecule is present.

- `Value`: Initial value of the molecule. For values that are defined by
  a formula, the return value can be `NaN`.

- `Unit`: Unit of the initial value.

- `Scale Divisor`: Scale divisor for the initial value.

- `Neg. Values Allowed`: Boolean indicating if negative values are
  allowed.
