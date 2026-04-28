# Convert an Initial Conditions Building Block to a data frame.

Convert an Initial Conditions Building Block to a data frame.

## Usage

``` r
initialConditionsBBToDataFrame(initialConditionsBuildingBlock)
```

## Arguments

- initialConditionsBuildingBlock:

  A `BuildingBlock` object of type `Initial Conditions`.

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

## Examples

``` r
sim <- loadSimulation(system.file(
  "extdata",
  "simple.pkml",
  package = "ospsuite"
))
module <- sim$configuration$modules[[1]]
icBB <- module$getInitialConditionsBBs()[[1]]
df <- initialConditionsBBToDataFrame(icBB)
```
