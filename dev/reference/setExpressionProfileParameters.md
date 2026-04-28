# Set Parameters of an Expression Profile Building Block

Sets one or more parameter values in an expression profile building
block by providing the corresponding quantity paths and values. The
number of paths and values must be equal.

## Usage

``` r
setExpressionProfileParameters(
  expressionProfileBuildingBlock,
  quantityPaths,
  quantityValues,
  units
)
```

## Arguments

- expressionProfileBuildingBlock:

  A `BuildingBlock` object as returned by
  [`createExpressionProfileBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createExpressionProfileBuildingBlock.md).

- quantityPaths:

  A character vector of quantity paths to set.

- quantityValues:

  A numeric vector of values to assign. Must have the same length as
  `quantityPaths`.

- units:

  A character vector of units corresponding to the quantity values. Must
  have the same length as `quantityPaths`. Units must be compatible with
  the dimensions of the corresponding quantities in the building block.
  If a single string is provided, it will be used for all quantities.

## Value

`expressionProfileBuildingBlock`, invisibly.

## Details

Note: Setting initial conditions of an expression profile is currently
not supported.

## Examples

``` r
expressionProfile <- createExpressionProfileBuildingBlock(
  type = ExpressionProfileCategories$`Metabolizing Enzyme`,
  moleculeName = "CYP3A4",
  speciesName = "Human"
)
setExpressionProfileParameters(
  expressionProfile,
  quantityPaths = c("Organism|Kidney|Intracellular|CYP3A4|Relative expression"),
  quantityValues = c(1.0),
  units = ""
)
```
