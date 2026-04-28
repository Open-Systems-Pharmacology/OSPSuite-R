# Save an Expression Profile Building Block to pkml

Exports an `Expression Profile` building block to a pkml file that can
be loaded by MoBi.

## Usage

``` r
saveExpressionProfileToPKML(expressionProfileBuildingBlock, filePath)
```

## Arguments

- expressionProfileBuildingBlock:

  A `BuildingBlock` object of type `Expression Profile`, as returned by
  [`createExpressionProfileBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createExpressionProfileBuildingBlock.md).

- filePath:

  Path where the pkml file will be created. Must end with the `.pkml`
  extension.

## Value

`filePath`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
expressionProfile <- createExpressionProfileBuildingBlock(
  type = ExpressionProfileCategories$`Metabolizing Enzyme`,
  moleculeName = "CYP3A4",
  speciesName = "Human"
)
saveExpressionProfileToPKML(expressionProfile, "CYP3A4-Human.pkml")
} # }
```
