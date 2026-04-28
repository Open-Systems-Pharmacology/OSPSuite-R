# Create an Expression Profile Building Block

Creates an expression profile building block in MoBi for a given
molecule, species, and category. The expression profile is created with
default parameter values, which can be modified using
[`setExpressionProfileParameters()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setExpressionProfileParameters.md).
Important: The expression profile is not queried from the expression
database.

## Usage

``` r
createExpressionProfileBuildingBlock(
  type,
  moleculeName,
  speciesName,
  phenotype = "Healthy"
)
```

## Arguments

- type:

  String. Type of the protein the profile will be created for. Must be
  one of the values defined in the `ExpressionProfileCategories` enum.

- moleculeName:

  Name of the protein molecule.

- speciesName:

  Name of the species. Must be one of the values defined in the
  `Species` enum.

- phenotype:

  Phenotype of the expression profile. Defaults to `"Healthy"`. Used for
  naming only; it has no other implication.

## Value

A `BuildingBlock` object representing the created expression profile.

## Details

Disease states are currently not supported.

## Examples

``` r
expressionProfile <- createExpressionProfileBuildingBlock(
  type = ExpressionProfileCategories$`Metabolizing Enzyme`,
  moleculeName = "CYP3A4",
  speciesName = "Human"
)
```
