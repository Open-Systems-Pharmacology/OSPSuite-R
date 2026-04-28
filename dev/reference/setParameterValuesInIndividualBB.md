# Set Parameters of a MoBi Individual Building Block

Sets one or more parameter values in an individual building block by
providing the corresponding quantity paths and values. The number of
paths and values must be equal.

## Usage

``` r
setParameterValuesInIndividualBB(
  individualBuildingBlock,
  quantityPaths,
  quantityValues,
  units
)
```

## Arguments

- individualBuildingBlock:

  A `BuildingBlock` object as returned by
  [`createIndividualBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createIndividualBuildingBlock.md).

- quantityPaths:

  A character vector of quantity paths to set.

- quantityValues:

  A numeric vector of values to assign. Must have the same length as
  `quantityPaths`.

- units:

  A single unit string or a list of unit strings for the quantity
  values. If a single string is provided, it will be used for all
  quantities.

## Value

`individualBuildingBlock`, invisibly.

## Examples

``` r
individual <- createIndividualBuildingBlock(
  species = Species$Human,
  population = HumanPopulation$European_ICRP_2002
)
setParameterValuesInIndividualBB(
  individual,
  quantityPaths = c("Organism|Age", "Organism|Height"),
  quantityValues = c(30, 173),
  units = c("year(s)", "cm")
)
```
