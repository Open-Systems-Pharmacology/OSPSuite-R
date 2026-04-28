# Save an Individual Building Block to pkml

Exports an `Individual` building block to a pkml file that can be loaded
by MoBi.

## Usage

``` r
saveIndividualToPKML(individualBuildingBlock, filePath)
```

## Arguments

- individualBuildingBlock:

  A `BuildingBlock` object of type `Individual`, as returned by
  [`createIndividualBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createIndividualBuildingBlock.md).

- filePath:

  Path where the pkml file will be created. Must end with the `.pkml`
  extension.

## Value

`filePath`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
individual <- createIndividualBuildingBlock(
  species = Species$Human,
  population = HumanPopulation$European_ICRP_2002
)
saveIndividualToPKML(individual, "Individual.pkml")
} # }
```
