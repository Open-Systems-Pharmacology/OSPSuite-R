# Convert an Individual Building Block to a data frame.

Convert an Individual Building Block to a data frame.

## Usage

``` r
individualsBBToDataFrame(individualBuildingBlock)
```

## Arguments

- individualBuildingBlock:

  A `BuildingBlock` object of type `Individual`.

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
individual <- createIndividualBuildingBlock(
  species = Species$Human,
  population = HumanPopulation$European_ICRP_2002
)
df <- individualsBBToDataFrame(individual)
```
