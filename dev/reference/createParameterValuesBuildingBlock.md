# Create an empty Parameter Values Building Block

Creates an empty Parameter Values building block that can subsequently
be populated via
[`setParameterValuesInBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setParameterValuesInBB.md)
or
[`addLocalMoleculeParametersToParameterValuesBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addLocalMoleculeParametersToParameterValuesBB.md),
and added to a module via
[`createMoBiModule()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createMoBiModule.md).

## Usage

``` r
createParameterValuesBuildingBlock(name = "Parameter Values")
```

## Arguments

- name:

  Name of the building block. Must be a non-empty string. Defaults to
  `"Parameter Values"`.

## Value

A `BuildingBlock` object of type `Parameter Values` with no entries.

## Examples

``` r
pvBB <- createParameterValuesBuildingBlock()
pvBB <- createParameterValuesBuildingBlock(name = "My PVs")
```
