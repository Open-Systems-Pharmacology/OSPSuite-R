# Create an empty Initial Conditions Building Block

Creates an empty Initial Conditions building block that can subsequently
be populated via
[`setInitialConditionsInBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setInitialConditionsInBB.md)
or
[`extendInitialConditionsBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/extendInitialConditionsBB.md),
and added to a module via
[`createMoBiModule()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createMoBiModule.md).

## Usage

``` r
createInitialConditionsBuildingBlock(name = "Initial Conditions")
```

## Arguments

- name:

  Name of the building block. Must be a non-empty string. Defaults to
  `"Initial Conditions"`.

## Value

A `BuildingBlock` object of type `Initial Conditions` with no entries.

## Examples

``` r
icBB <- createInitialConditionsBuildingBlock()
icBB <- createInitialConditionsBuildingBlock(name = "My ICs")
```
