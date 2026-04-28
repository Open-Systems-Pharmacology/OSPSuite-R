# Set or add initial conditions to an existing Initial Conditions building block.

This functions allows adding or modifying initial condition entries.
Only constant values are allowed. For setting or adding initial
conditions defined by formulas, use the `setInitialConditionsFormulas`
function.

## Usage

``` r
setInitialConditionsInBB(
  initialConditionsBuildingBlock,
  quantityPaths,
  quantityValues,
  scaleDivisors = 1,
  isPresent = TRUE,
  negativeValuesAllowed = FALSE
)
```

## Arguments

- initialConditionsBuildingBlock:

  A `BuildingBlock` object of type `Initial Conditions`. The entries
  will be added to or set in this building block.

- quantityPaths:

  A list of full paths of the quantities (usually molecules). Should
  contain all path elements and the molecule name, separated by `|`.

- quantityValues:

  A list of values for the quantities. The length of this list should be
  equal to the length of `quantityPaths`. The values must be in base
  unit (µmol).

- scaleDivisors:

  Either a single value or a list of scale divisors for the quantities.
  By default, the value is set to 1 for all quantities. If only single
  value is provided, the value will be set for all quantities. If a list
  is provided, the length of this list should be equal to the length of
  `quantityPaths`.

- isPresent:

  Either a single value (`TRUE` or `FALSE`) or a list of boolean values
  indicating whether the quantity is present or not. If a list is
  provided, the length of this list should be equal to the length of
  `quantityPaths`.

- negativeValuesAllowed:

  A single boolean value or a list of boolean values indicating whether
  negative values are allowed for the quantities. If a list is provided,
  the length of this list should be equal to the length of
  `quantityPaths`.

## Value

Invisibly returns the updated `initialConditionsBuildingBlock` object.

## Examples

``` r
sim <- loadSimulation(system.file(
  "extdata",
  "simple.pkml",
  package = "ospsuite"
))
module <- sim$configuration$modules[[1]]
icBB <- module$getInitialConditionsBBs()[[1]]
setInitialConditionsInBB(
  icBB,
  quantityPaths = c("Organ|Tissue|Molecule1", "Organ|Tissue|Molecule2"),
  quantityValues = c(1, 0),
  scaleDivisors = c(1, 10),
  isPresent = c(TRUE, FALSE),
  negativeValuesAllowed = c(TRUE, FALSE)
)
```
