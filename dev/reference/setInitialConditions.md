# Set or add initial conditions to an existing Initial Conditions building block.

This functions allows adding or modifying initial condition entries.

## Usage

``` r
setInitialConditions(
  initialConditionsBuildingBlock,
  quantityPaths,
  quantityValues,
  scaleDivisors = 1,
  isPresent = TRUE,
  negativeValuesAllowed = FALSE,
  formulas = NULL
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

  A list of values for the quantities. Should be `NULL` if the argument
  `formulas` is provided. If not `NULL`, the length of this list should
  be equal to the length of `quantityPaths`.

- scaleDivisors:

  Either a single value or a list of scale divisors for the quantities.
  If only single value is provided, the value will be set for all
  quantities. If a list is provided, the length of this list should be
  equal to the length of `quantityPaths`.

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

- formulas:

  A list of `Formula` objects that will be set for the quantities.
  Mixing of `formulas` and `quantityValues` is not allowed. If
  `formulas` is provided, `quantityValues` should be `NULL`. If not
  `NULL`, the length of this list should be equal to the length of
  `quantityPaths`.

## Value

The updated `initialConditionsBuildingBlock` object. TBD: no return? To
be consistent (or not confusing) with the extend functions?
