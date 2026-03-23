# Set or add parameter values to an existing Parameter Values building block.

This functions allows adding or modifying parameter values entries.

## Usage

``` r
setParameterValues(parameters, values, units = NULL)

setParameterValues(parameters, values, units = NULL)
```

## Arguments

- parameters:

  A single or a list of `Parameter`

- values:

  A numeric value that should be assigned to the parameter or a vector
  of numeric values, if the value of more than one parameter should be
  changed. Must have the same length as 'parameters'. Alternatively, the
  value can be a unique number. In that case, the same value will be set
  in all parameters

- units:

  A string or a list of strings defining the units of the `values`. If
  `NULL` (default), values are assumed to be in base units. If not
  `NULL`, must have the same length as `quantities`.

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`. The entries will
  be added to or set in this building block.

- quantityPaths:

  A list of full paths of the quantities (usually parameters). Should
  contain all path elements and the parameter name, separated by `|`.

- formulas:

  A list of `Formula` objects that will be set for the quantities.
  Mixing of `formulas` and `quantityValues` is not allowed. If
  `formulas` is provided, `quantityValues` should be `NULL`. The length
  of this list should be equal to the length of `quantityPaths`.

- dimensions:

  A single dimension or a list of dimensions (string names) of parameter
  values. Supported dimensions are listed in `ospDimension`. By default,
  new entries get the `Dimensionless` dimension.

- quantityValues:

  A list of values for the quantities. Should be `NULL` if the argument
  `formulas` is provided. If not `NULL`, the length of this list should
  be equal to the length of `quantityPaths`.

## See also

[`getParameter()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getParameter.md)
and
[`getAllParametersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getAllParametersMatching.md)
to create objects of type Parameter

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
param <- getParameter("Organism|Liver|Volume", sim)
setParameterValues(param, 1)
params <- getAllParametersMatching("Organism|**|Volume", sim)
setParameterValues(params, c(2, 3), units = c("ml", "l"))
```
