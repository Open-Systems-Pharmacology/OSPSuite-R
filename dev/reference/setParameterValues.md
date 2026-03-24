# Set values of parameters

Set values of parameters

## Usage

``` r
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
