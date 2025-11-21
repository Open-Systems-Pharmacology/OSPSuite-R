# Scale current values of parameters using a factor

Scale current values of parameters using a factor

## Usage

``` r
scaleParameterValues(parameters, factor)
```

## Arguments

- parameters:

  A single or a list of `Parameter`

- factor:

  A numeric value that will be used to scale all parameters

## See also

[`getParameter()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getParameter.md)
and
[`getAllParametersMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getAllParametersMatching.md)
to create objects of type Parameter

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
param <- getParameter("Organism|Liver|Volume", sim)
scaleParameterValues(param, 1)
params <- getAllParametersMatching("Organism|**|Volume", sim)
scaleParameterValues(params, 1.5)
```
