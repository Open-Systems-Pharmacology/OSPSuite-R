# Converts a value given in a specified unit into the base unit of a quantity

Converts a value given in a specified unit into the base unit of a
quantity

## Usage

``` r
toBaseUnit(
  quantityOrDimension,
  values,
  unit,
  molWeight = NULL,
  molWeightUnit = NULL
)
```

## Arguments

- quantityOrDimension:

  Instance of a quantity from which the dimension will be retrieved or
  name of dimension

- values:

  Value in unit (single or vector)

- unit:

  Unit of value

- molWeight:

  Optional molecule weight to use when converting, for example, from
  molar to mass amount or concentration. If `molWeightUnit` is not
  specified, `molWeight` is assumed to be in kg/µmol

- molWeightUnit:

  Unit of the molecular weight value. If `NULL` (default), kg/µmol is
  assumed.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
par <- getParameter("Organism|Liver|Volume", sim)

# Converts the value in unit (1000 ml) to the base unit (l) => 1
valueInBaseUnit <- toBaseUnit(par, 1000, "ml")

valuesInBaseUnit <- toBaseUnit(par, c(1000, 2000, 3000), "ml")
```
