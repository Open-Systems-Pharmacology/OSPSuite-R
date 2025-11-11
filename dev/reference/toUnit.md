# Converts a value given in base unit of a quantity into a target unit

Converts a value given in base unit of a quantity into a target unit

## Usage

``` r
toUnit(
  quantityOrDimension,
  values,
  targetUnit,
  sourceUnit = NULL,
  molWeight = NULL,
  molWeightUnit = NULL
)
```

## Arguments

- quantityOrDimension:

  Instance of a quantity from which the dimension will be retrieved or
  name of dimension

- values:

  Values to convert (single or vector). If `sourceUnit` is not
  specified, `values` are in the base unit of the dimension

- targetUnit:

  Unit to convert to

- sourceUnit:

  Optional Name of the unit to convert from. If `NULL` (default), the
  values are assumed to be in base unit.

- molWeight:

  Optional molecular weight to use when converting, for example, from
  molar to mass amount or concentration. If `molWeightUnit` is not
  specified, `molWeight` is assumed to be in kg/µmol

- molWeightUnit:

  Optional Unit of the molecular weight value. If `NULL` (default),
  kg/µmol is assumed.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
par <- getParameter("Organism|Liver|Volume", sim)

# Converts the value in base unit (1L) to ml => 1000
valueInMl <- toUnit(par, 1, "ml")

valuesInMl <- toUnit(par, c(1, 5, 5), "ml")

# Converts a numerical value in from mmol/l to mg/dl
valuesInMgDl <- toUnit(ospDimensions$`Concentration (molar)`, 5,
  targetUnit = "mmol/l",
  sourceUnit = "mg/dl", molWeight = 180, molWeightUnit = "g/mol"
)
```
