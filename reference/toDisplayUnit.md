# Convert base unit to display unit

Converts a value given in base unit of a quantity into the display unit
of a quantity

## Usage

``` r
toDisplayUnit(quantity, values, unit = NULL)
```

## Arguments

- quantity:

  Instance of a quantity from which the base unit will be retrieved

- values:

  Value (single or vector). If `unit` is not specified, values are
  assumed to be in base unit

- unit:

  Optional Name of the unit to convert from. If `NULL` (default), the
  values are assumed to be in base unit.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
par <- getParameter("Organism|Liver|Volume", sim)

# Converts the value in base unit (1L) to display unit
valueInMl <- toDisplayUnit(par, 1)

valuesInDisplayUnit <- toDisplayUnit(par, c(1, 5, 5))

# Converts the value in ml to display unit
valueInDisplayUnit <- toDisplayUnit(par, 1000, "ml")
```
