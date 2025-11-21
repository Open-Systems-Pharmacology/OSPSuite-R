# Convert base unit to display unit

Converts a value given in base unit of a quantity into the display unit
of a quantity

## Usage

``` r
toDisplayUnit(quantity, values)
```

## Arguments

- quantity:

  Instance of a quantity from which the base unit will be retrieved

- values:

  Value in base unit (single or vector)

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
par <- getParameter("Organism|Liver|Volume", sim)

# Converts the value in base unit (1L) to display unit
valueInMl <- toDisplayUnit(par, 1)

valuesInDisplayUnit <- toDisplayUnit(par, c(1, 5, 5))
```
