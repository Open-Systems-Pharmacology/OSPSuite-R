# Retrieve molecular weight for a quantity's molecule

Returns the molecular weight of the molecule for a given quantity. If no
unit is provided, the value is returned in the base unit (`kg/µmol`).

## Usage

``` r
getMolWeightFor(quantity, unit = NULL, stopIfNotFound = FALSE)
```

## Arguments

- quantity:

  A `Quantity` object.

- unit:

  Optional. Target unit for the molecular weight. Defaults to `kg/µmol`.

- stopIfNotFound:

  Logical. If `TRUE`, throws an error when the molecular weight cannot
  be retrieved. If `FALSE`, returns `NA`. Default is `FALSE`.

## Value

The molecular weight in the specified unit or `NA` if not found.

## Examples

``` r
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
parameterPath <- "Organism|VenousBlood|Plasma|Aciclovir|Concentration"
quantity <- getQuantity(parameterPath, container = sim)
getMolWeightFor(quantity, unit = "g/mol")
#> [1] 225.21
```
