# Dimensions and Units

## Unit conversion

Every entity - a molecule, a parameter, or an observer - has a certain
dimension, like *Amount*, *Concentration*, or *Volume*. The dimension is
a property of an entity:

``` r
library(ospsuite)

# Load a simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter volume of the liver.
livParam <- getParameter("Organism|Liver|Volume", sim)
print(livParam)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Volume
#>   • Value: 2.17 [l]
#>   • Value Origin: Publication-ICRP, 2002. Basic Anatomical and Physiological
#>   Data for Use in Radiological Protection Reference Values. ICRP Publication
#>   89. Ann. ICRP 32 (3-4). https://doi.org/10.1016/0146-6453(79)90123-4
#> 
#> ── Formula ──
#> 
#>   • isDistributed: TRUE

# Dimension of the parameter
livParam$dimension
#> [1] "Volume"
```

The values of a certain dimension can be presented in different units -
for example, *l* or *ml* for the dimension *Volume*, or *mol* and *µmol*
for the dimension *Amount*. The list of all available units for an
entity can be obtained using `allUnits()` method:

``` r
# Dimension of the parameter
livParam$dimension
#> [1] "Volume"

# Units of the parameter
livParam$allUnits
#> [1] "l"  "ml" "µl"
```

Internally, `OSPS` works with the **base units**, and all the values
that are shown or passed to functions are in base units by default.
These base units are often different from the units that are displayed
by default in PK-Sim (and MoBi). The list of base and default display
units can be found in the
[documentation](https://docs.open-systems-pharmacology.org/appendix/appendix).

As an example, the parameter **BMI** is given in the default unit
`kg/dm²`, while the default display unit is the more convenient `kg/m²`.

The `{ospuite}` package provides a set of methods for conversion between
different units. The methods
[`toUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/toUnit.md),
[`toBaseUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/toBaseUnit.md),
and
[`toDisplayUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/toDisplayUnit.md)
require the quantity to get the correct dimension and units; however, it
does not change the value of the quantity!

``` r
# Get the BMI parameter
heightParam <- getParameter("Organism|Height", sim)
print(heightParam)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Height
#>   • Value: 17.00 [dm]
#> 
#> ── Formula ──
#> 
#>   • isConstant: TRUE

# Print the base and the default display units
heightParam$unit
#> [1] "dm"
heightParam$displayUnit
#> [1] "cm"

# Convert the value from the base into the default display unit
toDisplayUnit(quantity = heightParam, values = heightParam$value)
#> [1] 170

# Convert the value to the base unit, that can be used. e.g. for setting new parameter value
toBaseUnit(quantity = heightParam, values = 180, unit = "cm")
#> [1] 18

liverVolume <- getParameter("Organism|Liver|Volume", sim)
print(liverVolume)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Volume
#>   • Value: 2.17 [l]
#>   • Value Origin: Publication-ICRP, 2002. Basic Anatomical and Physiological
#>   Data for Use in Radiological Protection Reference Values. ICRP Publication
#>   89. Ann. ICRP 32 (3-4). https://doi.org/10.1016/0146-6453(79)90123-4
#> 
#> ── Formula ──
#> 
#>   • isDistributed: TRUE

liverVolume$allUnits
#> [1] "l"  "ml" "µl"

# Convert from base volume unit to µl
toUnit(quantity = liverVolume, values = c(1, 2, 3, 4), targetUnit = "ml")
#> [1] 1000 2000 3000 4000
```
