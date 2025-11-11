# Create a list of all units available for each dimension

Create a list of all units available for each dimension

## Usage

``` r
.getUnitsEnum()
```

## Value

enum of all units for each dimension

## Details

Loop through dimensions and build a list containing an enum of all units
available for each dimension

## Examples

``` r
ospsuite:::.getUnitsEnum()
#> $`Abundance per mass protein`
#> $`Abundance per mass protein`$`pmol/mg mic. protein`
#> [1] "pmol/mg mic. protein"
#> 
#> $`Abundance per mass protein`$`nmol/mg mic. protein`
#> [1] "nmol/mg mic. protein"
#> 
#> $`Abundance per mass protein`$`µmol/mg mic. protein`
#> [1] "µmol/mg mic. protein"
#> 
#> $`Abundance per mass protein`$`µmol/kg mic. protein`
#> [1] "µmol/kg mic. protein"
#> 
#> 
#> $`Abundance per tissue`
#> $`Abundance per tissue`$`pmol/g tissue`
#> [1] "pmol/g tissue"
#> 
#> $`Abundance per tissue`$`nmol/g tissue`
#> [1] "nmol/g tissue"
#> 
#> $`Abundance per tissue`$`µmol/g tissue`
#> [1] "µmol/g tissue"
#> 
#> $`Abundance per tissue`$`µmol/kg tissue`
#> [1] "µmol/kg tissue"
#> 
#> 
#> $`Age in weeks`
#> $`Age in weeks`$`week(s)`
#> [1] "week(s)"
#> 
#> $`Age in weeks`$`year(s)`
#> [1] "year(s)"
#> 
#> $`Age in weeks`$`month(s)`
#> [1] "month(s)"
#> 
#> $`Age in weeks`$`day(s)`
#> [1] "day(s)"
#> 
#> 
#> $`Age in years`
#> $`Age in years`$`year(s)`
#> [1] "year(s)"
#> 
#> $`Age in years`$`month(s)`
#> [1] "month(s)"
#> 
#> $`Age in years`$`week(s)`
#> [1] "week(s)"
#> 
#> $`Age in years`$`day(s)`
#> [1] "day(s)"
#> 
#> 
#> $Amount
#> $Amount$mol
#> [1] "mol"
#> 
#> $Amount$mmol
#> [1] "mmol"
#> 
#> $Amount$µmol
#> [1] "µmol"
#> 
#> $Amount$nmol
#> [1] "nmol"
#> 
#> $Amount$pmol
#> [1] "pmol"
#> 
#> 
#> $`Amount per area`
#> $`Amount per area`$`µmol/µm²`
#> [1] "µmol/µm²"
#> 
#> $`Amount per area`$`µmol/mm²`
#> [1] "µmol/mm²"
#> 
#> $`Amount per area`$`µmol/cm²`
#> [1] "µmol/cm²"
#> 
#> $`Amount per area`$`µmol/dm²`
#> [1] "µmol/dm²"
#> 
#> $`Amount per area`$`nmol/µm²`
#> [1] "nmol/µm²"
#> 
#> $`Amount per area`$`nmol/mm²`
#> [1] "nmol/mm²"
#> 
#> $`Amount per area`$`nmol/cm²`
#> [1] "nmol/cm²"
#> 
#> $`Amount per area`$`nmol/dm²`
#> [1] "nmol/dm²"
#> 
#> 
#> $`Amount per area per time`
#> $`Amount per area per time`$`µmol/cm²/h`
#> [1] "µmol/cm²/h"
#> 
#> $`Amount per area per time`$`mmol/cm²/h`
#> [1] "mmol/cm²/h"
#> 
#> $`Amount per area per time`$`mmol/m²/h`
#> [1] "mmol/m²/h"
#> 
#> $`Amount per area per time`$`µmol/dm²/min`
#> [1] "µmol/dm²/min"
#> 
#> 
#> $`Amount per time`
#> $`Amount per time`$`mmol/s`
#> [1] "mmol/s"
#> 
#> $`Amount per time`$`mmol/min`
#> [1] "mmol/min"
#> 
#> $`Amount per time`$`mmol/h`
#> [1] "mmol/h"
#> 
#> $`Amount per time`$`mmol/day(s)`
#> [1] "mmol/day(s)"
#> 
#> $`Amount per time`$`µmol/s`
#> [1] "µmol/s"
#> 
#> $`Amount per time`$`µmol/min`
#> [1] "µmol/min"
#> 
#> $`Amount per time`$`µmol/h`
#> [1] "µmol/h"
#> 
#> $`Amount per time`$`µmol/day(s)`
#> [1] "µmol/day(s)"
#> 
#> $`Amount per time`$`nmol/s`
#> [1] "nmol/s"
#> 
#> $`Amount per time`$`nmol/min`
#> [1] "nmol/min"
#> 
#> $`Amount per time`$`nmol/h`
#> [1] "nmol/h"
#> 
#> $`Amount per time`$`nmol/day(s)`
#> [1] "nmol/day(s)"
#> 
#> $`Amount per time`$`pmol/s`
#> [1] "pmol/s"
#> 
#> $`Amount per time`$`pmol/min`
#> [1] "pmol/min"
#> 
#> $`Amount per time`$`pmol/h`
#> [1] "pmol/h"
#> 
#> $`Amount per time`$`pmol/day(s)`
#> [1] "pmol/day(s)"
#> 
#> 
#> $Ampere
#> $Ampere$A
#> [1] "A"
#> 
#> 
#> $Area
#> $Area$`dm²`
#> [1] "dm²"
#> 
#> $Area$`cm²`
#> [1] "cm²"
#> 
#> $Area$`m²`
#> [1] "m²"
#> 
#> 
#> $`AUC [mass]`
#> $`AUC [mass]`$`mg*h/ml`
#> [1] "mg*h/ml"
#> 
#> $`AUC [mass]`$`µg*h/ml`
#> [1] "µg*h/ml"
#> 
#> $`AUC [mass]`$`ng*h/ml`
#> [1] "ng*h/ml"
#> 
#> $`AUC [mass]`$`pg*h/ml`
#> [1] "pg*h/ml"
#> 
#> $`AUC [mass]`$`mg*h/l`
#> [1] "mg*h/l"
#> 
#> $`AUC [mass]`$`µg*h/l`
#> [1] "µg*h/l"
#> 
#> $`AUC [mass]`$`ng*h/l`
#> [1] "ng*h/l"
#> 
#> $`AUC [mass]`$`pg*h/l`
#> [1] "pg*h/l"
#> 
#> $`AUC [mass]`$`mg*min/ml`
#> [1] "mg*min/ml"
#> 
#> $`AUC [mass]`$`µg*min/ml`
#> [1] "µg*min/ml"
#> 
#> $`AUC [mass]`$`ng*min/ml`
#> [1] "ng*min/ml"
#> 
#> $`AUC [mass]`$`pg*min/ml`
#> [1] "pg*min/ml"
#> 
#> $`AUC [mass]`$`mg*min/l`
#> [1] "mg*min/l"
#> 
#> $`AUC [mass]`$`µg*min/l`
#> [1] "µg*min/l"
#> 
#> $`AUC [mass]`$`ng*min/l`
#> [1] "ng*min/l"
#> 
#> $`AUC [mass]`$`pg*min/l`
#> [1] "pg*min/l"
#> 
#> $`AUC [mass]`$`kg*min/l`
#> [1] "kg*min/l"
#> 
#> $`AUC [mass]`$`kg*h/l`
#> [1] "kg*h/l"
#> 
#> $`AUC [mass]`$`kg*h/ml`
#> [1] "kg*h/ml"
#> 
#> $`AUC [mass]`$`kg*min/ml`
#> [1] "kg*min/ml"
#> 
#> 
#> $`AUC [molar]`
#> $`AUC [molar]`$`mmol*h/ml`
#> [1] "mmol*h/ml"
#> 
#> $`AUC [molar]`$`µmol*h/ml`
#> [1] "µmol*h/ml"
#> 
#> $`AUC [molar]`$`nmol*h/ml`
#> [1] "nmol*h/ml"
#> 
#> $`AUC [molar]`$`pmol*h/ml`
#> [1] "pmol*h/ml"
#> 
#> $`AUC [molar]`$`mmol*h/l`
#> [1] "mmol*h/l"
#> 
#> $`AUC [molar]`$`µmol*h/l`
#> [1] "µmol*h/l"
#> 
#> $`AUC [molar]`$`nmol*h/l`
#> [1] "nmol*h/l"
#> 
#> $`AUC [molar]`$`pmol*h/l`
#> [1] "pmol*h/l"
#> 
#> $`AUC [molar]`$`mmol*min/ml`
#> [1] "mmol*min/ml"
#> 
#> $`AUC [molar]`$`µmol*min/ml`
#> [1] "µmol*min/ml"
#> 
#> $`AUC [molar]`$`nmol*min/ml`
#> [1] "nmol*min/ml"
#> 
#> $`AUC [molar]`$`pmol*min/ml`
#> [1] "pmol*min/ml"
#> 
#> $`AUC [molar]`$`µmol*min/l`
#> [1] "µmol*min/l"
#> 
#> $`AUC [molar]`$`mmol*min/l`
#> [1] "mmol*min/l"
#> 
#> $`AUC [molar]`$`nmol*min/l`
#> [1] "nmol*min/l"
#> 
#> $`AUC [molar]`$`pmol*min/l`
#> [1] "pmol*min/l"
#> 
#> 
#> $`AUCM [molar]`
#> $`AUCM [molar]`$`mmol*h²/ml`
#> [1] "mmol*h²/ml"
#> 
#> $`AUCM [molar]`$`µmol*h²/ml`
#> [1] "µmol*h²/ml"
#> 
#> $`AUCM [molar]`$`nmol*h²/ml`
#> [1] "nmol*h²/ml"
#> 
#> $`AUCM [molar]`$`pmol*h²/ml`
#> [1] "pmol*h²/ml"
#> 
#> $`AUCM [molar]`$`mmol*h²/l`
#> [1] "mmol*h²/l"
#> 
#> $`AUCM [molar]`$`µmol*h²/l`
#> [1] "µmol*h²/l"
#> 
#> $`AUCM [molar]`$`nmol*h²/l`
#> [1] "nmol*h²/l"
#> 
#> $`AUCM [molar]`$`pmol*h²/l`
#> [1] "pmol*h²/l"
#> 
#> $`AUCM [molar]`$`mmol*min²/ml`
#> [1] "mmol*min²/ml"
#> 
#> $`AUCM [molar]`$`µmol*min²/ml`
#> [1] "µmol*min²/ml"
#> 
#> $`AUCM [molar]`$`nmol*min²/ml`
#> [1] "nmol*min²/ml"
#> 
#> $`AUCM [molar]`$`pmol*min²/ml`
#> [1] "pmol*min²/ml"
#> 
#> $`AUCM [molar]`$`µmol*min²/l`
#> [1] "µmol*min²/l"
#> 
#> $`AUCM [molar]`$`mmol*min²/l`
#> [1] "mmol*min²/l"
#> 
#> $`AUCM [molar]`$`nmol*min²/l`
#> [1] "nmol*min²/l"
#> 
#> $`AUCM [molar]`$`pmol*min²/l`
#> [1] "pmol*min²/l"
#> 
#> 
#> $Becquerel
#> $Becquerel$Bq
#> [1] "Bq"
#> 
#> $Becquerel$`1/min`
#> [1] "1/min"
#> 
#> $Becquerel$`1/h`
#> [1] "1/h"
#> 
#> $Becquerel$`1/s`
#> [1] "1/s"
#> 
#> 
#> $BMI
#> $BMI$`kg/m²`
#> [1] "kg/m²"
#> 
#> $BMI$`kg/dm²`
#> [1] "kg/dm²"
#> 
#> 
#> $Candela
#> $Candela$cd
#> [1] "cd"
#> 
#> 
#> $`CL per mass protein`
#> $`CL per mass protein`$`pl/min/mg mic. protein`
#> [1] "pl/min/mg mic. protein"
#> 
#> $`CL per mass protein`$`nl/min/mg mic. protein`
#> [1] "nl/min/mg mic. protein"
#> 
#> $`CL per mass protein`$`µl/min/mg mic. protein`
#> [1] "µl/min/mg mic. protein"
#> 
#> $`CL per mass protein`$`l/min/kg mic. protein`
#> [1] "l/min/kg mic. protein"
#> 
#> 
#> $`CL per recombinant enzyme`
#> $`CL per recombinant enzyme`$`µl/min/pmol rec. enzyme`
#> [1] "µl/min/pmol rec. enzyme"
#> 
#> $`CL per recombinant enzyme`$`ml/min/pmol rec. enzyme`
#> [1] "ml/min/pmol rec. enzyme"
#> 
#> $`CL per recombinant enzyme`$`l/min/µmol rec. enzyme`
#> [1] "l/min/µmol rec. enzyme"
#> 
#> 
#> $Compliance
#> $Compliance$`ml/mmHg`
#> [1] "ml/mmHg"
#> 
#> $Compliance$`ml/Pa`
#> [1] "ml/Pa"
#> 
#> $Compliance$`ml/(N/m²)`
#> [1] "ml/(N/m²)"
#> 
#> $Compliance$`ml/(dyn/cm²)`
#> [1] "ml/(dyn/cm²)"
#> 
#> $Compliance$`ml/(kg/(m*s²))`
#> [1] "ml/(kg/(m*s²))"
#> 
#> $Compliance$`l/(kg/(dm*min²))`
#> [1] "l/(kg/(dm*min²))"
#> 
#> 
#> $`Compliance [Area]`
#> $`Compliance [Area]`$`cm²/mmHg`
#> [1] "cm²/mmHg"
#> 
#> $`Compliance [Area]`$`cm²/Pa`
#> [1] "cm²/Pa"
#> 
#> $`Compliance [Area]`$`cm²/(N/m²)`
#> [1] "cm²/(N/m²)"
#> 
#> $`Compliance [Area]`$`cm²/(dyn/cm²)`
#> [1] "cm²/(dyn/cm²)"
#> 
#> $`Compliance [Area]`$`cm²/(kg/(m*s²))`
#> [1] "cm²/(kg/(m*s²))"
#> 
#> $`Compliance [Area]`$`dm²/(kg/(dm*min²))`
#> [1] "dm²/(kg/(dm*min²))"
#> 
#> 
#> $`Concentration [mass]`
#> $`Concentration [mass]`$`g/l`
#> [1] "g/l"
#> 
#> $`Concentration [mass]`$`mg/l`
#> [1] "mg/l"
#> 
#> $`Concentration [mass]`$`µg/l`
#> [1] "µg/l"
#> 
#> $`Concentration [mass]`$`ng/l`
#> [1] "ng/l"
#> 
#> $`Concentration [mass]`$`pg/l`
#> [1] "pg/l"
#> 
#> $`Concentration [mass]`$`mg/dl`
#> [1] "mg/dl"
#> 
#> $`Concentration [mass]`$`mg/ml`
#> [1] "mg/ml"
#> 
#> $`Concentration [mass]`$`µg/ml`
#> [1] "µg/ml"
#> 
#> $`Concentration [mass]`$`ng/ml`
#> [1] "ng/ml"
#> 
#> $`Concentration [mass]`$`pg/ml`
#> [1] "pg/ml"
#> 
#> $`Concentration [mass]`$`kg/l`
#> [1] "kg/l"
#> 
#> 
#> $`Concentration [molar]`
#> $`Concentration [molar]`$`mol/l`
#> [1] "mol/l"
#> 
#> $`Concentration [molar]`$`mmol/l`
#> [1] "mmol/l"
#> 
#> $`Concentration [molar]`$`µmol/l`
#> [1] "µmol/l"
#> 
#> $`Concentration [molar]`$`nmol/l`
#> [1] "nmol/l"
#> 
#> $`Concentration [molar]`$`pmol/l`
#> [1] "pmol/l"
#> 
#> $`Concentration [molar]`$`fmol/l`
#> [1] "fmol/l"
#> 
#> $`Concentration [molar]`$M
#> [1] "M"
#> 
#> $`Concentration [molar]`$mM
#> [1] "mM"
#> 
#> $`Concentration [molar]`$µM
#> [1] "µM"
#> 
#> $`Concentration [molar]`$nM
#> [1] "nM"
#> 
#> $`Concentration [molar]`$pM
#> [1] "pM"
#> 
#> $`Concentration [molar]`$fM
#> [1] "fM"
#> 
#> $`Concentration [molar]`$`mol/ml`
#> [1] "mol/ml"
#> 
#> $`Concentration [molar]`$`mmol/ml`
#> [1] "mmol/ml"
#> 
#> $`Concentration [molar]`$`µmol/ml`
#> [1] "µmol/ml"
#> 
#> $`Concentration [molar]`$`nmol/ml`
#> [1] "nmol/ml"
#> 
#> $`Concentration [molar]`$`pmol/ml`
#> [1] "pmol/ml"
#> 
#> $`Concentration [molar]`$`fmol/ml`
#> [1] "fmol/ml"
#> 
#> 
#> $`Concentration [molar] per time`
#> $`Concentration [molar] per time`$`µmol/l/min`
#> [1] "µmol/l/min"
#> 
#> $`Concentration [molar] per time`$`mol/l/s`
#> [1] "mol/l/s"
#> 
#> $`Concentration [molar] per time`$`mmol/l/s`
#> [1] "mmol/l/s"
#> 
#> $`Concentration [molar] per time`$`µmol/l/s`
#> [1] "µmol/l/s"
#> 
#> $`Concentration [molar] per time`$`nmol/l/s`
#> [1] "nmol/l/s"
#> 
#> $`Concentration [molar] per time`$`pmol/l/s`
#> [1] "pmol/l/s"
#> 
#> $`Concentration [molar] per time`$`µmol/ml/min`
#> [1] "µmol/ml/min"
#> 
#> $`Concentration [molar] per time`$`nmol/ml/min`
#> [1] "nmol/ml/min"
#> 
#> $`Concentration [molar] per time`$`pmol/ml/min`
#> [1] "pmol/ml/min"
#> 
#> 
#> $Coulomb
#> $Coulomb$C
#> [1] "C"
#> 
#> $Coulomb$`A*min`
#> [1] "A*min"
#> 
#> $Coulomb$`A*s`
#> [1] "A*s"
#> 
#> $Coulomb$`A*h`
#> [1] "A*h"
#> 
#> 
#> $Count
#> $Count$`#`
#> [1] "#"
#> 
#> $Count$`x10^6`
#> [1] "x10^6"
#> 
#> 
#> $`Count per mass`
#> $`Count per mass`$`x10^6/g`
#> [1] "x10^6/g"
#> 
#> $`Count per mass`$`x10^6/kg`
#> [1] "x10^6/kg"
#> 
#> 
#> $`Count per volume`
#> $`Count per volume`$`x10^6/ml`
#> [1] "x10^6/ml"
#> 
#> $`Count per volume`$`1/nl`
#> [1] "1/nl"
#> 
#> $`Count per volume`$`x10^6/l`
#> [1] "x10^6/l"
#> 
#> $`Count per volume`$`1/µl`
#> [1] "1/µl"
#> 
#> 
#> $`CV mmHg*s²/ml`
#> $`CV mmHg*s²/ml`$`(kg/dm)/l`
#> [1] "(kg/dm)/l"
#> 
#> $`CV mmHg*s²/ml`$`mmHg*s²/ml`
#> [1] "mmHg*s²/ml"
#> 
#> 
#> $`CV Viscosity`
#> $`CV Viscosity`$`kg/(dm*min)`
#> [1] "kg/(dm*min)"
#> 
#> $`CV Viscosity`$`s*mmHg`
#> [1] "s*mmHg"
#> 
#> 
#> $`CV Viscosity per Volume`
#> $`CV Viscosity per Volume`$`(kg/(dm*min))/l`
#> [1] "(kg/(dm*min))/l"
#> 
#> $`CV Viscosity per Volume`$`s*mmHg/ml`
#> [1] "s*mmHg/ml"
#> 
#> 
#> $Density
#> $Density$`g/cm³`
#> [1] "g/cm³"
#> 
#> $Density$`kg/m³`
#> [1] "kg/m³"
#> 
#> $Density$`kg/dm³`
#> [1] "kg/dm³"
#> 
#> 
#> $`Diffusion coefficient`
#> $`Diffusion coefficient`$`cm²/min`
#> [1] "cm²/min"
#> 
#> $`Diffusion coefficient`$`cm²/s`
#> [1] "cm²/s"
#> 
#> $`Diffusion coefficient`$`dm²/min`
#> [1] "dm²/min"
#> 
#> $`Diffusion coefficient`$`m²/s`
#> [1] "m²/s"
#> 
#> 
#> $Dimensionless
#> $Dimensionless$Unitless
#> [1] "Unitless"
#> 
#> 
#> $`Dose per body surface area`
#> $`Dose per body surface area`$`mg/m²`
#> [1] "mg/m²"
#> 
#> $`Dose per body surface area`$`kg/dm²`
#> [1] "kg/dm²"
#> 
#> $`Dose per body surface area`$`µg/cm²`
#> [1] "µg/cm²"
#> 
#> $`Dose per body surface area`$`mg/cm²`
#> [1] "mg/cm²"
#> 
#> 
#> $`Dose per body weight`
#> $`Dose per body weight`$`ng/kg`
#> [1] "ng/kg"
#> 
#> $`Dose per body weight`$`µg/kg`
#> [1] "µg/kg"
#> 
#> $`Dose per body weight`$`mg/kg`
#> [1] "mg/kg"
#> 
#> $`Dose per body weight`$`g/kg`
#> [1] "g/kg"
#> 
#> $`Dose per body weight`$`kg/kg`
#> [1] "kg/kg"
#> 
#> 
#> $Elastance
#> $Elastance$`mmHg/ml`
#> [1] "mmHg/ml"
#> 
#> $Elastance$`(kg/(dm*min²))/l`
#> [1] "(kg/(dm*min²))/l"
#> 
#> $Elastance$`Pa/ml`
#> [1] "Pa/ml"
#> 
#> $Elastance$`(N/m²)/ml`
#> [1] "(N/m²)/ml"
#> 
#> $Elastance$`(dyn/cm²)/ml`
#> [1] "(dyn/cm²)/ml"
#> 
#> $Elastance$`(kg/(m*s²))/ml`
#> [1] "(kg/(m*s²))/ml"
#> 
#> 
#> $Energy
#> $Energy$kcal
#> [1] "kcal"
#> 
#> $Energy$cal
#> [1] "cal"
#> 
#> $Energy$`kg*dm²/min²`
#> [1] "kg*dm²/min²"
#> 
#> $Energy$J
#> [1] "J"
#> 
#> $Energy$kJ
#> [1] "kJ"
#> 
#> 
#> $Farad
#> $Farad$F
#> [1] "F"
#> 
#> $Farad$`s/Ohm`
#> [1] "s/Ohm"
#> 
#> $Farad$`min/Ohm`
#> [1] "min/Ohm"
#> 
#> $Farad$`(A²*s^4)/(kg*m²)`
#> [1] "(A²*s^4)/(kg*m²)"
#> 
#> $Farad$`(A²*min^4)/(kg*dm²)`
#> [1] "(A²*min^4)/(kg*dm²)"
#> 
#> 
#> $Flow
#> $Flow$`l/h`
#> [1] "l/h"
#> 
#> $Flow$`l/min`
#> [1] "l/min"
#> 
#> $Flow$`l/s`
#> [1] "l/s"
#> 
#> $Flow$`ml/h`
#> [1] "ml/h"
#> 
#> $Flow$`ml/min`
#> [1] "ml/min"
#> 
#> $Flow$`ml/s`
#> [1] "ml/s"
#> 
#> 
#> $`Flow per body surface area`
#> $`Flow per body surface area`$`l/min/dm²`
#> [1] "l/min/dm²"
#> 
#> $`Flow per body surface area`$`ml/min/1.73m²`
#> [1] "ml/min/1.73m²"
#> 
#> 
#> $`Flow per weight`
#> $`Flow per weight`$`l/min/kg`
#> [1] "l/min/kg"
#> 
#> $`Flow per weight`$`l/h/kg`
#> [1] "l/h/kg"
#> 
#> $`Flow per weight`$`ml/min/kg`
#> [1] "ml/min/kg"
#> 
#> $`Flow per weight`$`ml/h/kg`
#> [1] "ml/h/kg"
#> 
#> 
#> $`Flow per weight organ`
#> $`Flow per weight organ`$`ml/min/100g organ`
#> [1] "ml/min/100g organ"
#> 
#> $`Flow per weight organ`$`ml/min/g organ`
#> [1] "ml/min/g organ"
#> 
#> $`Flow per weight organ`$`ml/min/kg organ`
#> [1] "ml/min/kg organ"
#> 
#> $`Flow per weight organ`$`l/h/100g organ`
#> [1] "l/h/100g organ"
#> 
#> $`Flow per weight organ`$`l/h/g organ`
#> [1] "l/h/g organ"
#> 
#> $`Flow per weight organ`$`l/h/kg organ`
#> [1] "l/h/kg organ"
#> 
#> $`Flow per weight organ`$`l/min/kg organ`
#> [1] "l/min/kg organ"
#> 
#> 
#> $`Flow²`
#> $`Flow²`$`(l/min)²`
#> [1] "(l/min)²"
#> 
#> $`Flow²`$`(ml/min)²`
#> [1] "(ml/min)²"
#> 
#> $`Flow²`$`(l/h)²`
#> [1] "(l/h)²"
#> 
#> $`Flow²`$`(ml/h)²`
#> [1] "(ml/h)²"
#> 
#> 
#> $Fraction
#> $Fraction$Unitless
#> [1] "Unitless"
#> 
#> $Fraction$`%`
#> [1] "%"
#> 
#> 
#> $Gray
#> $Gray$Gy
#> [1] "Gy"
#> 
#> $Gray$`J/kg`
#> [1] "J/kg"
#> 
#> $Gray$`m²/s²`
#> [1] "m²/s²"
#> 
#> $Gray$`dm²/min²`
#> [1] "dm²/min²"
#> 
#> 
#> $Henry
#> $Henry$H
#> [1] "H"
#> 
#> $Henry$`Wb/A`
#> [1] "Wb/A"
#> 
#> $Henry$`Ohm*s`
#> [1] "Ohm*s"
#> 
#> $Henry$`Ohm*min`
#> [1] "Ohm*min"
#> 
#> $Henry$`(kg*m²)/(A²*s²)`
#> [1] "(kg*m²)/(A²*s²)"
#> 
#> $Henry$`(kg*dm²)/(A²*min²)`
#> [1] "(kg*dm²)/(A²*min²)"
#> 
#> 
#> $Hertz
#> $Hertz$Hz
#> [1] "Hz"
#> 
#> $Hertz$`1/min`
#> [1] "1/min"
#> 
#> $Hertz$`1/s`
#> [1] "1/s"
#> 
#> $Hertz$`1/h`
#> [1] "1/h"
#> 
#> 
#> $`Hydraulic conductivity`
#> $`Hydraulic conductivity`$`l/min/(kg*dm/min²)`
#> [1] "l/min/(kg*dm/min²)"
#> 
#> $`Hydraulic conductivity`$`ml/min/N`
#> [1] "ml/min/N"
#> 
#> 
#> $`Inversed area`
#> $`Inversed area`$`1/µm²`
#> [1] "1/µm²"
#> 
#> $`Inversed area`$`1/mm²`
#> [1] "1/mm²"
#> 
#> $`Inversed area`$`1/cm²`
#> [1] "1/cm²"
#> 
#> $`Inversed area`$`1/dm²`
#> [1] "1/dm²"
#> 
#> 
#> $`Inversed concentration [molar]`
#> $`Inversed concentration [molar]`$`l/mol`
#> [1] "l/mol"
#> 
#> $`Inversed concentration [molar]`$`l/mmol`
#> [1] "l/mmol"
#> 
#> $`Inversed concentration [molar]`$`l/µmol`
#> [1] "l/µmol"
#> 
#> $`Inversed concentration [molar]`$`l/nmol`
#> [1] "l/nmol"
#> 
#> $`Inversed concentration [molar]`$`l/pmol`
#> [1] "l/pmol"
#> 
#> $`Inversed concentration [molar]`$`l/fmol`
#> [1] "l/fmol"
#> 
#> $`Inversed concentration [molar]`$`1/M`
#> [1] "1/M"
#> 
#> $`Inversed concentration [molar]`$`1/mM`
#> [1] "1/mM"
#> 
#> $`Inversed concentration [molar]`$`1/µM`
#> [1] "1/µM"
#> 
#> $`Inversed concentration [molar]`$`1/nM`
#> [1] "1/nM"
#> 
#> $`Inversed concentration [molar]`$`1/pM`
#> [1] "1/pM"
#> 
#> $`Inversed concentration [molar]`$`1/fM`
#> [1] "1/fM"
#> 
#> $`Inversed concentration [molar]`$`ml/mol`
#> [1] "ml/mol"
#> 
#> $`Inversed concentration [molar]`$`ml/mmol`
#> [1] "ml/mmol"
#> 
#> $`Inversed concentration [molar]`$`ml/µmol`
#> [1] "ml/µmol"
#> 
#> $`Inversed concentration [molar]`$`ml/nmol`
#> [1] "ml/nmol"
#> 
#> $`Inversed concentration [molar]`$`ml/pmol`
#> [1] "ml/pmol"
#> 
#> $`Inversed concentration [molar]`$`ml/fmol`
#> [1] "ml/fmol"
#> 
#> 
#> $`Inversed length`
#> $`Inversed length`$`1/cm`
#> [1] "1/cm"
#> 
#> $`Inversed length`$`1/dm`
#> [1] "1/dm"
#> 
#> $`Inversed length`$`1/m`
#> [1] "1/m"
#> 
#> 
#> $`Inversed mol`
#> $`Inversed mol`$`1/µmol`
#> [1] "1/µmol"
#> 
#> $`Inversed mol`$`1/mol`
#> [1] "1/mol"
#> 
#> 
#> $`Inversed time`
#> $`Inversed time`$`1/min`
#> [1] "1/min"
#> 
#> $`Inversed time`$`1/h`
#> [1] "1/h"
#> 
#> $`Inversed time`$`1/s`
#> [1] "1/s"
#> 
#> 
#> $`Inversed volume`
#> $`Inversed volume`$`1/l`
#> [1] "1/l"
#> 
#> $`Inversed volume`$`1/ml`
#> [1] "1/ml"
#> 
#> $`Inversed volume`$`1/µl`
#> [1] "1/µl"
#> 
#> 
#> $Joule
#> $Joule$J
#> [1] "J"
#> 
#> $Joule$`N*m`
#> [1] "N*m"
#> 
#> $Joule$`N*dm`
#> [1] "N*dm"
#> 
#> $Joule$`V*A*s`
#> [1] "V*A*s"
#> 
#> $Joule$`V*A*min`
#> [1] "V*A*min"
#> 
#> $Joule$`C*V`
#> [1] "C*V"
#> 
#> $Joule$`W*s`
#> [1] "W*s"
#> 
#> $Joule$`W*min`
#> [1] "W*min"
#> 
#> $Joule$`(kg*m²)/s²`
#> [1] "(kg*m²)/s²"
#> 
#> $Joule$`(kg*dm²)/min²`
#> [1] "(kg*dm²)/min²"
#> 
#> 
#> $Katal
#> $Katal$kat
#> [1] "kat"
#> 
#> $Katal$`mol/min`
#> [1] "mol/min"
#> 
#> $Katal$`mol/s`
#> [1] "mol/s"
#> 
#> $Katal$`mol/h`
#> [1] "mol/h"
#> 
#> $Katal$`µmol/min`
#> [1] "µmol/min"
#> 
#> 
#> $Kelvin
#> $Kelvin$K
#> [1] "K"
#> 
#> 
#> $Length
#> $Length$m
#> [1] "m"
#> 
#> $Length$dm
#> [1] "dm"
#> 
#> $Length$cm
#> [1] "cm"
#> 
#> $Length$mm
#> [1] "mm"
#> 
#> $Length$µm
#> [1] "µm"
#> 
#> $Length$nm
#> [1] "nm"
#> 
#> $Length$pm
#> [1] "pm"
#> 
#> 
#> $`Log Units`
#> $`Log Units`$`Log Units`
#> [1] "Log Units"
#> 
#> 
#> $Lumen
#> $Lumen$lm
#> [1] "lm"
#> 
#> $Lumen$`cd*sr`
#> [1] "cd*sr"
#> 
#> 
#> $Lux
#> $Lux$lx
#> [1] "lx"
#> 
#> $Lux$`lm/m²`
#> [1] "lm/m²"
#> 
#> $Lux$`lm/dm²`
#> [1] "lm/dm²"
#> 
#> $Lux$`(cd*sr)/m²`
#> [1] "(cd*sr)/m²"
#> 
#> $Lux$`(cd*sr)/dm²`
#> [1] "(cd*sr)/dm²"
#> 
#> $Lux$`(cd*sr)/cm²`
#> [1] "(cd*sr)/cm²"
#> 
#> 
#> $Mass
#> $Mass$kg
#> [1] "kg"
#> 
#> $Mass$g
#> [1] "g"
#> 
#> $Mass$mg
#> [1] "mg"
#> 
#> $Mass$µg
#> [1] "µg"
#> 
#> $Mass$ng
#> [1] "ng"
#> 
#> $Mass$pg
#> [1] "pg"
#> 
#> 
#> $`Mass per area`
#> $`Mass per area`$`µg/cm²`
#> [1] "µg/cm²"
#> 
#> $`Mass per area`$`mg/cm²`
#> [1] "mg/cm²"
#> 
#> $`Mass per area`$`mg/m²`
#> [1] "mg/m²"
#> 
#> $`Mass per area`$`kg/dm²`
#> [1] "kg/dm²"
#> 
#> 
#> $`Mass per area per time`
#> $`Mass per area per time`$`µg/cm²/h`
#> [1] "µg/cm²/h"
#> 
#> $`Mass per area per time`$`mg/cm²/h`
#> [1] "mg/cm²/h"
#> 
#> $`Mass per area per time`$`mg/m²/h`
#> [1] "mg/m²/h"
#> 
#> $`Mass per area per time`$`kg/dm²/min`
#> [1] "kg/dm²/min"
#> 
#> 
#> $`Mass per time`
#> $`Mass per time`$`kg/min`
#> [1] "kg/min"
#> 
#> $`Mass per time`$`g/min`
#> [1] "g/min"
#> 
#> $`Mass per time`$`mg/min`
#> [1] "mg/min"
#> 
#> $`Mass per time`$`µg/min`
#> [1] "µg/min"
#> 
#> $`Mass per time`$`ng/min`
#> [1] "ng/min"
#> 
#> $`Mass per time`$`pg/min`
#> [1] "pg/min"
#> 
#> $`Mass per time`$`kg/s`
#> [1] "kg/s"
#> 
#> $`Mass per time`$`g/s`
#> [1] "g/s"
#> 
#> $`Mass per time`$`mg/s`
#> [1] "mg/s"
#> 
#> $`Mass per time`$`µg/s`
#> [1] "µg/s"
#> 
#> $`Mass per time`$`ng/s`
#> [1] "ng/s"
#> 
#> $`Mass per time`$`pg/s`
#> [1] "pg/s"
#> 
#> 
#> $`Mass per tissue`
#> $`Mass per tissue`$`mg/g`
#> [1] "mg/g"
#> 
#> $`Mass per tissue`$`kg/kg`
#> [1] "kg/kg"
#> 
#> 
#> $`Molecular weight`
#> $`Molecular weight`$`kg/µmol`
#> [1] "kg/µmol"
#> 
#> $`Molecular weight`$`kg/mol`
#> [1] "kg/mol"
#> 
#> $`Molecular weight`$kDa
#> [1] "kDa"
#> 
#> $`Molecular weight`$`g/mol`
#> [1] "g/mol"
#> 
#> 
#> $Newton
#> $Newton$`(kg*dm)/min²`
#> [1] "(kg*dm)/min²"
#> 
#> $Newton$N
#> [1] "N"
#> 
#> $Newton$`(kg*cm)/s²`
#> [1] "(kg*cm)/s²"
#> 
#> $Newton$`(g*cm)/s²`
#> [1] "(g*cm)/s²"
#> 
#> 
#> $Ohm
#> $Ohm$Ohm
#> [1] "Ohm"
#> 
#> $Ohm$`V/A`
#> [1] "V/A"
#> 
#> $Ohm$`(kg*m²)/(A²*s³)`
#> [1] "(kg*m²)/(A²*s³)"
#> 
#> $Ohm$`(kg*dm²)/(A²*min³)`
#> [1] "(kg*dm²)/(A²*min³)"
#> 
#> 
#> $Pressure
#> $Pressure$mmHg
#> [1] "mmHg"
#> 
#> $Pressure$Pa
#> [1] "Pa"
#> 
#> $Pressure$`N/m²`
#> [1] "N/m²"
#> 
#> $Pressure$`dyn/cm²`
#> [1] "dyn/cm²"
#> 
#> $Pressure$`kg/(m*s²)`
#> [1] "kg/(m*s²)"
#> 
#> $Pressure$`kg/(dm*min²)`
#> [1] "kg/(dm*min²)"
#> 
#> 
#> $Radian
#> $Radian$rad
#> [1] "rad"
#> 
#> $Radian$`dm/dm`
#> [1] "dm/dm"
#> 
#> $Radian$`m/m`
#> [1] "m/m"
#> 
#> 
#> $Resistance
#> $Resistance$`mmHg*s/ml`
#> [1] "mmHg*s/ml"
#> 
#> $Resistance$`Pa*s/ml`
#> [1] "Pa*s/ml"
#> 
#> $Resistance$`N/m²*s/ml`
#> [1] "N/m²*s/ml"
#> 
#> $Resistance$`dyn/cm²*s/ml`
#> [1] "dyn/cm²*s/ml"
#> 
#> $Resistance$`kg/(m*s²)*s/ml`
#> [1] "kg/(m*s²)*s/ml"
#> 
#> $Resistance$W
#> [1] "W"
#> 
#> $Resistance$HRU
#> [1] "HRU"
#> 
#> $Resistance$`MPa*s/m³`
#> [1] "MPa*s/m³"
#> 
#> $Resistance$`Pa*s/m³`
#> [1] "Pa*s/m³"
#> 
#> $Resistance$`kg/(dm*min²)*s/m³`
#> [1] "kg/(dm*min²)*s/m³"
#> 
#> $Resistance$`dyn*s*cm−5`
#> [1] "dyn*s*cm−5"
#> 
#> $Resistance$`kg/(dm*min²)*min/l`
#> [1] "kg/(dm*min²)*min/l"
#> 
#> 
#> $Resolution
#> $Resolution$`pts/s`
#> [1] "pts/s"
#> 
#> $Resolution$`pts/min`
#> [1] "pts/min"
#> 
#> $Resolution$`pts/h`
#> [1] "pts/h"
#> 
#> $Resolution$`pts/day`
#> [1] "pts/day"
#> 
#> 
#> $RT
#> $RT$`N*cm/mol`
#> [1] "N*cm/mol"
#> 
#> $RT$`(kg*dm/min²)*dm/µmol`
#> [1] "(kg*dm/min²)*dm/µmol"
#> 
#> 
#> $`Second order rate constant`
#> $`Second order rate constant`$`l/mol/h`
#> [1] "l/mol/h"
#> 
#> $`Second order rate constant`$`l/mmol/h`
#> [1] "l/mmol/h"
#> 
#> $`Second order rate constant`$`l/µmol/h`
#> [1] "l/µmol/h"
#> 
#> $`Second order rate constant`$`l/nmol/h`
#> [1] "l/nmol/h"
#> 
#> $`Second order rate constant`$`l/pmol/h`
#> [1] "l/pmol/h"
#> 
#> $`Second order rate constant`$`l/mol/min`
#> [1] "l/mol/min"
#> 
#> $`Second order rate constant`$`l/mmol/min`
#> [1] "l/mmol/min"
#> 
#> $`Second order rate constant`$`l/µmol/min`
#> [1] "l/µmol/min"
#> 
#> $`Second order rate constant`$`l/nmol/min`
#> [1] "l/nmol/min"
#> 
#> $`Second order rate constant`$`l/pmol/min`
#> [1] "l/pmol/min"
#> 
#> $`Second order rate constant`$`l/mol/s`
#> [1] "l/mol/s"
#> 
#> $`Second order rate constant`$`l/mmol/s`
#> [1] "l/mmol/s"
#> 
#> $`Second order rate constant`$`l/µmol/s`
#> [1] "l/µmol/s"
#> 
#> $`Second order rate constant`$`l/nmol/s`
#> [1] "l/nmol/s"
#> 
#> $`Second order rate constant`$`l/pmol/s`
#> [1] "l/pmol/s"
#> 
#> 
#> $Siemens
#> $Siemens$S
#> [1] "S"
#> 
#> $Siemens$`A/V`
#> [1] "A/V"
#> 
#> $Siemens$`(A²*s³)/(kg*m²)`
#> [1] "(A²*s³)/(kg*m²)"
#> 
#> $Siemens$`(A²*min³)/(kg*dm²)`
#> [1] "(A²*min³)/(kg*dm²)"
#> 
#> 
#> $Sievert
#> $Sievert$Sv
#> [1] "Sv"
#> 
#> $Sievert$`J/kg`
#> [1] "J/kg"
#> 
#> $Sievert$`dm²/min²`
#> [1] "dm²/min²"
#> 
#> $Sievert$`m²/s²`
#> [1] "m²/s²"
#> 
#> 
#> $Slope
#> $Slope$`ml/mmHg/m²`
#> [1] "ml/mmHg/m²"
#> 
#> $Slope$`ml/(kg/(dm*min²))/m²`
#> [1] "ml/(kg/(dm*min²))/m²"
#> 
#> $Slope$`l/(kg/(dm*min²))/dm²`
#> [1] "l/(kg/(dm*min²))/dm²"
#> 
#> 
#> $Steradian
#> $Steradian$sr
#> [1] "sr"
#> 
#> $Steradian$`dm²/dm²`
#> [1] "dm²/dm²"
#> 
#> $Steradian$`m²/m²`
#> [1] "m²/m²"
#> 
#> 
#> $Temperature
#> $Temperature$`°C`
#> [1] "°C"
#> 
#> $Temperature$`°F`
#> [1] "°F"
#> 
#> $Temperature$K
#> [1] "K"
#> 
#> 
#> $Tesla
#> $Tesla$T
#> [1] "T"
#> 
#> $Tesla$`(V*s)/m²`
#> [1] "(V*s)/m²"
#> 
#> $Tesla$`(V*min)/dm²`
#> [1] "(V*min)/dm²"
#> 
#> $Tesla$`kg/(A*min²)`
#> [1] "kg/(A*min²)"
#> 
#> $Tesla$`kg/(A*s²)`
#> [1] "kg/(A*s²)"
#> 
#> 
#> $Time
#> $Time$s
#> [1] "s"
#> 
#> $Time$min
#> [1] "min"
#> 
#> $Time$h
#> [1] "h"
#> 
#> $Time$`day(s)`
#> [1] "day(s)"
#> 
#> $Time$`week(s)`
#> [1] "week(s)"
#> 
#> $Time$`month(s)`
#> [1] "month(s)"
#> 
#> $Time$`year(s)`
#> [1] "year(s)"
#> 
#> $Time$ks
#> [1] "ks"
#> 
#> 
#> $`Time²`
#> $`Time²`$`s²`
#> [1] "s²"
#> 
#> $`Time²`$`min²`
#> [1] "min²"
#> 
#> $`Time²`$`h²`
#> [1] "h²"
#> 
#> 
#> $Velocity
#> $Velocity$`cm/min`
#> [1] "cm/min"
#> 
#> $Velocity$`cm/s`
#> [1] "cm/s"
#> 
#> $Velocity$`dm/min`
#> [1] "dm/min"
#> 
#> 
#> $Viscosity
#> $Viscosity$`s/ml`
#> [1] "s/ml"
#> 
#> $Viscosity$`min/ml`
#> [1] "min/ml"
#> 
#> $Viscosity$`min/l`
#> [1] "min/l"
#> 
#> 
#> $`Vmax per mass protein`
#> $`Vmax per mass protein`$`pmol/min/mg mic. protein`
#> [1] "pmol/min/mg mic. protein"
#> 
#> $`Vmax per mass protein`$`nmol/min/mg mic. protein`
#> [1] "nmol/min/mg mic. protein"
#> 
#> $`Vmax per mass protein`$`µmol/min/mg mic. protein`
#> [1] "µmol/min/mg mic. protein"
#> 
#> $`Vmax per mass protein`$`µmol/min/kg mic. protein`
#> [1] "µmol/min/kg mic. protein"
#> 
#> 
#> $`Vmax per recombinant enzyme`
#> $`Vmax per recombinant enzyme`$`nmol/min/pmol rec. enzyme`
#> [1] "nmol/min/pmol rec. enzyme"
#> 
#> $`Vmax per recombinant enzyme`$`pmol/min/pmol rec. enzyme`
#> [1] "pmol/min/pmol rec. enzyme"
#> 
#> $`Vmax per recombinant enzyme`$`µmol/min/µmol rec. enzyme`
#> [1] "µmol/min/µmol rec. enzyme"
#> 
#> 
#> $`Vmax per transporter`
#> $`Vmax per transporter`$`nmol/min/pmol transporter`
#> [1] "nmol/min/pmol transporter"
#> 
#> $`Vmax per transporter`$`pmol/min/pmol transporter`
#> [1] "pmol/min/pmol transporter"
#> 
#> $`Vmax per transporter`$`µmol/min/µmol transporter`
#> [1] "µmol/min/µmol transporter"
#> 
#> 
#> $`Vmax per weight organ tissue`
#> $`Vmax per weight organ tissue`$`µmol/min/kg tissue`
#> [1] "µmol/min/kg tissue"
#> 
#> $`Vmax per weight organ tissue`$`µmol/min/g tissue`
#> [1] "µmol/min/g tissue"
#> 
#> $`Vmax per weight organ tissue`$`nmol/min/g tissue`
#> [1] "nmol/min/g tissue"
#> 
#> $`Vmax per weight organ tissue`$`pmol/min/g tissue`
#> [1] "pmol/min/g tissue"
#> 
#> 
#> $Volt
#> $Volt$V
#> [1] "V"
#> 
#> $Volt$`W/A`
#> [1] "W/A"
#> 
#> $Volt$`J/C`
#> [1] "J/C"
#> 
#> $Volt$`(N*m)/(A*s)`
#> [1] "(N*m)/(A*s)"
#> 
#> $Volt$`(N*dm)/(A*min)`
#> [1] "(N*dm)/(A*min)"
#> 
#> $Volt$`(kg*m²)/(A*s³)`
#> [1] "(kg*m²)/(A*s³)"
#> 
#> $Volt$`(kg*dm²)/(A*min³)`
#> [1] "(kg*dm²)/(A*min³)"
#> 
#> 
#> $Volume
#> $Volume$l
#> [1] "l"
#> 
#> $Volume$ml
#> [1] "ml"
#> 
#> $Volume$µl
#> [1] "µl"
#> 
#> 
#> $`Volume per body weight`
#> $`Volume per body weight`$`l/kg`
#> [1] "l/kg"
#> 
#> $`Volume per body weight`$`ml/kg`
#> [1] "ml/kg"
#> 
#> $`Volume per body weight`$`µl/kg`
#> [1] "µl/kg"
#> 
#> 
#> $Watt
#> $Watt$W
#> [1] "W"
#> 
#> $Watt$`J/s`
#> [1] "J/s"
#> 
#> $Watt$`J/min`
#> [1] "J/min"
#> 
#> $Watt$`V*A`
#> [1] "V*A"
#> 
#> $Watt$`(kg*dm²)/min³`
#> [1] "(kg*dm²)/min³"
#> 
#> $Watt$`(kg*m²)/s³`
#> [1] "(kg*m²)/s³"
#> 
#> $Watt$`(g*cm²)/s³`
#> [1] "(g*cm²)/s³"
#> 
#> 
#> $Weber
#> $Weber$Wb
#> [1] "Wb"
#> 
#> $Weber$`V*s`
#> [1] "V*s"
#> 
#> $Weber$`V*min`
#> [1] "V*min"
#> 
#> $Weber$`(kg*dm²)/(A*min²)`
#> [1] "(kg*dm²)/(A*min²)"
#> 
#> $Weber$`(kg*m²)/(A*s²)`
#> [1] "(kg*m²)/(A*s²)"
#> 
#> 
```
