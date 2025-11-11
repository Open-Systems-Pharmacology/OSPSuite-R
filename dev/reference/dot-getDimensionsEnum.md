# Function to return an enum of all available dimensions

Function to return an enum of all available dimensions

## Usage

``` r
.getDimensionsEnum()
```

## Value

enum of all dimensions

## Examples

``` r
ospsuite:::.getDimensionsEnum()
#> $`Abundance per mass protein`
#> [1] "Abundance per mass protein"
#> 
#> $`Abundance per tissue`
#> [1] "Abundance per tissue"
#> 
#> $`Age in weeks`
#> [1] "Age in weeks"
#> 
#> $`Age in years`
#> [1] "Age in years"
#> 
#> $Amount
#> [1] "Amount"
#> 
#> $`Amount per area`
#> [1] "Amount per area"
#> 
#> $`Amount per area per time`
#> [1] "Amount per area per time"
#> 
#> $`Amount per time`
#> [1] "Amount per time"
#> 
#> $Ampere
#> [1] "Ampere"
#> 
#> $Area
#> [1] "Area"
#> 
#> $`AUC (mass)`
#> [1] "AUC (mass)"
#> 
#> $`AUC (molar)`
#> [1] "AUC (molar)"
#> 
#> $`AUCM (molar)`
#> [1] "AUCM (molar)"
#> 
#> $Becquerel
#> [1] "Becquerel"
#> 
#> $BMI
#> [1] "BMI"
#> 
#> $Candela
#> [1] "Candela"
#> 
#> $`CL per mass protein`
#> [1] "CL per mass protein"
#> 
#> $`CL per recombinant enzyme`
#> [1] "CL per recombinant enzyme"
#> 
#> $Compliance
#> [1] "Compliance"
#> 
#> $`Compliance (Area)`
#> [1] "Compliance (Area)"
#> 
#> $`Concentration (mass)`
#> [1] "Concentration (mass)"
#> 
#> $`Concentration (molar)`
#> [1] "Concentration (molar)"
#> 
#> $`Concentration (molar) per time`
#> [1] "Concentration (molar) per time"
#> 
#> $Coulomb
#> [1] "Coulomb"
#> 
#> $Count
#> [1] "Count"
#> 
#> $`Count per mass`
#> [1] "Count per mass"
#> 
#> $`Count per volume`
#> [1] "Count per volume"
#> 
#> $`CV mmHg*s²/ml`
#> [1] "CV mmHg*s²/ml"
#> 
#> $`CV Viscosity`
#> [1] "CV Viscosity"
#> 
#> $`CV Viscosity per Volume`
#> [1] "CV Viscosity per Volume"
#> 
#> $Density
#> [1] "Density"
#> 
#> $`Diffusion coefficient`
#> [1] "Diffusion coefficient"
#> 
#> $Dimensionless
#> [1] "Dimensionless"
#> 
#> $`Dose per body surface area`
#> [1] "Dose per body surface area"
#> 
#> $`Dose per body weight`
#> [1] "Dose per body weight"
#> 
#> $Elastance
#> [1] "Elastance"
#> 
#> $Energy
#> [1] "Energy"
#> 
#> $Farad
#> [1] "Farad"
#> 
#> $Flow
#> [1] "Flow"
#> 
#> $`Flow per body surface area`
#> [1] "Flow per body surface area"
#> 
#> $`Flow per weight`
#> [1] "Flow per weight"
#> 
#> $`Flow per weight organ`
#> [1] "Flow per weight organ"
#> 
#> $`Flow²`
#> [1] "Flow²"
#> 
#> $Fraction
#> [1] "Fraction"
#> 
#> $Gray
#> [1] "Gray"
#> 
#> $Henry
#> [1] "Henry"
#> 
#> $Hertz
#> [1] "Hertz"
#> 
#> $`Hydraulic conductivity`
#> [1] "Hydraulic conductivity"
#> 
#> $`Inversed area`
#> [1] "Inversed area"
#> 
#> $`Inversed concentration (molar)`
#> [1] "Inversed concentration (molar)"
#> 
#> $`Inversed length`
#> [1] "Inversed length"
#> 
#> $`Inversed mol`
#> [1] "Inversed mol"
#> 
#> $`Inversed time`
#> [1] "Inversed time"
#> 
#> $`Inversed volume`
#> [1] "Inversed volume"
#> 
#> $Joule
#> [1] "Joule"
#> 
#> $Katal
#> [1] "Katal"
#> 
#> $Kelvin
#> [1] "Kelvin"
#> 
#> $Length
#> [1] "Length"
#> 
#> $`Log Units`
#> [1] "Log Units"
#> 
#> $Lumen
#> [1] "Lumen"
#> 
#> $Lux
#> [1] "Lux"
#> 
#> $Mass
#> [1] "Mass"
#> 
#> $`Mass per area`
#> [1] "Mass per area"
#> 
#> $`Mass per area per time`
#> [1] "Mass per area per time"
#> 
#> $`Mass per time`
#> [1] "Mass per time"
#> 
#> $`Mass per tissue`
#> [1] "Mass per tissue"
#> 
#> $`Molecular weight`
#> [1] "Molecular weight"
#> 
#> $Newton
#> [1] "Newton"
#> 
#> $Ohm
#> [1] "Ohm"
#> 
#> $Pressure
#> [1] "Pressure"
#> 
#> $Radian
#> [1] "Radian"
#> 
#> $Resistance
#> [1] "Resistance"
#> 
#> $Resolution
#> [1] "Resolution"
#> 
#> $RT
#> [1] "RT"
#> 
#> $`Second order rate constant`
#> [1] "Second order rate constant"
#> 
#> $Siemens
#> [1] "Siemens"
#> 
#> $Sievert
#> [1] "Sievert"
#> 
#> $Slope
#> [1] "Slope"
#> 
#> $Steradian
#> [1] "Steradian"
#> 
#> $Temperature
#> [1] "Temperature"
#> 
#> $Tesla
#> [1] "Tesla"
#> 
#> $Time
#> [1] "Time"
#> 
#> $`Time²`
#> [1] "Time²"
#> 
#> $Velocity
#> [1] "Velocity"
#> 
#> $Viscosity
#> [1] "Viscosity"
#> 
#> $`Vmax per mass protein`
#> [1] "Vmax per mass protein"
#> 
#> $`Vmax per recombinant enzyme`
#> [1] "Vmax per recombinant enzyme"
#> 
#> $`Vmax per transporter`
#> [1] "Vmax per transporter"
#> 
#> $`Vmax per weight organ tissue`
#> [1] "Vmax per weight organ tissue"
#> 
#> $Volt
#> [1] "Volt"
#> 
#> $Volume
#> [1] "Volume"
#> 
#> $`Volume per body weight`
#> [1] "Volume per body weight"
#> 
#> $Watt
#> [1] "Watt"
#> 
#> $Weber
#> [1] "Weber"
#> 
```
