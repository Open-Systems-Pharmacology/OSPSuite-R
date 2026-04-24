# Changing parameter and molecule start values

## Changing initial values

It is possible to change the initial values of all parameters and
molecules in a simulation. It is important to differentiate between the
*constant* values and the values that are defined by a *formula*.

### Formula types

Every (initial) value is described either by a constant or by a formula.
If the value is defined by a simple constant, the field `isConstant` of
the respective parameter or molecule has the value `TRUE`.

``` r
library(ospsuite)
#> The option 'ospsuite.plots.watermarkEnabled' is not set.
#> To enable watermarks, add the following to your .Rprofile:
#>   options(ospsuite.plots.watermarkEnabled = TRUE)
#> To disable watermarks, add:
#>   options(ospsuite.plots.watermarkEnabled = FALSE)
#> You can edit your .Rprofile with usethis::edit_r_profile()

# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter "Age" of the Organism
ageParam <- getParameter("Organism|Age", sim)
print(ageParam)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Age
#>   • Value: 25.70 [year(s)]
#> 
#> ── Formula ──
#> 
#>   • isConstant: TRUE
ageParam$isConstant
#> [1] TRUE
```

If the value is not constant, it is described by one of the formula
types:

**Distributed**: Distributed parameters describe a variation around a
constant value or between two numerical limits. In the simulation, the
behavior equals that of constant values. See [OSPS
documentation](https://docs.open-systems-pharmacology.org/working-with-mobi/mobi-documentation/model-building-components#working-with-constant-and-distributed-parameters)
for more information.

``` r
# Get the parameter "Volume" of the Liver
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
liverVolume$isDistributed
#> [1] TRUE
```

**Explicit formula**: The value of the parameter is given by an explicit
formula. The value of an explicit formula can change during the
simulation. The string of the formula can be accessed via
`parameter$formulaString`.

``` r
# Get the parameter "Volume" of the Liver interstital
liverIntVolume <- getParameter("Organism|Liver|Interstitial|Volume", sim)
print(liverIntVolume)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Interstitial|Volume
#>   • Value: 0.35 [l]
#>   • Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
#>   Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
#>   ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
#>   http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
#> 
#> ── Formula ──
#> 
#>   • isExplicit: TRUE
#>   • formula: f_int*V
#>   • Value overrides formula: FALSE
liverIntVolume$formulaString
#> [1] "f_int*V"
```

**Table formula**: The value of the parameter is given by a table with
x-y value pairs. X values usually refer to the simulation time. See
[Table
parameters](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/articles/table-parameters.md)
for additional information on how to retrieve or change the table
values.

``` r
# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|TableParameter" located under container <Vergin 1995 IV>!
print(tableParam)
#> Error:
#> ! object 'tableParam' not found
```

Additionally, some parameters are modeled as *state variables*. In this
case, the parameter has an *initial value* and a *right hand side (RHS)*
formula, both of which can be any formula type.

``` r
# Get the parameter defined by a state variable.
stateVariableParam <- getParameter("Organism|StateVariable_Parameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|StateVariable_Parameter" located under container <Vergin 1995 IV>!
print(stateVariableParam)
#> Error:
#> ! object 'stateVariableParam' not found

# `value` refers to the initial value of the parameter
stateVariableParam$value
#> Error:
#> ! object 'stateVariableParam' not found
# `rhsFormula` is the right hand side of the parameter
stateVariableParam$rhsFormula
#> Error:
#> ! object 'stateVariableParam' not found
```

### Changing parameters and molecules initial values

The user can change initial values of parameters and molecules with the
methods `setParameterValues` and `setMoleculeInitialValues`,
respectively.

``` r
# Get the parameter Dose
doseParamPath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
doseParam <- getParameter(doseParamPath, sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" located under container <Vergin 1995 IV>!
print(doseParam)
#> Error:
#> ! object 'doseParam' not found

# Change the dose to 350mg. The value has to be converted to base unit, first
newValue <- toBaseUnit(quantity = doseParam, values = 350, unit = "mg")
#> Error:
#> ! object 'doseParam' not found
setParameterValues(parameters = doseParam, values = newValue)
#> Error:
#> ! object 'doseParam' not found
print(doseParam)
#> Error:
#> ! object 'doseParam' not found
```

Another way to change parameter values is to scale them. The scaling is
always performed relative to the current value:

``` r
doseParamPath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
doseParam <- getParameter(doseParamPath, sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" located under container <Vergin 1995 IV>!
print(doseParam)
#> Error:
#> ! object 'doseParam' not found

# Double the dose
scaleParameterValues(doseParam, factor = 2)
#> Error:
#> ! object 'doseParam' not found
print(doseParam)
#> Error:
#> ! object 'doseParam' not found

# Half the dose
scaleParameterValues(doseParam, factor = 0.5)
#> Error:
#> ! object 'doseParam' not found
print(doseParam)
#> Error:
#> ! object 'doseParam' not found
```

Only constant values can be set. If the parameters value is defined by a
formula (explicit or table), the newly assigned value will override the
formula. This will be reflected by the field `isFixedValue`. Changing
values of formula parameters should be done with caution, as the
potential dependency on another simulation parameters will be destroyed.

``` r
# Get the parameter "Volume" of the Liver interstital
liverIntVolume <- getParameter("Organism|Liver|Interstitial|Volume", sim)
print(liverIntVolume)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Interstitial|Volume
#>   • Value: 0.35 [l]
#>   • Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
#>   Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
#>   ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
#>   http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
#> 
#> ── Formula ──
#> 
#>   • isExplicit: TRUE
#>   • formula: f_int*V
#>   • Value overrides formula: FALSE

# isFixedValue is FALSE as the parameter is defined by its formula
liverIntVolume$isFixedValue
#> [1] FALSE

setParameterValues(liverIntVolume, 1)
print(liverIntVolume)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Interstitial|Volume
#>   • Value: 1.00 [l]
#>   • Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
#>   Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
#>   ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
#>   http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
#> 
#> ── Formula ──
#> 
#>   • isExplicit: TRUE
#>   • formula: f_int*V
#>   • Value overrides formula: TRUE
# isFixedValue is TRUE as the value of parameter is overridden by the constant value
liverIntVolume$isFixedValue
#> [1] TRUE
```

The parameter value can be reset to its formula after assigning a
constant value:

``` r
print(liverIntVolume)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Interstitial|Volume
#>   • Value: 1.00 [l]
#>   • Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
#>   Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
#>   ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
#>   http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
#> 
#> ── Formula ──
#> 
#>   • isExplicit: TRUE
#>   • formula: f_int*V
#>   • Value overrides formula: TRUE

# isFixedValue is TRUE as the value of parameter is overridden by the constant value
liverIntVolume$isFixedValue
#> [1] TRUE

liverIntVolume$reset()
print(liverIntVolume)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Interstitial|Volume
#>   • Value: 0.35 [l]
#>   • Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
#>   Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
#>   ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
#>   http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
#> 
#> ── Formula ──
#> 
#>   • isExplicit: TRUE
#>   • formula: f_int*V
#>   • Value overrides formula: FALSE
liverIntVolume$isFixedValue
#> [1] FALSE
```

Changing the value of a state variable parameter will only change its
initial value, but not the right hand side (i.e., the value in the
simulation will still change according to the RHS formula). To switch to
RHS formula off and, by that, make the parameter be only dependent on
its initial value, the field `isStateVariable` can be set to `FALSE`.
This function is one-way only! It is not possible to re-activate the RHS
formula after switching it off.

``` r
# Get the parameter defined by a state variable.
stateVariableParam <- getParameter("Organism|StateVariable_Parameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|StateVariable_Parameter" located under container <Vergin 1995 IV>!
print(stateVariableParam)
#> Error:
#> ! object 'stateVariableParam' not found

# Setting its value only changes the initial value
setParameterValues(stateVariableParam, 10)
#> Error:
#> ! object 'stateVariableParam' not found
print(stateVariableParam)
#> Error:
#> ! object 'stateVariableParam' not found

# Switching the RHS formula off
stateVariableParam$isStateVariable <- FALSE
#> Error:
#> ! object 'stateVariableParam' not found
print(stateVariableParam)
#> Error:
#> ! object 'stateVariableParam' not found

# Switching it on is not supported
stateVariableParam$isStateVariable <- TRUE
#> Error:
#> ! object 'stateVariableParam' not found
```

An example how to set the initial values of molecules in all containers
to a certain value:

``` r
# Get objects representing the molecule Aciclovir in all containers
allAciclovirMolecules <- getAllMoleculesMatching("Organism|**|Aciclovir", sim)

# Set initial values to 10 µmol in all containers
setMoleculeInitialValues(allAciclovirMolecules, rep(10, length(allAciclovirMolecules)))
```
