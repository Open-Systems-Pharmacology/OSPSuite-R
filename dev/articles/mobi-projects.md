# Working with MoBi projects

## Working with MoBi projects

First, load a MoBi project:

``` r
# Load the ospsuite package
library(ospsuite)
```

    ## The option 'ospsuite.plots.watermarkEnabled' is not set.
    ## To enable watermarks, add the following to your .Rprofile:
    ##   options(ospsuite.plots.watermarkEnabled = TRUE)
    ## To disable watermarks, add:
    ##   options(ospsuite.plots.watermarkEnabled = FALSE)
    ## You can edit your .Rprofile with usethis::edit_r_profile()

``` r
projectPath <- system.file(
  "extdata",
  "TH_QST_Platform.mbp3",
  package = "ospsuite"
)
myProject <- loadMoBiProject(projectPath)
```

We can browse the contents of the project, e.g., get the list of
simulations in the project:

``` r
simulationsInProject <- myProject$simulationNames

ospsuite.utils::ospPrintItems(simulationsInProject, title = "Simulations")
```

    ## Simulations:
    ##   • Thyroid_QST_Human
    ##   • Thyroid_QST_Phenobarbital

get the list of parameter identifications:

``` r
PIs <- myProject$parameterIdentificationNames

ospsuite.utils::ospPrintItems(
  PIs,
  title = "Parameter identifications"
)
```

    ## Parameter identifications:
    ##   • Parameter Identification 1

get observed data from the project as `DataSet` objects:

``` r
obsData <- myProject$getObservedData()

print(names(obsData))
```

    ##  [1] "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A"  
    ##  [2] "Liu 1995_Total T3__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.5A" 
    ##  [3] "Liu 1995_Total T3__Rat__VenousBlood_Plasma_91 mg/kg/day_po_Fig.5A" 
    ##  [4] "Liu 1995_Total T3__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.5A"
    ##  [5] "Liu 1995_Total T3__Rat__VenousBlood_Plasma_179 mg/kg/day_po_Fig.5A"
    ##  [6] "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.6"  
    ##  [7] "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.6" 
    ##  [8] "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_91 mg/kg/day_po_Fig.6" 
    ##  [9] "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.6"
    ## [10] "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_179 mg/kg/day_po_Fig.6"
    ## [11] "Liu 1995_Total T4__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.2"   
    ## [12] "Liu 1995_Total T4__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.2"  
    ## [13] "Liu 1995_Total T4__Rat__VenousBlood_Plasma_91 mg/kg/day_po_Fig.2"  
    ## [14] "Liu 1995_Total T4__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.2" 
    ## [15] "Liu 1995_Total T4__Rat__VenousBlood_Plasma_179 mg/kg/day_po_Fig.2"

get the names of individuals and expression profiles from the project:

``` r
individualsInProject <- myProject$individualNames
expProfilesInProject <- myProject$expressionProfilesNames

ospsuite.utils::ospPrintItems(individualsInProject, title = "Individuals names")
```

    ## Individuals names:
    ##   • Human
    ##   • Rat

``` r
ospsuite.utils::ospPrintItems(
  expProfilesInProject,
  title = "Expression profiles names"
)
```

    ## Expression profiles names:
    ##   • UDPGT1|Human|Healthy
    ##   • DIO1|Human|Healthy
    ##   • DIO3|Human|Healthy
    ##   • UDPGT2|Human|Healthy
    ##   • UGT1A1|Rat|Healthy
    ##   • PB-LiverBindingPartner|Human|Healthy

get the names of the modules in the project:

``` r
modulesInProject <- myProject$moduleNames

ospsuite.utils::ospPrintItems(modulesInProject, title = "Modules")
```

    ## Modules:
    ##   • Thyroid_QST
    ##   • TH_activeTransports
    ##   • Pituitary
    ##   • Phenobarbital_Extension
    ##   • Phenobarbital_PBPK
    ##   • Endogenous_TH
    ##   • TH_plasma_binding
    ##   • Thyroid
    ##   • Rat physiology

and retrieve the respective objects by their names using the methods
`getSimulation`, `getObservedData`, `getIndividual`,
`getExpressionProfiles`, and `getModules`.

For modules, it is possible to retrieve the initial conditions and
parameter values building blocks:

``` r
# Get the module "Thyroid" from the project
module <- myProject$getModules("Thyroid")[[1]]
print(module)
```

    ## <MoBiModule>
    ##   • Name: Thyroid
    ##   • PK-Sim module: FALSE
    ##   • Merge behavior: Extend
    ## Parameter Values Building Blocks:
    ##   • Human
    ##   • Rat
    ## Initial Conditions Building Blocks:
    ##   • Human

``` r
icBBs <- module$getInitialConditionsBBs()

# Get all parameter values BBs
pvBBs <- module$getParameterValuesBBs()
```

Each module has a merge behavior that controls how it is combined with
other modules when building a simulation. The merge behavior is
accessible via the `mergeBehavior` field and can be set to either
`"Extend"` (default) or `"Overwrite"`. Details on the merge behavior and
how it affects the combination of modules can be found in the [MoBi
documentation](https://docs.open-systems-pharmacology.org/working-with-mobi/mobi-documentation/modularization-concept#creating-simulations-from-modules-and-combination-rules).

``` r
# Inspect the current merge behavior
print(module$mergeBehavior)
```

    ## [1] "Extend"

``` r
# Change the merge behavior to "Overwrite"
module$mergeBehavior <- "Overwrite"
print(module$mergeBehavior)
```

    ## [1] "Overwrite"

``` r
# Reset back to the default
module$mergeBehavior <- "Extend"
```

You can easily convert the retrieved initial conditions, parameter
values, individuals, and expression profiles to data frames for further
processing:

``` r
icBBDf <- initialConditionsBBToDataFrame(icBBs[[1]])
# Print the first few rows of the data frame
head(icBBDf)
```

    ##                Container Path Molecule Name Is Present Value Unit Scale Divisor
    ## 1     Organism|Thyroid|Plasma          DIO1       TRUE   NaN µmol             1
    ## 2     Organism|Thyroid|Plasma            T3       TRUE     0 µmol             1
    ## 3     Organism|Thyroid|Plasma            T4       TRUE     0 µmol             1
    ## 4     Organism|Thyroid|Plasma           TSH       TRUE     0 µmol             1
    ## 5 Organism|Thyroid|BloodCells          DIO1       TRUE   NaN µmol             1
    ## 6 Organism|Thyroid|BloodCells            T3       TRUE     0 µmol             1
    ##   Neg. Values Allowed
    ## 1               FALSE
    ## 2               FALSE
    ## 3               FALSE
    ## 4               FALSE
    ## 5               FALSE
    ## 6               FALSE

``` r
pvBBDf <- parameterValuesBBToDataFrame(pvBBs[["Rat"]])
# Print the first few rows of the data frame
head(pvBBDf)
```

    ##     Container Path                   Parameter Name     Value     Unit
    ## 1 Organism|Thyroid Acidic phospholipids [mg/g] - RR 3.774e-01         
    ## 2 Organism|Thyroid    Albumin ratio (tissue/plasma) 5.000e-02         
    ## 3 Organism|Thyroid Albumin ratio (tissue/plasma)-PT 5.000e-02         
    ## 4 Organism|Thyroid          Allometric scale factor 7.500e-01         
    ## 5 Organism|Thyroid                     Cell Density 5.089e+05 x10^6/kg
    ## 6 Organism|Thyroid            Fraction interstitial 1.000e-01         
    ##   Value Origin
    ## 1             
    ## 2             
    ## 3             
    ## 4             
    ## 5             
    ## 6

``` r
individual <- myProject$getIndividual("Rat")
individualDf <- individualsBBToDataFrame(individual)
# Print the individual data frame
head(individualDf)
```

    ##   Container Path                                   Parameter Name Value Unit
    ## 1       Organism Ontogeny factor (alpha1-acid glycoprotein) table 1.000     
    ## 2       Organism                  Ontogeny factor (albumin) table 1.000     
    ## 3       Organism                                       Hematocrit 0.450     
    ## 4       Organism                                 pH (blood cells) 7.220     
    ## 5       Organism                                      pH (plasma) 7.400     
    ## 6       Organism                               Vf (lipid, plasma) 0.007     
    ##                                                                                                                                                                                                 Value Origin
    ## 1                                                                                                                                                                                                           
    ## 2                                                                                                                                                                                                           
    ## 3                                                                               Publication-Davies B, Morris T. Physiological parameters in laboratory animals and humans. Pharm Res. 1993 Jul;10(7):1093-5.
    ## 4 Publication-ICRP, 2002. Basic Anatomical and Physiological Data for Use in Radiological Protection Reference Values. ICRP Publication 89. Ann. ICRP 32 (3-4). https://doi.org/10.1016/0146-6453(79)90123-4
    ## 5 Publication-ICRP, 2002. Basic Anatomical and Physiological Data for Use in Radiological Protection Reference Values. ICRP Publication 89. Ann. ICRP 32 (3-4). https://doi.org/10.1016/0146-6453(79)90123-4
    ## 6 Publication-ICRP, 2002. Basic Anatomical and Physiological Data for Use in Radiological Protection Reference Values. ICRP Publication 89. Ann. ICRP 32 (3-4). https://doi.org/10.1016/0146-6453(79)90123-4

``` r
expProfiles <- myProject$getExpressionProfiles("UDPGT1|Human|Healthy")
expProfilesDf <- expressionProfileBBToDataFrame(expProfiles)
# Print the first few rows of the data frame
head(expProfilesDf$expressionParameters)
```

    ##                             Container Path        Parameter Name Value   Unit
    ## 1       Organism|VenousBlood|Plasma|UDPGT1 Initial concentration   NaN µmol/l
    ## 2   Organism|VenousBlood|BloodCells|UDPGT1 Initial concentration   NaN µmol/l
    ## 3     Organism|ArterialBlood|Plasma|UDPGT1 Initial concentration   NaN µmol/l
    ## 4 Organism|ArterialBlood|BloodCells|UDPGT1 Initial concentration   NaN µmol/l
    ## 5              Organism|Bone|Plasma|UDPGT1 Initial concentration   NaN µmol/l
    ## 6          Organism|Bone|BloodCells|UDPGT1 Initial concentration   NaN µmol/l
    ##   Value Origin
    ## 1             
    ## 2             
    ## 3             
    ## 4             
    ## 5             
    ## 6

``` r
head(expProfilesDf$initialConditions)
```

    ##                      Container Path Molecule Name Is Present Value Unit
    ## 1       Organism|VenousBlood|Plasma        UDPGT1       TRUE   NaN µmol
    ## 2   Organism|VenousBlood|BloodCells        UDPGT1       TRUE   NaN µmol
    ## 3     Organism|ArterialBlood|Plasma        UDPGT1       TRUE   NaN µmol
    ## 4 Organism|ArterialBlood|BloodCells        UDPGT1       TRUE   NaN µmol
    ## 5              Organism|Bone|Plasma        UDPGT1       TRUE   NaN µmol
    ## 6          Organism|Bone|BloodCells        UDPGT1       TRUE   NaN µmol
    ##   Scale Divisor Neg. Values Allowed
    ## 1             1               FALSE
    ## 2             1               FALSE
    ## 3             1               FALSE
    ## 4             1               FALSE
    ## 5             1               FALSE
    ## 6             1               FALSE

## Simulation configurations

Each simulation has a `SimulationConfiguration` object that contains the
modules used in the simulation, the Individual, the Expression Profiles,
selection of the Initial Conditions and Parameter Values building
blocks, and selection of partitioning coefficients for the molecules in
the simulation. Additionally, it can contain settings for the simulation
time, output settings, and solver settings.

The user can create a new simulation configuration either from scratch,
or alter an existing configuration retrieved from a simulation, or
create a configuration from modules in a project.

### Get simulation configuration from simulation

One way to get a simulation configuration is to retrieve a simulation
either from a project or by loading a simulation from a pkml file. The
simulation configuration can then be altered afterwards.

*NOTE:* Only simulations created with OSPS version \>=12 have simulation
configurations.

Get a configuration of a simulation loaded from a pkml file:

``` r
simulation <- loadSimulation(system.file(
  "extdata",
  "Aciclovir.pkml",
  package = "ospsuite"
))
configuration <- simulation$configuration

print(configuration)
```

    ## <SimulationConfiguration>
    ## 
    ## ── Modules ─────────────────────────────────────────────────────────────────────
    ## 
    ## ── Vergin 1995 IV ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Parameter Values
    ## 
    ## ────────────────────────────────────────────────────────────────────────────────
    ## Individual:
    ##   • Vergin_1995_IV
    ## Expression profiles:
    ##   • CYP3A4|Human|Healthy
    ## Partition coefficient method overrides:
    ##   • Aciclovir: PK-Sim Standard
    ## Cellular permeability method overrides:
    ##   • Aciclovir: PK-Sim Standard

Get a configuration of a simulation from a MoBi project:

``` r
simulation <- myProject$getSimulation("Thyroid_QST_Human")
configuration <- simulation$configuration

print(configuration)
```

    ## <SimulationConfiguration>
    ## 
    ## ── Modules ─────────────────────────────────────────────────────────────────────
    ## 
    ## ── Thyroid_QST ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Parameter Values
    ## 
    ## ── Thyroid ──
    ## 
    ##   • Selected Initial Conditions: Human
    ##   • Selected Parameter Values: Human
    ## 
    ## ── Pituitary ──
    ## 
    ##   • Selected Initial Conditions: Pituitary
    ##   • Selected Parameter Values: Human
    ## 
    ## ── Endogenous_TH ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Human
    ## 
    ## ── TH_activeTransports ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Parameters
    ## 
    ## ── TH_plasma_binding ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Human
    ## 
    ## ────────────────────────────────────────────────────────────────────────────────
    ## Individual:
    ##   • Human
    ## Expression profiles:
    ##   • UDPGT1|Human|Healthy
    ##   • DIO1|Human|Healthy
    ##   • DIO3|Human|Healthy
    ##   • UDPGT2|Human|Healthy

### Simulation configuration and simulation settings

When retrieving a simulation configuration from a simulation, the
simulation settings (time settings, output settings, solver settings)
are part of the simulation configuration. These settings will be applied
when creating a simulation using the configuration. However, the
simulation settings cannot be directly modified. To change these
settings, the user must first create a simulation from the configuration
and then modify the settings of the created simulation (see the article
on [simulations](#run-simulation)).

### Create simulation configuration from the project

The user can create new simulation configuration from the modules
available in the project. In this example, we create a simulation for
the rat species from the modules “Thyroid_QST”, “Rat physiology”,
“Thyroid”, “Pituitary”, “Endogenous_TH”, “TH_activeTransports”, and
“TH_plasma_binding”. We will select the expression profiles
“UDPGT1\|Human\|Healthy”, “DIO1\|Human\|Healthy”,
“DIO3\|Human\|Healthy”, “UDPGT2\|Human\|Healthy”, and specify the
parameter values building block “Rat” from the module “Thyroid”:

``` r
simConfiguration <- myProject$createSimulationConfiguration(
  modulesNames = c(
    "Thyroid_QST",
    "Rat physiology",
    "Thyroid",
    "Pituitary",
    "Endogenous_TH",
    "TH_activeTransports",
    "TH_plasma_binding"
  ),
  individualName = "Rat",
  expressionProfilesNames = c(
    "UDPGT1|Human|Healthy",
    "DIO1|Human|Healthy",
    "DIO3|Human|Healthy",
    "UDPGT2|Human|Healthy"
  ),
  selectedParameterValues = list("Thyroid" = "Rat")
)
```

### Create simulation configuration from modules

An alternative to creating simulation configurations from modules from a
project is to use the function
[`createSimulationConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createSimulationConfiguration.md)
and provide a list of `MoBiModule` objects. This allows to create
simulations from modules contained in different projects, or from
modules that are stored as pkml files. In the following example, we will
create a simulation configuration using the modules from the project as
in the example before, but the extension module “Thyroid” will be loaded
from a pkml file, and a new individual will be created.

``` r
# # Get modules from the project
modules <- myProject$getModules()
# Get the expression profiles from the project
expressionProfiles <- myProject$getExpressionProfiles(
  names = c(
    "UDPGT1|Human|Healthy",
    "DIO1|Human|Healthy",
    "DIO3|Human|Healthy",
    "UDPGT2|Human|Healthy"
  )
)
# Create an individual building block
myIndividual <- createIndividualBuildingBlock(
  species = Species$Human,
  population = HumanPopulation$Asian_Tanaka_1996,
  gender = Gender$Male,
  weight = 79,
  height = 175,
  age = 35
)
```

Modules that are intended to be used in combination with a variety of
other modules, especially with every generic PBPK module, are usually
set up in a generic way and require some extensions in order to be used
directly. For example, the initial conditions of the module “Thyroid”
must be extended by the molecules of the model it will be used with.
Similarly, some compound-specific parameter could require manual
adjustments. Therefore, initial conditions and parameter values building
block can be modified from R.

A module’s `Molecules` building block is exposed via
`MoBiModule$getMoleculesBB()`, which returns a `MoleculesBuildingBlock`
object. The methods below let you query molecule names from the building
block, which is useful when extending other building blocks (initial
conditions, parameter values, expression parameters) for the molecules
defined in the module:

- `allMoleculeNames()`, `allFloatingMoleculeNames()`,
  `allStationaryMoleculeNames()`: Names of all molecules, all floating
  molecules (e.g., drugs, metabolites), or all stationary molecules
  (e.g., enzymes, transporters).
- `allMoleculeNamesOfType(MoleculeType$<type>)`: Names of all molecules
  of a given type. Pass `MoleculeType$Protein` for the union of
  `Enzyme`, `Transporter`, and `Binding Partner`. Other supported keys
  are `Drug`, `Metabolite`, `Enzyme`, `Transporter`, `Binding Partner`,
  and `Complex`.
- `allXenobioticFloatingMoleculeNames()`,
  `allEndogenousStationaryMoleculeNames()`: Names of xenobiotic floating
  (drug-like) or endogenous stationary (protein-like) molecules.
- `moleculeTypeFor(moleculeName)`: Type of a single molecule by name
  (e.g., `"Drug"`, `"Enzyme"`, `"Transporter"`, `"Binding Partner"`).

``` r
moleculesBB <- modules[["Thyroid_QST"]]$getMoleculesBB()
moleculesBB$allMoleculeNames()
moleculesBB$allMoleculeNamesOfType(MoleculeType$Protein)
moleculesBB$moleculeTypeFor("DIO1")
```

Following functionalities are available for the Initial Conditions BBs:

- [`setInitialConditionsInBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setInitialConditionsInBB.md):
  Allows adding new entries or changing the values of existing entries
  in the Initial Conditions BB.
- [`deleteInitialConditionsFromBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/deleteInitialConditionsFromBB.md):
  Allows deleting entries from the Initial Conditions BB.
- [`extendInitialConditionsBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/extendInitialConditionsBB.md):
  Extending an IC BB by entries for specified molecules in all physical
  containers of a spatial structure.

Following functionalities are available for the Parameter Values (PV)
BBs:

- [`setParameterValuesInBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setParameterValuesInBB.md):
  Allows adding new entries or changing the values of existing entries
  in the PV BB.
- [`deleteParameterValuesFromBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/deleteParameterValuesFromBB.md):
  Allows deleting entries from the PV BB.
- [`addLocalMoleculeParametersToParameterValuesBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addLocalMoleculeParametersToParameterValuesBB.md):
  Extending a PV BB by entries for local parameters of specified
  molecules in all physical containers of a spatial structure. This
  function does not add parameters whose values are defined by a
  formula”, e.g., the “Relative expression” parameters for proteins.
- [`addProteinExpressionToParameterValuesBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addProteinExpressionToParameterValuesBB.md):
  Extending a PV BB by parameters defining expression of protein
  molecules in selected organs.

Furthermore, for building blocks of type “Parameter Values”, “Initial
Conditions”, “Expression Profiles”, and “Individual”, there are
functions to export them to a PKML (`.pkml`) file.

So in the first step we load the extension module “Thyroid” from a pkml
file:

``` r
thyroidModule <- loadModuleFromPKML(system.file(
  "extdata",
  "Thyroid.pkml",
  package = "ospsuite"
))
```

Then we extend the initial conditions BB of the loaded module by
molecules defined in other modules of our project:

``` r
# Get the IC BB from the Thyroid module. The module has only one IC BB.
icBB <- thyroidModule$getInitialConditionsBBs()[[1]]
# Extend the IC BB of the module "Thyroid" with molecules from base module "Thyroid_QST"
newPaths <- extendInitialConditionsBB(
  initialConditionsBuildingBlock = icBB,
  spatialStructureModule = thyroidModule,
  moleculesModule = modules[["Thyroid_QST"]],
  moleculeNames = c("DIO1", "T3", "T4", "TSH")
)

print(newPaths)
```

    ##  [1] "Organism|Thyroid|Plasma|DIO1"        "Organism|Thyroid|Plasma|T3"         
    ##  [3] "Organism|Thyroid|Plasma|T4"          "Organism|Thyroid|Plasma|TSH"        
    ##  [5] "Organism|Thyroid|BloodCells|DIO1"    "Organism|Thyroid|BloodCells|T3"     
    ##  [7] "Organism|Thyroid|BloodCells|T4"      "Organism|Thyroid|BloodCells|TSH"    
    ##  [9] "Organism|Thyroid|Interstitial|DIO1"  "Organism|Thyroid|Interstitial|T3"   
    ## [11] "Organism|Thyroid|Interstitial|T4"    "Organism|Thyroid|Interstitial|TSH"  
    ## [13] "Organism|Thyroid|Intracellular|DIO1" "Organism|Thyroid|Intracellular|T3"  
    ## [15] "Organism|Thyroid|Intracellular|T4"   "Organism|Thyroid|Intracellular|TSH" 
    ## [17] "Organism|Thyroid|Endosome|DIO1"      "Organism|Thyroid|Endosome|T3"       
    ## [19] "Organism|Thyroid|Endosome|T4"        "Organism|Thyroid|Endosome|TSH"      
    ## [21] "Organism|Thyroid|Lumen|DIO1"         "Organism|Thyroid|Lumen|T3"          
    ## [23] "Organism|Thyroid|Lumen|T4"           "Organism|Thyroid|Lumen|TSH"

The molecule “DIO1” that has been added to the IC BB in the previous
step is a protein. As the module “Thyroid” adds a new organ for which no
protein expression is defined in the Expression Profiles, parameters
defining the expression must be added to the PV BB of the new module
using the
[`addProteinExpressionToParameterValuesBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addProteinExpressionToParameterValuesBB.md)
function. Default values for the added parameters are taken from
reference Expression Profiles supplied as a plain list; the function
matches each entry to a molecule in `moleculeNames` by reading the
profile’s `MoleculeName`. The organs for which expression parameters
should be created are specified via `organPaths`; each path must resolve
to an existing organ in the spatial structure.

Both `moleculeNames` and `referenceExpressionProfiles` are optional:

- When `moleculeNames = NULL` (default), all proteins (`Enzyme`,
  `Transporter`, `Binding Partner`) defined in the Molecules building
  block of `moleculesModule` are used.
- When `referenceExpressionProfiles = NULL` or only a subset of profiles
  is supplied, default expression profiles for species `"Human"` are
  auto-created via
  [`createExpressionProfileBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createExpressionProfileBuildingBlock.md)
  for each molecule without a supplied profile. The profile category is
  derived from `moleculeTypeFor()`: `Enzyme` -\> `Metabolizing Enzyme`,
  `Transporter` -\> `Transport Protein`, `Binding Partner` -\>
  `Protein Binding Partner`.

``` r
# Get the PV BB from the Thyroid module
pvBB <- thyroidModule$getParameterValuesBBs()[["Human"]]
# Add protein expression to the PV BB
proteinExpressionPaths <- addProteinExpressionToParameterValuesBB(
  parameterValuesBuildingBlock = pvBB,
  spatialStructureModule = thyroidModule,
  moleculesModule = modules[["Thyroid_QST"]],
  moleculeNames = c("DIO1"),
  referenceExpressionProfiles = list(expressionProfiles[["DIO1|Human|Healthy"]]),
  organPaths = "Organism|Thyroid"
)

print(proteinExpressionPaths)
```

    ## [1] "Organism|Thyroid|Plasma|DIO1|Initial concentration"                  
    ## [2] "Organism|Thyroid|BloodCells|DIO1|Initial concentration"              
    ## [3] "Organism|Thyroid|Interstitial|DIO1|Initial concentration"            
    ## [4] "Organism|Thyroid|Interstitial|DIO1|Fraction expressed interstitial"  
    ## [5] "Organism|Thyroid|Intracellular|DIO1|Relative expression"             
    ## [6] "Organism|Thyroid|Intracellular|DIO1|Initial concentration"           
    ## [7] "Organism|Thyroid|Intracellular|DIO1|Fraction expressed intracellular"
    ## [8] "Organism|Thyroid|Endosome|DIO1|Initial concentration"                
    ## [9] "Organism|Thyroid|Lumen|DIO1|Initial concentration"

The same call without explicit `moleculeNames` or
`referenceExpressionProfiles` adds expression parameters for every
protein defined in the molecules module, using auto-created `"Human"`
default profiles for any molecule without a supplied reference profile:

``` r
addProteinExpressionToParameterValuesBB(
  parameterValuesBuildingBlock = pvBB,
  spatialStructureModule = thyroidModule,
  moleculesModule = modules[["Thyroid_QST"]],
  organPaths = "Organism|Thyroid"
)
```

and overwrite the value of “Relative expression”:

``` r
setParameterValuesInBB(
  parameterValuesBuildingBlock = pvBB,
  quantityPaths = "Organism|Thyroid|Intracellular|DIO1|Relative expression",
  quantityValues = 1.33,
  units = "",
  dimensions = ospDimensions$Fraction
)
```

Finally, we can create a simulation configuration from the modules and
the individual we created:

``` r
simulationConfigurationHuman <- createSimulationConfiguration(
  modules = c(
    modules[["Thyroid_QST"]],
    thyroidModule,
    modules[["Pituitary"]],
    modules[["Endogenous_TH"]],
    modules[["TH_activeTransports"]],
    modules[["TH_plasma_binding"]]
  ),
  individual = myIndividual,
  expressionProfiles = expressionProfiles,
  selectedParameterValues = list("Thyroid" = "Human")
)

print(simulationConfigurationHuman)
```

    ## <SimulationConfiguration>
    ## 
    ## ── Modules ─────────────────────────────────────────────────────────────────────
    ## 
    ## ── Thyroid_QST ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Parameter Values
    ## 
    ## ── Thyroid_Generic ──
    ## 
    ##   • Selected Initial Conditions: Human
    ##   • Selected Parameter Values: Human
    ## 
    ## ── Pituitary ──
    ## 
    ##   • Selected Initial Conditions: Pituitary
    ##   • Selected Parameter Values: Human
    ## 
    ## ── Endogenous_TH ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Human
    ## 
    ## ── TH_activeTransports ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Parameters
    ## 
    ## ── TH_plasma_binding ──
    ## 
    ##   • Selected Initial Conditions: Initial Conditions
    ##   • Selected Parameter Values: Human
    ## 
    ## ────────────────────────────────────────────────────────────────────────────────
    ## Individual:
    ##   • Human
    ## Expression profiles:
    ##   • UDPGT1|Human|Healthy
    ##   • DIO1|Human|Healthy
    ##   • DIO3|Human|Healthy
    ##   • UDPGT2|Human|Healthy

## Creating simulations

The simulation configuration can be used to create a simulation. The
simulation can then be saved to a pkml file, or used for further
analysis.

``` r
simulation <- myProject$getSimulation("Thyroid_QST_Human")
configuration <- simulation$configuration

simulation <- createSimulation(
  simulationName = "Thyroid_QST_Human_copy",
  configuration
)
```

### Warnings and errors during simulation creation

[`createSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createSimulation.md)
validates the simulation configuration and reports any issues
encountered during the creation process. If the simulation cannot be
created (e.g., due to invalid or inconsistent configuration), an error
is thrown with a description of all issues. If the simulation is created
successfully but with non-critical issues, these are collected as
warnings and can be displayed by setting `showWarnings = TRUE`. The
following example demonstrates an error case:

``` r
simulation <- loadSimulation(
  system.file(
    "extdata",
    "Aciclovir.pkml",
    package = "ospsuite"
  ),
  loadFromCache = TRUE
)
simConfig <- simulation$configuration

# Introduce an error in the configuration
simConfig$selectedInitialConditions <- list("Vergin 1995 IV" = NULL)

newSimulation <- createSimulation(
  simulationConfiguration = simConfig,
  simulationName = "MySim",
  showWarnings = TRUE
)
```

    ## Error in `createSimulation()`:
    ## ! Cannot create simulation. The following errors were generated during simulation creation:
    ##  Cannot create application 'Intravenous_Transport': molecule 'Aciclovir' not available in the target container 'Plasma'

### Setting calculation methods and creating process rate parameters

Before creating a simulation from configuration, the user can change the
calculation methods for molecules in the simulation. Available methods
are listed in the enums `PartitionCoefficientMethods` and
`CellularPermeabilityMethods`. Methods are set per molecule using the
`setPartitionCoefficientMethods()` and
`setCellularPermeabilityMethods()` methods of the simulation
configuration. Additionally, the user can select whether to create all
process rate parameters in the simulation or not by setting the argument
`createAllProcessRateParameters` in the function
[`createSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createSimulation.md).
These parameters can be used for model debugging and analysis.

``` r
configuration$setPartitionCoefficientMethods(
  "T3",
  PartitionCoefficientMethods$Berezhkovskiy
)
configuration$setPartitionCoefficientMethods(
  "T4",
  PartitionCoefficientMethods$`PK-Sim Standard`
)

configuration$setCellularPermeabilityMethods(
  "T3",
  CellularPermeabilityMethods$`Charge dependent Schmitt normalized to PK-Sim`
)
configuration$setCellularPermeabilityMethods(
  "T4",
  CellularPermeabilityMethods$`Charge dependent Schmitt normalized to PK-Sim`
)

sim1 <- createSimulation(
  simulationName = "Thyroid_QST_human",
  configuration,
  createAllProcessRateParameters = TRUE
)
```
