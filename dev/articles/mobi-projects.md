# Working with MoBi projects

## Working with MoBi projects

First, load a MoBi project:

``` r
# Load the ospsuite package
library(ospsuite)

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
    ## Individual:
    ##   • Vergin_1995_IV
    ## Expression profiles:

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
# sim1 <- myProject$createSimulationConfiguration(
#   modulesNames = c(
#     "Thyroid_QST",
#     "Rat physiology",
#     "Thyroid",
#     "Pituitary",
#     "Endogenous_TH",
#     "TH_activeTransports",
#     "TH_plasma_binding"
#   ),
#   individualName = "Rat",
#   expressionProfilesNames = c(
#     "UDPGT1|Human|Healthy",
#     "DIO1|Human|Healthy",
#     "DIO3|Human|Healthy",
#     "UDPGT2|Human|Healthy"
#   ),
#   parameterValues = list("Thyroid" = "Rat")
# )
```

### Create simulation configuration from modules

An alternative to creating simulation configurations from modules from a
project is to use the function
[`createSimulationConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createSimulationConfiguration.md)
and provide a list of `Module` objects. This allows to create
simulations from modules contained in different projects, or from
modules that are stored as pkml files. In the following example, we will
create a simulation configuration using the modules from the project as
in the example before, but the extension module “Thyroid” will be loaded
from a pkml file, and a new individual will be created.

``` r
# # Get modules from the project
# modules <- myProject$getModules()
# # Get the expression profiles from the project
# expressionProfiles <- myProject$getExpressionProfiles(
#   names = c(
#     "UDPGT1|Human|Healthy",
#     "DIO1|Human|Healthy",
#     "DIO3|Human|Healthy",
#     "UDPGT2|Human|Healthy"
#   )
# )
# # Create an individual building block
# myIndividual <- createIndividualBuildingBlock(
#   species = Species$Human,
#   population = HumanPopulation$Asian_Tanaka_1996,
#   gender = Gender$Male,
#   weight = 79,
#   height = 175,
#   age = 35
# )
```

Modules that are intended to be used in combination with a variety of
other modules, especially with every generic PBPK module, are usually
set up in a generic way and require some extensions in order to be used
directly. For example, the initial conditions of the module “Thyroid”
must be extended by the molecules of the model it will be used with.
Similarly, some compound-specific parameter could require manual
adjustments. Therefore, initial conditions and parameter values building
block can be modified from R.

Following functionalities are available for the Initial Conditions BBs:

- [`setInitialConditions()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setInitialConditions.md):
  Allows adding new entries or changing the values of existing entries
  in the Initial Conditions BB.
- [`deleteInitialConditions()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/deleteInitialConditions.md):
  Allows deleting entries from the Initial Conditions BB.
- [`extendInitialConditions()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/extendInitialConditions.md):
  Extending an IC BB by entries for specified molecules in all physical
  containers of a spatial structure.

Followning functionalities are available for the Parameter Values (PV)
BBs:

- [`setParameterValues()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/setParameterValues.md):
  Allows adding new entries or changing the values of existing entries
  in the PV BB.
- [`deleteParameterValues()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/deleteParameterValues.md):
  Allows deleting entries from the PV BB.
- [`addLocalMoleculeParameters()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addLocalMoleculeParameters.md):
  Extending a PV BB by entries for local parameters of specified
  molecules in all physical containers of a spatial structure.
- [`addProteinExpressionToParameterValuesBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addProteinExpressionToParameterValuesBB.md):
  Extending a PV BB by parameters defining expression of protein
  molecules in selected organs.

So in the first step we load the extension module “Thyroid” from a pkml
file:

``` r
# thyroidModule <- loadModuleFromPKML(system.file(
#   "extdata",
#   "Thyroid.pkml",
#   package = "ospsuite"
# ))
```

Then we extend the initial conditions BB of the loaded module by
molecules defined in other modules of our project:

``` r
# # Get the IC BB from the Thyroid module
# icBB <- thyroidModule$getBuildingBlocks(type = "Initial Conditions")[[1]]
# # Extend the IC BB of the module "Thyroid" with molecules from base module "Thyroid_QST"
# newPaths <- extendInitialConditions(
#   icBB,
#   spatialStructureBB = thyroidModule$getBuildingBlocks(
#     type = "Spatial Structure"
#   ),
#   moleculesBB = modules[["Thyroid_QST"]]$getBuildingBlocks(type = "Molecules"),
#   moleculeNames = c("DIO1", "T3", "T4", "TSH")
# )
```

The molecule “DIO1” that has been added to the IC BB in the previous
step is a protein. As the module “Thyroid” adds a new organ for which no
protein expression is defined in the Expression Profiles, parameters
defining the expression must be added to the PV BB of the new module
using the
[`addProteinExpressionToParameterValuesBB()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/addProteinExpressionToParameterValuesBB.md)
function:

``` r
# # Get the PV BB from the Thyroid module
# pvBB <- thyroidModule$getBuildingBlocks(type = "Parameter Values")[["Human"]]
# # Add protein expression to the PV BB
# proteinExpressionPaths <- addProteinExpressionToParameterValuesBB(
#   pvBB,
#   spatialStructureBB = thyroidModule$getBuildingBlocks(
#     type = "Spatial Structure"
#   ),
#   moleculesBB = modules[["Thyroid_QST"]]$getBuildingBlocks(type = "Molecules"),
#   moleculeNames = c("DIO1")
# )

# # EXPECTED BEHAVIOR
# proteinExpressionPaths <- c(
#   "Organism|Thyroid|Plasma|DIO1|Initial concentration",
#   "Organism|Thyroid|BloodCells|DIO1|Initial concentration",
#   "Organism|Thyroid|Interstitial|DIO1|Initial concentration",
#   "Organism|Thyroid|Intracellular|DIO1|Initial concentration",
#   "Organism|Thyroid|Endosome|DIO1|Initial concentration",
#   "Organism|Thyroid|Lumen|DIO1|Initial concentration",
#   "Organism|Thyroid|Interstitial|DIO1|Fraction expressed interstitial",
#   "Organism|Thyroid|Intracellular|DIO1|Fraction expressed intracellular",
#   "Organism|Thyroid|Intracellular|DIO1|Relative expression"
# )
```

and overwrite the value of “Relative expression”:

``` r
# setParameterValues(
#   parameterValuesBuildingBlock = pvBB,
#   quantityPaths = "Organism|Thyroid|Intracellular|DIO1|Relative expression",
#   dimensions = ospDimensions$Fraction,
#   quantityValues = 1.33
# )
```

Finally, we can create a simulation configuration from the modules and
the individual we created:

``` r
# simulationConfigurationHuman <- createSimulationConfiguration(
#   simulationName = "Thyroid_QST_human",
#   modules = c(
#     modules[["Thyroid_QST"]],
#     thyroidModule,
#     modules[["Pituitary"]],
#     modules[["Endogenous_TH"]],
#     modules[["TH_activeTransports"]],
#     modules[["TH_plasma_binding"]]
#   ),
#   individual = myIndividual,
#   expressionProfiles = expressionProfiles,
#   parameterValues = list("Thyroid" = "Human")
# )
```

Expression profile BBs cannot be created from R but can be loaded from
PKML. (Tier 3 or so).

## Creating simulations

The simulation configuration can be used to create a simulation. The
simulation can then be saved to a pkml file, or used for further
analysis.

``` r
simulation <- myProject$getSimulation("Thyroid_QST_Human")
configuration <- simulation$configuration

# TODO enable after https://github.com/Open-Systems-Pharmacology/MoBi/issues/2252
# simulation <- createSimulation(
#   simulationName = "Thyroid_QST_Human_copy",
#   configuration
# )
```

Before creating a simulation from configuration, the user can change the
calculation methods for molecules in the simulation. Available methods
are listed in the enums `MoleculeCalculationMethod` and
`PartitionCoefficientMethod`. Additionally, the user can select whether
to create all process rate parameters in the simulation or not by
setting the argument `createAllProcessRateParameters` in the function
[`createSimulation()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createSimulation.md).
These parameters can be used for model debugging and analysis.

``` r
# simulationConfigurationHuman$partitionCoefficientMethods(
#   list(
#     "T3" = PartitionCoefficientMethods$Berezhkovskiy,
#     "T4" = PartitionCoefficientMethods$`PK-Sim Standard`
#   )
# )

# simulationConfigurationHuman$cellularPermeabilityMethods(
#   list(
#     "T3" = CellularPermeabilityMethods$`Charge dependent Schmitt normalized to PK-Sim`,
#     "T4" = CellularPermeabilityMethods$`Charge dependent Schmitt normalized to PK-Sim`
#   )
# )

# sim1 <- createSimulation(
#   simulationName = "Thyroid_QST_human",
#   simulationConfigurationHuman,
#   createAllProcessRateParameters = TRUE
# )
```
