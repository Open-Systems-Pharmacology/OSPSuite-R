## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadSim------------------------------------------------------------------
library(ospsuite)

simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

sim <- loadSimulation(simFilePath)

## ----getEntities--------------------------------------------------------------
# Get the molecule Aciclovir located in kidney intracellular space
moleculeInKid <- getMolecule("Organism|Kidney|Intracellular|Aciclovir", sim)
print(moleculeInKid)

# Get the container "Liver"
livContainer <- getContainer("Organism|Liver", sim)
print(livContainer)

# Get the parameter volume of the liver interstitial space
# Note that the path used is relative to the liver container
livParam <- getParameter("Interstitial|Volume", livContainer)
print(livParam)

## ----getAllEntitiesMatching---------------------------------------------------
# Get the parameter `Volume` of the intracellular space of all organs,
# with exactly one path element before `Intracellular`
volumeParams <- getAllParametersMatching("Organism|*|Intracellular|Volume", sim)
length(volumeParams)
# The PBPK model has 15 organs with an "Intracellular" sub-compartment

# Get the parameter `Volume` of the intracellular space of all organs,
# no matter how many sub-containers the organ has
volumeParams <- getAllParametersMatching("Organism|**|Intracellular|Volume", sim)
length(volumeParams)
# The list also includes parameters of organs like "Liver|Periportal",
# or the mucosal compartments of the intestine.

## ----getAllEntitiesMatching_multiplePaths-------------------------------------
# Get the molecule Aciclovir located in `Liver|Periportal|Intracellular` and `VenousBlood|Plasma`
molecules <- getAllMoleculesMatching(c(
  "Organism|VenousBlood|Plasma|Aciclovir",
  "Organism|Liver|Periportal|Intracellular|Aciclovir"
), sim)
print(molecules)

## ----containerProperties------------------------------------------------------
# Path of the container
livContainer$path

# Parent container
livContainer$parentContainer

## ----moleculeProperties-------------------------------------------------------
# Initial value of the molecule
moleculeInKid$value

# Dimension of the molecule. See section "Unit conversion" for more information.
moleculeInKid$dimension

# Is the initial value defined by a formula?
moleculeInKid$isFormula

# Type of the formula. CONSTANT if the value is defined by a constant.
moleculeInKid$formula

## ----parameterProperties------------------------------------------------------
# Initial value of the parameter
livParam$value

# Dimension of the parameter. See section "Unit conversion" for more information.
livParam$dimension

# Base unit of the parameter. See section "Unit conversion" for more information.
livParam$unit

# Is the initial value defined by a formula?
livParam$isFormula

# Type of the formula. CONSTANT if the value is defined by a constant.
livParam$formula

## ----simTree------------------------------------------------------------------
# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Create simulation tree
simTree <- getSimulationTree(sim)

# Calling simTree would list all entities within the simulation
# (given how long this tree can be, we will skip it here, but you can try
# running the following command in your console)
# simTree

# Accessing the parameter "Organism|Weight"
simTree$Organism$Weight

# Getting all entities located under "Organism|Liver|Periportal|Intracellular"
entitiesList <- simTree$Organism$Liver$Periportal$Intracellular
entitiesList

