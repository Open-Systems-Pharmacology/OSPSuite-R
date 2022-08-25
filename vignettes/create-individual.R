## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----createIndividualCharacteristics------------------------------------------
library(ospsuite)
initPKSim("c:\\Program Files\\Open Systems Pharmacology\\PK-Sim 11.1\\")

# If no unit is specified, the default units are used. For "weight" it is "kg", for "age" it is "year(s)".
individualCharacteristics <- createIndividualCharacteristics(
  species    = Species$Human,
  population = HumanPopulation$Japanese_Population,
  gender     = Gender$Female,
  weight     = 75,
  height     = 1.75,
  heightUnit = "m",
  age        = 43
)
print(individualCharacteristics)

individual <- createIndividual(individualCharacteristics = individualCharacteristics)

# we will not be printing this given the long length of the output, but you can
# see the details by running:
# print(individual)

## ----setIndividualParameters--------------------------------------------------
library(ospsuite)

# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
print(simFilePath)
sim <- loadSimulation(simFilePath)

# Apply individual parameters
setParameterValuesByPath(
  parameterPaths = individual$distributedParameters$paths,
  values         = individual$distributedParameters$values,
  simulation     = sim
)

## ----addOntogeny--------------------------------------------------------------
library(ospsuite)

# All supported ontogenies
print(StandardOntogeny)

# Create the ontogeny for the protein "MyProtein" based on ontology of CYP3A4
myProteinOntogeny <- MoleculeOntogeny$new(molecule = "MyProtein", ontogeny = StandardOntogeny$CYP3A4)
print(myProteinOntogeny)

# Add this ontogeny to the individual characteristics used to create the individual parameters set
individualCharacterstics <- createIndividualCharacteristics(
  species            = Species$Human,
  population         = HumanPopulation$Japanese_Population,
  gender             = Gender$Female,
  weight             = 75,
  height             = 1.75,
  heightUnit         = "m",
  age                = 43,
  moleculeOntogenies = myProteinOntogeny
)
print(individualCharacterstics)

individual <- createIndividual(individualCharacteristics = individualCharacterstics)

