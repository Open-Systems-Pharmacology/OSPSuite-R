## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadPop------------------------------------------------------------------
library(ospsuite)
# Load population information from csv
popFilePath <- system.file("extdata", "pop.csv", package = "ospsuite")
myPopulation <- loadPopulation(csvPopulationFile = popFilePath)
print(myPopulation)

## ----createPop----------------------------------------------------------------
library(ospsuite)

# If no unit is specified, the default units are used. For "height" it is "dm", for "weight" it is "kg", for "age" it is "year(s)".
populationCharacteristics <- createPopulationCharacteristics(
  species             = Species$Human,
  population          = HumanPopulation$Asian_Tanaka_1996,
  numberOfIndividuals = 50,
  proportionOfFemales = 50,
  weightMin           = 30,
  weightMax           = 98,
  weightUnit          = "kg",
  heightMin           = NULL,
  heightMax           = NULL,
  ageMin              = 0,
  ageMax              = 80,
  ageUnit             = "year(s)"
)
print(populationCharacteristics)

# Create population from population characteristics
result <- createPopulation(populationCharacteristics = populationCharacteristics)
myPopulation <- result$population
print(myPopulation)

## ----runPop-------------------------------------------------------------------
library(ospsuite)

# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Run population simulation
simulationResults <- runSimulation(simulation = sim, population = myPopulation)
print(simulationResults)

## ----simulationRunOptions-----------------------------------------------------
# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Create a SimulationRunOptions object
simRunOptions <- SimulationRunOptions$new()
print(simRunOptions)

# Change the maximal number of cores to use and show a progress bar during simulation
simRunOptions$numberOfCores <- 3
simRunOptions$showProgress <- TRUE

# Run population simulation with custom options
populationResults <- runSimulation(simulation = sim, population = myPopulation, simulationRunOptions = simRunOptions)
print(populationResults)

## ----getAllOutputSelections---------------------------------------------------
populationResults$allQuantityPaths

## ----getOutputValues----------------------------------------------------------
# Get simulated results by path
resultsPath <- populationResults$allQuantityPaths[[1]]
print(resultsPath)

resultsData <- getOutputValues(populationResults, quantitiesOrPaths = resultsPath)

resultsTime <- resultsData$data$Time
resultsValues <- resultsData$data$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`

plot(resultsTime, resultsValues, type = "l")

## ----getOutputValuesForIndividual---------------------------------------------
# Get simulated results by path
resultsPath <- populationResults$allQuantityPaths[[1]]
print(resultsPath)

# Get only the results for individuals with IDs 1 and 2
resultsData <- getOutputValues(populationResults, quantitiesOrPaths = resultsPath, individualIds = c(1, 2))

resultsTime <- resultsData$data$Time
resultsValues <- resultsData$data$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`

plot(resultsTime, resultsValues, type = "l")

