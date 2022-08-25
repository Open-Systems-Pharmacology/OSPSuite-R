## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----runSim-------------------------------------------------------------------
library(ospsuite)

# Load the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simulationResults <- runSimulations(simulations = sim)
# Extract `SimulationResults` by simulation id
simulationResults <- simulationResults[[sim$id]]
print(simulationResults)

## ----getAllOutputSelections---------------------------------------------------
simulationResults$allQuantityPaths

## ----getOutputValues----------------------------------------------------------
# Get simulated results by path
resultsPath <- simulationResults$allQuantityPaths[[1]]
print(resultsPath)

resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)

resultsTime <- resultsData$data$Time
resultsValues <- resultsData$data$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`

plot(resultsTime, resultsValues, type = "l")

## ----SimResultsExport---------------------------------------------------------
# Load and run the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
# `runSimulations` returns a list of `SimulationResults`. For single simulation,
# we directly extract the first element, as only one object is created.
simulationResults <- runSimulations(simulations = sim)[[1]]

# Export to csv
csvResultsPath <- system.file("extdata", "SimResults.csv", package = "ospsuite")
exportResultsToCSV(results = simulationResults, filePath = csvResultsPath)

# Load from csv
resultsLoaded <- importResultsFromCSV(filePaths = csvResultsPath, simulation = sim)
print(resultsLoaded)

## ----MultipleSimRun-----------------------------------------------------------
# Load and run multiple simulations concurrently.
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

# We load 3 times the same simulation for convenience. But in real life scenarios,
# they should be different simulations
sim1 <- loadSimulation(simFilePath)
sim2 <- loadSimulation(simFilePath)
sim3 <- loadSimulation(simFilePath)

simulationResults <- runSimulations(simulations = list(sim1, sim2, sim3))

## ----MultipleRunSimResultsIds-------------------------------------------------
# Get the id of the second simulation
id <- sim2$id
print(id)
# get the corresponding result
sim2Results <- simulationResults[[id]]
print(sim2Results$simulation$id)

## ----addOutputs---------------------------------------------------------------
# Clear the list of generated outputs
clearOutputs(sim)

# Add new outputs as objects
molecule <- getMolecule("Organism|Kidney|Intracellular|Aciclovir", sim)
observer <- getQuantity("Organism|Lumen|Aciclovir|Fraction dissolved", sim)

addOutputs(c(molecule, observer), simulation = sim)

# Add new outputs as path strings
addOutputs(
  c(
    "Organism|Lumen|Stomach|Aciclovir",
    "Organism|PeripheralVenousBlood|Aciclovir|Whole Blood (Peripheral Venous Blood)"
  ),
  simulation = sim
)

# Run simulation
simulationResults <- runSimulations(simulations = sim)

# Retrieve all generated outputs (e.g. omitting the quantitiesOrPaths property
# will return all available values)
resultsData <- getOutputValues(simulationResults)

# Note that "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
# is not in the list of generated results any more
names(resultsData$data)

## ----$outputSchema------------------------------------------------------------
print(sim$outputSchema)

