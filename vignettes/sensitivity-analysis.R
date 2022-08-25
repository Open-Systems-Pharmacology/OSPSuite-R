## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----createSensitivityAnalysis------------------------------------------------
library(ospsuite)

# Load simulation
simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the paths of parameters that will be considered by default.
potentialSAParameters <- potentialVariableParameterPathsFor(sim)
print(potentialSAParameters)

# Create a default `SensitivityAnalysis` for the simulation
sa <- SensitivityAnalysis$new(simulation = sim)
print(sa)

# Create a `SensitivityAnalysis` with specified parameters
sa <- SensitivityAnalysis$new(simulation = sim, parameterPaths = c("Organism|Q", "Organism|Volume"))
print(sa)

# Show which parameters will be varied
sa$parameterPaths

## ----addPathsToSensitivityAnalysis--------------------------------------------
library(ospsuite)

# Load simulation
simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Create a `SensitivityAnalysis` with specified parameters
sa <- SensitivityAnalysis$new(simulation = sim, parameterPaths = c(
  "Organism|Q",
  "Organism|Volume"
))
# Add new parameter
sa$addParameterPaths("R1|k1")

# Show which parameters will be varied
sa$parameterPaths

## ----runSA--------------------------------------------------------------------
# Load simulation
simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Create a `SensitivityAnalysis` with all constant and suitable for sensitivity calculations parameters and suitable for sensitivity calculations parameters
sa <- SensitivityAnalysis$new(simulation = sim)

# Run the sensitivity analysis
saResult <- runSensitivityAnalysis(sa)
print(saResult)

## ----allPKParameterSensitivitiesFor-------------------------------------------
# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Default value of the threshold
getOSPSuiteSetting("sensitivityAnalysisConfig")$totalSensitivityThreshold

# Create a `SensitivityAnalysis` with all constant parameters and run it
sa <- SensitivityAnalysis$new(
  simulation = sim,
  parameterPaths = c(
    "Aciclovir|Lipophilicity",
    "Aciclovir|Fraction unbound (plasma)",
    "Organism|Age"
  )
)
saResult <- runSensitivityAnalysis(sa)
print(saResult)

# Get sensitivities for the parameter "AUC_inf" of the simulated output with a threshold of 0.8
outputPath <- sim$outputSelections$allOutputs[[1]]$path
sensitivities <- saResult$allPKParameterSensitivitiesFor(pkParameterName = "AUC_inf", outputPath = outputPath, totalSensitivityThreshold = 0.8)
print(sensitivities)

## ----SAExport-----------------------------------------------------------------
# Load and run the simulation
simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simulationResults <- runSimulation(simulation = sim)

# Create a `SensitivityAnalysis` with all constant parameters and run it
sa <- SensitivityAnalysis$new(simulation = sim)
saResult <- runSensitivityAnalysis(sa)

# Export to csv
saResultPath <- system.file("extdata", "SAResult.csv", package = "ospsuite")
exportSensitivityAnalysisResultsToCSV(results = saResult, filePath = saResultPath)

# Load from csv
saResultLoaded <- importSensitivityAnalysisResultsFromCSV(filePaths = saResultPath, simulation = sim)

