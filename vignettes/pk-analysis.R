## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----calculatePKAnalyses------------------------------------------------------
library(ospsuite)

# Load the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Run the simulation
simulationResults <- runSimulation(simulation = sim)

# Calculate PK-analyses
pkAnalysis <- calculatePKAnalyses(results = simulationResults)

# Get the path of the simulated output
outputPath <- simulationResults$allQuantityPaths[[1]]
print(outputPath)

# Get all calculated PK parameters for the output path
allPkParams <- pkAnalysis$allPKParametersFor(outputPath)
print(allPkParams)

# Get C_max parameter
c_max <- pkAnalysis$pKParameterFor(quantityPath = outputPath, pkParameter = "C_max")
c_max

## ----QuantityPKParameter------------------------------------------------------
# Get C_max parameter
c_max <- pkAnalysis$pKParameterFor(quantityPath = outputPath, pkParameter = "C_max")

print(c_max)

c_max$name
c_max$quantityPath
c_max$values

## -----------------------------------------------------------------------------
pkAnalysesToDataFrame(pkAnalysis)

## ----PKAnalysesExport---------------------------------------------------------
# Load and run the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simulationResults <- runSimulation(simulation = sim)

# Calculate PK-analysis
pkAnalysis <- calculatePKAnalyses(results = simulationResults)

# Export to csv
csvPKAnalysisPath <- system.file("extdata", "PKAnalyses.csv", package = "ospsuite")
exportPKAnalysesToCSV(pkAnalyses = pkAnalysis, filePath = csvPKAnalysisPath)

# Load from csv
pkAnalysisLoaded <- importPKAnalysesFromCSV(filePath = csvPKAnalysisPath, simulation = sim)

## ----UserDefinedTimeInterval--------------------------------------------------
# Adds a user defined parameter named `MyAuc` that will calculate the value of AUC between t=50 min and t=80min only.

# Create a new parameter based on the standard AUC parameter
myAUC <- addUserDefinedPKParameter(
  name = "MyAUC",
  standardPKParameter = StandardPKParameter$AUC_tEnd
)

# Specifies start time and end time in minute
myAUC$startTime <- 50
myAUC$endTime <- 80

## ----UserDefinedApplicationInterval-------------------------------------------
# Adds a user defined parameter named `MyCMax` that will calculate the value of Cmax between the 4th and 5th application

# Create a new parameter based on the standard C_max parameter
myCMax <- addUserDefinedPKParameter(
  name = "MyCMax",
  standardPKParameter = StandardPKParameter$C_max
)

# Specifies start application (4th) and end applicaiton (5th)
myCMax$startApplicationIndex <- 4
myCMax$endApplicationIndex <- 5

## ----UserDefinedApplicationIntervalWithOffset---------------------------------
# Adds a user defined parameter named `MyCMax` that will calculate the value of Cmax between the 4th application start time + 10 min and
# the 5th application start time + 20min

# Create a new parameter based on the standard C_max parameter
myCMax <- addUserDefinedPKParameter(
  name = "MyCMax",
  standardPKParameter = StandardPKParameter$C_max
)

# Specifies start application (4th) and end applicaiton (5th)
myCMax$startApplicationIndex <- 4
myCMax$endApplicationIndex <- 5

# Specifies start time offset. The PK calculations wil effectively start at StartTime of 4th Application + 10 min
myCMax$startTimeOffset <- 10

# Specifies end time offset . The PK calculations wil effectively ends at StartTime of 5th Application + 20 min
myCMax$endTimeOffset <- 20

## ----UserDefinedDimensionChange-----------------------------------------------
# Let's assume there is an observer called Q_observer in mg/m2 using the dimension Dose per body surface area.
# Simply using C_max would result in the parameter being shown in umol\l.
# To see the correct unit and dimension, the following can be done:

QMax <- addUserDefinedPKParameter(
  name = "Q_max",
  standardPKParameter = StandardPKParameter$C_max,
  displayName = "Q max",
  displayUnit = "mg/m²"
)

