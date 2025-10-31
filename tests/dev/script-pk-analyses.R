library(ospsuite)

# Remove to ensure that we can add the parameters again
removeAllUserDefinedPKParameters()

pkParameter <- updatePKParameter(
  name = "t_max",
  displayName = "MyTmax",
  displayUnit = "min"
)
print(pkParameter)

pkParameter <- updatePKParameter(
  name = "C_max",
  displayName = "MyCMax",
  displayUnit = "mg/ml"
)
print(pkParameter)

userDefinedPKParameter <- addUserDefinedPKParameter(
  name = "Test",
  standardPKParameter = StandardPKParameter$AUC_tEnd,
  displayName = "toto",
  displayUnit = "mg"
)
userDefinedPKParameter$normalizationFactor <- 100

quantityPath <- "Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)"

sim <- loadSimulation("tests/data/S1.pkml")
toto <- sim$molWeightFor(quantityPath)

popFile <- getTestDataFilePath("pop.csv")
population <- loadPopulation(popFile)
simRunOptions <- SimulationRunOptions$new(
  numberOfCores = 4,
  checkForNegativeValues = TRUE,
  showProgress = TRUE
)
populationResults <- runSimulations(sim, population, simRunOptions)[[1]]
populationPkAnalyses <- calculatePKAnalyses(populationResults)
df <- pkAnalysesToDataFrame(populationPkAnalyses)

exportPKAnalysesToCSV(populationPkAnalyses, "C:/temp/export/pk.csv")
newPKAnalyses <- importPKAnalysesFromCSV("C:/temp/export/pk.csv", sim)
df <- pkAnalysesToDataFrame(newPKAnalyses)

print(populationPkAnalyses)
pkParameters <- populationPkAnalyses$allPKParametersFor(quantityPath)

for (pkParameter in pkParameters) {
  print(pkParameter)
}

allPKParameterNames()
