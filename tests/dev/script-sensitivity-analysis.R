library(ospsuite)

# Remove to ensure that we can add the parameters again
removeAllUserDefinedPKParameters()

pkParameter <- updatePKParameter(name = "t_max", displayName = "MyTmax", displayUnit = "min")
print(pkParameter)

pkParameter <- updatePKParameter(name = "C_max", displayName = "MyCMax", displayUnit = "mg/ml")
print(pkParameter)

userDefinedPKParameter <- addUserDefinedPKParameter(name = "Test", standardPKParameter = StandardPKParameter$AUC_tEnd, displayName = "toto", displayUnit = "mg")
userDefinedPKParameter$startApplicationIndex
print(userDefinedPKParameter)

updatePKParameter(name = "Test", displayName = "toto", displayUnit = "mg")
sim <- loadSimulation("inst/extdata/simple.pkml", loadFromCache = FALSE)

outputSelections <- sim$outputSelections
params <- getAllParametersMatching("**", sim)
print(params)

sensitivity <- SensitivityAnalysis$new(sim)
sensitivity$addParameterPaths("Organism|Liver|Volume")

print(sensitivity)

sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(showProgress = TRUE)

results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)

exportSensitivityAnalysisResultsToCSV(results, "inst/extdata/sa.csv")

importedResults <- importSensitivityAnalysisResultsFromCSV(simulation = sim, "inst/extdata/sa.csv")

for (output in outputSelections$allOutputs) {
  pkSensitivities <- results$allPKParameterSensitivitiesFor(pkParameterName = "Test", outputPath = output$path)
  for (pkSensitivity in pkSensitivities) {
    print(pkSensitivity)
  }

  pkSensitivities <- results$allPKParameterSensitivitiesFor(pkParameterName = "t_max", outputPath = output$path)
  for (pkSensitivity in pkSensitivities) {
    print(pkSensitivity)
  }
}
