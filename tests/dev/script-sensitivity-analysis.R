library(ospsuite)

sim <- loadSimulation("inst/extdata/simple.pkml", loadFromCache = FALSE)

outputSelections <- sim$outputSelections
params <- getAllParametersMatching("**", sim)
print(params)
sensitivity <- SensitivityAnalysis$new(sim)
print(sensitivity)
print(outputSelections)
sensitivity$addParameterPaths("Organism|Liver|Volume")
sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(showProgress = TRUE)
results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)

exportSensitivityAnalysisResultsToCSV(results, "inst/extdata/sa.csv")

importedResults <- importSensitivityAnalysisResultsFromCSV(simulation = sim, "inst/extdata/sa.csv")

for (output in outputSelections$allOutputs) {
  pkSensitivities <- results$allPKParameterSensitivitiesFor(pkParameterName = "AUC", outputPath = output$path)
  for (pkSensitivity in pkSensitivities) {
    print(pkSensitivity)
  }
}
