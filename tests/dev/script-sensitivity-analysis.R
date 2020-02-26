library(ospsuite)

sim <- loadSimulation("inst/extdata/simple.pkml", loadFromCache = FALSE)

outputSelections <- sim$outputSelections
params <- getAllParametersMatching("**", sim)
print(params)

sensitivity <- SensitivityAnalysis$new(sim)
sensitivity$addParameterPaths("Organism|Liver|Volume")
dynamicPkParameter = DynamicPKParameter$new(name = "Test", standardPKParameter =  StandardPKParameter$Cmax)
print(dynamicPkParameter)
sensitivity$addDynamicPKParameters(dynamicPkParameter)

print(sensitivity)

sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(showProgress = TRUE)



p<-sensitivity$allDynamicPKParameters
print(p)
results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)

exportSensitivityAnalysisResultsToCSV(results, "inst/extdata/sa.csv")

importedResults <- importSensitivityAnalysisResultsFromCSV(simulation = sim, "inst/extdata/sa.csv")

for (output in outputSelections$allOutputs) {
  pkSensitivities <- results$allPKParameterSensitivitiesFor(pkParameterName = "Test", outputPath = output$path)
  for (pkSensitivity in pkSensitivities) {
    print(pkSensitivity)
  }
}
