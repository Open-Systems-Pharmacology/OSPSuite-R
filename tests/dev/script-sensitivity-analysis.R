library(ospsuite)

sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml", loadFromCache = FALSE)

outputSelections <- sim$outputSelections

sensitivity <- SensitivityAnalysis$new(sim)

sensitivityAnalysisOptions = SensitivityAnalysisRunOptions$new(showProgress = TRUE);

results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)


for (output in outputSelections$allOutputs) {
  pkSensitivities = results$allPKParameterSensitivitiesFor(pkParameterName = "AUC", outputPath = output$path, totalSensitivityThreshold =  0.8)
  for (pkSensitivity in pkSensitivities) {
    print(pkSensitivity)
  }
}
