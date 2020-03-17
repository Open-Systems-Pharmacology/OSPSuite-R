sim <- loadTestSimulation("simple")
sensitivity <- SensitivityAnalysis$new(sim)
sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)
results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)

context("SensitivityAnalysisResults")
test_that("It returns the name of all pk parameters available in the SA results", {
  pkParameterNames <- results$allPKParameterNames
  expect_gt(length(pkParameterNames), 0)
})

test_that("It returns the number of QuantityPKParameters calcualted", {
  expect_gt(length(results$count), 0)
})
