sim <- loadTestSimulation("simple")
sensitivity <- SensitivityAnalysis$new(sim)
sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)
results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)
parameters <- getAllParameterPathsIn(sim)

context("SensitivityAnalysisResults")
test_that("It returns the name of all pk parameters available in the SA results", {
  pkParameterNames <- results$allPKParameterNames
  expect_gt(length(pkParameterNames), 0)
})

test_that("It returns the number of QuantityPKParameters calcualted", {
  expect_gt(length(results$count), 0)
})

test_that("It returns the sensitivity value of an existing parameter", {
  value <- results$pkParameterSensitivityValueFor("C_max", "Organism|B", "R1-k1")
  expect_gt(value, 0)
})

test_that("It returns NA for a parameter that does not exist", {
  value <- results$pkParameterSensitivityValueFor("C_max", "Organism|B", "DOES NOT EXIST")
  expect_true(is.na(value))
})
