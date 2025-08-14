sim <- loadTestSimulation("simple")
sensitivity <- SensitivityAnalysis$new(sim)
sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(showProgress = FALSE)
results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)
parameters <- getAllParameterPathsIn(sim)

# SensitivityAnalysisResults

test_that("It can print sensitivity analysis results", {
  expect_snapshot(results$print())
})

test_that("It returns the name of all pk parameters available in the SA results", {
  pkParameterNames <- results$allPKParameterNames
  expect_gt(length(pkParameterNames), 0)
})

test_that("It returns the number of QuantityPKParameters calculated", {
  expect_gt(length(results$count), 0)
})

# pkParameterSensitivityValueFor

test_that("It produces error when both parameter path and name are specified", {
  expect_error(
    results$pkParameterSensitivityValueFor("C_max", "Organism|B", parameterName = "R1|k1", parameterPath = "R1|k1"),
    messages$errorOneOfNameAndPathMustBeSpecified()
  )
})

test_that("It produces error when both parameter path and name are missing", {
  expect_error(
    results$pkParameterSensitivityValueFor("C_max", "Organism|B"),
    messages$errorOneOfNameAndPathMustBeSpecified()
  )
})

test_that("It produces error when either parameter path or name are not character", {
  expect_error(
    results$pkParameterSensitivityValueFor("C_max", "Organism|B", parameterName = 0),
    messages$errorWrongType("parameterName", "numeric", "character"),
    fixed = TRUE
  )

  expect_error(
    results$pkParameterSensitivityValueFor("C_max", "Organism|B", parameterPath = TRUE),
    messages$errorWrongType("parameterPath", "logical", "character"),
    fixed = TRUE
  )
})

test_that("It returns the sensitivity value of an existing parameter by name", {
  value <- results$pkParameterSensitivityValueFor("C_max", "Organism|B", parameterName = "R1-k1")
  expect_gt(value, 0)
})

test_that("It returns the sensitivity value of an existing parameter by path", {
  value <- results$pkParameterSensitivityValueFor("C_max", "Organism|B", parameterPath = "R1|k1")
  expect_gt(value, 0)
})

test_that("It returns NA for a parameter that does not exist", {
  value <- results$pkParameterSensitivityValueFor("C_max", "Organism|B", "DOES NOT EXIST")
  expect_true(is.na(value))
})
