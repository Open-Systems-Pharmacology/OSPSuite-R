simple <- loadTestSimulation("simple")

# SensitivityAnalysis

test_that("It can create a sensitivity analysis for a given simulation without any other parameter in the constructor", {
  sa <- SensitivityAnalysis$new(simple)
  expect_equal(sa$numberOfSteps, ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps)
  expect_equal(sa$variationRange, ospsuiteEnv$sensitivityAnalysisConfig$variationRange)
})

test_that("It can create a sensitivity analysis for a given simulation with parmaeters in the constructor", {
  sa <- SensitivityAnalysis$new(simple, parameterPaths = c("A", "B"), numberOfSteps = 5, variationRange = 2)
  expect_equal(sa$numberOfSteps, 5)
  expect_equal(sa$variationRange, 2)
  expect_identical(sa$parameterPaths, c("A", "B"))
})

test_that("The parameter paths property is readonly", {
  sa <- SensitivityAnalysis$new(simple, parameterPaths = c("A", "B"))
  expect_error(sa$parameterPaths <- c("D"))
})

test_that("It can add a single parameter path", {
  sa <- SensitivityAnalysis$new(simple)
  sa$addParameterPaths("A")
  expect_identical(sa$parameterPaths, c("A"))
})

test_that("It can clear the parameters added to a SA", {
  sa <- SensitivityAnalysis$new(simple)
  sa$addParameterPaths("A")
  sa$clearParameterPaths()
  expect_length(sa$parameterPaths, 0)
  sa$addParameterPaths("A")
  expect_length(sa$parameterPaths, 1)
})


test_that("It can add a multiple parameter paths", {
  sa <- SensitivityAnalysis$new(simple)
  sa$addParameterPaths(c("A", "B"))
  expect_identical(sa$parameterPaths, c("A", "B"))
})

test_that("It can print a sensitivity analysis", {
  sa <- SensitivityAnalysis$new(simple)
  expect_snapshot(sa$print())
})
