context("SimulationRunnerConcurrentOptions")

test_that("It can print simulation runner concurrent options", {
  runnerConcOptions <- SimulationRunnerConcurrentOptions$new()
  expect_error(capture.output(runnerConcOptions$print()), NA)
})

test_that("It returns NULL for simulationRunOptions if none have been set", {
  runnerConcOptions <- SimulationRunnerConcurrentOptions$new()
  expect_null(runnerConcOptions$simulationRunOptions)
})

test_that("It can set and get simulationRunOptions", {
  runnerConcOptions <- SimulationRunnerConcurrentOptions$new()
  runnerConcOptions$simulationRunOptions <- SimulationRunOptions$new()
  expect_true(isOfType(runnerConcOptions$simulationRunOptions, "SimulationRunOptions"))

  expect_error(runnerConcOptions$simulationRunOptions <- NULL)
})

test_that("It adds a simulation without population", {
  runnerConcOptions <- SimulationRunnerConcurrentOptions$new()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  runnerConcOptions$addSimulation(sim)
})

test_that("It adds a simulation with population", {
  runnerConcOptions <- SimulationRunnerConcurrentOptions$new()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  runnerConcOptions$addSimulation(sim, population)
})
