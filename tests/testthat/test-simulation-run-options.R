# SimulationRunOptions

runOptions <- SimulationRunOptions$new()
test_that("It has the expected default value", {
  expect_true(runOptions$checkForNegativeValues)
  expect_equal(runOptions$numberOfCores, getOSPSuiteSetting("numberOfCores"))
})

test_that("It can set the basic options parameters", {
  runOptions$checkForNegativeValues <- FALSE
  expect_false(runOptions$checkForNegativeValues)

  runOptions$numberOfCores <- 5
  expect_equal(runOptions$numberOfCores, 5)
})

test_that("It can set autoReduceTolerances option correctly", {
  runOptions <- SimulationRunOptions$new()
  runOptions$autoReduceTolerances <- FALSE
  sim <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"), loadFromCache = FALSE)
  # Setting relative tolerance to a high value to enforce error reduction
  sim$solver$relTol <- 5
  expect_error(runSimulations(sim, simulationRunOptions = runOptions))

  # Allow reducing tolerances
  runOptions$autoReduceTolerances <- TRUE
  results <- runSimulations(simulations = sim, simulationRunOptions = runOptions)[[1]]
  expect_equal(results$count, 1)
})
