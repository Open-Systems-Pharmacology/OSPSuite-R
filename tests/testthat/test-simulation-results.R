# SimulationResults

sim <- loadTestSimulation("S1")
simResults <- runSimulations(sim)[[1]]

resultsPath <- "Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)"

test_that("It returns TRUE if results for an individual exist", {
  expect_true(simResults$hasResultsForIndividual(0))
})

test_that("It returns FALSE if results for an individual do not exist", {
  expect_false(simResults$hasResultsForIndividual(1))
})

test_that("It can return values for a specific path for an individual", {
  values <- simResults$getValuesByPath(resultsPath, 0)
  expect_gt(length(values), 0)
})

test_that("It returns an array of NA if specific result for specific individual is not found and stopIfNotFound = FALSE", {
  values <- simResults$getValuesByPath(resultsPath, 1, stopIfNotFound = FALSE)
  expect_true(all(is.na(values)))

  values <- simResults$getValuesByPath("blabla", 0, stopIfNotFound = FALSE)
  expect_true(all(is.na(values)))

  values <- simResults$getValuesByPath("blabla", 1, stopIfNotFound = FALSE)
  expect_true(all(is.na(values)))
})

test_that("It can retrieve the number of individuals", {
  expect_equal(simResults$count, 1)
})

test_that("It throws an error when trying to set the number of individuals", {
  expect_error(simResults$count <- 1)
})

test_that("It can retrieve the simulation", {
  expect_equal(simResults$simulation, sim)
})

test_that("It throws an error when trying to set the simulation", {
  expect_error(simResults$simulation <- loadTestSimulation("S1"))
})

test_that("It can retrieve the time values", {
  expect_gt(length(simResults$timeValues), 0)
})

test_that("It throws an error when trying to set the time values", {
  expect_error(simResults$timeValues <- 1:10)
})

test_that("It can retrieve the paths of all outputs", {
  expect_equal(length(simResults$allQuantityPaths), 5)
})

test_that("It throws an error when trying to set the paths of all outputs", {
  expect_error(simResults$allQuantityPaths <- "1:10")
})

test_that("It can retrieve the list of all individual ids", {
  expect_equal(simResults$allIndividualIds, 0)
})

test_that("It throws an error when trying to set individual ids", {
  expect_error(simResults$allIndividualIds <- c(0, 1, 2))
})

test_that("It can print simulation results", {
  expect_error(capture.output(simResults$print()), NA)
})
