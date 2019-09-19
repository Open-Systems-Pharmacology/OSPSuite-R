sim <- loadTestSimulation("S1")
simResults <- runSimulation(sim)

resultsPaths <- simResults$allQuantityPaths

context("getPopulationResultsValues")

test_that("It throws an error when no valid simulation results are provided", {
  expect_error(getPopulationResultsValues(sim, resultsPaths))
})

test_that("It throws an error when no valid paths or quantities are provided", {
  expect_error(getPopulationResultsValues(simResults, 1))
})

test_that("It throws an error when no valid individual ids are provided", {
  expect_error(getPopulationResultsValues(simResults, resultsPaths, "one"))
})

test_that("It can retrieve results by paths", {
  results <- getPopulationResultsValues(simResults, resultsPaths)
  expect_equal(length(results), length(resultsPaths))
})

test_that("It can retrieve results by quantities", {
  results <- getPopulationResultsValues(simResults, getAllQuantitiesMatching(resultsPaths, sim))
  expect_equal(length(results), length(resultsPaths))
})

test_that("It can retrieve results with provided individual id", {
  results <- getPopulationResultsValues(simResults, resultsPaths, individualIds = c(0, 0, 1))
  expect_equal(length(results), length(resultsPaths))
  expect_false(is.null(results[[1]]))
})

test_that("It returns NULL for results if no individual id was simulated", {
  results <- getPopulationResultsValues(simResults, resultsPaths, individualIds = 1)
  expect_equal(length(results), length(resultsPaths))
  expect_null(results[[1]])
})

test_that("It returns NULL for paths that were not simulated", {
  results <- getPopulationResultsValues(simResults, "testPath")
  expect_equal(length(results), 1)
  expect_null(results$"testPath")
})

context("getResultsValues")

test_that("It can retrieve results by paths", {
  results <- getResultsValues(simResults, resultsPaths)
  expect_equal(length(results), length(resultsPaths))
})


test_that("It throws an error when the results contain more than one individual", {
  # TODO
})
