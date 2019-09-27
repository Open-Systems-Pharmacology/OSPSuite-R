sim <- loadTestSimulation("S1")
simResults <- runSimulation(sim)

resultsPaths <- simResults$allQuantityPaths

context("getOutputValues")

test_that("It throws an error when no valid simulation results are provided", {
  expect_error(getOutputValues(sim, resultsPaths))
})

test_that("It throws an error when no valid paths or quantities are provided", {
  expect_error(getOutputValues(simResults, 1))
})

test_that("It throws an error when no valid individual ids are provided", {
  expect_error(getOutputValues(simResults, resultsPaths, "one"))
})

test_that("It can retrieve results by paths", {
  results <- getOutputValues(simResults, resultsPaths)
  expect_equal(length(results), length(resultsPaths))
})

test_that("It can retrieve results by quantities", {
  results <- getOutputValues(simResults, getAllQuantitiesMatching(resultsPaths, sim))
  expect_equal(length(results), length(resultsPaths))
})

test_that("It can retrieve results with provided individual id", {
  results <- getOutputValues(simResults, resultsPaths, individualIds = c(0, 0, 1))
  expect_equal(length(results), length(resultsPaths))
  expect_false(is.null(results[[1]]))
})

test_that("It returns NULL for results if no individual id was simulated", {
  results <- getOutputValues(simResults, resultsPaths, individualIds = 1)
  expect_equal(length(results), length(resultsPaths))
  expect_null(results[[1]])
})

test_that("It returns NULL for paths that were not simulated", {
  results <- getOutputValues(simResults, "testPath")
  expect_equal(length(results), 1)
  expect_null(results$"testPath")
})

context("exportResultsToCSV")

test_that("It can export valid simulation results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportResultsToCSV(simResults, csvFile)
    expect_true(file.exists(csvFile))
  })
})

context("importResultsFromCSV")

test_that("It can import valid simulation results from one CSV file", {
  resFile <- getTestDataFilePath("res_10.csv")
  simResults <- importResultsFromCSV(sim, resFile)
  expect_equal(simResults$count, 10)
})

test_that("It save the reference to the original simulation", {
  resFile <- getTestDataFilePath("res_10.csv")
  results <- importResultsFromCSV(sim, resFile)
  expect_equal(results$simulation, sim)
})

test_that("It can import valid simulation results from multiple CSV files", {
  res1_10File <- getTestDataFilePath("res_10.csv")
  res11_20File <- getTestDataFilePath("res_11-20.csv")
  simResults <- importResultsFromCSV(sim, c(res1_10File, res11_20File))
  expect_equal(simResults$count, 20)
})

test_that("It throws an exception if the file imported are not valid results file", {
  junkFile <- getTestDataFilePath("pop_10.csv")
  expect_that(importResultsFromCSV(sim, junkFile), throws_error())
})

test_that("It throws an exception when importing a valid result file that does not match the simulation", {
  otherSim <- loadTestSimulation("simple")
  resFile <- getTestDataFilePath("res_10.csv")
  expect_that(importResultsFromCSV(otherSim, resFile), throws_error())
})

