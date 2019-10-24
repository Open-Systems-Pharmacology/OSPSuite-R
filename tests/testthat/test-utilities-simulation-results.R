sim <- loadTestSimulation("S1")
individualResults <- runSimulation(sim)
resultsPaths <- individualResults$allQuantityPaths

population <- loadPopulation(getTestDataFilePath("pop_10.csv"))
populationResults <- runSimulation(sim, population)

NUMBER_OF_STATIC_COLUMNS <-5

context("getOutputValues")

test_that("It throws an error when no valid simulation results are provided", {
  expect_error(getOutputValues(sim, resultsPaths))
})

test_that("It throws an error when no valid paths or quantities are provided", {
  expect_error(getOutputValues(individualResults, 1))
})

test_that("It throws an error when no valid individual ids are provided", {
  expect_error(getOutputValues(individualResults, resultsPaths, "one"))
})

test_that("It can retrieve results by paths", {
  results <- getOutputValues(individualResults, resultsPaths)
  expect_equal(length(results), length(resultsPaths))
})

test_that("It can retrieve results by quantities", {
  results <- getOutputValues(individualResults, getAllQuantitiesMatching(resultsPaths, sim))
  expect_equal(length(results), length(resultsPaths))
})

test_that("It can retrieve results with provided individual id", {
  results <- getOutputValues(individualResults, resultsPaths, individualIds = c(0, 0, 1))
  expect_equal(length(results), length(resultsPaths))
  expect_false(is.null(results[[1]]))
})

test_that("It returns NA for results if no individual id was simulated", {
  results <- getOutputValues(individualResults, resultsPaths, individualIds = 1)
  expect_equal(length(results), length(resultsPaths))
  expect_null(results[[1]])
})

test_that("It returns NULL for paths that were not simulated", {
  results <- getOutputValues(individualResults, "testPath")
  expect_equal(length(results), 1)
  expect_null(results$"testPath")
 })


 context("getOutputValuesTLF")

test_that("It throws an error when no valid simulation results are provided", {
  expect_error(getOutputValuesTLF(sim, resultsPaths))
})

test_that("It throws an error when no valid paths or quantities are provided", {
  expect_error(getOutputValuesTLF(populationResults, population, 1))
})

test_that("It throws an error when no valid population is provided ids are provided", {
  expect_error(getOutputValuesTLF(populationResults, resultsPaths, 1))
})

test_that("It can retrieve results by paths", {
  res <- getOutputValuesTLF(populationResults, population)
  data <-res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_STATIC_COLUMNS)
})

test_that("It can retrieve results by quantities", {
  res <- getOutputValuesTLF(populationResults, population, getAllQuantitiesMatching(resultsPaths, sim))
  data <-res$data
  expect_equal(length(data), length(resultsPaths)  + NUMBER_OF_STATIC_COLUMNS)
})

test_that("It should return a data and meta data data frame per output paths", {
  path <- resultsPaths[[1]]
  res <- getOutputValuesTLF(populationResults, population, path, individualIds= c(0, 1))
  data <-res$data
  metaData <- res$metaData
  expect_equal(length(data), 1  + NUMBER_OF_STATIC_COLUMNS)
  expect_false(is.null(data))
  expect_false(is.null(metaData))
  expect_null(data[[resultsPaths[[2]]]])
})

test_that("It can retrieve results with provided individual id", {
  res <-getOutputValuesTLF(populationResults, population, individualIds = c(1, 3, 5))
  data <-res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_STATIC_COLUMNS)
  indInd <- unique(data$IndividualId)
  expect_identical(indInd, c(1, 3, 5))
  for (path in resultsPaths) {
    dataForPath <-data[[path]]
    expect_false(is.null(dataForPath))
  }
})


context("exportResultsToCSV")

test_that("It can export valid simulation results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportResultsToCSV(individualResults, csvFile)
    expect_true(file.exists(csvFile))
  })
})

context("importResultsFromCSV")

test_that("It can import valid simulation results from one CSV file", {
  resFile <- getTestDataFilePath("res_10.csv")
  individualResults <- importResultsFromCSV(sim, resFile)
  expect_equal(individualResults$count, 10)
})

test_that("It save the reference to the original simulation", {
  resFile <- getTestDataFilePath("res_10.csv")
  results <- importResultsFromCSV(sim, resFile)
  expect_equal(results$simulation, sim)
})

test_that("It can import valid simulation results from multiple CSV files", {
  res1_10File <- getTestDataFilePath("res_10.csv")
  res11_20File <- getTestDataFilePath("res_11-20.csv")
  individualResults <- importResultsFromCSV(sim, c(res1_10File, res11_20File))
  expect_equal(individualResults$count, 20)
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
