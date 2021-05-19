sim <- loadTestSimulation("S1")
individualResults <- runSimulation(sim)
resultsPaths <- individualResults$allQuantityPaths

population <- loadPopulation(getTestDataFilePath("pop_10.csv"))
populationResults <- runSimulation(sim, population)


NUMBER_OF_COVARIATES_COLUMNS <- 3
NUMBER_OF_STATIC_COLUMNS <- 2
NUMBER_OF_EXTRA_COLUMNS <- NUMBER_OF_STATIC_COLUMNS + NUMBER_OF_COVARIATES_COLUMNS

context("getOutputValues")

test_that("It throws an error when no valid simulation results are provided", {
  expect_error(getOutputValues(individualResults, "NoPath"))
})

test_that("It returns an array of NA if specific result is not found and stopIfNotFound = FALSE", {
  res <- getOutputValues(simulationResults = individualResults, quantitiesOrPaths = "NoPath", stopIfNotFound = FALSE)

  data <- res$data
  expect_equal(length(data), 1 + NUMBER_OF_STATIC_COLUMNS)

  expect_true(all(is.na(data$NoPath)))
})

test_that("It can retrieve values without a population specified", {
  res <- getOutputValues(populationResults)
  data <- res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_STATIC_COLUMNS)
})

test_that("It throws an error when no valid paths or quantities are provided", {
  expect_error(getOutputValues(populationResults, population = population, 1))
})

test_that("It can retrieve results by paths", {
  res <- getOutputValues(populationResults, population = population)
  data <- res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_EXTRA_COLUMNS)
})

test_that("It can retrieve results by quantities", {
  res <- getOutputValues(populationResults, population = population, getAllQuantitiesMatching(resultsPaths, sim))
  data <- res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_EXTRA_COLUMNS)
})

test_that("It can retrieve correct unit and dimension", {
  res <- getOutputValues(populationResults, population = population, getAllQuantitiesMatching(resultsPaths, sim))
  path <- resultsPaths[[1]]
  quantity <- getQuantity(path = path, sim)

  metadata <- res$metaData
  expect_equal(metadata[[path]]$unit, quantity$unit)
  expect_equal(metadata[[path]]$dimension, quantity$dimension)
})

test_that("It should return a data and meta data data frame per output paths", {
  path <- resultsPaths[[1]]
  res <- getOutputValues(populationResults, population = population, path, individualIds = c(0, 1))
  data <- res$data
  metaData <- res$metaData
  expect_equal(length(data), 1 + NUMBER_OF_EXTRA_COLUMNS)
  expect_false(is.null(data))
  expect_false(is.null(metaData))
  expect_null(data[[resultsPaths[[2]]]])
})

test_that("It should return NULL for meta data if addMetaData = FALSE", {
  path <- resultsPaths[[1]]
  res <- getOutputValues(populationResults, population = population, path, individualIds = c(0, 1), addMetaData = FALSE)
  data <- res$data
  metaData <- res$metaData
  expect_equal(length(data), 1 + NUMBER_OF_EXTRA_COLUMNS)
  expect_false(is.null(data))
  expect_null(metaData)
  expect_null(data[[resultsPaths[[2]]]])
})

test_that("It can retrieve results with provided individual id", {
  res <- getOutputValues(populationResults, population = population, individualIds = c(1, 3, 5))
  data <- res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_EXTRA_COLUMNS)
  indInd <- unique(data$IndividualId)
  expect_identical(indInd, c(1, 3, 5))
  for (path in resultsPaths) {
    dataForPath <- data[[path]]
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
