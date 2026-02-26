sim <- loadTestSimulation("S1")
individualResults <- runSimulations(sim)[[1]]
resultsPaths <- individualResults$allQuantityPaths

population <- loadPopulation(getTestDataFilePath("pop.csv"))
populationResults <- runSimulations(sim, population)[[1]]


NUMBER_OF_COVARIATES_COLUMNS <- 3
NUMBER_OF_STATIC_COLUMNS <- 2
NUMBER_OF_EXTRA_COLUMNS <- NUMBER_OF_STATIC_COLUMNS +
  NUMBER_OF_COVARIATES_COLUMNS

# getOutputValues

test_that("It throws an error when no valid simulation results are provided", {
  expect_error(getOutputValues(individualResults, "NoPath"))
})

test_that("It returns an array of NA if specific result is not found and stopIfNotFound = FALSE", {
  res <- getOutputValues(
    simulationResults = individualResults,
    quantitiesOrPaths = "NoPath",
    stopIfNotFound = FALSE
  )

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
  res <- getOutputValues(
    populationResults,
    population = population,
    getAllQuantitiesMatching(resultsPaths, sim)
  )
  data <- res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_EXTRA_COLUMNS)
})

test_that("It can retrieve correct unit and dimension", {
  res <- getOutputValues(
    populationResults,
    population = population,
    getAllQuantitiesMatching(resultsPaths, sim)
  )
  path <- resultsPaths[[1]]
  quantity <- getQuantity(path = path, sim)

  metadata <- res$metaData
  expect_equal(metadata[[path]]$unit, quantity$unit)
  expect_equal(metadata[[path]]$dimension, quantity$dimension)
})

test_that("It should return a data and meta data data frame per output paths", {
  path <- resultsPaths[[1]]
  res <- getOutputValues(
    populationResults,
    population = population,
    path,
    individualIds = c(0, 1)
  )
  data <- res$data
  metaData <- res$metaData
  expect_equal(length(data), 1 + NUMBER_OF_EXTRA_COLUMNS)
  expect_false(is.null(data))
  expect_false(is.null(metaData))
  expect_null(data[[resultsPaths[[2]]]])
})

test_that("It should return NULL for meta data if addMetaData = FALSE", {
  path <- resultsPaths[[1]]
  res <- getOutputValues(
    populationResults,
    population = population,
    path,
    individualIds = c(0, 1),
    addMetaData = FALSE
  )
  data <- res$data
  metaData <- res$metaData
  expect_equal(length(data), 1 + NUMBER_OF_EXTRA_COLUMNS)
  expect_false(is.null(data))
  expect_null(metaData)
  expect_null(data[[resultsPaths[[2]]]])
})

test_that("It can retrieve results with provided individual id", {
  res <- getOutputValues(
    populationResults,
    population = population,
    individualIds = c(1, 3, 5)
  )
  data <- res$data
  expect_equal(length(data), length(resultsPaths) + NUMBER_OF_EXTRA_COLUMNS)
  indInd <- unique(data$IndividualId)
  expect_identical(indInd, c(1, 3, 5))
  for (path in resultsPaths) {
    dataForPath <- data[[path]]
    expect_false(is.null(dataForPath))
  }
})


# exportResultsToCSV

test_that("It can export valid simulation results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportResultsToCSV(individualResults, csvFile)
    expect_true(file.exists(csvFile))
  })
})

test_that("It throws an error when a list of simulation results is provided", {
  listOfResults <- list(individualResults, individualResults)
  expect_error(
    exportResultsToCSV(listOfResults, "dummy.csv"),
    "Only one 'SimulationResults' object is allowed"
  )
})

test_that("It throws an error when output from runSimulations is provided directly", {
  allResults <- runSimulations(sim)
  expect_error(
    exportResultsToCSV(allResults, "dummy.csv"),
    "Only one 'SimulationResults' object is allowed"
  )
})

# importResultsFromCSV

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
  junkFile <- getTestDataFilePath("pop.csv")
  expect_error(importResultsFromCSV(sim, junkFile))
})

test_that("It throws an exception when importing a valid result file that does not match the simulation", {
  otherSim <- loadTestSimulation("simple")
  resFile <- getTestDataFilePath("res_10.csv")
  expect_error(importResultsFromCSV(otherSim, resFile))
})


test_that("simulationResultsToDataFrame works as expected - minimal pkml", {
  simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulations(sim)[[1]]

  df1 <- simulationResultsToDataFrame(results)
  df2 <- simulationResultsToDataFrame(results, quantitiesOrPaths = "Organism|A")

  # should not be grouped
  expect_false(dplyr::is_grouped_df(df1))
  expect_false(dplyr::is_grouped_df(df2))

  # with all paths
  expect_equal(dim(df1), c(84L, 9L))
  expect_equal(
    unique(df1$paths),
    c("Organism|Liver|A", "Organism|Liver|B", "Organism|A", "Organism|B")
  )
  expect_equal(unique(df1$IndividualId), 0)
  expect_equal(unique(df1$unit), .encodeUnit("µmol"))
  expect_equal(unique(df1$dimension), "Amount")
  expect_equal(unique(df1$TimeUnit), "min")
  expect_equal(unique(df1$TimeDimension), "Time")

  # certain path
  expect_equal(dim(df2), c(21L, 9L))
  expect_equal(unique(df2$paths), "Organism|A")

  # names
  expect_equal(
    sort(names(df1)),
    sort(c(
      "paths",
      "IndividualId",
      "Time",
      "simulationValues",
      "unit",
      "dimension",
      "TimeUnit",
      "TimeDimension",
      "molWeight"
    ))
  )
})

test_that("simulationResultsToDataFrame works as expected - Aciclovir", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulations(sim)[[1]]

  df1 <- simulationResultsToDataFrame(results)

  # with all paths
  expect_equal(dim(df1), c(491L, 9L))
  expect_s3_class(df1, "data.frame")
  expect_equal(
    unique(df1$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(unique(df1$IndividualId), 0)
  expect_equal(unique(df1$unit), .encodeUnit("µmol/l"))
  expect_equal(unique(df1$dimension), "Concentration (molar)")
  expect_equal(unique(df1$TimeUnit), "min")
})

test_that("simulationResultsToTibble works as expected - Aciclovir", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulations(sim)[[1]]

  df1 <- simulationResultsToTibble(results)

  # with all paths
  expect_equal(dim(df1), c(491L, 9L))
  expect_s3_class(df1, "tbl_df")
})

test_that("simulationResultsToDataFrame with lists", {
  # Load and run multiple simulations concurrently.
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

  # We load 3 times the same simulation for convenience. But in real life scenarios,
  # they should be different simulations
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  sim3 <- loadSimulation(simFilePath)

  # list is not allowed, so this should fail
  simulationResults <- runSimulations(simulations = list(sim1, sim2, sim3))
  expect_error(simulationResultsToDataFrame(simulationResults))
})

test_that("It retrieves simulation results of an individual simulation after changing simulation name", {
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim$name <- "foo"

  res <- runSimulations(sim)
  expect_no_error(
    resultValues <- getOutputValues(
      simulationResults = res[[1]],
      quantitiesOrPaths = res[[1]]$allQuantityPaths
    )
  )
})

test_that("It retrieves simulation results of a population simulation after changing simulation name", {
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  populationFileName <- getTestDataFilePath(fileName = "pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  sim$name <- "foo"

  simResults <- runSimulations(sim, population = population)
  expect_no_error(
    resultValues <- getOutputValues(
      simulationResults = simResults[[1]],
      quantitiesOrPaths = simResults[[1]]$allQuantityPaths
    )
  )
})

test_that("It throws an error when trying to change the name of the simulation to a forbidden name", {
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  expect_error(
    sim$name <- "MoleculeProperties",
    regexp = messages$forbiddenSimulationName("MoleculeProperties", sim)
  )
  expect_error(
    sim$name <- "CYP3A4",
    regexp = messages$forbiddenSimulationName("CYP3A4", sim)
  )
})
