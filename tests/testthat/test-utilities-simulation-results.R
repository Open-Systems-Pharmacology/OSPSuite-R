sim <- loadTestSimulation("S1")
individualResults <- runSimulation(sim)
resultsPaths <- individualResults$allQuantityPaths

population <- loadPopulation(getTestDataFilePath("pop.csv"))
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
  results <- runSimulation(sim)

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
  expect_equal(unique(df1$unit), "µmol")
  expect_equal(unique(df1$dimension), "Amount")
  expect_equal(unique(df1$TimeUnit), "min")
  expect_equal(unique(df1$TimeDimension), "Time")

  # certain path
  expect_equal(dim(df2), c(21L, 9L))
  expect_equal(unique(df2$paths), "Organism|A")

  # names
  expect_equal(
    names(df1),
    c(
      "paths", "IndividualId", "Time", "simulationValues", "unit",
      "dimension", "TimeUnit", "TimeDimension", "molWeight"
    )
  )
})

test_that("simulationResultsToDataFrame works as expected - Aciclovir", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulation(sim)

  df1 <- simulationResultsToDataFrame(results)

  # with all paths
  expect_equal(dim(df1), c(491L, 9L))
  expect_s3_class(df1, "data.frame")
  expect_equal(
    unique(df1$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(unique(df1$IndividualId), 0)
  expect_equal(unique(df1$unit), "µmol/l")
  expect_equal(unique(df1$dimension), "Concentration (molar)")
  expect_equal(unique(df1$TimeUnit), "min")
})

test_that("simulationResultsToTibble works as expected - Aciclovir", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulation(sim)

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


test_that("simulationResultsToDataFrame with population", {
  skip_on_os("linux")
  skip_on_ci()

  # If no unit is specified, the default units are used. For "height" it is "dm",
  # for "weight" it is "kg", for "age" it is "year(s)".
  populationCharacteristics <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$Asian_Tanaka_1996,
    numberOfIndividuals = 50,
    proportionOfFemales = 50,
    weightMin = 30,
    weightMax = 98,
    weightUnit = "kg",
    heightMin = NULL,
    heightMax = NULL,
    ageMin = 0,
    ageMax = 80,
    ageUnit = "year(s)"
  )

  # Create population from population characteristics
  result <- createPopulation(populationCharacteristics = populationCharacteristics)
  myPopulation <- result$population

  # Load simulation
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  populationResults <- runSimulation(
    simulation = sim,
    population = myPopulation
  )

  df1 <- simulationResultsToDataFrame(populationResults)

  expect_equal(dim(df1), c(24550L, 9L))
  expect_equal(
    unique(df1$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(min(df1$IndividualId), 0)
  expect_equal(max(df1$IndividualId), 49)
  expect_equal(unique(df1$unit), "µmol/l")
  expect_equal(unique(df1$dimension), "Concentration (molar)")
  expect_equal(unique(df1$TimeUnit), "min")

  df2 <- simulationResultsToDataFrame(populationResults, individualIds = c(1, 4, 5))

  expect_equal(dim(df2), c(1473L, 9L))
  expect_equal(
    unique(df2$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(min(df2$IndividualId), 1)
  expect_equal(max(df2$IndividualId), 5)
  expect_equal(unique(df2$unit), "µmol/l")
  expect_equal(unique(df2$dimension), "Concentration (molar)")
  expect_equal(unique(df2$TimeUnit), "min")
})
