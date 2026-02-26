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

test_that("It throws an error for empty list", {
  expect_error(
    getOutputValues(list()),
    regexp = "simulationResults cannot be an empty list"
  )
})

test_that("It throws an error for list with mixed valid and invalid types", {
  # Should validate all elements and catch invalid ones
  expect_error(
    getOutputValues(list(individualResults, "invalid")),
    regexp = "SimulationResults"
  )
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

test_that("It throws an error when a list with multiple simulation results is provided", {
  listOfResults <- list(individualResults, individualResults)
  expect_error(
    exportResultsToCSV(listOfResults, "dummy.csv"),
    regexp = messages$errorExportResultsOnlyOneObject()
  )
})

test_that("It works with a list containing one simulation result", {
  # Create a list with one element (mimics runSimulations output)
  listOfResults <- list(individualResults)
  executeWithTestFile(function(csvFile) {
    exportResultsToCSV(listOfResults, csvFile)
    expect_true(file.exists(csvFile))
  })
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

test_that("simulationResultsToDataFrame with lists now works", {
  # Load and run multiple simulations concurrently.
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

  # We load 3 times the same simulation for convenience. But in real life scenarios,
  # they should be different simulations
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  sim3 <- loadSimulation(simFilePath)

  # list is now allowed - should return a list of data frames
  simulationResults <- runSimulations(simulations = list(sim1, sim2, sim3))
  dfList <- simulationResultsToDataFrame(simulationResults)
  
  # Should return a list of data frames
  expect_true(is.list(dfList))
  expect_equal(length(dfList), 3)
  expect_true(all(sapply(dfList, is.data.frame)))
  
  # Each data frame should have the expected structure
  expect_equal(dim(dfList[[1]]), c(491L, 9L))
  expect_equal(dim(dfList[[2]]), c(491L, 9L))
  expect_equal(dim(dfList[[3]]), c(491L, 9L))
})

test_that("getOutputValues works with a list of SimulationResults", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  
  # Run simulations to get a list
  simulationResults <- runSimulations(simulations = list(sim1, sim2))
  
  # getOutputValues should work with the list
  outputList <- getOutputValues(simulationResults)
  
  # Should return a list
  expect_true(is.list(outputList))
  expect_equal(length(outputList), 2)
  
  # Each element should have data and metaData
  expect_true(!is.null(outputList[[1]]$data))
  expect_true(!is.null(outputList[[1]]$metaData))
  expect_true(!is.null(outputList[[2]]$data))
  expect_true(!is.null(outputList[[2]]$metaData))
})

test_that("getOutputValues works with a single SimulationResults (backward compatibility)", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)
  
  # Run single simulation
  results <- runSimulations(sim)[[1]]
  
  # getOutputValues should work with single result
  output <- getOutputValues(results)
  
  # Should return a single result (not a list of lists)
  expect_true(is.list(output))
  expect_true(!is.null(output$data))
  expect_true(!is.null(output$metaData))
  # Should not be nested
  expect_false(is.list(output$data[[1]]))
})

test_that("simulationResultsToDataFrame works with single result (backward compatibility)", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)
  
  results <- runSimulations(sim)[[1]]
  df <- simulationResultsToDataFrame(results)
  
  # Should return a data.frame, not a list
  expect_true(is.data.frame(df))
  expect_false(is.list(df) && !is.data.frame(df))
})

test_that("simulationResultsToTibble works with a list of SimulationResults", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  
  simulationResults <- runSimulations(simulations = list(sim1, sim2))
  tibbleList <- simulationResultsToTibble(simulationResults)
  
  # Should return a list of tibbles
  expect_true(is.list(tibbleList))
  expect_equal(length(tibbleList), 2)
  expect_true(all(sapply(tibbleList, function(x) inherits(x, "tbl_df"))))
})

test_that("calculatePKAnalyses works with a list of SimulationResults", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simPath)
  sim2 <- loadSimulation(simPath)
  
  addOutputs("Organism|VenousBlood|*|Aciclovir", sim1)
  addOutputs("Organism|VenousBlood|*|Aciclovir", sim2)
  
  simulationResults <- runSimulations(simulations = list(sim1, sim2))
  pkAnalysesList <- calculatePKAnalyses(simulationResults)
  
  # Should return a list of SimulationPKAnalyses
  expect_true(is.list(pkAnalysesList))
  expect_equal(length(pkAnalysesList), 2)
  expect_true(all(sapply(pkAnalysesList, function(x) inherits(x, "SimulationPKAnalyses"))))
})

test_that("calculatePKAnalyses works with single result (backward compatibility)", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)
  
  addOutputs("Organism|VenousBlood|*|Aciclovir", sim)
  results <- runSimulations(sim)[[1]]
  pkAnalyses <- calculatePKAnalyses(results)
  
  # Should return a single SimulationPKAnalyses object, not a list
  expect_s3_class(pkAnalyses, "SimulationPKAnalyses")
  expect_false(is.list(pkAnalyses) && !inherits(pkAnalyses, "SimulationPKAnalyses"))
})

test_that("getOutputValues works with list of one element (no [[1]] needed)", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)
  
  # Run simulation to get a list with one element
  simulationResults <- runSimulations(sim)
  
  # getOutputValues should work and return a single result (not a list)
  output <- getOutputValues(simulationResults)
  
  # Should return a single result structure (not a list of lists)
  expect_true(is.list(output))
  expect_true(!is.null(output$data))
  expect_true(!is.null(output$metaData))
  # Should not be a list of results
  expect_false("data" %in% names(output[[1]]))
})

test_that("simulationResultsToDataFrame works with list of one element (no [[1]] needed)", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)
  
  # Run simulation to get a list with one element
  simulationResults <- runSimulations(sim)
  
  # Should work and return a single data.frame (not a list)
  df <- simulationResultsToDataFrame(simulationResults)
  
  # Should return a data.frame, not a list of data.frames
  expect_true(is.data.frame(df))
  expect_false(is.list(df) && !is.data.frame(df))
  expect_equal(dim(df)[2], 9L)
})

test_that("calculatePKAnalyses works with list of one element (no [[1]] needed)", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)
  
  addOutputs("Organism|VenousBlood|*|Aciclovir", sim)
  
  # Run simulation to get a list with one element
  simulationResults <- runSimulations(sim)
  
  # Should work and return a single SimulationPKAnalyses object (not a list)
  pkAnalyses <- calculatePKAnalyses(simulationResults)
  
  # Should return a single object, not a list
  expect_s3_class(pkAnalyses, "SimulationPKAnalyses")
  expect_false(is.list(pkAnalyses) && !inherits(pkAnalyses, "SimulationPKAnalyses"))
})

test_that("exportResultsToCSV works with a list containing one element", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)
  
  # Run simulation to get a list with one element
  simulationResults <- runSimulations(sim)
  
  executeWithTestFile(function(csvFile) {
    # Should work with list containing one element
    exportResultsToCSV(simulationResults, csvFile)
    expect_true(file.exists(csvFile))
  })
})

test_that("exportResultsToCSV fails with a list containing multiple elements", {
  simFilePath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simFilePath)
  sim2 <- loadSimulation(simFilePath)
  
  # Run simulations to get a list with multiple elements
  simulationResults <- runSimulations(simulations = list(sim1, sim2))
  
  # Should fail with list containing multiple elements
  expect_error(
    exportResultsToCSV(simulationResults, "dummy.csv"),
    regexp = messages$errorExportResultsOnlyOneObject()
  )
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
