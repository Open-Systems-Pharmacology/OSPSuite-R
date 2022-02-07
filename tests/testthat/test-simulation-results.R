context("SimulationResults")

sim <- loadTestSimulation("S1")
simResults <- runSimulation(sim)

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


test_that("simulationResultsToDataFrame works as expected - minimal pkml", {
  simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulations(sim)

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
  results <- runSimulations(sim)

  df1 <- simulationResultsToDataFrame(results)

  # with all paths
  expect_equal(dim(df1), c(491L, 9L))
  expect_equal(
    unique(df1$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(unique(df1$IndividualId), 0)
  expect_equal(unique(df1$unit), "µmol/l")
  expect_equal(unique(df1$dimension), "Concentration (molar)")
  expect_equal(unique(df1$TimeUnit), "min")
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
  skip_if(.Platform$OS.type != "windows")
  # ospsuite::initPKSim("C:\\Program Files\\Open Systems Pharmacology\\PK-Sim 10.0")

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
