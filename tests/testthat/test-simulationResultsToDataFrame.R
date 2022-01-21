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
