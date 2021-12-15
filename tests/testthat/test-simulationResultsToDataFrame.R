test_that("simulationResultsToDataFrame works as expected - minimal pkml", {
  simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulations(sim)

  df1 <- simulationResultsToDataFrame(results)
  df2 <- simulationResultsToDataFrame(results, quantitiesOrPaths = "Organism|A")

  # with all paths
  expect_equal(dim(df1), c(84L, 7L))
  expect_equal(
    unique(df1$paths),
    c("Organism|Liver|A", "Organism|Liver|B", "Organism|A", "Organism|B")
  )
  expect_equal(unique(df1$IndividualId), 0)
  expect_equal(unique(df1$unit), "µmol")
  expect_equal(unique(df1$dimension), "Amount")
  expect_equal(unique(df1$TimeUnit), "min")

  # certain path
  expect_equal(dim(df2), c(21L, 7L))
  expect_equal(unique(df2$paths), "Organism|A")
})

test_that("simulationResultsToDataFrame works as expected - Aciclovir", {
  simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simPath)

  # Running an individual simulation
  # results is an instance of `SimulationResults`
  results <- runSimulations(sim)

  df1 <- simulationResultsToDataFrame(results)

  # with all paths
  expect_equal(dim(df1), c(491L, 7L))
  expect_equal(
    unique(df1$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(unique(df1$IndividualId), 0)
  expect_equal(unique(df1$unit), "µmol/l")
  expect_equal(unique(df1$dimension), "Concentration (molar)")
  expect_equal(unique(df1$TimeUnit), "min")
})
