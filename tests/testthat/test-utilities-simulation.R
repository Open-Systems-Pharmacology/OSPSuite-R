
context("exportIndividualSimulations")

test_that("It can export the simulation for file for given individual in a population", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  paths <- exportIndividualSimulations(population, c(1, 2), tempdir(), sim)
  expect_length(paths, 2)
  sapply(paths, function(p) file.remove(p))
})


test_that("It throws an exception when trying to export for an individual id that does not exist in the population", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  expect_that(exportIndividualSimulations(population, c(50, 2), tempdir(), sim), throws_error())
})


context("setSimulationParameterValue")

test_that("It can set single parameter values", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  setSimulationParameterValues(parameterPath, 100, sim)
  parameter <- getParameter(parameterPath, sim)
  expect_equal(parameter$value, 100)
})

test_that("It can set multiple parameter values", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath1 <- "Organism|Liver|Intracellular|Volume"
  parameterPath2 <- "Organism|Kidney|Intracellular|Volume"
  setSimulationParameterValues(c(parameterPath1, parameterPath2), c(40, 50), sim)
  parameter1 <- getParameter(parameterPath1, sim)
  parameter2 <- getParameter(parameterPath2, sim)
  expect_equal(parameter1$value, 40)
  expect_equal(parameter2$value, 50)
})

test_that("It throws an exception when setting values for a parameter that does not exist", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath1 <- "Organism|Liver|NOPE|Volume"
  expect_that(setSimulationParameterValues(parameterPath, 100, sim), throws_error())
})

test_that("It can get the value of an individual from a population and set them into a simulation", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  individualValues <- population$getParameterValuesForIndividual(8)
  setSimulationParameterValues(individualValues$paths, individualValues$values, sim)
  parameter <- getParameter(individualValues$paths[1], sim)
  expect_equal(parameter$value, individualValues$values[1])
})

context("loadSimulation")

test_that("It can load a valid pkml simulation file with 'loadFromCache = TRUE' without previously loaded sim", {
  resetSimulationCache()

  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  expect_true(!is.null(sim))
})

test_that("It can load a valid pkml simulation file with 'loadFromCache = FALSE' without previously loaded sim", {
  resetSimulationCache()

  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  expect_true(!is.null(sim))
})

test_that("It can load a simulation from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation("S1", loadFromCache = TRUE)
  sim2 <- loadTestSimulation("S1", loadFromCache = TRUE)

  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim1)
  parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim2)

  setParameterValues(parameters = parameter1, values = 0)
  expect_equal(parameter1$value, parameter2$value)
})

test_that("It can load two simulations not from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)

  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim1)
  parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim2)

  setParameterValues(parameters = parameter1, values = 0)
  expect_false(isTRUE(identical(parameter1$value, parameter2$value)))
})

test_that("Two sims not from cache and third from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation("S1", loadFromCache = TRUE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim3 <- loadTestSimulation("S1", loadFromCache = TRUE)

  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim1)
  parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim2)
  parameter3 <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim3)

  setParameterValues(parameters = parameter1, values = 1)

  expect_false(isTRUE(identical(parameter1$value, parameter3$value)))
  expect_equal(parameter2$value, parameter3$value)
})

test_that("It throws an exception if the pkml loaded is not a valid simulation file", {
  expect_that(loadTestSimulation("molecules"), throws_error("Could not load simulation"))
})

test_that("It can remove simulation from cache", {
  resetSimulationCache()
  sim1 <- loadTestSimulation("S1")

  expect_true(removeSimulationFromCache(sim1))
})

test_that("It returns false when attempting to remove a simulation from cache that is not cached", {
  resetSimulationCache()
  sim1 <- loadTestSimulation("S1")
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE, addToCache = FALSE)

  expect_false(removeSimulationFromCache(sim2))
})


context("runSimulation")
test_that("It can run a valid individual simulation and returns results", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(sim)
  expect_equal(results$count, 1)
})

test_that("It can run a valid population simulation and returns results", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(sim, population)
  expect_equal(results$count, population$count)
})

test_that("It throws an exception when running a population simulation with the wrong argumetns", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  expect_that(runSimulation(population, simulation), throws_error())
})

context("getStandardMoleculeParameters")
test_that("It returns all molecule parameters for an existing molecule in a simulation", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameters <- getStandardMoleculeParameters("CYP3A4", sim)
  expect_equal(length(parameters), length(MoleculeParameter))
})

test_that("It returns an empty list of parameters for a molecule that does not exist", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameters <- getStandardMoleculeParameters("NOPE", sim)
  expect_equal(length(parameters), 0)
})

context("getAllParametersForSensitivityAnalysisMatching")
test_that("It returns all parameter potentially interesting for sensitivity analysis for a given wild card path", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameters <- getAllParametersMatching("**|Volume", sim)
  varableParameters <- getAllParametersForSensitivityAnalysisMatching("**|Volume", sim)
  expect_gt(length(parameters), length(varableParameters))
})
