
context("exportIndividualSimulations")

test_that("It can export the simulation for file for given individual in a population", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  paths <- exportIndividualSimulations(population, c(1, 2), tempdir(), sim)
  expect_length(paths, 2)
  expect_true(grepl("S1_1.pkml", paths[1], fixed = TRUE))
  expect_true(grepl("S1_2.pkml", paths[2], fixed = TRUE))
  sapply(paths, function(p) file.remove(p))
})


test_that("It throws an exception when trying to export for an individual id that does not exist in the population", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  expect_that(exportIndividualSimulations(population, c(50, 2), tempdir(), sim), throws_error())
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

test_that("It can run a valid population simulation with aging data and returns results", {
  populationFileName <- getTestDataFilePath("baby.csv")
  agingDataFileName <- getTestDataFilePath("baby_aging.csv")
  population <- loadPopulation(populationFileName)
  agingData <- loadAgingDataFromCSV(agingDataFileName)

  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(sim, population, agingData)
  expect_equal(results$count, population$count)
})

test_that("It can run a valid population simulation created directly from create population", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  list <- list(population = population)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(sim, list)
  expect_equal(results$count, population$count)
})

test_that("It throws an exception when running a population simulation with the wrong arguments", {
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
  variableParameters <- getAllParametersForSensitivityAnalysisMatching("**|Volume", sim)
  expect_gt(length(parameters), length(variableParameters))
})


context("createSimulationBatch")

test_that("It throws an error when initializing a simulation batch without any variable parameter or molecule", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  expect_that(createSimulationBatch(sim), throws_error())
})

test_that("It creates a simulation batch when using only parameter paths", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parameters)
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only molecule paths", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- c("Organism|Liver|A")
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only parameter instances", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim)
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameter1)
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only molecule instances", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecule <- getMolecule(toPathString(c("Organism", "Liver", "A")), sim)
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecule)
  expect_false(is.null(simulationBatch))
})

context("runSimulationsConcurrent")

test_that("It runs one individual simulation without simulationRunOptions", {
  resetSimulationCache()

  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  results <- runSimulationsConcurrently(sim)
  expect_equal(length(results), 1)
  expect_true(isOfType(results[[1]], "SimulationResults"))
  })

test_that("It runs one individual simulation with simulationRunOptions", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  simRunOptions <- SimulationRunOptions$new()
  results <- runSimulationsConcurrently(sim, simulationRunOptions = simRunOptions)
  expect_equal(length(results), 1)
  expect_true(isOfType(results[[1]], "SimulationResults"))
})

test_that("It runs multiple individual simulations", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  results <- runSimulationsConcurrently(c(sim, sim2))
  expect_equal(length(results), 2)
  #Check the ids
  expect_equal(names(results)[[1]], sim$id)
  expect_true(isOfType(results[[1]], "SimulationResults"))
})

test_that("It shows a warning if one of simulations fails. Results for this simulation are NULL", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim$solver$relTol <- 1000

  expect_warning(results <- runSimulationsConcurrently(c(sim, sim2)))
  expect_equal(length(results), 2)
  expect_equal(names(results)[[2]], sim2$id)
  expect_null(results[[sim$id]])
  expect_true(isOfType(results[[2]], "SimulationResults"))
})

test_that("It does not show a warning if one of simulations fails in silent mode. Results for this simulation are NULL", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim$solver$relTol <- 1000

  expect_warning(results <- runSimulationsConcurrently(c(sim, sim2), silentMode = TRUE), regexp = NA)
  expect_equal(length(results), 2)
  expect_equal(names(results)[[2]], sim2$id)
  expect_null(results[[sim$id]])
  expect_true(isOfType(results[[2]], "SimulationResults"))
})
