context("exportIndividualSimulations")

test_that("It can export the simulation for file for given individual in a population", {
  populationFileName <- getTestDataFilePath(fileName = "pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  sim <- loadTestSimulation(simulationName = "S1", loadFromCache = FALSE)
  paths <- exportIndividualSimulations(population = population, individualIds = c(1, 2), outputFolder = tempdir(), simulation = sim)
  expect_length(paths, 2)
  expect_true(grepl("S1_1.pkml", paths[1], fixed = TRUE))
  expect_true(grepl("S1_2.pkml", paths[2], fixed = TRUE))
  sapply(paths, function(p) file.remove(p))
})


test_that("It throws an exception when trying to export for an individual id that does not exist in the population", {
  populationFileName <- getTestDataFilePath(fileName = "pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  sim <- loadTestSimulation(simulationName = "S1", loadFromCache = FALSE)
  expect_error(exportIndividualSimulations(population = population, individualIds = c(50, 2), outputFolder = tempdir(), simulation = sim))
})

context("loadSimulation")

test_that("It can load a valid pkml simulation file with 'loadFromCache = TRUE' without previously loaded sim", {
  resetSimulationCache()

  sim <- loadTestSimulation(simulationName = "S1", loadFromCache = TRUE)
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

  parameter1 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim1)
  parameter2 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim2)

  setParameterValues(parameters = parameter1, values = 0)
  expect_equal(parameter1$value, parameter2$value)
})

test_that("It can load two simulations not from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)

  parameter1 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim1)
  parameter2 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim2)

  setParameterValues(parameters = parameter1, values = 0)
  expect_false(isTRUE(identical(parameter1$value, parameter2$value)))
})

test_that("Two sims not from cache and third from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation("S1", loadFromCache = TRUE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim3 <- loadTestSimulation("S1", loadFromCache = TRUE)

  parameter1 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim1)
  parameter2 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim2)
  parameter3 <- getParameter(path = toPathString(c("Organism", "Liver", "Intracellular", "Volume")), container = sim3)

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
  results <- runSimulation(simulation = sim)
  expect_equal(results$count, 1)
})

test_that("It can run a valid population simulation and returns results", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(simulation = sim, population = population)
  expect_equal(results$count, population$count)
})

test_that("It can run a valid population simulation with aging data and returns results", {
  populationFileName <- getTestDataFilePath("baby.csv")
  agingDataFileName <- getTestDataFilePath("baby_aging.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  agingData <- loadAgingDataFromCSV(filePath = agingDataFileName)

  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(simulation = sim, population = population, agingData = agingData)
  expect_equal(results$count, population$count)
})

test_that("It can run a valid population simulation created directly from create population", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  list <- list(population = population)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  results <- runSimulation(simulation = sim, population = list)
  expect_equal(results$count, population$count)
})

test_that("It throws an exception when running a population simulation with the wrong arguments", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  expect_error(runSimulation(simulation = population, population = simulation))
})

test_that("It runs one individual simulation without simulationRunOptions", {
  resetSimulationCache()

  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  results <- runSimulation(simulation = sim)
  expect_true(isOfType(results, "SimulationResults"))
})

test_that("It runs one individual simulation with simulationRunOptions", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  simRunOptions <- SimulationRunOptions$new()
  results <- runSimulation(simulation = sim, simulationRunOptions = simRunOptions)
  expect_true(isOfType(results, "SimulationResults"))
})

test_that("It throws an erro when trying to run multiple simulations", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  expect_error(results <- runSimulation(simulation = c(sim, sim2)))
})

test_that("runSimulations returns a named list for one simulation", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  results <- runSimulations(simulations = sim)
  # Check the ids
  expect_equal(names(results)[[1]], sim$id)
  expect_true(isOfType(results[[1]], "SimulationResults"))
})

test_that("It runs multiple individual simulations", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  results <- runSimulations(simulations = c(sim, sim2))
  expect_equal(length(results), 2)
  # Check the ids
  expect_equal(names(results)[[1]], sim$id)
  expect_true(isOfType(results[[1]], "SimulationResults"))
})

test_that("It shows a warning if one of simulations fails. Results for this simulation are NULL", {
  resetSimulationCache()
  sim <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
  sim$solver$relTol <- 1000

  expect_warning(results <- runSimulations(simulations = c(sim, sim2)))
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

  expect_warning(results <- runSimulations(simulations = c(sim, sim2), silentMode = TRUE), regexp = NA)
  expect_equal(length(results), 2)
  expect_equal(names(results)[[2]], sim2$id)
  expect_null(results[[sim$id]])
  expect_true(isOfType(results[[2]], "SimulationResults"))
})

test_that("It throws an error when running multiple simulations with a population", {
  sim1 <- loadTestSimulation("simple", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("simple", loadFromCache = FALSE)
  populationFileName <- getTestDataFilePath(fileName = "pop.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  expect_error(runSimulations(simulations = c(sim1, sim2), population = population))
})

test_that("It throws an error when running the same instance of a simulation multiple time", {
  resetSimulationCache()
  sim1 <- loadTestSimulation("simple", loadFromCache = TRUE)
  sim2 <- loadTestSimulation("simple", loadFromCache = TRUE)
  expect_error(runSimulations(simulations = c(sim1, sim2)))
})

context("getStandardMoleculeParameters")
test_that("It returns all molecule parameters for an existing molecule in a simulation", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameters <- getStandardMoleculeParameters(moleculeName = "CYP3A4", simulation = sim)
  expect_equal(length(parameters), length(MoleculeParameter))
})

test_that("It returns an empty list of parameters for a molecule that does not exist", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameters <- getStandardMoleculeParameters(moleculeName = "NOPE", simulation = sim)
  expect_equal(length(parameters), 0)
})

context("getAllParametersForSensitivityAnalysisMatching")
test_that("It returns all parameter potentially interesting for sensitivity analysis for a given wild card path", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameters <- getAllParametersMatching(paths = "**|Volume", container = sim)
  variableParameters <- getAllParametersForSensitivityAnalysisMatching(paths = "**|Volume", simulation = sim)
  expect_gt(length(parameters), length(variableParameters))
})

context("createSimulationBatch")
test_that("It throws an error when initializing a simulation batch without any variable parameter or molecule", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  expect_error(createSimulationBatch(simulation = sim))
})

test_that("It creates a simulation batch when using only parameter paths", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(simulation = sim, parametersOrPaths = parameters)
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only molecule paths", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- c("Organism|Liver|A")
  simulationBatch <- createSimulationBatch(simulation = sim, moleculesOrPaths = molecules)
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only parameter instances", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameter1 <- getParameter(path = toPathString(c("Organism", "Liver", "Volume")), container = sim)
  simulationBatch <- createSimulationBatch(simulation = sim, parametersOrPaths = parameter1)
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only molecule instances", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecule <- getMolecule(path = toPathString(c("Organism", "Liver", "A")), container = sim)
  simulationBatch <- createSimulationBatch(simulation = sim, moleculesOrPaths = molecule)
  expect_false(is.null(simulationBatch))
})


context("runSimulationBatches")
sim <- loadTestSimulation("simple", loadFromCache = TRUE)

test_that("It can run a simulation batch by varying some parameters and molecules", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(simulation = sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(parameterValues = c(1.2, 2.4), initialValues = 2.5)
  res <- runSimulationBatches(simulationBatches = simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
})

test_that("It can run a simulation batch by varying some parameters only", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  id <- simulationBatch$addRunValues(parameterValues = c(1.2, 2.4))
  res <- runSimulationBatches(simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
})

test_that("It can run a simulation batch by varying some molecule only", {
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(initialValues = 1.2)
  res <- runSimulationBatches(simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
})

test_that("The result is NULL when the number of values does not match the initialization count for initial values", {
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(initialValues = c(1, 1.2))
  expect_warning(res <- runSimulationBatches(simulationBatch))
  expect_null(res[[1]][[1]])
})

test_that("The result is NULL when the number of values does not match the initialization count for parameters", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  id <- simulationBatch$addRunValues(parameterValues = c(1.2))
  expect_warning(res <- runSimulationBatches(simulationBatch))
  expect_null(res[[1]][[1]])
})

test_that("It throws an error when multiple values sets are added", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  ids <- c()
  ids[[1]] <- simulationBatch$addRunValues(parameterValues = c(1, 2), initialValues = 1)
  expect_error(simulationBatch$addRunValues(parameterValues = list(c(1, 2), c(2, 3)), initialValues = 1), regexp = messages$errorOnlyOneValuesSetAllowed("parameterValues, initialValues"))
})

test_that("It can run a simulation batch with multiple parameters and molecules values sets", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
  ids <- c()
  ids[[1]] <- simulationBatch$addRunValues(parameterValues = c(1, 2), initialValues = 1)
  ids[[2]] <- simulationBatch$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
  res <- runSimulationBatches(simulationBatches = simulationBatch)

  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
})

test_that("It can run multiple simulation batches with multiple parameters and molecules values sets", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch1 <- createSimulationBatch(simulation = sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
  simulationBatch2 <- createSimulationBatch(simulation = sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
  # Ids of run values
  ids <- c()
  ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
  ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
  ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
  ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
  res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
  expect_equal(length(res), 2)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
  expect_equal(names(res[[1]])[[2]], ids[[2]])
  expect_equal(names(res[[2]])[[1]], ids[[3]])
  expect_equal(names(res[[2]])[[2]], ids[[4]])
})

test_that("It can run a simulation batch, add new values, and run again", {
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(simulation = sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
  # Ids of run values
  ids <- c()
  ids[[1]] <- simulationBatch$addRunValues(parameterValues = c(1, 2), initialValues = 1)
  ids[[2]] <- simulationBatch$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
  res <- runSimulationBatches(simulationBatches = simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
  expect_equal(names(res[[1]])[[2]], ids[[2]])

  ids[[1]] <- simulationBatch$addRunValues(parameterValues = c(1, 2), initialValues = 1)
  ids[[2]] <- simulationBatch$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
  res <- runSimulationBatches(simulationBatches = simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
  expect_equal(names(res[[1]])[[2]], ids[[2]])
})

context("getAllStateVariablesPaths")

test_that("It returns the correct paths of the state variables", {
  sim <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  stateVariablePaths <- getAllStateVariablesPaths(simulation = sim)
  expect_equal(length(stateVariablePaths), 5)
})
