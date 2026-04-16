populationFileName <- system.file("extdata", "pop.csv", package = "ospsuite")
population <- loadPopulation(csvPopulationFile = populationFileName)

aciclovirSimulation <- loadSimulation(
  aciclovirSimulationPath,
  loadFromCache = TRUE
)
# exportIndividualSimulations

test_that("It can export the simulation for file for given individual in a population", {
  # Loading a simulation because the function will mutate it by calling "setParameterValuesByPath".
  aciclovirSimulation <- loadSimulation(
    aciclovirSimulationPath,
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  paths <- exportIndividualSimulations(
    population = population,
    individualIds = c(1, 2),
    outputFolder = tempdir(),
    simulation = aciclovirSimulation
  )
  simulationName <- aciclovirSimulation$name
  expect_length(paths, 2)
  expect_true(grepl(paste0(simulationName, "_1.pkml"), paths[1], fixed = TRUE))
  expect_true(grepl(paste0(simulationName, "_2.pkml"), paths[2], fixed = TRUE))
  sapply(paths, function(p) file.remove(p))
})

test_that("It throws an exception when trying to export for an individual id that does not exist in the population", {
  expect_error(
    exportIndividualSimulations(
      population = population,
      individualIds = c(50, 2),
      outputFolder = tempdir(),
      simulation = aciclovirSimulation
    ),
    regexp = "Individual with id"
  )
})

# loadSimulation

test_that("It throws an exception if the pkml loaded is not a valid simulation file", {
  expect_error(
    loadTestSimulation("molecules"),
    regexp = "Could not find file"
  )
})

test_that("It can load a simulation from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation(simulationName = "simple", loadFromCache = TRUE)
  sim2 <- loadTestSimulation(simulationName = "simple", loadFromCache = TRUE)

  # Testing that the same instance is returned from cache by comparing ids
  expect_equal(sim1$id, sim2$id)
})

test_that("It can load two simulations not from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  sim2 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)

  expect_false(identical(sim1$id, sim2$id))
})

test_that("Two sims not from cache and third from cache", {
  resetSimulationCache()

  sim1 <- loadTestSimulation("simple", loadFromCache = TRUE)
  sim2 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  sim3 <- loadTestSimulation("simple", loadFromCache = TRUE)

  expect_false(identical(sim1$id, sim2$id))
  expect_true(identical(sim1$id, sim3$id))
})

test_that("It can remove simulation from cache", {
  resetSimulationCache()
  sim1 <- loadTestSimulation("simple")

  expect_true(ospsuiteEnv$loadedSimulationsCache$hasKey(sim1$sourceFile))
  removeSimulationFromCache(sim1)
  expect_false(ospsuiteEnv$loadedSimulationsCache$hasKey(sim1$sourceFile))
})

test_that("It returns false when attempting to remove a simulation from cache that is not cached", {
  resetSimulationCache()
  sim1 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  expect_false(removeSimulationFromCache(sim1))
})

# runSimulations
test_that("It throws an error when simulation has empty output selections", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  setOutputs("blubb", sim, stopIfNotFound = FALSE)
  expect_error(
    runSimulations(simulations = sim),
    regexp = "has no output selections"
  )
})

test_that("It throws an error when any simulation in a list has empty output selections", {
  sim1 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  sim2 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  setOutputs("blubb", sim2, stopIfNotFound = FALSE)
  expect_error(
    runSimulations(simulations = c(sim1, sim2)),
    regexp = "has no output selections"
  )
})

test_that("It can run a valid population simulation and returns results", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  results <- runSimulations(simulations = sim, population = population)[[1]]
  expect_equal(results$count, population$count)
})

test_that("It can run a valid population simulation with aging data and returns results", {
  populationFileName <- getTestDataFilePath("baby.csv")
  agingDataFileName <- getTestDataFilePath("baby_aging.csv")
  population <- loadPopulation(csvPopulationFile = populationFileName)
  agingData <- loadAgingDataFromCSV(filePath = agingDataFileName)

  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  results <- runSimulations(
    simulations = sim,
    population = population,
    agingData = agingData
  )[[1]]
  expect_equal(results$count, population$count)
})

test_that("It throws an exception when running simulation with the wrong arguments", {
  expect_error(
    runSimulations(
      simulations = population
    ),
    regexp = "argument \"simulation\" is of type"
  )
})

test_that("It runs one individual simulation without simulationRunOptions and returns a named list", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  results <- runSimulations(simulations = sim)
  expect_true(isOfType(results[[1]], "SimulationResults"))
  # Check the ids
  expect_equal(names(results)[[1]], sim$id)
})

test_that("It runs one individual simulation with simulationRunOptions", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  simRunOptions <- SimulationRunOptions$new()
  results <- runSimulations(
    simulations = sim,
    simulationRunOptions = simRunOptions
  )[[1]]
  expect_true(isOfType(results, "SimulationResults"))
})

test_that("runSimulations returns named list using input list names without duplicates", {
  sim1 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  sim2 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )

  # No names
  expect_contains(
    names(runSimulations(list(sim1, sim2))),
    c(sim1$id, sim2$id)
  )

  # One name element
  expect_equal(
    names(runSimulations(list(sim1 = sim1))),
    "sim1"
  )

  # Full named list
  expect_equal(
    names(runSimulations(list(sim1 = sim1, sim2 = sim2))),
    c("sim1", "sim2")
  )

  # Partially named list
  expect_equal(
    names(runSimulations(list(sim1 = sim1, sim2))),
    c("sim1", sim2$id)
  )
  expect_equal(
    names(runSimulations(list(sim1, sim2 = sim2))),
    c(sim1$id, "sim2")
  )

  # Not unique names
  expect_error(
    runSimulations(list(sim1 = sim1, sim1 = sim2)),
    regexp = "Object has duplicated values; only unique values are allowed."
  )
})

test_that("Behavior when one of simulations fails. Results for this simulation are NULL", {
  sim1 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  sim2 <- loadTestSimulation(
    "simple",
    loadFromCache = FALSE,
    addToCache = FALSE
  )
  # sim1 is supposed to fail because of negative start values, while sim2 should run fine
  setQuantityValuesByPath(
    "Organism|Liver|A",
    -1,
    sim1
  )
  expect_warning(
    results <- runSimulations(simulations = c(sim1, sim2)),
    regexp = "Initial value"
  )
  expect_equal(length(results), 2)
  expect_equal(names(results)[[2]], sim2$id)
  expect_null(results[[sim1$id]])
  expect_true(isOfType(results[[2]], "SimulationResults"))

  # It does not show a warning if one of simulations fails in silent mode.
  expect_no_warning(
    results <- runSimulations(simulations = c(sim1, sim2), silentMode = TRUE)
  )
  expect_equal(length(results), 2)
  expect_equal(names(results)[[2]], sim2$id)
  expect_null(results[[sim1$id]])
  expect_true(isOfType(results[[2]], "SimulationResults"))

  # It throws an error if one of simulations fails and stopIfFails is TRUE
  expect_error(
    runSimulations(simulations = c(sim1, sim2), stopIfFails = TRUE),
    regexp = "Initial value"
  )
})

test_that("It throws an error when running multiple simulations with a population", {
  sim1 <- loadTestSimulation("simple", loadFromCache = TRUE)
  sim2 <- loadTestSimulation("simple", loadFromCache = TRUE)
  expect_error(
    runSimulations(
      simulations = c(sim1, sim2),
      population = population
    ),
    regexp = "Multiple simulations cannot be run concurrently"
  )
})

test_that("It throws an error when running the same instance of a simulation multiple time", {
  sim1 <- loadTestSimulation("simple", loadFromCache = TRUE)
  sim2 <- loadTestSimulation("simple", loadFromCache = TRUE)
  expect_error(
    runSimulations(simulations = c(sim1, sim2)),
    regexp = "is used multiple times in"
  )
})

# getStandardMoleculeParameters
test_that("It returns all molecule parameters for an existing molecule in a simulation", {
  parameters <- getStandardMoleculeParameters(
    moleculeName = "CYP3A4",
    simulation = aciclovirSimulation
  )
  expect_equal(length(parameters), length(MoleculeParameter))
})

test_that("It returns an empty list of parameters for a molecule that does not exist", {
  parameters <- getStandardMoleculeParameters(
    moleculeName = "NOPE",
    simulation = aciclovirSimulation
  )
  expect_equal(length(parameters), 0)
})

# getAllParametersForSensitivityAnalysisMatching
test_that("It returns all parameter potentially interesting for sensitivity analysis for a given wild card path", {
  parameters <- getAllParametersMatching(
    paths = "**|Volume",
    container = aciclovirSimulation
  )
  variableParameters <- getAllParametersForSensitivityAnalysisMatching(
    paths = "**|Volume",
    simulation = aciclovirSimulation
  )
  expect_gt(length(parameters), length(variableParameters))
})

# createSimulationBatch
test_that("It throws an error when initializing a simulation batch without any variable parameter or molecule", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  expect_error(createSimulationBatch(simulation = sim))
})

test_that("It creates a simulation batch when using only parameter paths", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameters
  )
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only molecule paths", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- c("Organism|Liver|A")
  simulationBatch <- createSimulationBatch(
    simulation = sim,
    moleculesOrPaths = molecules
  )
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only parameter instances", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameter1 <- getParameter(
    path = toPathString(c("Organism", "Liver", "Volume")),
    container = sim
  )
  simulationBatch <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameter1
  )
  expect_false(is.null(simulationBatch))
})

test_that("It creates a simulation batch when using only molecule instances", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecule <- getMolecule(
    path = toPathString(c("Organism", "Liver", "A")),
    container = sim
  )
  simulationBatch <- createSimulationBatch(
    simulation = sim,
    moleculesOrPaths = molecule
  )
  expect_false(is.null(simulationBatch))
})


# runSimulationBatches
test_that("It can run a simulation batch by varying some parameters and molecules", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules
  )
  id <- simulationBatch$addRunValues(
    parameterValues = c(1.2, 2.4),
    initialValues = 2.5
  )
  res <- runSimulationBatches(simulationBatches = simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
})

test_that("It can run a simulation batch by varying some parameters only", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  id <- simulationBatch$addRunValues(parameterValues = c(1.2, 2.4))
  res <- runSimulationBatches(simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
})

test_that("It can run a simulation batch by varying some molecules only", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(initialValues = 1.2)
  res <- runSimulationBatches(simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
})

test_that("The result is NULL when the number of values does not match the initialization count for initial values", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(initialValues = c(1, 1.2))
  expect_warning(res <- runSimulationBatches(simulationBatch))
  expect_null(res[[1]][[1]])
})

test_that("The result is NULL when the number of values does not match the initialization count for parameters", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  id <- simulationBatch$addRunValues(parameterValues = c(1.2))
  expect_warning(res <- runSimulationBatches(simulationBatch))
  expect_null(res[[1]][[1]])
})

test_that("It throws an error when multiple values sets are added", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters)
  ids <- c()
  ids[[1]] <- simulationBatch$addRunValues(
    parameterValues = c(1, 2),
    initialValues = 1
  )
  expect_error(
    simulationBatch$addRunValues(
      parameterValues = list(c(1, 2), c(2, 3)),
      initialValues = 1
    ),
    regexp = messages$errorOnlyOneValuesSetAllowed(
      "parameterValues, initialValues"
    ),
    fixed = TRUE
  )
})

test_that("It can run a simulation batch with multiple parameters and molecules values sets", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(
    sim,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules
  )
  ids <- c()
  ids[[1]] <- simulationBatch$addRunValues(
    parameterValues = c(1, 2),
    initialValues = 1
  )
  ids[[2]] <- simulationBatch$addRunValues(
    parameterValues = c(1.6, 2.4),
    initialValues = 3
  )
  res <- runSimulationBatches(simulationBatches = simulationBatch)

  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
})

test_that("It can run multiple simulation batches with multiple parameters and molecules values sets", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch1 <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules
  )
  simulationBatch2 <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules
  )
  # Ids of run values
  ids <- c()
  ids[[1]] <- simulationBatch1$addRunValues(
    parameterValues = c(1, 2),
    initialValues = 1
  )
  ids[[2]] <- simulationBatch1$addRunValues(
    parameterValues = c(1.6, 2.4),
    initialValues = 3
  )
  ids[[3]] <- simulationBatch2$addRunValues(
    parameterValues = c(4, 2),
    initialValues = 4
  )
  ids[[4]] <- simulationBatch2$addRunValues(
    parameterValues = c(2.6, 4.4),
    initialValues = 5
  )
  res <- runSimulationBatches(
    simulationBatches = list(simulationBatch1, simulationBatch2)
  )
  expect_equal(length(res), 2)
  # Check for batch ids as names
  expect_equal(names(res), c(simulationBatch1$id, simulationBatch2$id))
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
  expect_equal(names(res[[1]])[[2]], ids[[2]])
  expect_equal(names(res[[2]])[[1]], ids[[3]])
  expect_equal(names(res[[2]])[[2]], ids[[4]])
})

test_that("It can run a simulation batch, add new values, and run again", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  parameters <- c("Organism|Liver|Volume", "R1|k1")
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules
  )
  # Ids of run values
  ids <- c()
  ids[[1]] <- simulationBatch$addRunValues(
    parameterValues = c(1, 2),
    initialValues = 1
  )
  ids[[2]] <- simulationBatch$addRunValues(
    parameterValues = c(1.6, 2.4),
    initialValues = 3
  )
  res <- runSimulationBatches(simulationBatches = simulationBatch)
  # One simulation batch - therefore one element in the list, and two runs values - therefore two results in the batch result
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
  expect_equal(names(res[[1]])[[2]], ids[[2]])

  ids[[1]] <- simulationBatch$addRunValues(
    parameterValues = c(1, 2),
    initialValues = 1
  )
  ids[[2]] <- simulationBatch$addRunValues(
    parameterValues = c(1.6, 2.4),
    initialValues = 3
  )
  res <- runSimulationBatches(simulationBatches = simulationBatch)
  expect_equal(length(res), 1)
  expect_true(isOfType(res[[1]][[1]], "SimulationResults"))
  expect_equal(names(res[[1]])[[1]], ids[[1]])
  expect_equal(names(res[[1]])[[2]], ids[[2]])
})

test_that("It does not show a warning when simulation fails in silentMode", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(initialValues = c(1, 1.2))
  expect_no_warning(runSimulationBatches(simulationBatch, silentMode = TRUE))
})

test_that("Throws an error when a simulation does not succeed", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  molecules <- "Organism|Liver|A"
  simulationBatch <- createSimulationBatch(sim, moleculesOrPaths = molecules)
  id <- simulationBatch$addRunValues(initialValues = c(1, 1.2))
  expect_error(runSimulationBatches(simulationBatch, stopIfFails = TRUE))
})

# getAllStateVariablesPaths

test_that("It returns the correct paths of the state variables", {
  sim <- loadTestSimulation(simulationName = "simple", loadFromCache = TRUE)
  stateVariablePaths <- getAllStateVariablesPaths(simulation = sim)
  expect_equal(length(stateVariablePaths), 11)
})

test_that("It returns the correct paths of the state variable parameters", {
  sim <- loadTestSimulation(simulationName = "simple", loadFromCache = TRUE)
  stateVariableParametersPaths <- getAllStateVariableParametersPaths(
    simulation = sim
  )
  expect_equal(length(stateVariableParametersPaths), 1)
})

# getSimulationTree

test_that("it can explore a simulation by path", {
  simPath <- getSimulationFilePath("simple")
  tree <- getSimulationTree(simPath)

  path <- tree$Organism$Liver$Volume$path
  expect_equal(path, "Organism|Liver|Volume")
})


test_that("it can explore a simulation by instance", {
  sim <- loadTestSimulation("simple", loadFromCache = TRUE)
  tree <- getSimulationTree(sim)

  path <- tree$Organism$Liver$Volume$path
  expect_equal(path, "Organism|Liver|Volume")
})

test_that("It calculates steady-state for multiple simulations, single steadyStateTime", {
  sim1 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  sim2 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  output <- getSteadyState(
    simulations = c(sim1, sim2),
    steadyStateTime = 1
  )
  expect_equal(names(output), c(sim1$id, sim2$id))
})

test_that("It calculates steady-state for multiple simulations, multiple steadyStateTime", {
  sim1 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  sim2 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  output <- getSteadyState(
    simulations = c(sim1, sim2),
    steadyStateTime = list(1, NULL)
  )
  expect_equal(names(output), c(sim1$id, sim2$id))
  expect_equal(names(output[[sim1$id]]), c("paths", "values"))
})


test_that("Getting Steady State works with named simulations without duplicated names", {
  sim1 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  sim2 <- loadTestSimulation(simulationName = "simple", loadFromCache = FALSE)
  output <- getSteadyState(
    simulations = c("sim1" = sim1, "sim2" = sim2),
    steadyStateTime = 1
  )
  expect_equal(names(output), c("sim1", "sim2"))

  expect_error({
    getSteadyState(
      simulations = c("sim1" = sim1, "sim1" = sim2),
      steadyStateTime = 1
    )
  })
})

test_that("lowerThreshold in getSteadyState correctly handles positive and negative values", {
  # Test that lowerThreshold is applied using absolute value
  # Get steady state with a relatively large threshold to test the logic
  threshold <- 1

  steadyState <- getSteadyState(
    simulations = aciclovirSimulation,
    steadyStateTime = 1,
    lowerThreshold = threshold
  )

  # Verify that the structure is correct
  expect_true(is.list(steadyState))
  expect_true(all(
    c("paths", "values") %in% names(steadyState[[aciclovirSimulation$id]])
  ))

  # All values should be either 0 or have absolute value >= lowerThreshold
  values <- unlist(steadyState[[aciclovirSimulation$id]]$values)
  expect_true(all(values == 0 | abs(values) >= threshold))

  # Test with NULL threshold (no cut-off applied)
  steadyStateNoThreshold <- getSteadyState(
    simulations = aciclovirSimulation,
    steadyStateTime = 1,
    lowerThreshold = NULL
  )
  values <- unlist(steadyStateNoThreshold[[aciclovirSimulation$id]]$values)
  expect_false(all(values == 0 | abs(values) >= threshold))
})

test_that("`exportSteadyStateToXLS` generates excel file with correct sheets", {
  withr::with_tempfile("resultsXLSPath", code = {
    sim <- loadTestSimulation("simple", loadFromCache = TRUE)
    wb <- exportSteadyStateToXLS(sim, resultsXLSPath = resultsXLSPath)
    expect_true(file.exists(resultsXLSPath))
    expect_equal(wb$sheet_names, c("Molecules", "Parameters"))
  })
})

# runSimulation

test_that("`runSimulation()` is deprecated", {
  expect_snapshot({
    sim <- loadTestSimulation("simple", loadFromCache = TRUE)
    results <- runSimulation(sim)
    expect_equal(results$count, 1)
  })
})

test_that("It throws an error when trying to run multiple simulations", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE)
  sim2 <- loadTestSimulation("simple", loadFromCache = FALSE)
  suppressWarnings(expect_error(results <- runSimulation(c(sim, sim2))))
})

#### Creating simulation ####
test_that("It can create a simulation from a project configuration retrieved from a simulation with no expression profiles", {
  simulation <- loadSimulation(
    system.file(
      "extdata",
      "Aciclovir.pkml",
      package = "ospsuite"
    ),
    loadFromCache = TRUE
  )
  simConfig <- simulation$configuration
  newSimulation <- createSimulation(
    simulationConfiguration = simConfig,
    simulationName = "MySim"
  )

  # Check simulation configuration
  expect_equal(newSimulation$name, "MySim")
  expect_equal(
    names(newSimulation$configuration$expressionProfiles),
    names(simConfig$expressionProfiles)
  )
  expect_equal(
    newSimulation$configuration$individual$name,
    simConfig$individual$name
  )
  # Checking for the names of the modules, because the module instances are different
  expect_equal(
    names(newSimulation$configuration$modules),
    names(simConfig$modules)
  )

  # Check simulation properties
  expect_equal(
    newSimulation$allFloatingMoleculeNames(),
    simulation$allFloatingMoleculeNames()
  )
  expect_equal(
    newSimulation$allStationaryMoleculeNames(),
    simulation$allStationaryMoleculeNames()
  )
  expect_equal(
    length(newSimulation$outputSchema$intervals),
    length(simulation$outputSchema$intervals)
  )
  expect_equal(
    newSimulation$outputSchema$intervals[[1]]$name,
    simulation$outputSchema$intervals[[1]]$name
  )
  expect_equal(
    newSimulation$outputSelections$allOutputs[[1]]$path,
    simulation$outputSelections$allOutputs[[1]]$path
  )
})

# show warnings true
test_that("createSimulation shows warnings when showWarnings is TRUE", {
  simulation <- loadSimulation(
    system.file(
      "extdata",
      "Aciclovir.pkml",
      package = "ospsuite"
    ),
    loadFromCache = TRUE
  )
  simConfig <- simulation$configuration

  expect_snapshot(
    newSimulation <- createSimulation(
      simulationConfiguration = simConfig,
      simulationName = "MySim",
      showWarnings = TRUE
    )
  )
})

# errors
test_that("createSimulation throws an error when simulation cannot be created", {
  simulation <- loadSimulation(
    system.file(
      "extdata",
      "Aciclovir.pkml",
      package = "ospsuite"
    ),
    loadFromCache = TRUE
  )
  simConfig <- simulation$configuration

  # Introduce an error in the configuration
  simConfig$selectedInitialConditions <- list("Vergin 1995 IV" = NULL)

  expect_snapshot(
    newSimulation <- createSimulation(
      simulationConfiguration = simConfig,
      simulationName = "MySim"
    ),
    error = TRUE
  )
})

# Test for process rate parameters
test_that("createSimulation can create process rate parameters when requested", {
  simulation <- loadSimulation(
    system.file(
      "extdata",
      "Aciclovir.pkml",
      package = "ospsuite"
    ),
    loadFromCache = TRUE
  )
  simConfig <- simulation$configuration

  newSimulation <- createSimulation(
    simulationConfiguration = simConfig,
    simulationName = "MySim",
    createAllProcessRateParameters = TRUE
  )

  # Check that process rate parameters were created
  paramPath <- "Neighborhoods|ArterialBlood_bc_Bone_bc|Aciclovir|MassTransferBloodPool2OrgRBC|ProcessRate"
  expect_no_error(
    processRateParameter <- getParameter(
      path = paramPath,
      container = newSimulation
    )
  )
})

# PC and CP methods overrides
test_that("It creates a simulation with overridden PC and CP methods", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  config <- sim$configuration
  config$setCellularPermeabilityMethods(
    "A",
    CellularPermeabilityMethods$`Charge dependent Schmitt`
  )
  config$setPartitionCoefficientMethods(
    "A",
    PartitionCoefficientMethods$`Rodgers and Rowland`
  )

  newSim <- createSimulation(
    simulationConfiguration = config,
    simulationName = "MySim"
  )
  # TODO add test for correct application of the methods once the logic to retrieve them in the simulation is implemented
  # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1872
})

test_that("It ignores molecules that are not present in the simulation when overriding PC and CP methods", {
  sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
  config <- sim$configuration
  config$setCellularPermeabilityMethods(
    "foo",
    CellularPermeabilityMethods$`Charge dependent Schmitt`
  )
  config$setPartitionCoefficientMethods(
    "foo",
    PartitionCoefficientMethods$`Rodgers and Rowland`
  )
  config$setCellularPermeabilityMethods(
    "B",
    CellularPermeabilityMethods$`Charge dependent Schmitt`
  )
  config$setPartitionCoefficientMethods(
    "B",
    PartitionCoefficientMethods$`Rodgers and Rowland`
  )

  newSim <- createSimulation(
    simulationConfiguration = config,
    simulationName = "MySim"
  )
  # TODO add test for correct application of the methods once the logic to retrieve them in the simulation is implemented
  # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1872
})
