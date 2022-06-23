#' @title Load a simulation from a pkml file
#'
#' @description
#' Loads a simulation from a pkml file and returns the simulation. If the passed
#' simulation file has been loaded before, the simulation is not loaded again
#' but a cached object is returned. This behavior can be overridden.
#'
#' @param filePath Full path of pkml simulation file to load.
#'
#' @param loadFromCache If `TRUE`, an already loaded pkml file will not be
#'   loaded again, but the simulation object will be retrieved from cache. If
#'   `FALSE`, a new simulation object will be created. Default value is `FALSE`.
#' @param addToCache If `TRUE`, the loaded simulation is added to cache. If
#'   `FALSE`, the returned simulation only exists locally. Default is `TRUE`.
#' @param resetIds If `TRUE`, the internal object ids in the simulation are
#'   reset to a unique value. If `FALSE`, the ids are kept as defined in the
#'   pkml simulation. Default is `TRUE`.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load sim1 for the first time
#' sim1 <- loadSimulation(simPath)
#'
#' # sim2 will be loaded from cache and will represent the same object as sim1
#' sim2 <- loadSimulation(simPath, loadFromCache = TRUE)
#'
#' parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim1)
#' parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim2)
#'
#' # parameter1 and parameter2 belong to the same simulation object, so changing
#' # one of the them will also change another
#' setParameterValues(parameters = parameter2, values = 0)
#' parameter1$value == parameter2$value # TRUE
#'
#' # sim3 will not be loaded from cache
#' sim3 <- loadSimulation(simPath, loadFromCache = FALSE)
#' # parameter3 belong to different simulation object than parameter1 and parameter2
#' parameter3 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim3)
#' setParameterValues(parameters = parameter3, values = 1)
#' parameter2$value == parameter3$value # FALSE#'
#' @export
loadSimulation <- function(filePath, loadFromCache = FALSE, addToCache = TRUE, resetIds = TRUE) {
  validateIsLogical(c(loadFromCache, addToCache))
  validateIsString(filePath)
  if (loadFromCache) {
    # If the file has already been loaded, return the last loaded object
    if (ospsuiteEnv$loadedSimulationsCache$hasKey(filePath)) {
      return(ospsuiteEnv$loadedSimulationsCache$get(filePath))
    }
  }

  # If the simulation has not been loaded so far, or loadFromCache == FALSE,
  # new simulation object will be created
  simulationPersister <- .getNetTask("SimulationPersister")

  # Note: We do not expand the variable filePath here as we want the cache to be created using the path given by the user
  netSim <- rClr::clrCall(simulationPersister, "LoadSimulation", .expandPath(filePath), resetIds)

  simulation <- Simulation$new(netSim, filePath)

  # Add the simulation to the cache of loaded simulations
  if (addToCache) {
    ospsuiteEnv$loadedSimulationsCache$set(filePath, simulation)
  }

  return(simulation)
}

#' Saves a simulation to pkml file
#'
#' @param simulation Instance of a simulation to save.
#' @param filePath Full path of where the simulation will be saved.
#'
#' @export
saveSimulation <- function(simulation, filePath) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  simulationPersister <- .getNetTask("SimulationPersister")
  rClr::clrCall(simulationPersister, "SaveSimulation", simulation$ref, filePath)
  invisible()
}

#' @title
#' Runs one  simulation (individual or population) and returns a `SimulationResults` object containing all results of the simulation.
#'
#' @param simulation One `Simulation` to simulate.
#' @param population Optional instance of a `Population` to use for the simulation. This is only used when simulating one simulation
#' Alternatively, you can also pass the result of `createPopulation` directly. In this case, the population will be extracted
#' @param agingData Optional instance of `AgingData` to use for the simulation. This is only used with a population simulation
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions` used during the simulation run
#'
#' @return SimulationResults (one entry per Individual) for a single simulation
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Running an individual simulation
#' # results is an instance of `SimulationResults`
#' results <- runSimulation(sim)
#'
#' # Creating custom simulation run options
#'
#' simRunOptions <- SimulationRunOptions$new()
#' simRunOptions$numberOfCores <- 3
#' simRunOptions$showProgress <- TRUE
#'
#' # Running a population simulation
#' popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#' population <- loadPopulation(popPath)
#' results <- runSimulation(sim, population, simulationRunOptions = simRunOptions)
#' @export
runSimulation <- function(simulation, population = NULL, agingData = NULL, simulationRunOptions = NULL) {
  # Check that only one simulation is passed
  simulation <- c(simulation)
  validateIsOfLength(simulation, 1)
  # Returning the first element of `runSimulations` output, as the latter returns
  # a named list with ID of the simulation as element name.
  runSimulations(simulations = simulation, population = population, agingData = agingData, simulationRunOptions = simulationRunOptions)[[1]]
}

#' @title  Runs multiple simulations concurrently.
#'
#' @details For multiple simulations, only individual simulations are possible.
#' For single simulatio, either individual or population simulations can be
#' performed.
#'
#' @param simulations One `Simulation` or list of `Simulation` objects
#' to simulate.
#' @param population Optional instance of a `Population` to use for the simulation.
#' Only allowed when simulating one simulation.
#' Alternatively, you can also pass the result of `createPopulation` directly.
#' In this case, the population will be extracted.
#' @param agingData Optional instance of `AgingData` to use for the simulation.
#' This is only used with a population simulation
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions` used during the simulation run
#' @param silentMode If `TRUE`, no warnings are displayed if a simulation fails. Default is `FALSE`
#'
#' @return A named list of `SimulationResults` objects with names being the IDs
#' of the respective simulations. If a simulation fails, the result for this
#' simulation is `NULL`
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Running an individual simulation
#' # Results is a list with one object `SimulationResults`
#' results <- runSimulations(sim)
#'
#' # Creating custom simulation run options
#'
#' simRunOptions <- SimulationRunOptions$new()
#' simRunOptions$numberOfCores <- 3
#' simRunOptions$showProgress <- TRUE
#'
#' # Running a population simulation
#' popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#' population <- loadPopulation(popPath)
#' results <- runSimulations(sim, population, simulationRunOptions = simRunOptions)
#'
#' # Running multiple simulations in parallel
#' sim2 <- loadSimulation(simPath)
#' sim3 <- loadSimulation(simPath)
#'
#' # Results is an array of `SimulationResults`
#' results <- runSimulations(list(sim, sim2, sim3))
#' @export
runSimulations <- function(simulations, population = NULL, agingData = NULL, simulationRunOptions = NULL, silentMode = FALSE) {
  simulations <- c(simulations)
  validateIsOfType(simulationRunOptions, "SimulationRunOptions", nullAllowed = TRUE)
  simulationRunOptions <- simulationRunOptions %||% SimulationRunOptions$new()

  # only one simulation? We allow population run
  if (length(simulations) == 1) {
    results <- .runSingleSimulation(
      simulation = simulations[[1]],
      simulationRunOptions = simulationRunOptions,
      population = population,
      agingData = agingData
    )
    outputList <- list()
    outputList[[simulations[[1]]$id]] <- results
    return(outputList)
  }

  # more than one simulation? This is a concurrent run. We do not allow population variation
  if (!is.null(population)) {
    stop(messages$errorMultipleSimulationsCannotBeUsedWithPopulation)
  }

  # we are now running the simulations concurrently
  return(.runSimulationsConcurrently(
    simulations = simulations,
    simulationRunOptions = simulationRunOptions,
    silentMode = silentMode
  ))
}

.runSingleSimulation <- function(simulation, simulationRunOptions, population = NULL, agingData = NULL) {
  validateIsOfType(simulation, "Simulation")
  if (is.list(population)) {
    # if a list was given as parameter, we assume that the user wants to run a population simulation
    # The population object must be present otherwise, this is an error => nullAllowed is FALSE
    population <- population$population
    validateIsOfType(population, "Population")
  } else {
    validateIsOfType(population, "Population", nullAllowed = TRUE)
  }
  validateIsOfType(agingData, "AgingData", nullAllowed = TRUE)
  simulationRunner <- .getNetTask("SimulationRunner")
  simulationRunArgs <- rClr::clrNew("OSPSuite.R.Services.SimulationRunArgs")
  rClr::clrSet(simulationRunArgs, "Simulation", simulation$ref)
  rClr::clrSet(simulationRunArgs, "SimulationRunOptions", simulationRunOptions$ref)

  if (!is.null(population)) {
    rClr::clrSet(simulationRunArgs, "Population", population$ref)
  }

  if (!is.null(agingData)) {
    rClr::clrSet(simulationRunArgs, "AgingData", agingData$ref)
  }

  results <- rClr::clrCall(simulationRunner, "Run", simulationRunArgs)

  SimulationResults$new(results, simulation)
}

.runSimulationsConcurrently <- function(simulations, simulationRunOptions, silentMode = FALSE) {
  simulationRunner <- .getNetTask("ConcurrentSimulationRunner")
  tryCatch(
    {
      validateIsOfType(simulations, "Simulation")
      rClr::clrSet(simulationRunner, "SimulationRunOptions", simulationRunOptions$ref)

      # Map of simulations ids to simulations objects
      simulationIdSimulationMap <- vector("list", length(simulations))

      # Add simulations
      for (simulationIdx in seq_along(simulations)) {
        simulation <- simulations[[simulationIdx]]
        simulationIdSimulationMap[[simulationIdx]] <- simulation
        names(simulationIdSimulationMap)[[simulationIdx]] <- simulation$id

        rClr::clrCall(simulationRunner, "AddSimulation", simulation$ref)
      }
      # Run all simulations
      results <- rClr::clrCall(simulationRunner, "RunConcurrently")

      # Ids of the results are Ids of the simulations
      resultsIdSimulationIdMap <- names(simulationIdSimulationMap)
      names(resultsIdSimulationIdMap) <- names(simulationIdSimulationMap)
      simulationResults <- .getConcurrentSimulationRunnerResults(results = results, resultsIdSimulationIdMap = resultsIdSimulationIdMap, simulationIdSimulationMap = simulationIdSimulationMap, silentMode = silentMode)

      return(simulationResults)
    },
    finally = {
      # Dispose of the runner to release any possible instances still in memory (.NET side)
      rClr::clrCall(simulationRunner, "Dispose")
    }
  )
}

#' @title  Creates and returns an instance of a `SimulationBatch` that can be used to efficiently vary parameters and initial values in a simulation
#'
#' @param simulation Instance of a `Simulation` to simulate in a batch mode
#' @param parametersOrPaths  Parameter instances (element or vector) typically retrieved using
#' `getAllParametersMatching` or parameter path (element or vector of strings) that will be varied in the simulation. (optional)
#' When providing the paths, only absolute full paths are supported (i.e., no matching with '*' possible).
#' If parametersOrPaths is `NULL`, you will not be able to set parameter values during batch run.
#'
#' @param moleculesOrPaths  Molecule instances (element or vector) typically retrieved using
#' `getAllMoleculesMatching` or molecule path (element or vector of strings) that will be varied in the simulation. (optional)
#' When providing the paths, only absolute full paths are supported (i.e., no matching with '*' possible).
#' If moleculesOrPaths is `NULL`, you will not be able to set molecule initial values during batch run.
#'
#' @return SimulationBatch that can be used to vary parameter values or molecule initial values and run simulation in an optimized manner
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Create a simulation batch that will allow batch run for one parameter value
#' simulationBatch <- createSimulationBatch(sim, "Organism|Liver|Volume")
#'
#' # Create a simulation batch that will allow batch run for multiple parameter
#' # values and initial values
#' simulationBatch <- createSimulationBatch(
#'   sim,
#'   c("Organism|Liver|Volume", "R1|k1"),
#'   c("Organism|Liver|A")
#' )
#' @export
createSimulationBatch <- function(simulation, parametersOrPaths = NULL, moleculesOrPaths = NULL) {
  validateIsOfType(simulation, "Simulation")
  validateIsOfType(parametersOrPaths, c("Parameter", "character"), nullAllowed = TRUE)
  validateIsOfType(moleculesOrPaths, c("Molecule", "character"), nullAllowed = TRUE)

  if (length(parametersOrPaths) == 0 && length(moleculesOrPaths) == 0) {
    stop(messages$errorSimulationBatchNothingToVary)
  }

  variableParameters <- c(parametersOrPaths)

  if (isOfType(variableParameters, "Parameter")) {
    variableParameters <- unlist(lapply(variableParameters, function(x) x$path))
  }

  variableMolecules <- c(moleculesOrPaths)

  if (isOfType(variableMolecules, "Molecule")) {
    variableMolecules <- unlist(lapply(variableMolecules, function(x) x$path))
  }

  simulationBatchOptions <- SimulationBatchOptions$new(
    variableParameters = variableParameters,
    variableMolecules = variableMolecules
  )

  net <- rClr::clrNew("OSPSuite.R.Domain.ConcurrentRunSimulationBatch", simulation$ref, simulationBatchOptions$ref)
  SimulationBatch$new(net, simulation)
}

#' Run simulation batches
#' @details Runs a set of simulation batches. The simulation batches must be populated
#' with sets of parameter and start values with `SimulationBatch$addRunValues()`
#' prior to running. After the run, the list of parameter and start values is cleared.
#'
#' @param simulationBatches List of `SimulationBatch` objects with added parameter and initial values
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions` used during the simulation run.
#' @param silentMode If `TRUE`, no warnings are displayed if a simulation fails.
#' Default is `FALSE`.
#'
#' @return Nested list of `SimulationResults` objects. The first level of the list are the IDs of the simulations of SimulationBatches, containing a list of `SimulationResults` for each set of parameter/initial values. If a simulation with a parameter/initial values set fails, the result for this run is `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
#' sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
#' parameters <- c("Organism|Liver|Volume", "R1|k1")
#' molecules <- "Organism|Liver|A"
#' # Create two simulation batches.
#' simulationBatch1 <- createSimulationBatch(
#'   simulation = sim1,
#'   parametersOrPaths = parameters,
#'   moleculesOrPaths = molecules
#' )
#' simulationBatch2 <- createSimulationBatch(
#'   simulation = sim2,
#'   parametersOrPaths = parameters,
#'   moleculesOrPaths = molecules
#' )
#' # Ids of run values
#' ids <- c()
#' ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
#' ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
#' ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
#' ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
#' res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
#' }
runSimulationBatches <- function(simulationBatches, simulationRunOptions = NULL, silentMode = FALSE) {
  validateIsOfType(simulationBatches, "SimulationBatch")
  simulationRunner <- .getNetTask("ConcurrentSimulationRunner")
  validateIsOfType(simulationRunOptions, "SimulationRunOptions", nullAllowed = TRUE)
  simulationRunOptions <- simulationRunOptions %||% SimulationRunOptions$new()
  rClr::clrSet(simulationRunner, "SimulationRunOptions", simulationRunOptions$ref)

  simulationBatches <- c(simulationBatches)
  # Result Id <-> simulation batch pointer id map to get the correct simulation for the results.
  # Using the Id of the pointer instead of the Id of the simulation as multiple
  # SimulationBatches can be created with the same simulation
  # Each SimulationBatchRunValues has its own id, which will be the id of the result
  resultsIdSimulationIdMap <- list()
  # Map of simulations ids to simulations objects
  simulationIdSimulationMap <- vector("list", length(simulationBatches))
  # Iterate through all simulation batches
  for (simBatchIndex in seq_along(simulationBatches)) {
    simBatch <- simulationBatches[[simBatchIndex]]
    simBatchId <- rClr::clrGet(simBatch$ref, "Id")
    simulationIdSimulationMap[[simBatchIndex]] <- simBatch$simulation
    names(simulationIdSimulationMap)[[simBatchIndex]] <- simBatchId
    # Ids of the values of the batch
    valuesIds <- simBatch$runValuesIds
    # All results of this batch have the id of the same simulation
    resultsIdSimulationIdMap[valuesIds] <- simBatchId
    # Add the batch to concurrent runner
    rClr::clrCall(simulationRunner, "AddSimulationBatch", simBatch$ref)
  }

  # Run the batch with the ConcurrentSimulationRunner
  results <- rClr::clrCall(simulationRunner, "RunConcurrently")
  simulationResults <- .getConcurrentSimulationRunnerResults(results = results, resultsIdSimulationIdMap = resultsIdSimulationIdMap, simulationIdSimulationMap = simulationIdSimulationMap, silentMode = silentMode)

  # output: list of lists of SimulationResults, one list per SimulationBatch
  output <- lapply(names(simulationIdSimulationMap), function(simId) {
    simulationResults[which(resultsIdSimulationIdMap == simId)]
  })

  # Dispose of the runner to release any possible instances still in memory (.NET side)
  rClr::clrCall(simulationRunner, "Dispose")

  return(output)
}

#' Clears cache of loaded simulations
#' @export
resetSimulationCache <- function() {
  ospsuiteEnv$loadedSimulationsCache$reset()
}

#' @title  Removes a simulation from simulations cache.
#'
#' @param simulation Simulation to  be removed from the cache
#'
#' @return TRUE if the simulation was cached and could be removed from cache.
#' FALSE otherwise, usually indicating that the specific simulation was not cached.
#' @export
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim1 <- loadSimulation(simPath)
#' sim2 <- loadSimulation(simPath, loadFromCache = FALSE, addToCache = FALSE)
#'
#' removeSimulationFromCache(sim1) # returns TRUE
#' removeSimulationFromCache(sim2) # returns FALSE
removeSimulationFromCache <- function(simulation) {
  validateIsOfType(simulation, "Simulation")

  simulationFilePath <- simulation$sourceFile

  # Can not remove simulation from cache if no simultion with the corresponding
  # file path has been cached.
  if (!ospsuiteEnv$loadedSimulationsCache$hasKey(simulationFilePath)) {
    return(FALSE)
  }

  cachedSim <- ospsuiteEnv$loadedSimulationsCache$get(simulationFilePath)

  if (!identical(cachedSim, simulation)) {
    return(FALSE)
  }

  ospsuiteEnv$loadedSimulationsCache$dropKey(simulationFilePath)

  return(TRUE)
}

#' @title  Returns a list containing all standard global parameters defined in a `simulation` for given `moleculeName`.
#' These parameters are typically located directly under the container named after the `moleculeName`.
#' For the list of standard parameters
#' @seealso  [MoleculeParameter]
#'
#' @param simulation Simulation to query for molecule parameters
#' @param moleculeName Name of molecule (Enzyme, Transporter etc..) for which global parameters should be returned
#'
#' @return A list of all standard global parameters defined for `moleculeName` if the molecule exists in the `simulation`.
#' Otherwise an empty list is returned
#'
#' @export
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim1 <- loadSimulation(simPath)
#'
#' parameters <- getStandardMoleculeParameters("CYP3A4", sim1)
getStandardMoleculeParameters <- function(moleculeName, simulation) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(moleculeName)
  paths <- sapply(MoleculeParameter, function(p) toPathString(moleculeName, p))
  getAllParametersMatching(paths = paths, container = simulation)
}

#' Retrieve all parameters of the given simulation matching the given path criteria and also potential candidate
#' for sensitivity variation
#'
#' @param paths A vector of strings representing the path of the parameters (potentially using wildcards)
#' @param simulation Simulation used to find the parameters
#'
#' @return A list of parameters matching the path criteria and also candidates for a sensitivity analysis.
#' The list is empty if no parameters matching were found.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Volume` parameters defined in all direct containers of the organism
#' params <- getAllParametersForSensitivityAnalysisMatching("Organism|*|Volume", sim)
#' @export
getAllParametersForSensitivityAnalysisMatching <- function(paths, simulation) {
  validateIsOfType(simulation, "Simulation")
  .getAllEntitiesMatching(
    paths = paths,
    container = simulation,
    entityType = Parameter,
    method = "AllParametersForSensitivityAnalysisMatching"
  )
}

#' Get the paths of all state variable quantities of the simulation
#'
#' @param simulation `Simulation` object
#' @details List of paths of all molecules in all compartments and all parameters that are
#' state variables.
#'
#' @return A list of paths
#' @export
getAllStateVariablesPaths <- function(simulation) {
  validateIsOfType(simulation, type = "Simulation")
  allMoleculesPaths <- getAllMoleculePathsIn(container = simulation)
  allStateVariableParamsPaths <- .getAllEntityPathsIn(container = simulation, entityType = Parameter, method = "AllStateVariableParameterPathsIn")
  allQantitiesPaths <- append(allMoleculesPaths, allStateVariableParamsPaths)
  return(allQantitiesPaths)
}

#' Export simulation PKMLs for given `individualIds`. Each pkml file will contain the original simulation updated with parameters of the corresponding individual.
#'
#' @param population A population object typically loaded with `loadPopulation`
#' @param individualIds Ids of individual (single value or array) to export
#' @param outputFolder Folder where the individual simulations will be exported. File format will be `simulationName_individualId`
#' @param simulation Simulation uses to generate PKML files
#'
#' @return An array containing the path of all exported simulations.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' popPath <- system.file("extdata", "simple_pop.csv", package = "ospsuite")
#' population <- loadPopulation(popPath)
#'
#' exportIndividualSimulations(population, c(1, 2), tempdir(), sim)
#' @export
exportIndividualSimulations <- function(population, individualIds, outputFolder, simulation) {
  validateIsString(outputFolder)
  validateIsNumeric(individualIds)
  validateIsOfType(simulation, "Simulation")
  validateIsOfType(population, "Population")
  individualIds <- c(individualIds)
  outputFolder <- .expandPath(outputFolder)

  simuationPaths <- NULL
  for (individualId in individualIds) {
    simulationPath <- file.path(outputFolder, paste0(simulation$name, "_", individualId, ".pkml"))
    simuationPaths <- c(simuationPaths, simulationPath)
    parameterValues <- population$getParameterValuesForIndividual(individualId)
    setParameterValuesByPath(parameterValues$paths, parameterValues$values, simulation)
    saveSimulation(simulation, simulationPath)
  }

  return(simuationPaths)
}

#' Get SimulationResults from ConcurrentSimulationRunner
#'
#' @details Create a list of `SimulationResults`-objects from the results of a
#' `ConcurrentSimulationRunner`
#' @param results .NET object created by `RunConcurrently()`
#' @param resultsIdSimulationIdMap Map of results ids as keys with values being the ids of simulations the respective batch was created with. The order of IDs is as they were added to the batch.
#' @param simulationIdSimulationMap A named list of simulation ids as keys and simulation objects as values
#' to the id of a result
#' @param silentMode If `TRUE`, no warnings are displayed if a simulation fails.
#'
#' @return A named list of `SimulationResults` objects with the names being the ids of simulations or
#' simulation-batch values pairs they were produced by
.getConcurrentSimulationRunnerResults <- function(results, resultsIdSimulationIdMap, simulationIdSimulationMap, silentMode) {
  # Pre-allocate lists for SimulationResult
  simulationResults <- vector("list", length(results))
  # Set the correct order of IDs
  names(simulationResults) <- names(resultsIdSimulationIdMap)

  for (resultObject in results) {
    resultsId <- rClr::clrGet(resultObject, "Id")
    succeeded <- rClr::clrGet(resultObject, "Succeeded")
    if (succeeded) {
      # Id of the simulation of the batch
      simId <- resultsIdSimulationIdMap[[resultsId]]
      # Get the correct simulation and create a SimulationResults object
      simulationResults[[resultsId]] <- SimulationResults$new(ref = rClr::clrGet(resultObject, "Result"), simulation = simulationIdSimulationMap[[simId]])
      next()
    }
    # If the simulation run failed, show a warning
    if (!silentMode) {
      errorMessage <- rClr::clrGet(resultObject, "ErrorMessage")
      warning(errorMessage)
    }
  }
  return(simulationResults)
}


#' @keywords internal
#' @noRd
.addBranch <- function(originalPathString, arrayToGo) {
  # Function to create a multilayered list called endList with a branched
  # structure corresponding to the structure of arrayToGo that terminates with a
  # string called 'path' that is equal to the string originalString
  if (length(arrayToGo) == 0) {
    # If arrayToGo is empty, create a terminal list with a string called 'path'
    # and value equal to originalString
    endList <- list()
    endList$path <- originalPathString
    return(endList)
  } else {
    # If arrayToGo is still not empty, remove its leading element and create a
    # sub-branch list corresponding to the structure of the remaining elements
    # of arrayToGo
    newBranch <- list()
    newBranch[[arrayToGo[1]]] <- .addBranch(originalPathString, tail(arrayToGo, -1))

    return(newBranch)
  }
}

#' @keywords internal
#' @noRd
.nextStep <- function(listSoFar, originalString, arrayToGo) {
  # Recursive function that adds a multilayer list to listSoFar that has a
  # branched structure representing the vector of strings arrayToGo.
  if (length(arrayToGo) == 0) {
    # If end of string vector arrayToGo has been reached, create a vector called
    # 'path' and give it the value 'originalString'.
    listSoFar$path <- originalString
  } else {
    # End of branch has not been reached. If this portion of the string vector
    # arrayToGo has not been added to listToGo yet, add it using the function
    # .addBranch
    if (is.null(listSoFar[[arrayToGo[1]]])) {
      listSoFar[[arrayToGo[1]]] <- .addBranch(originalString, tail(arrayToGo, -1))
    }
    # If this portion of the string vector arrayToGo has already been added to
    # listSoFar, remove the leading element of arrayToGo and recursively apply
    # this function using the remaining elements of arrayToGo.
    else {
      listSoFar[[arrayToGo[1]]] <- .nextStep(listSoFar[[arrayToGo[1]]], originalString, tail(arrayToGo, -1))
    }
  }

  return(listSoFar)
}


#' Get simulation tree
#'
#' @description
#'
#' Given a simulation file path or an instance of a simulation, traverses the
#' simulation structure and returns a tree like structure allowing for intuitive
#' navigation in the simulation tree.
#
#' @param simulationOrFilePath Full path of the simulation to load or instance
#'   of a simulation.
#' @param quantityType A vector of strings that specify the types of the
#'   entities to be included in the tree.  The types can be any combination of
#'   "Quantity", "Molecule", "Parameter" and "Observer".
#'
#' @return
#'
#' A list with a branched structure representing the path tree of entities in
#' the simulation file that fall under the types specified in `quantityType`. At
#' the end of each branch is a string called 'path' that is the path of the
#' quantity represented by the branch.
#'
#' @importFrom utils tail
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' tree <- getSimulationTree(sim)
#'
#' liver_volume_path <- tree$Organism$Liver$Volume$path
#' @export
getSimulationTree <- function(simulationOrFilePath, quantityType = "Quantity") {
  validateIsOfType(simulationOrFilePath, c("Simulation", "character"))

  quantityTypeList <- list(
    "Quantity" = getAllQuantityPathsIn,
    "Molecule" = getAllMoleculePathsIn,
    "Parameter" = getAllParameterPathsIn,
    "Observer" = getAllObserverPathsIn
  )

  validateIsIncluded(values = quantityType, parentValues = names(quantityTypeList))

  simulation <- simulationOrFilePath
  if (isOfType(simulationOrFilePath, "character")) {
    simulation <- loadSimulation(simulationOrFilePath)
  }

  # Build a vector, with no duplicated entries, of all paths corresponding to
  # entities in `simulation` that fall under the types specified in quantityType
  allPaths <- sapply(quantityType, function(type) {
    quantityTypeList[[type]](simulation)
  }) %>%
    unname() %>%
    unlist() %>%
    unique()

  # Initiate list to be returned as a null list.
  pathEnumList <- list()

  for (path in allPaths) {
    # Convert the path string to a vector of strings, each representing a branch portion.
    pathArray <- toPathArray(path)

    # Begin recursive loop to generate branched list.
    pathEnumList <- .nextStep(pathEnumList, path, pathArray)
  }

  return(pathEnumList)
}
