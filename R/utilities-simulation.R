#' @title Loads a simulation from a pkml file and returns the simulation. If the passed simulation file
#' has been loaded before, the simulation is not loaded again but a cached object is returned.
#' This behavior can be overriden.
#'
#' @param filePath Full path of pkml simulation file to load.
#'
#' @param loadFromCache If \code{TRUE}, an already loaded pkml file will not be loaded
#' again, but the simulation object will be retrieved from cache. This is the
#' default behavior. If \code{FALSE}, new object will be created. Default value is \code{FALSE}
#'
#' @param addToCache If \code{TRUE}, the loaded simulation is added to cache. If \code{FALSE},
#' the returned simulation only exists locally. Default is \code{TRUE}
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
loadSimulation <- function(filePath, loadFromCache = FALSE, addToCache = TRUE) {
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
  simulationPersister <- getNetTask("SimulationPersister")

  # Note: We do not expand the variable filePath here as we want the cache to be created using the path given by the user
  netSim <- rClr::clrCall(simulationPersister, "LoadSimulation", expandPath(filePath))

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
  validateIsOfType(simulation, Simulation)
  validateIsString(filePath)
  filePath <- expandPath(filePath)
  simulationPersister <- getNetTask("SimulationPersister")
  rClr::clrCall(simulationPersister, "SaveSimulation", simulation$ref, filePath)
  invisible()
}

#' @title  Runs a simulation (individual or population) and returns a \code{SimulationResults} object containing all results of the simulation
#'
#' @param simulation Instance of a \code{Simulation} to simulate.
#' @param population Optional instance of a \code{Population} to use for the simulation
#' @param simulationRunOptions Optional instance of a \code{SimulationRunOptions} used during the simulation run
#'
#' @return SimulationResults (one entry per Individual)
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Running an individual simulation
#' results <- runSimulation(sim)
#'
#' # Creating custom simulation run options
#'
#' simRunOptions <- SimulationRunOptions$new()
#' simRunOptions$numberOfCoresToUse <- 3
#' simRunOptions$showProgress <- TRUE
#'
#' # Running a population simulation
#' popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#' population <- loadPopulation(popPath)
#' results <- runSimulation(sim, population, simulationRunOptions = simRunOptions)
#' @export
runSimulation <- function(simulation, population = NULL, simulationRunOptions = NULL) {
  validateIsOfType(simulation, Simulation)
  validateIsOfType(population, Population, nullAllowed = TRUE)
  validateIsOfType(simulationRunOptions, SimulationRunOptions, nullAllowed = TRUE)
  options <- simulationRunOptions %||% SimulationRunOptions$new()
  simulationRunner <- getNetTask("SimulationRunner")

  results <- ifNotNull(
    population,
    rClr::clrCall(simulationRunner, "Run", simulation$ref, population$ref, options$ref),
    rClr::clrCall(simulationRunner, "Run", simulation$ref, options$ref)
  )

  SimulationResults$new(results, simulation)
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
  validateIsOfType(simulation, Simulation)

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

#' @title  Returns a list containing all standard global parameters defined in a \code{simulation} for given \code{moleculeName}.
#' These parameters are typically located directly under the container named after the \code{moleculeName}.
#' For the list of standard parameters
#' @seealso  \link{MoleculeParameter}
#'
#' @param simulation Simulation to query for molecule parameters
#' @param moleculeName Name of molecule (Enzyme, Transporter etc..) for which global parameters should be returned
#'
#' @return A list of all standard global parameters defined for \code{moleculeName} if the molecule exists in the \code{simulation}.
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
  validateIsOfType(simulation, Simulation)
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
#' @return A list of parameters matching the path criteria and also candiates for a sensitivity analysis.
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
  validateIsOfType(simulation, Simulation)
  getAllEntitiesMatching(
    paths = paths,
    container = simulation,
    entityType = Parameter,
    method = "AllParametersForSensitivityAnalysisMatching"
  )
}

#' Set the values of parameters in the simulation by path
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameterPaths'
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' setSimulationParameterValues("Organism|Liver|Volume", 1, sim)
#'
#' setSimulationParameterValues(c("Organism|Liver|Volume", "Organism|Volume"), c(2, 3), sim)
#' @export
setSimulationParameterValues <- function(parameterPaths, values, simulation) {
  validateIsString(parameterPaths)
  validateIsNumeric(values)
  parameters <- sapply(parameterPaths, function(p) getParameter(p, simulation))
  setParameterValues(parameters, values)
}

#' Export simulation PKMLs for given `individualIds`. Each pkml file will contain the orginial simulation updated with parameters of the corresponding individual.
#'
#' @param population A population object typically loaded with `loadPopulation`
#' @param individualIds Ids of individual (single value or array) to export
#' @param outputFolder Folder where the individiual simulations will be exported. File format will be `simulationName_individualId`
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
  validateIsOfType(simulation, Simulation)
  validateIsOfType(population, Population)
  individualIds <- c(individualIds)
  outputFolder <- expandPath(outputFolder)

  simuationPaths <- NULL
  for (individualId in individualIds) {
    simulationPath <- file.path(outputFolder, paste0(simulation$name, "_", individualId))
    simuationPaths <- c(simuationPaths, simulationPath)
    parameterValues <- population$getParameterValuesForIndividual(individualId)
    setSimulationParameterValues(parameterValues$paths, parameterValues$values, simulation)
    saveSimulation(simulation, simulationPath)
  }

  return(simuationPaths)
}
