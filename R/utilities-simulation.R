#' @title Loads a simulation from a pkml file and returns the simulation. If the passed simulation file
#' has been loaded before, the simulation is not loaded again but a cached object is returned.
#' This behavior can be overriden.
#'
#' @param filePath Full path of pkml simulation file to load.
#' @param loadFromCache If TRUE, an already loaded pkml file will not be loaded
#' again, but the simulation object will be retrieved from cache. This is the
#' default behavior. If FALSE, new object will be created.
#' @param addToCache If TRUE, the loaded simulation is added to cache. If false,
#' the returned simulation only exists locally. Default is TRUE.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load sim1 for the first time
#' sim1 <- loadSimulation(simPath)
#'
#' # sim2 will be loaded from cache and will represent the same object as sim1
#' sim2 <- loadSimulation(simPath)
#'
#' parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim1)
#' parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim2)
#'
#' # parameter1 and parameter2 belong to the same simulation object, so changing
#' # one of the them will also change another
#' setParametersValues(parameters = parameter2, values = 0)
#' parameter1$value == parameter2$value # TRUE
#'
#' # sim3 will be loaded from not from cache
#' sim3 <- loadSimulation(simPath, loadFromCache = FALSE)
#' # parameter3 belong to different simulation object than parameter1 and parameter2
#' parameter3 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim3)
#' setParametersValues(parameters = parameter3, values = 1)
#' parameter2$value == parameter3$value # FALSE#'
#' @export
loadSimulation <- function(filePath, loadFromCache = TRUE, addToCache = TRUE) {
  validateIsLogical(c(loadFromCache, addToCache))

  if (loadFromCache) {
    # If the file has already been loaded, return the last loaded object
    if (ospsuiteEnv$loadedSimulationsCache$hasKey(filePath)) {
      return(ospsuiteEnv$loadedSimulationsCache$get(filePath))
    }
  }

  # If the simulation has not been loaded so far, or loadFromCache == FALSE,
  # new simulation object will be created
  simulationPersister <- getNetTask("SimulationPersister")
  netSim <- rClr::clrCall(simulationPersister, "LoadSimulation", filePath)
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
#' # Running a population simulation
#' popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#' population <- loadPopulation(popPath)
#' results <- runSimulation(sim, population)
#' @export
runSimulation <- function(simulation, population = NULL, simulationRunOptions = NULL) {
  validateIsOfType(simulation, Simulation)
  validateIsOfType(population, Population, nullAllowed = TRUE)
  validateIsOfType(simulationRunOptions, SimulationRunOptions, nullAllowed = TRUE)
  options <- simulationRunOptions %||% SimulationRunOptions$new()
  simulationRunner <- getNetTask("SimulationRunner")

  results <- ifNotNull(
    population,
    rClr::clrCall(simulationRunner, "RunSimulation", simulation$ref, population$ref, options$ref),
    rClr::clrCall(simulationRunner, "RunSimulation", simulation$ref, options$ref)
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
