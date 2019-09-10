#' Load a simulation from a pkml file and returns the simulation. If the passed simulation file
#' has been loaded before, the simulation is not loaded again but a cached object is returned.
#' This behavior can be overriden.
#'
#' @param pkmlSimulationFile Full path of pkml simulation file to load.
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
loadSimulation <- function(pkmlSimulationFile, loadFromCache = TRUE, addToCache = TRUE) {
  validateIsOfType(c(loadFromCache, addToCache), "logical")

  if (loadFromCache) {
    # If the file has already been loaded, return the last loaded object
    if (ospsuiteEnv$loadedSimulationsCache$hasKey(pkmlSimulationFile)) {
      return(ospsuiteEnv$loadedSimulationsCache$get(pkmlSimulationFile))
    }
  }

  # If the simulation has not been loaded so far, or loadFromCache == FALSE,
  # new simulation object will be created
  simulationPersister <- getNetTask("SimulationPersister")
  netSim <- rClr::clrCall(simulationPersister, "LoadSimulation", pkmlSimulationFile)
  simulation <- Simulation$new(netSim, pkmlSimulationFile)

  # Add the simulation to the cache of loaded simulations
  if (addToCache) {
    ospsuiteEnv$loadedSimulationsCache$set(pkmlSimulationFile, simulation)
  }

  return(simulation)
}

#' Saves a simulation to pkml file
#'
#' @param simulation Instance of a simulation to save.
#' @param pkmlSimulationFile Full path of where the simulation will be saved.
#'
#' @export
saveSimulation <- function(simulation, pkmlSimulationFile) {
  validateIsOfType(simulation, "Simulation")
  simulationPersister <- getNetTask("SimulationPersister")
  rClr::clrCall(simulationPersister, "SaveSimulation", simulation$ref, pkmlSimulationFile)
  invisible()
}

#' Runs a simulation and returns a \code{SimulationResults} object containing all results of the simulation
#'
#' @param simulation Instance of a simulation to simulate.
#'
#' @return SimulationResults (one entry per Individual)
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' results <- runSimulation(sim)
#' @export
runSimulation <- function(simulation) {
  validateIsOfType(simulation, "Simulation")
  simulationRunner <- getNetTask("SimulationRunner")
  results <- rClr::clrCall(simulationRunner, "RunSimulation", simulation$ref)
  SimulationResults$new(results)
}


#' Adds the quantities as output into the  \code{simulation}. The quantities can either be specified using explicit instances or using paths.
#'
#' @param quantitiesOrPaths Quantity instances (element or vector) (typically retrieved using \code{getAllQuantitiesMatching}) or quantity path (element or vector) to add.
#' @param simulation Instance of a simulation for which output selection should be updated.
#'
#' @return A list of quantities added as output (Especially useful when a wildcard was used to verify)
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' paths <- c("Organism|VenousBlood|Plasma|Caffeine", "Organism|ArterialBlood|**|Caffeine")
#' addOutputs(paths, sim)
#'
#' parameter <- getParameter("Organism|Liver|Volume", sim)
#' addOutputs(parameter, sim)
#' @export
addOutputs <- function(quantitiesOrPaths, simulation) {
  quantitiesOrPaths <- c(quantitiesOrPaths)

  validateIsOfType(quantitiesOrPaths, c("Quantity", "character"))
  validateIsOfType(simulation, "Simulation")
  quantities <- quantitiesOrPaths

  if (isOfType(quantitiesOrPaths, "character")) {
    quantities <- getAllQuantitiesMatching(quantitiesOrPaths, simulation)
  }

  quantities <- uniqueEntities(quantities, compareBy = "path")
  outputSelections <- simulation$settings$outputSelections

  for (quantity in quantities) {
    outputSelections$addQuantity(quantity)
  }

  invisible(quantities)
}

#' Removes all selected output from the given \code{simulation}
#'
#' @param simulation Instance of a simulation for which output selection should be cleared.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' clearOutputs(sim)
#' @export
clearOutputs <- function(simulation) {
  validateIsOfType(simulation, "Simulation")
  simulation$settings$outputSelections$clear()
  invisible(simulation)
}

#' Clears cache of loaded simulations
#' @export
resetSimulationCache <- function() {
  ospsuiteEnv$loadedSimulationsCache$reset()
}

#' Removes a simulation from simulations cache.
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
