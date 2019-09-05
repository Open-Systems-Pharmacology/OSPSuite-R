#' Loads a simulation from a pkml file and returns the simulation
#'
#' @param pkmlSimulationFile Full path of pkml simulation file to load.
#'
#' @return The \code{Simulation}individual results (one entry per Individual)
#'
#' @export
loadSimulation <- function(pkmlSimulationFile) {
  simulationPersister <- getNetTask("SimulationPersister")
  netSim <- clrCall(simulationPersister, "LoadSimulation", pkmlSimulationFile)
  Simulation$new(netSim)
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
  clrCall(simulationPersister, "SaveSimulation", simulation$ref, pkmlSimulationFile)
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
  results <- clrCall(simulationRunner, "RunSimulation", simulation$ref)
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
