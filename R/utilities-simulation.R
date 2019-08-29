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

#' Runs a simulation and returns a list of \code{IndividualResults}
#'
#' @param simulation Instance of a simulation to simulate.
#'
#' @return List of individual results (one entry per Individual)
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
  return(clrCall(results, "IndividualResultsAsArray"))
}
