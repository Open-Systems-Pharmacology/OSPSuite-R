#' Load a simulation from a pkml file and returns the simulation
#'
#' @param pkmlSimulationFile Full path of pkml simulation file to load.
#'
#' @export
loadSimulation <- function(pkmlSimulationFile) {
  simulationLoader <- getNetTask("SimulationLoader")
  netSim <- rClr::clrCall(simulationLoader, "LoadSimulation", pkmlSimulationFile)
  Simulation$new(netSim)
}
