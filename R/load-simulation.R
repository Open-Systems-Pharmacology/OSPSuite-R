#' Load a simulation from a pkml file and returns the simulation
#'
#' @param pkmlSimulationFile Full path of pkml simulation file to load.
#'
#' @import rClr
#'
#' @export
loadSimulation <- function(pkmlSimulationFile) {
  simulationLoader <- rClr::clrNew("OSPSuite.R.Services.SimulationLoader")
  netSim <- rClr::clrCall(simulationLoader, "LoadSimulation", pkmlSimulationFile)
  Simulation$new(netSim)
}
