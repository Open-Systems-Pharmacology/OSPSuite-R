#' Load a simulation from a pkml file and returns the simulation
#'
#' @param pkmlSimulationFile Full path of pkml simulation file to load.
#'
#' @export
loadSimulation <- function(pkmlSimulationFile) {
  rClr::clrGetStaticMethods("OSPSuite.R.Api")
  simulationLoader <- rClr::clrCallStatic("OSPSuite.R.Api", "GetSimulationLoader")
  netSim <- rClr::clrCall(simulationLoader, "LoadSimulation", pkmlSimulationFile)
  Simulation$new(netSim)
}
