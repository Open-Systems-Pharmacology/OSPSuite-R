#' Run a simulation from a pkml file and returns the simulation
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
