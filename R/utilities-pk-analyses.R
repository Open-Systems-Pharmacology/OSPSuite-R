#' Calculates the pkAnalyses for all output values available in \code{results}.
#'
#' @param results Results of simulation. Typically the \code{results} are calculated using \code{runSimulation} or imported from csv file via \code{importResults}
#' @param simulation Instance of a simulation for which pk-analyses should be calculated
#'
#' @return A list of QuantityPKParameter (one object for each output-pk parameter combination)
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' addOutputs("Organism|VenousBlood|*|Caffeine", sim)
#' results <- runSimulation(sim)
#' pkAnalyses <- calculatePKAnalyses(results, sim)
#' @export
calculatePKAnalyses <- function(results, simulation) {
  validateIsOfType(results, SimulationResults)
  validateIsOfType(simulation, Simulation)
  pkAnalysesTask <- getNetTask("PKAnalysesTask")
  #TODO: The one is because we only have one element in the simulation. This will have to be updated when we are dealing with population simulations
  pkAnalyses <- rClr::clrCall(pkAnalysesTask, "CalculateFor", simulation$ref,  as.integer(1),  results$ref)
  SimulationPKAnalyses$new(pkAnalyses)
}


toPKParameter <- function(netPKParameters) {
  toObjectType(netPKParameters, PKParameter)
}
