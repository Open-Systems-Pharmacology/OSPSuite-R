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
#' pkAnalyses <- getPKAnalyses(results, sim)
#' @export
getPKAnalyses <- function(results, simulation) {
  pkAnalysesTask <- getNetTask("PKAnalysesTask")
  pkAnalyses <- clrCall(pkAnalysesTask, "CalculateFor", simulation$ref,  as.integer(1),  results$ref)
  SimulationPKAnalyses$new(pkAnalyses)
}


toPKParameter <- function(netPKParameters) {
  toObjectType(netPKParameters, PKParameter)
}
