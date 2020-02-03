#' @title Calculates the pkAnalyses for all output values available in \code{results}.
#'
#' @param results Results of simulation. Typically the \code{results} are calculated using \code{runSimulation} or imported from csv file via \code{importResults}
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
#' pkAnalyses <- calculatePKAnalyses(results)
#' @export
calculatePKAnalyses <- function(results) {
  validateIsOfType(results, SimulationResults)
  pkAnalysesTask <- getNetTask("PKAnalysesTask")
  pkAnalyses <- rClr::clrCall(pkAnalysesTask, "CalculateFor", results$simulation$ref, as.integer(results$count), results$ref)
  SimulationPKAnalyses$new(pkAnalyses, results$simulation)
}


#' @title Saves the pK-analyses  to csv file
#'
#' @param pkAnalyses pK-Analyses to exporte (typically calculated using \code{calculatePKAnalyses} or imported from file)
#' @param filePath Full path where the pK-Analyses will be saved.
#'
#' @export
exportPKAnalysesToCSV <- function(pkAnalyses, filePath) {
  validateIsOfType(pkAnalyses, SimulationPKAnalyses)
  validateIsString(filePath)
  filePath <- path.expand(filePath)
  simulationExporter <- getNetTask("SimulationExporter")
  rClr::clrCall(simulationExporter, "ExportPKAnalysesToCSV", pkAnalyses$ref, pkAnalyses$simulation$ref, filePath)
  invisible()
}
