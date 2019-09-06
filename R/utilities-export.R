#' Saves the simulation results to csv file
#'
#' @param results Results to export (typically calculated using \code{runSimulation} or imported from file)
#' @param simulation Instance of a simulation used to calculate the results
#' @param filename Full path of where the results will be saved.
#'
#' @export
exportResultsToCSV <- function(results, simulation, filename){
  validateIsOfType(results, "SimulationResults")
  validateIsOfType(simulation, "Simulation")
  validateIsString(filename)
  simulationExporter <- getNetTask("SimulationExporter")
  rClr::clrCall(simulationExporter, "ExportResultsToCSV", results$ref, simulation$ref, filename)
  invisible()
}

#' Saves the pK-analyses  to csv file
#'
#' @param pkAnalyses pK-Analyses to exporte (typically calculated using \code{calculatePKAnalyses} or imported from file)
#' @param simulation Instance of a simulation used to calculate the pK-Analyses
#' @param filename Full path of where the pK-Analyses will be saved.
#'
#' @export
exportPKAnalysesToCSV <- function(pkAnalyses, simulation, filename){
  validateIsOfType(results, "SimulationPKAnalyses")
  validateIsOfType(simulation, "Simulation")
  validateIsString(filename)
  simulationExporter <- getNetTask("SimulationExporter")
  rClr::clrCall(simulationExporter, "ExportPKAnalysesToCSV", pkAnalyses$ref, simulation$ref, filename)
  invisible()
}
