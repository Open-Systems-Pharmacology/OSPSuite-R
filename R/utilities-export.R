#' @export
exportResultsToCSV <- function(results, simulation, filename){
  simulationExporter <- getNetTask("SimulationExporter")
  clrCall(simulationExporter, "ExportResultsToCSV", results$ref, simulation$ref, filename)
  invisible(results)
}

#' @export
exportPKAnalysesToCSV <- function(pkAnalyses, simulation, filename){
  simulationExporter <- getNetTask("SimulationExporter")
  clrCall(simulationExporter, "ExportPKAnalysesToCSV", pkAnalyses$ref, simulation$ref, filename)
  invisible(pkAnalyses)
}
