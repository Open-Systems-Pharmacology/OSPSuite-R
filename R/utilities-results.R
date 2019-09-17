#' Saves the simulation results to csv file
#'
#' @param results Results to export (typically calculated using \code{runSimulation} or imported from file)
#' @param filePath Full path where the results will be saved.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath)
#'
#' # Add some outputs to the simulation
#' addOutputs("Organism|**|*", sim)
#'
#' # Run the simulation
#' results <- runSimulation(sim)
#'
#' # Export the results to csv file
#' exportResultsToCSV(results, tempfile())
#' @export
exportResultsToCSV <- function(results, filePath) {
  validateIsOfType(results, "SimulationResults")
  validateIsString(filePath)
  simulationResultsTask <- getNetTask("SimulationResultsTask")
  rClr::clrCall(simulationResultsTask, "ExportResultsToCSV", results$ref, results$simulation$ref, filePath)
  invisible()
}



#' Imports the simulation results from one or more csv files
#'
#' @param simulation Instance of a simulation used to calculate the results
#' @param filePaths Full path of result files to import. Typically only one
#' file is provided but a list of files is sometimes available when the simulation
#' was parallelized and computed on different machines
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' resultPath <- system.file("extdata", "res.csv", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath)
#'
#' # Run the simulation
#' results <- importResultsFromCSV(sim, resultPath)
#' @export
importResultsFromCSV <- function(simulation, filePaths) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(filePaths)
  simulationResultsTask <- getNetTask("SimulationResultsTask")
  results <- rClr::clrCall(simulationResultsTask, "ImportResultsFromCSV", simulation$ref, filePaths)
  SimulationResults$new(results, simulation)
}
