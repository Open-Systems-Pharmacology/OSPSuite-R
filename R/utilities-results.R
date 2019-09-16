#' Saves the simulation results to csv file
#'
#' @param results Results to export (typically calculated using \code{runSimulation} or imported from file)
#' @param simulation Instance of a simulation used to calculate the results
#' @param fileName Full path where the results will be saved.
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
#' exportResultsToCSV(results, sim, tempfile())
#' @export
exportResultsToCSV <- function(results, simulation, fileName) {
  validateIsOfType(results, "SimulationResults")
  validateIsOfType(simulation, "Simulation")
  validateIsString(fileName)
  simulationResultsTask <- getNetTask("SimulationResultsTask")
  rClr::clrCall(simulationResultsTask, "ExportResultsToCSV", results$ref, simulation$ref, fileName)
  invisible()
}



#' Imports the simulation results from one or more csv files
#'
#' @param simulation Instance of a simulation used to calculate the results
#' @param fileNames Full path of result files to import. Typically only one
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
#'
#' @export
importResultsFromCSV <- function(simulation, fileNames) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(fileNames)
  simulationResultsTask <- getNetTask("SimulationResultsTask")
  results <- rClr::clrCall(simulationResultsTask, "ImportResultsFromCSV", simulation$ref, fileNames)
  SimulationResults$new(results)
}
