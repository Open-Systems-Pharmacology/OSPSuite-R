#' @title  Runs a sensitivity analaysis
#'
#' @param sensitivityAnalysis Instance of a \code{SensitivityAnalysis} to run
#' @param sensitivityAnalysisRunOptions Optional instance of a \code{SensitivityAnalysisRunOptions} used during the sensitivity analysis run
#'
#' @return SimulationResults (one entry per Individual)
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Create a new sensitivity object for the simulation
#' sensitivity <- SensitivityAnalysis$new(sim)
#'
#' # Runs the sensitivity analysis
#' results <- runSensitivityAnalysis(sensitivity)
#' @export
runSensitivityAnalysis <- function(sensitivityAnalysis, sensitivityAnalysisRunOptions = NULL) {
  validateIsOfType(sensitivityAnalysis, SensitivityAnalysis)
  validateIsOfType(sensitivityAnalysisRunOptions, SensitivityAnalysisRunOptions, nullAllowed = TRUE)
  options <- sensitivityAnalysisRunOptions %||% SensitivityAnalysisRunOptions$new()
  sensitivityAnalysisRunner <- getNetTask("SensitivityAnalysisRunner")

  results <- rClr::clrCall(sensitivityAnalysisRunner, "Run", sensitivityAnalysis$ref, options$ref)

  SensitivityAnalysisResults$new(results, sensitivityAnalysis$simulation)
}

#' Saves the simulation analysis results to csv file
#'
#' @param results Results to export (typically calculated using \code{runSensitivityAnalysis} or imported from file)
#' @param filePath Full path where the results will be saved.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath)
#'
#' # Create a new sensitivity object for the simulation
#' sensitivity <- SensitivityAnalysis$new(sim)
#'
#' # Runs the sensitivity analysis
#' results <- runSensitivityAnalysis(sensitivity)
#'
#' # Export the results to csv file
#' exportSensitivityAnalysisResultsToCSV(results, tempfile())
#' @export
exportSensitivityAnalysisResultsToCSV <- function(results, filePath) {
  validateIsOfType(results, SensitivityAnalysisResults)
  validateIsString(filePath)
  sensitivityAnalysisTask <- getNetTask("SensitivityAnalysisTask")
  rClr::clrCall(sensitivityAnalysisTask, "ExportResultsToCSV", results$ref, results$simulation$ref, filePath)
  invisible()
}


#' Imports the simulation analysis results from one or more csv files
#'
#' @param simulation Instance of a simulation for which the sensitivity analysis was performed
#' @param filePaths Full path of sensitivity analysis result files to import. Typically only one
#' file is provided but a list of files is sometimes available when the sensitivity analysis run
#' was parallelized and computed on different machines
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' resultPath <- system.file("extdata", "sa.csv", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath)
#'
#' # Run the simulation
#' results <- importSensitivityAnalysisResultsFromCSV(sim, resultPath)
#' @export
importSensitivityAnalysisResultsFromCSV <- function(simulation, filePaths) {
  validateIsOfType(simulation, Simulation)
  validateIsString(filePaths)
  sensitivityAnalysisTask <- getNetTask("SensitivityAnalysisTask")
  results <- rClr::clrCall(sensitivityAnalysisTask, "ImportResultsFromCSV", simulation$ref, filePaths)
  SensitivityAnalysisResults$new(results, simulation)
}

#' Returns an array of parameter path with one entry for each parameter that is used in the simulation
#' and can potentially be used for sensitivity analysis
#'
#' @param simulation Instance of a simulation for which variable parameters should be retrieved
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath)
#'
#' parameterPaths <- potentialVariableParameterPathsFor(sim)
#' @export
potentialVariableParameterPathsFor <- function(simulation) {
  validateIsOfType(simulation, Simulation)
  sensitivityAnalysisTask <- getNetTask("SensitivityAnalysisTask")
  rClr::clrCall(sensitivityAnalysisTask, "PotentialVariableParameterPathsFor", simulation$ref)
}
