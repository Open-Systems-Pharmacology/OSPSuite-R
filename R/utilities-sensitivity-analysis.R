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
#' # Running an individual simulation
#' results <- runSimulation(sim)
#'
#' # Running a population simulation
#' popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#' population <- loadPopulation(popPath)
#' results <- runSimulation(sim, population)
#' @export
runSensitivityAnalysis <- function(sensitivityAnalysis, sensitivityAnalysisRunOptions = NULL) {
  validateIsOfType(sensitivityAnalysis, SensitivityAnalysis)
  validateIsOfType(sensitivityAnalysisRunOptions, SensitivityAnalysisRunOptions, nullAllowed = TRUE)
  options <- sensitivityAnalysisRunOptions %||% SensitivityAnalysisRunOptions$new()
  sensitivityAnalysisRunner <- getNetTask("SensitivityAnalysisRunner")

  results <- rClr::clrCall(sensitivityAnalysisRunner, "Run", sensitivityAnalysis$ref, options$ref)

  SensitivityAnalysisResults$new(results)
}
