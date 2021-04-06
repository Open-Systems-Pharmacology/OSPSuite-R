#' @title Calculates the pkAnalyses for all output values available in \code{results}.
#'
#' @param results Results of simulation. Typically the \code{results} are calculated using \code{runSimulation} or imported from csv file via \code{importResults}
#'
#' @return An instance of \code{SimulationPKAnalyses} class.
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
  pkAnalysisTask <- getNetTask("PKAnalysisTask")
  calculatePKAnalysisArgs <- rClr::clrNew("OSPSuite.R.Services.CalculatePKAnalysisArgs")
  rClr::clrSet(calculatePKAnalysisArgs, "Simulation", results$simulation$ref)
  rClr::clrSet(calculatePKAnalysisArgs, "SimulationResults", results$ref)
  pkAnalyses <- rClr::clrCall(pkAnalysisTask, "CalculateFor", calculatePKAnalysisArgs)
  SimulationPKAnalyses$new(pkAnalyses, results$simulation)
}


#' @title Saves the pK-analyses  to csv file
#'
#' @param pkAnalyses pK-Analyses to export (typically calculated using \code{calculatePKAnalyses} or imported from file)
#' @param filePath Full path where the pK-Analyses will be saved.
#'
#' @export
exportPKAnalysesToCSV <- function(pkAnalyses, filePath) {
  validateIsOfType(pkAnalyses, SimulationPKAnalyses)
  validateIsString(filePath)
  filePath <- expandPath(filePath)
  pkAnalysisTask <- getNetTask("PKAnalysisTask")
  rClr::clrCall(pkAnalysisTask, "ExportPKAnalysesToCSV", pkAnalyses$ref, pkAnalyses$simulation$ref, filePath)
  invisible()
}

#' @title Loads the pK-analyses from csv file
#'
#' @param filePath Full path of the file containing the pK-Analyses to load.
#' @param simulation Instance of the simulation for which the pk-Analyses were calculated. This is required to verify that the file
#' matches the simulation
#'
#' @export
importPKAnalysesFromCSV <- function(filePath, simulation) {
  validateIsOfType(simulation, Simulation)
  validateIsString(filePath)
  filePath <- expandPath(filePath)
  pkAnalysisTask <- getNetTask("PKAnalysisTask")
  pkAnalyses <- rClr::clrCall(pkAnalysisTask, "ImportPKAnalysesFromCSV", filePath, simulation$ref)
  SimulationPKAnalyses$new(pkAnalyses, simulation)
}


#' @title Convert the pk-Analysis to data frame
#'
#' @param pkAnalyses pK-Analyses to convert to data frame (typically calculated using \code{calculatePKAnalyses} or imported from file)
#'
#' @export
pkAnalysesAsDataFrame <- function(pkAnalyses) {
  validateIsOfType(pkAnalyses, SimulationPKAnalyses)
  pkParameterResultsFilePath <- tempfile()
  dataFrame <- tryCatch(
    {
      exportPKAnalysesToCSV(pkAnalyses, pkParameterResultsFilePath)
      pkResultsDataFrame <- readr::read_csv(pkParameterResultsFilePath, locale = readr::locale(encoding = "UTF-8"), comment = "#", col_types = cols())
      colnames(pkResultsDataFrame) <- c("IndividualId", "QuantityPath", "Parameter", "Value", "Unit")
      pkResultsDataFrame$QuantityPath <- as.factor(pkResultsDataFrame$QuantityPath)
      pkResultsDataFrame$Parameter <- as.factor(pkResultsDataFrame$Parameter)
      pkResultsDataFrame$Unit <- as.factor(pkResultsDataFrame$Unit)
      return(pkResultsDataFrame)
    },
    finally = {
      file.remove(pkParameterResultsFilePath)
    }
  )
  return(dataFrame)
}
