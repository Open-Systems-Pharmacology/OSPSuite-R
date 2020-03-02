#' @title Calculates the pkAnalyses for all output values available in \code{results}.
#'
#' @param results Results of simulation. Typically the \code{results} are calculated using \code{runSimulation} or imported from csv file via \code{importResults}
#' @param dynamicPKParameters List of dynamic PK Parameters that should be used to calculate PK values (optional)
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
calculatePKAnalyses <- function(results, dynamicPKParameters = NULL) {
  dynamicPKParameters <- c(dynamicPKParameters)
  validateIsOfType(results, SimulationResults)
  validateIsOfType(dynamicPKParameters, DynamicPKParameter, nullAllowed = TRUE)
  pkAnalysesTask <- getNetTask("PKAnalysesTask")
  calculatePKAnalysesArgs <- rClr::clrNew("OSPSuite.R.Services.CalculatePKAnalysisArgs")
  rClr::clrSet(calculatePKAnalysesArgs, "Simulation", results$simulation$ref)
  rClr::clrSet(calculatePKAnalysesArgs, "NumberOfIndividuals", as.integer(results$count))
  rClr::clrSet(calculatePKAnalysesArgs, "SimulationResults", results$ref)
  for (dynamicPKParameter in dynamicPKParameters) {
    rClr::clrCall(calculatePKAnalysesArgs, "AddDynamicParameter", dynamicPKParameter$ref)
  }
  pkAnalyses <- rClr::clrCall(pkAnalysesTask, "CalculateFor", calculatePKAnalysesArgs)
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
  pkAnalysesTask <- getNetTask("PKAnalysesTask")
  rClr::clrCall(pkAnalysesTask, "ExportPKAnalysesToCSV", pkAnalyses$ref, pkAnalyses$simulation$ref, filePath)
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
  pkAnalysesTask <- getNetTask("PKAnalysesTask")
  pkAnalyses <- rClr::clrCall(pkAnalysesTask, "ImportPKAnalysesFromCSV", filePath, simulation$ref)
  SimulationPKAnalyses$new(pkAnalyses, simulation)
}


#' @title Convert the pk-Analysis to data frame
#'
#' @param pkAnalyses pK-Analyses to convert to data frame (typically calculated using \code{calculatePKAnalyses} or imported from file)
#' @importFrom utils read.csv
#'
#' @export
pkAnalysesAsDataFrame <- function(pkAnalyses) {
  validateIsOfType(pkAnalyses, SimulationPKAnalyses)
  pkParameterResultsFilePath <- tempfile()
  dataFrame <- tryCatch({
    exportPKAnalysesToCSV(pkAnalyses, pkParameterResultsFilePath)
    pkResultsDataFrame <- read.csv(pkParameterResultsFilePath, encoding = "UTF-8", check.names = FALSE)
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
