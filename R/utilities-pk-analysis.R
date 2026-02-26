#' @title Calculates the pkAnalyses for all output values available in `results`.
#'
#' @param results Results of simulation. Can be a single `SimulationResults` object
#'   or a list of `SimulationResults` objects (typically the output from
#'   `runSimulations`).
#'
#' @return If a single `SimulationResults` object is provided, returns a
#'   `SimulationPKAnalyses` object. If a list of `SimulationResults` is provided,
#'   returns a named list of `SimulationPKAnalyses` objects.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' addOutputs("Organism|VenousBlood|*|Aciclovir", sim)
#' results <- runSimulations(sim)[[1]]
#' pkAnalyses <- calculatePKAnalyses(results)
#'
#' # Working with a list of SimulationResults
#' results <- runSimulations(sim)
#' pkAnalysesList <- calculatePKAnalyses(results)
#' @export
calculatePKAnalyses <- function(results) {
  # Normalize input
  normalized <- .normalizeSimulationResults(results)
  resultsList <- normalized$list
  wasList <- normalized$wasList
  
  # Process each SimulationResults object
  pkAnalysesList <- lapply(resultsList, function(singleResult) {
    pkAnalysisTask <- .getNetTask("PKAnalysisTask")
    calculatePKAnalysisArgs <- rSharp::newObjectFromName(
      "OSPSuite.R.Services.CalculatePKAnalysisArgs"
    )
    calculatePKAnalysisArgs$set("Simulation", singleResult$simulation)
    calculatePKAnalysisArgs$set("SimulationResults", singleResult)
    pkAnalyses <- pkAnalysisTask$call("CalculateFor", calculatePKAnalysisArgs)
    SimulationPKAnalyses$new(pkAnalyses, singleResult$simulation)
  })
  
  # Name the output list using the names from the input list
  names(pkAnalysesList) <- names(resultsList)
  
  # If input was a single SimulationResults, return single result (not list)
  if (!wasList) {
    return(pkAnalysesList[[1]])
  }
  
  return(pkAnalysesList)
}

#' @title Saves the pK-analyses  to csv file
#'
#' @param pkAnalyses pK-Analyses to export (typically calculated using `calculatePKAnalyses` or imported from file)
#' @param filePath Full path where the pK-Analyses will be saved.
#'
#' @export
exportPKAnalysesToCSV <- function(pkAnalyses, filePath) {
  validateIsOfType(pkAnalyses, "SimulationPKAnalyses")
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  pkAnalysisTask <- .getNetTask("PKAnalysisTask")
  pkAnalysisTask$call(
    "ExportPKAnalysesToCSV",
    pkAnalyses,
    pkAnalyses$simulation,
    filePath
  )
  invisible()
}

#' @inherit exportPKAnalysesToCSV
.savePKAnalysesToCSV <- function(pkAnalyses, filePath) {
  exportPKAnalysesToCSV(pkAnalyses, filePath)
}

#' @title Loads the pK-analyses from csv file
#'
#' @param filePath Full path of the file containing the pK-Analyses to load.
#' @param simulation Instance of the simulation for which the pk-Analyses were
#'   calculated. This is required to verify that the file matches the simulation.
#'
#' @export
importPKAnalysesFromCSV <- function(filePath, simulation) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  pkAnalysisTask <- .getNetTask("PKAnalysisTask")
  pkAnalyses <- pkAnalysisTask$call(
    "ImportPKAnalysesFromCSV",
    filePath,
    simulation
  )
  SimulationPKAnalyses$new(pkAnalyses, simulation)
}


#' @title Convert the pk-Analysis to data frame
#'
#' @param pkAnalyses pK-Analyses to convert to data frame (typically calculated
#'   using `calculatePKAnalyses` or imported from file).
#'
#' @export
pkAnalysesToDataFrame <- function(pkAnalyses) {
  validateIsOfType(pkAnalyses, "SimulationPKAnalyses")
  pkParameterResultsFilePath <- tempfile()

  pkAnalysesData <- tryCatch(
    {
      exportPKAnalysesToCSV(pkAnalyses, pkParameterResultsFilePath)
      colTypes <- list(
        IndividualId = readr::col_integer(),
        QuantityPath = readr::col_character(),
        Parameter = readr::col_character(),
        Value = readr::col_double(),
        Unit = readr::col_character()
      )

      pkResultsDataFrame <- readr::read_csv(
        pkParameterResultsFilePath,
        locale = readr::locale(encoding = "UTF-8"),
        comment = "#",
        col_types = colTypes,
        na = c("NaN", "", NA, "NA", "Infinity", "-Infinity")
      )

      return(pkResultsDataFrame)
    },
    finally = {
      file.remove(pkParameterResultsFilePath)
    }
  )

  # consistently return a (classical) data frame
  return(pkAnalysesData)
}

#' @rdname pkAnalysesToDataFrame
#'
#' @export
pkAnalysesToTibble <- function(pkAnalyses) {
  pkAnalysesData <- pkAnalysesToDataFrame(pkAnalyses)

  # consistently return a tibble data frame
  return(dplyr::as_tibble(pkAnalysesData))
}
