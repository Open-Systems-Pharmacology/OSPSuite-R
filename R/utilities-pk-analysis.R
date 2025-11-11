#' @title Calculates the pkAnalyses for all output values available in `results`.
#'
#' @param results Results of simulation. Typically the `results` are calculated
#'   using `runSimulations` or imported from csv file via `importResults`.
#'
#' @return An instance of `SimulationPKAnalyses` class.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' addOutputs("Organism|VenousBlood|*|Caffeine", sim)
#' results <- runSimulations(sim)[[1]]
#' pkAnalyses <- calculatePKAnalyses(results)
#' @export
calculatePKAnalyses <- function(results) {
  validateIsOfType(results, "SimulationResults")
  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
  calculatePKAnalysisArgs <- rSharp::newObjectFromName(
    "OSPSuite.R.Services.CalculatePKAnalysisArgs"
  )
  calculatePKAnalysisArgs$set("Simulation", results$simulation)
  calculatePKAnalysisArgs$set("SimulationResults", results)
  pkAnalyses <- pkAnalysisTask$call("CalculateFor", calculatePKAnalysisArgs)
  SimulationPKAnalyses$new(pkAnalyses, results$simulation)
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
  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
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
  pkAnalysisTask <- .getCoreTask("PKAnalysisTask")
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
