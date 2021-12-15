#' Loads a population from a csv file and returns the population.
#'
#' @param csvPopulationFile Full path of csv population file to load.
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' population <- loadPopulation(csvPath)
#' @export
loadPopulation <- function(csvPopulationFile) {
  ospsuite.utils::validateIsString(csvPopulationFile)
  csvPopulationFile <- expandPath(csvPopulationFile)
  populationTask <- getNetTask("PopulationTask")
  population <- rClr::clrCall(populationTask, "ImportPopulation", csvPopulationFile)
  Population$new(population)
}

#' Loads a population from the `csvPopulationFile` and split the loaded population according to
#' `numberOfCores`.
#' @param csvPopulationFile Full path of csv population file to split.
#' @param numberOfCores Number of cores used for parallelization computing. The population will be split across all cores.
#' @param outputFolder Folder where all split files will be created
#' @param outputFileName File names will be constructed using this parameter concatenated with the core index.
#' @return A string vector containing the full path of the population files created. Note that there might be less files than cores
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' # Split the population in up to 3 files, saved in the temp folder
#' splitFiles <- splitPopulationFile(csvPath, 3, tempdir(), "PopFile")
#' @export
splitPopulationFile <- function(csvPopulationFile, numberOfCores, outputFolder, outputFileName) {
  ospsuite.utils::validateIsString(csvPopulationFile)
  ospsuite.utils::validateIsNumeric(numberOfCores)
  ospsuite.utils::validateIsString(outputFolder)
  ospsuite.utils::validateIsString(outputFileName)
  csvPopulationFile <- expandPath(csvPopulationFile)
  outputFileName <- enc2utf8(outputFileName)
  populationTask <- getNetTask("PopulationTask")
  rClr::clrCall(populationTask, "SplitPopulation", csvPopulationFile, as.integer(numberOfCores), outputFolder, outputFileName)
}


#' @title Creates a data.frame containing one column for each parameter defined in the population
#'
#' @param population Population to convert to data frame (typically imported from file using `loadPopulation`)
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' population <- loadPopulation(csvPath)
#' df <- populationAsDataFrame(population)
#' @export
populationAsDataFrame <- function(population) {
  ospsuite.utils::validateIsOfType(population, Population)
  columns <- list()
  columns$IndividualId <- population$allIndividualIds

  for (covariateName in population$allCovariateNames) {
    columns[[covariateName]] <- population$getCovariateValues(covariateName)
  }

  for (parameterPath in population$allParameterPaths) {
    columns[[parameterPath]] <- population$getParameterValues(parameterPath)
  }

  data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Saves the population to csv file
#'
#' @param population Population to export to csv (typically imported from file using `loadPopulation`)
#' @param filePath Full path where the population will be saved.
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' # Load the population
#' population <- loadPopulation(csvPath)
#'
#' # Exports the population
#' exportPopulationToCSV(population, tempfile())
#' @export
exportPopulationToCSV <- function(population, filePath) {
  ospsuite.utils::validateIsOfType(population, Population)
  ospsuite.utils::validateIsString(filePath)
  filePath <- expandPath(filePath)
  df <- populationAsDataFrame(population)
  write.csv(df, file = filePath, row.names = FALSE)
  invisible()
}

#' @inherit exportPopulationToCSV
savePopulationToCSV <- function(population, filePath) {
  exportPopulationToCSV(population, filePath)
}

#' Loads aging data (typically generated from PK-Sim) i
#'
#' @param filePath Full path containing an aging data table.
#'
#' @examples
#' csvPath <- system.file("extdata", "aging_data.csv", package = "ospsuite")
#'
#' agingData <- loadAgingDataFromCSV(csvPath)
#' @export
loadAgingDataFromCSV <- function(filePath) {
  ospsuite.utils::validateIsString(filePath)
  df <- readr::read_csv(filePath, locale = readr::locale(encoding = "UTF-8"), comment = "#", col_types = readr::cols())
  agingData <- AgingData$new()
  agingData$individualIds <- as.integer(df$IndividualId)
  agingData$parameterPaths <- df$ParameterPath
  agingData$times <- df$Time
  agingData$values <- df$Value
  return(agingData)
}
