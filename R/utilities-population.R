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
  validateIsString(csvPopulationFile)
  populationImporter <- getNetTask("PopulationTask")
  population <- rClr::clrCall(populationImporter, "ImportPopulation", csvPopulationFile)
  Population$new(population)
}
