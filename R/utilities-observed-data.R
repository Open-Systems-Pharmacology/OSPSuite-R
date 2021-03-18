#' Loads a population from a csv file and returns the population.
#'
#' @param filePath Full path of pkml file containing the observed data to load
#'
#' @examples
#' filePath <- system.file("extdata", "obsData.pkml", package = "ospsuite")
#'
#' obsData <- loadObservedData(filePath)
#' @export
loadObservedData <- function(filePath) {
  validateIsString(filePath)
  filePath <- expandPath(filePath)
  dataRepositoryTask <- getNetTask("DataRepositoryTask")
  dataRepository <- rClr::clrCall(dataRepositoryTask, "LoadDataRepository", filePath)
  DataRepository$new(dataRepository)
}
