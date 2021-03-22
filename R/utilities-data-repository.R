#' Loads a data repository (typically observed data) from a PKML file and returns the loaded observed data.
#' The pkml files are typically exported from PK-Sim or MoBi
#'
#' @param filePath Full path of pkml file containing the observed data to load
#'
#' @examples
#' filePath <- system.file("extdata", "obs_data.pkml", package = "ospsuite")
#'
#' obsData <- loadDataRepositoryFromPKML(filePath)
#' metaData <- obsData$metaData
#' @export
loadDataRepositoryFromPKML <- function(filePath) {
  validateIsString(filePath)
  filePath <- expandPath(filePath)
  dataRepositoryTask <- getNetTask("DataRepositoryTask")
  dataRepository <- rClr::clrCall(dataRepositoryTask, "LoadDataRepository", filePath)
  DataRepository$new(dataRepository)
}
