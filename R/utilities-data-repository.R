#' Loads a data repository (typically observed data) from a PKML file and returns the loaded observed data.
#' The pkml files are typically exported from PK-Sim or MoBi
#'
#' @param filePath Full path of pkml file containing the observed data to load
#' @keywords internal
.loadDataRepositoryFromPKML <- function(filePath) {
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  dataRepositoryTask <- .getCoreTaskFromCache("DataRepositoryTask")
  dataRepository <- dataRepositoryTask$call("LoadDataRepository", filePath)
  DataRepository$new(dataRepository)
}
