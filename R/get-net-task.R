#' Returns an instance of the specified .NET Task
#'
#' @param taskName The name of the task to retrieve (without the Get)
#'
#' @return An instance of the Task
#'
#' @details
#' simulationLoader <- getNetTask("SimulationLoader")
#' @export
getNetTask <- function(taskName) {
  rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}

#' @rdname getNetTask
#' @export
getContainerTask <- function() {
  if (is.null(ospsuiteEnv$containerTask)) {
    ospsuiteEnv$containerTask <- getNetTask("ContainerTask")
  }
  ospsuiteEnv$containerTask
}
