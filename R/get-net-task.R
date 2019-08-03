#' Returns an instace of the specified .NET Task
#'
#' @param taskName The name of the task to retrieve (without the Get)
#'
#' @return An instance of the Task
#'
#' @examples
#'
#' simulationLoader <- getNetTask("SimulationLoader")
 getNetTask <- function(taskName) {
  rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}
