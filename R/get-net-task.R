#' Get an instance of the specified `.NET` Task
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return
#'
#' - `.getNetTask()` returns an instance of of the specified `.NET` task.
#' - `.getContainerTask()` returns a reference to the container task for
#' optimization purposes only.
#'
#' @examples
#'
#' ospsuite:::.getNetTask("SimulationRunner")
#'
#' ospsuite:::.getContainerTask()
#'
#' @keywords internal
.getNewNetTask <- function(taskName) {
  rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}


#' @title .getContainerTask
#' @description
#'
#' @keywords internal
.getContainerTask <- function() {
  .getNetTask("ContainerTask")
}

#' @title .getNetTask
#'
#' @param taskName
#'
#' @keywords internal
.getNetTask <- function(taskName) {
  if (is.null(ospsuiteEnv[[taskName]])) {
    ospsuiteEnv[[taskName]] <- .getNewNetTask(taskName)
  }
  return(ospsuiteEnv[[taskName]])
}
