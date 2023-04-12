#' @title .getNewNetTask
#' @description Get an instance of the specified `.NET` Task
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns a new instance of of the specified `.NET` task.
#'
#' @keywords internal
.getNewNetTask <- function(taskName) {
  rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}

#' @title .getNetTask
#' @description Get an instance of the specified `.NET` Task that is retrieved
#' from cache if already initiated. Otherwise a new task will be initiated and
#' cached in the `tasksEnv`.
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns an instance of of the specified `.NET` task.
#'
#' @keywords internal
.getNetTask <- function(taskName) {
  if (is.null(tasksEnv[[taskName]])) {
    tasksEnv[[taskName]] <- .getNewNetTask(taskName)
  }
  return(tasksEnv[[taskName]])
}

#' @title .getDimensionTask
#'
#' @return an instance of the `.NET` Task `DimensionTask`
#' @keywords internal
.getDimensionTask <- function() {
  .getNetTask("DimensionTask")
}

#' @title .getContainerTask
#'
#' @return an instance of the `.NET` Task `ContainerTask`
#' @keywords internal
.getContainerTask <- function() {
  .getNetTask("ContainerTask")
}
