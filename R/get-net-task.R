#' @title .getNetTask
#' @description Get an instance of the specified `.NET` Task
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns a new instance of of the specified `.NET` task.
#'
#' @keywords internal
.getNetTask <- function(taskName) {
  rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}

#' @title .getNetTaskFromCache
#' @description Get an instance of the specified `.NET` Task that is retrieved
#' from cache if already initiated. Otherwise a new task will be initiated and
#' cached in the `tasksEnv`.
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns an instance of of the specified `.NET` task.
#'
#' @keywords internal
.getNetTaskFromCache <- function(taskName) {
  if (is.null(tasksEnv[[taskName]])) {
    tasksEnv[[taskName]] <- .getNetTask(taskName)
  }
  return(tasksEnv[[taskName]])
}
