#' @title .getCoreTask
#' @description Get an instance of the specified `.NET` Task in OSPSuite.R.Api
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns a new instance of of the specified `.NET` task.
#'
#' @keywords internal
.getCoreTask <- function(taskName) {
  rSharp::callStatic("OSPSuite.R.Api, OSPSuite.R", paste0("Get", taskName))
}

#' @title .getCoreTaskFromCache
#' @description Get an instance of the specified `.NET` Task from OSPSuite.R.Api that is retrieved
#' from cache if already initiated. Otherwise a new task will be initiated and
#' cached in the `tasksEnv`.
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns an instance of of the specified `.NET` task.
#'
#' @keywords internal
.getCoreTaskFromCache <- function(taskName) {
  cacheName <- paste("core", taskName)
  if (is.null(tasksEnv[[cacheName]])) {
    tasksEnv[[cacheName]] <- .getCoreTask(taskName)
  }
  return(tasksEnv[[cacheName]])
}
