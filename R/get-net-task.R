#' @title .getCoreTask
#' @description Get an instance of the specified `.NET` Task in OSPSuite.R.Api
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns a new instance of of the specified `.NET` task.
#'
#' @keywords internal
.getCoreTask <- function(taskName) {
  rSharp::callStatic("OSPSuite.R.Api", paste0("Get", taskName))
}

#' @title .getMoBiTask
#' @description Get an instance of the specified `.NET` Task in MoBi.R
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns a new instance of of the specified `.NET` task.
#'
#' @keywords internal
.getMoBiTask <- function(taskName) {
  rSharp::callStatic("MoBi.R.Api", paste0("Get", taskName))
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

#' @title .getMoBiTaskFromCache
#' @description Get an instance of the specified `.NET` Task from MoBi.R that is retrieved
#' from cache if already initiated. Otherwise a new task will be initiated and
#' cached in the `tasksEnv`.
#'
#' @param taskName The name of the task to retrieve (**without** `Get` prefix).
#'
#' @return returns an instance of of the specified `.NET` task.
#'
#' @keywords internal
.getMoBiTaskFromCache <- function(taskName) {
  cacheName <- paste("mobi", taskName)
  if (is.null(tasksEnv[[cacheName]])) {
    tasksEnv[[cacheName]] <- .getMoBiTask(taskName)
  }
  return(tasksEnv[[cacheName]])
}
