errorWrongType <- function(object, optionalMessage = NULL) {
  # Name of the calling function
  callingFunctions <- sys.calls()
  callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))

  message <- paste0(
    callingFunction, ": argument '", objectName,
    "' is of wrong type! Type in '?", callingFunction,
    "' for information on required types", optionalMessage
  )

  return(message)
}

#' Error message when single output is expected but multiple eis produced.
#'
#' @param path A vector of strings relative to the \code{container}
#' @param container A Container or Simulation used to find the entities
#' @param optionalMessage A string that will be appended to the end of the message
#'
#' @return
errorGetEntityMultipleOutputs <- function(path, container, optionalMessage = NULL) {
  # Name of the calling function
  callingFunction <- sys.call(-2)[[1]]



  message <- paste0(
    callingFunction, ": the path '", toString(path), "' located under container '",
    getContainerPath(path),
    "' leads to more than one entity! Use 'getAllXXXMatching'",
    "to get the list of all entities matching the path, where XXX stands for the entity type", optionalMessage
  )

  return(message)
}

errorDifferentLength <- function(..., optionalMessage = NULL) {
  # Name of the calling function
  callingFunctions <- sys.calls()
  callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

  # Name of the arguments
  argnames <- sys.call()
  arguments <- paste(lapply(argnames[-1], as.character), collapse = ", ")

  message <- paste0(callingFunction, ": Arguments '", arguments,
                    "' must have the same length, but they don't!", optionalMessage)

  return(message)
}

getContainerPath <- function(container) {
  # Need to check if container is of type Container and Simulation
  if (isOfType(container, "Simulation")) {
    return(container$root$path)
  }
  return(container$path)
}
