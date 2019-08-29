#' Check if the provided object is of certain type
#'
#' @param object An object or a list of objects
#' @param type String representation of the type that should be checked for
#'
#' @return TRUE if the object or all objects inside the list are of the given type.
#' Only the first level of the given list is considered.
#'
#'
isOfType <- function(object, type) {
  if (is.null(object)) {
    return(FALSE)
  }

  object <- c(object)
  isSameType <- all(sapply(
    object,
    function(x) inherits(x, type)
  ))

  return(isSameType)
}

isSameLength <- function(...) {
  args <- list(...)
  nrOfLengths <- length(unique(lengths(args)))

  return(nrOfLengths == 1)
}


validateIsOfType <- function(object, type, nullAllowed = FALSE) {
  if(nullAllowed && is.null(object)){
    return()
  }

  if (isOfType(object, type)) {
    return()
  }
  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))

  stop(messages$errorWrongType(objectName))
}

validateIsSameLength <- function(...) {
  if (isSameLength(...)) {
    return()
  }
  # Name of the variable in the calling function
  objectName <- deparse(substitute(list(...)))

  # Name of the arguments
  argnames <- sys.call()
  arguments <- paste(lapply(argnames[-1], as.character), collapse = ", ")

  stop(messages$errorDifferentLength(arguments))
}

validateNotAllNull <- function(...){

}
