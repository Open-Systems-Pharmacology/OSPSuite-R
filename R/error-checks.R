isSameLength <- function(...) {
  args <- list(...)
  nrOfLengths <- length(unique(lengths(args)))

  return(nrOfLengths == 1)
}

#' Check if the provided object is of certain type
#'
#' @param object An object or a list of objects
#' @param type String  representation or Class of the type that should be checked for
#'
#' @return TRUE if the object or all objects inside the list are of the given type.
#' Only the first level of the given list is considered.
isOfType <- function(object, type) {
  if (is.null(object)) {
    return(FALSE)
  }

  object <- c(object)
  type <- typeNamesFrom(type)

  paste(type)
  all(sapply(object, function(x) inherits(x, type)))
}

validateIsOfType <- function(object, type, nullAllowed = FALSE) {
  if (nullAllowed && is.null(object)) {
    return()
  }

  if (isOfType(object, type)) {
    return()
  }
  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))
  objectTypes <- typeNamesFrom(type);

  stop(messages$errorWrongType(objectName, class(object)[1], objectTypes))
}

typeNamesFrom <- function(type){
  if (is.character(type)) {
    return(type)
  }
  type <- c(type)
  sapply(type, function(t) t$classname)
}

validateIsString <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "character", nullAllowed)
}

validateIsNumeric <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, c("numeric", "integer"), nullAllowed)
}

validateIsLogical <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "logical", nullAllowed)
}


validateHasUnit <- function(quantity, unit) {
  validateIsOfType(quantity, Quantity)
  validateIsString(unit)
  if (quantity$hasUnit(unit)) {
    return()
  }
  stop(messages$errorUnitNotDefined(quantity$name, quantity$dimension, unit))
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
