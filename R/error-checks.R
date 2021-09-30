isSameLength <- function(...) {
  args <- list(...)
  nrOfLengths <- length(unique(lengths(args)))

  return(nrOfLengths == 1)
}

#' Check if the provided object is of certain type
#'
#' @param object An object or a list of objects
#' @param type String representation or Class of the type that should be checked for
#' @param nullAllowed Boolean flag if `NULL` is accepted for the `object`. If `TRUE`,
#' `NULL` always returns `TRUE`, otherwise `NULL` returns `FALSE`. Default is `FALSE`
#'
#' @return TRUE if the object or all objects inside the list are of the given type.
#' Only the first level of the given list is considered.
isOfType <- function(object, type, nullAllowed = FALSE) {
  if (is.null(object)) {
    return(nullAllowed)
  }

  type <- typeNamesFrom(type)
  inheritType <- function(x) {
    if (is.null(x) && nullAllowed) {
      return(TRUE)
    }
    inherits(x, type)
  }
  if (inheritType(object)) {
    return(TRUE)
  }

  object <- c(object)
  all(sapply(object, inheritType))
}

#' Check if the provided object is of certain type. If not, stop with an error.
#'
#' @param object An object or a list of objects
#' @param type String representation or Class of the type that should be checked for
#' @param nullAllowed Boolean flag if `NULL` is accepted for the `object`. If `TRUE`,
#' `NULL` is always valid, otherwise the error is thrown. Default is `FALSE`
validateIsOfType <- function(object, type, nullAllowed = FALSE) {
  if (type == "integer") {
    return(validateIsInteger(object, nullAllowed = nullAllowed))
  }

  if (isOfType(object, type, nullAllowed)) {
    return()
  }
  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))
  objectTypes <- typeNamesFrom(type)

  callStack <- as.character(sys.call(-1)[[1]])
  # Object name is one frame further for functions such as ValidateIsNumeric
  if ((length(callStack) > 0) && grepl(pattern = "validateIs", x = callStack)) {
    objectName <- deparse(substitute(object, sys.frame(-1)))
  }

  stop(messages$errorWrongType(objectName, class(object)[1], objectTypes))
}

#' Check if `value` is in the given {enum}. If not, stops with an error.
#'
#' @param enum `enum` where the `value` should be contained
#' @param value `value` to search for in the `enum`
#' @param nullAllowed If TRUE, `value` can be `NULL` and the test always passes.
#' If `FALSE` (default), NULL is not accepted and the test fails.
validateEnumValue <- function(value, enum, nullAllowed = FALSE) {
  if (is.null(value)) {
    if (nullAllowed) {
      return()
    }
    stop(messages$errorEnumValueUndefined(enum))
  }

  enumKey <- getEnumKey(enum, value)
  if (any(names(enum) == enumKey)) {
    return()
  }

  stop(messages$errorValueNotInEnum(enum, enumKey))
}

typeNamesFrom <- function(type) {
  type <- c(type)
  sapply(type, function(t) {
    if (is.character(t)) {
      return(t)
    }
    t$classname
  })
}

validateIsString <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "character", nullAllowed)
}

validateIsNumeric <- function(object, nullAllowed = FALSE) {
  # Only NA values. It is numeric
  if (all(is.na(object)) && !any(is.null(object))) {
    return()
  }

  validateIsOfType(object, c("numeric", "integer"), nullAllowed)
}

validateIsInteger <- function(object, nullAllowed = FALSE) {
  if (nullAllowed && is.null(object)) {
    return()
  }

  if (all(floor(object) == object, na.rm = TRUE)) {
    return()
  }

  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))
  objectTypes <- "integer"

  stop(messages$errorWrongType(objectName, class(object)[1], objectTypes))
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

validatePathIsAbsolute <- function(path) {
  wildcardChar <- "*"
  if (any(unlist(strsplit(path, ""), use.names = FALSE) == wildcardChar)) {
    stop(messages$errorEntityPathNotAbsolute(path))
  }
}
