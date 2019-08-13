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
  object <- unlist(list(object))

  return(all(
    sapply(
      object,
      function(x) inherits(x, type)
    )
  ))
}

isSameLength <- function(...) {
  args <- list(...)
  return(length(
    unique(lengths(args))
  )
  == 1)
}
