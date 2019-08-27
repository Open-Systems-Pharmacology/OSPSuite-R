messages <- list(
  errorWrongType = function(objectName, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    message <- paste0(
      callingFunction, ": argument '", objectName,
      "' is of wrong type '", typeof(objectName), "'! Type in '?", callingFunction,
      "' for information on required types", optionalMessage
    )

    return(message)
  },

  errorGetEntityMultipleOutputs = function(path, container, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    message <- paste0(
      callingFunction, ": the path '", toString(path), "' located under container '",
      container$path,
      "' leads to more than one entity! Use 'getAllXXXMatching'",
      "to get the list of all entities matching the path, where XXX stands for the entity type", optionalMessage
    )

    return(message)
  },

  errorDifferentLength = function(objectNames, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    message <- paste0(
      callingFunction, ": Arguments '", objectNames,
      "' must have the same length, but they don't!", optionalMessage
    )

    return(message)
  },

  errorPropertyReadOnly = function(propertyName, optionalMessage = NULL) {
    message <- paste0("Property '$", propertyName, "' is readonly")

    return(message)
  },

  errorEnumNotAllNames = "The enumValues has some but not all names assigned. They must be all assigned or none assigned",

  errorUniqueEntitiesWrongCompareBy = "Wrong value for 'compareBy', must be 'id', 'name', or 'path'"
)

formatNumerics <- function(numerics, digits = ospsuiteEnv$formatNumericsDigits,
                           nsmall = ospsuiteEnv$formatNumericsSmall) {
  format(numerics, digits = digits, nsmall = nsmall)
}
