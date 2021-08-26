messages <- list(
  errorWrongType = function(objectName, type, expectedType, optionalMessage = NULL) {
    callingFunction <- .getCallingFunctionName()
    expectedTypeMsg <- paste0(expectedType, collapse = ", or ")
    paste0(
      callingFunction, ": argument '", objectName,
      "' is of type '", type, "', but expected '", expectedTypeMsg, "'!", optionalMessage
    )
  },
  errorGetEntityMultipleOutputs = function(path, container, optionalMessage = NULL) {
    callingFunction <- .getCallingFunctionName()
    paste0(
      callingFunction, ": the path '", toString(path), "' located under container '",
      container$path,
      "' leads to more than one entity! Use 'getAllXXXMatching'",
      "to get the list of all entities matching the path, where XXX stands for the entity type", optionalMessage
    )
  },
  errorEntityNotFound = function(path, container, optionalMessage = NULL) {
    callingFunction <- .getCallingFunctionName()
    paste0(
      callingFunction, ": No entity exists for path '", toString(path), "' located under container '",
      container$path,
      "'!", optionalMessage
    )
  },
  errorResultNotFound = function(path, individualId, optionalMessage = NULL) {
    callingFunction <- .getCallingFunctionName()
    paste0(
      callingFunction, ": No results exists for path '", toString(path), "' for individual IDs ",
      "'", individualId, "'!", optionalMessage
    )
  },
  errorDifferentLength = function(objectNames, optionalMessage = NULL) {
    callingFunction <- .getCallingFunctionName()
    paste0(
      callingFunction, ": Arguments '", objectNames,
      "' must have the same length, but they don't!", optionalMessage
    )
  },
  errorPropertyReadOnly = function(propertyName, optionalMessage = NULL) {
    paste0("Property '$", propertyName, "' is readonly")
  },
  errorCannotSetRHSFormula = "Creating a RHS Formula is not supported at the moment. This should be done in MoBi.",
  errorEnumNotAllNames = "The enumValues has some but not all names assigned. They must be all assigned or none assigned",
  errorValueNotInEnum = function(enum, value) {
    paste0("Value '", value, "' is not in defined enumeration values: '", paste0(names(enum), collapse = ", "), "'.")
  },
  errorEnumValueUndefined = function(enum) {
    paste0("Provided value is not in defined enumeration values: '", paste0(names(enum), collapse = ", "), "'.")
  },
  errorKeyInEnumPresent = function(key, optionalMessage = NULL) {
    paste0("enum already contains the key '", key, "'! Use 'overwrite = TRUE' to overwrite the value. ", optionalMessage)
  },
  errorKeyNotInEnum = function(key) {
    paste0("No value with the key '", key, "' is present in the enum!")
  },
  errorUnitNotDefined = function(quantityName, dimension, unit) {
    paste0("Unit '", unit, "' is not defined in dimension '", dimension, "' used by '", quantityName, "'.")
  },
  errorDimensionNotSupported = function(dimension, optionalMessage = NULL) {
    paste0("Dimension '", dimension, "' is not supported! See enum Dimensions for the list of supported dimensions.")
  },
  errorUnitNotSupported = function(unit, dimension, optionalMessage = NULL) {
    paste0("Unit '", unit, "' is not supported by the dimension '", dimension, "'!")
  },
  errorPKParameterNotFound = function(pkParameterName, allPKParameterNames) {
    paste0("PK-Parameter '", pkParameterName, "' not found.\nAvailable PK-Parameters are:\n", paste0(allPKParameterNames, collapse = ", "))
  },
  errorEntityPathNotAbsolute = function(path) {
    callingFunction <- .getCallingFunctionName()
    paste0(
      callingFunction, ": Only absolute paths (i.e. without the wildcard(s) `*`) are allowed, but the given path is: ",
      path
    )
  },
  pkSimRPathInvalid = function(pksimPath) {
    paste0("Path to PKSim.R.dll '", pksimPath, "' is invalid.")
  },
  pkSimInstallPathNotFound = "Could not find an installation of PK-Sim on the machine. Please install the OSPSuite or use 'initPKSim()' to specify the installation path",
  errorOSPSuiteSettingNotFound = function(settingName) {
    paste0("No global setting with the name '", settingName, "' exists. Available global settings are:\n", paste0(names(ospsuiteEnv), collapse = ", "))
  },
  errorSimulationBatchNothingToVary = "You need to vary at least one parameter or one molecule in order to use the SimulationBatch",
  errorOnlyOneValuesSetAllowed = function(argumentName) {
    callingFunction <- .getCallingFunctionName()
    paste0(
      callingFunction, ": argument '", argumentName,
      "' is a list with multiple values sets, but only one value set is allowed!"
    )
  },
  errorMultipleMetaDataEntries = function(optionalMessage = NULL) {
    paste("Can only add a single meta data entry at once", optionalMessage)
  },
  errorMultipleSimulationsCannotBeUsedWithPopulation = "Multiple simulations cannot be run concurrently with a population."
)

formatNumerics <- function(numerics, digits = ospsuiteEnv$formatNumericsDigits,
                           nsmall = ospsuiteEnv$formatNumericsSmall) {
  format(numerics, digits = digits, nsmall = nsmall)
}

.getCallingFunctionName <- function() {
  callingFunctions <- sys.calls()
  callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]
  return(deparse(callingFunction))
}
