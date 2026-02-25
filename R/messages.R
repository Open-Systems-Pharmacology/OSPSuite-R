#' List of functions and strings used to signal error messages
#' Extends the `messages` list from ospsuite.utils
messages <- ospsuite.utils::messages

messages$errorWrongPopulation <- function(species, population) {
  paste0(
    "Could not find population '",
    population,
    "' for species '",
    species,
    "'"
  )
}

messages$errorOneOfNameAndPathMustBeSpecified <- function() {
  "One of the `parameterName` or `parameterPath` must be specified, but not both."
}

messages$noDatasetsToGroup <- function() {
  "There are currently no datasets to be grouped. You can add them with `$addDataSets()` and/or `$addSimulationResults()` methods."
}

messages$datasetsToGroupNotFound <- function() {
  "Following datasets were specified to be grouped but not found"
}

messages$unpairableDatasetsRemoved <- function() {
  "Following non-grouped or unpairable datasets have been removed"
}

messages$printMultipleEntries <- function(header, entries) {
  message(paste0(header, ":\n"), paste0(entries, collapse = "\n"))
}

messages$linearScaleWithFoldDistance <- function() {
  "Linear scale is inappropriate when `foldDistance` argument is specified."
}

messages$errorLoadingUnitsForDimension <- function(dimensions) {
  messages$printMultipleEntries(
    "Could not load units for the following dimensions",
    dimensions
  )
}

messages$plottingWithEmptyDataCombined <- function() {
  "No plot can be created because the entered `DataCombined` object does not contain any datasets."
}

messages$residualsCanNotBeComputed <- function() {
  "No residuals can be computed because the entered `DataCombined` object does not contain any observed-simulated datasets that can be paired."
}

messages$logScaleNotAllowed <- function() {
  "The Y-axis for this plot should not be on a log scale, since the residuals are expected to be centered around 0."
}

messages$lloqOnlyScalar <- function() {
  "Only one LLOQ value per `DataSet` is supported! Please provide a scalar value and not a vector."
}

messages$simBatchStartValueNaN <- function(entityPaths) {
  paste0(
    "Start values of the entities with paths '",
    paste(entityPaths, collapse = ", "),
    "' is `NaN`! Cannot add such run values set"
  )
}

messages$plotObservedVsSimulatedWrongFoldDistance <- function(
  parameterName,
  foldDistances
) {
  paste0(
    "Parameter '",
    parameterName,
    "' should be >1! Following values have
         been passed: '",
    paste(foldDistances, collapse = ", "),
    "'."
  )
}

messages$DataFrameNameAlreadyUsed <- function(DataFrameName) {
  warning(paste0(
    "\r\n",
    "The following name(s) already exist in DataCombined:",
    "\r\n",
    "  - ",
    paste(DataFrameName, collapse = "\r\n  - "),
    "\r\n",
    "Existing data will be overwritten."
  ))
}

messages$wrongUnitForQuantity <- function(quantityPath, unit, dimension) {
  paste0(
    "Unit '",
    unit,
    "' is not valid for quantity with path '",
    quantityPath,
    "' and dimension '",
    dimension,
    "'"
  )
}

messages$invalidDataType <- function(name, dataType) {
  paste0(
    "Data type '",
    dataType,
    "' specified for data set '",
    name,
    "' is not valid. Valid data types are: 'simulated' or 'observed'."
  )
}

messages$valueNotPositive <- function(value, propertyName) {
  paste0(
    "The value of `",
    propertyName,
    "` must be > 0, but it is '",
    paste(value, collapse = ", "),
    "'"
  )
}

messages$molWeightErrorMessage <- function(quantityPath) {
  paste0("Unable to retrieve the molecular weight for: ", quantityPath)
}

messages$illegalCharactersInName <- function(name) {
  paste0(
    "The name '",
    name,
    "' contains illegal characters. Illegal characters are: '",
    paste0(.getIllegalCharacters(), collapse = ", "),
    "'."
  )
}

messages$forbiddenSimulationName <- function(name, sim) {
  paste0(
    "The name '",
    name,
    "' is not allowed for this simulation. Forbidden names for this simulation are: '",
    paste0(.getIllegalSimulationNames(sim), collapse = ", "),
    "'."
  )
}

messages$plotNoDataAvailable <- function() {
  "No data for this plot available."
}

messages$plotUnitConsistency <- function() {
  "Units have to be consistent within one datatype."
}


messages$plotMissingColumnPredicted <- function() {
  "No column available for 'predicted'. Please use combinedData format or a data.frame with column 'predicted'."
}


messages$plotTooManyYDimension <- function(yDimensions) {
  paste0(
    "Data contains too many yDimensions: '",
    paste(yDimensions, collapse = "', '"),
    "'. Automatic y-Unit conversion failed."
  )
}

messages$plotWrongColumnsForCustomErrorType <- function(errorTypes) {
  paste0(
    "The error values for custom errorTypes '",
    paste(unique(errorTypes), collapse = "', '"),
    "' must be provided in 'yMin' and 'yMax' columns. ",
    "Only 'ArithmeticStdDev' and 'GeometricStdDev' can use 'yErrorValues'."
  )
}

messages$plotShowLegendPerDatasetHasNoEffect <- function(dataType) {
  paste0(
    "showLegendPerDataset = '",
    dataType,
    "' but no ",
    dataType,
    " data present. ",
    "This setting will have no effect."
  )
}

messages$plotUntypicalAesthetic <- function(aesthetic, dataType) {
  sprintf(
    "aesthetic '%s' is set to mapping for %s data.
      This aesthetic is usually only used for %s data mapping.",
    aesthetic,
    dataType,
    setdiff(c('simulated', 'observed'), dataType)
  )
}

messages$errorParameterValuesCountMismatch <- function(parameterPath, expectedCount, actualCount) {
  paste0(
    "Parameter values for '",
    parameterPath,
    "' does not have the expected number of elements. (Expected ",
    expectedCount,
    " vs Actual ",
    actualCount,
    ")"
  )
}
