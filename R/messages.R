#' List of functions and strings used to signal error messages
#' Extends the `messages` list from ospsuite.utils
messages <- ospsuite.utils::messages

messages$errorWrongPopulation <- function(species, population) {
  paste0("Could not find population '", population, "' for species '", species, "'")
}

messages$errorOneOfNameAndPathMustBeSpecified <- function() {
  "One of the `parameterName` or `parameterPath` must be specified, but not both."
}

messages$noDatasetsPresentInDataCombined <- function() {
  "There are currently no datasets. You can add them with `$addDataSets()` and/or `$addSimulationResults()` methods."
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

messages$valuesNotInterpolated <- function() {
  "Predicted values couldn't be interpolated at following time points"
}

messages$printMultipleEntries <- function(header, entries) {
  message(paste0(header, ":\n"), paste0(entries, collapse = "\n"))
}

messages$linearScaleWithFoldDistance <- function() {
  "Linear scale is inappropriate when `foldDistance` argument is specified."
}

messages$errorLoadingUnitsForDimension <- function(dimensions) {
  messages$printMultipleEntries("Could not load units for the following dimensions", dimensions)
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

messages$plotObservedVsSimulatedWrongFoldDistance <- function(parameterName, foldDistances){
  paste0("Parameter '", parameterName, "' should be >1! Following values have
         been passed: '", paste(foldDistances, collapse = ", "), "'.")
}
