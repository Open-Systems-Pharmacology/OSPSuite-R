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

messages$printMultipleEntries <- function(header, entries) {
  message(paste0(header, ":\n"), paste0(entries, collapse = "\n"))
}

messages$errorLoadingUnitsForDimension <- function(dimension, error) {
  paste0("Could not load units for dimension'", dimension, "'. Error is '", error, "'")
}
