#' List of functions and strings used to signal error messages
#' Extends the `messages` list from ospsuite.utils
messages <- ospsuite.utils::messages

messages$errorWrongPopulation <- function(species, population) {
  paste0("Could not find population '", population, "' for species '", species, "'")
}

messages$errorOneOfNameAndPathMustBeSpecified <- function() {
  "One of the `parameterName` or `parameterPath` must be specified, but not both."
}

messages$messageAboutDatasets <- function(messageText, datasetNames) {
  message(messageText, paste0(datasetNames, collapse = "\n"))
}
