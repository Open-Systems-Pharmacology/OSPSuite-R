#' Convert a path defined as string to a path array
#'
#' @param path A string representation of a path, with path entries separated
#' by '|'
#'
#' @return An array containing one element for each path entry
#' @examples
#'
#' array <- toPathArray("Organism|Organ|Liver")
#' @export
toPathArray <- function(path) {
  validateIsString(path)
  unlist(strsplit(path, paste0("\\", ospsuiteEnv$pathSeparator)), use.names = FALSE)
}

#' Convert a path array to a path as string with entries separated by '|'
#'
#' @param ... Path entries to concatenate into a path string
#'
#' @return A string built using each entry of the pathArray
#' @examples
#' path <- toPathString(c("Organism", "Organ", "Liver"))
#' @export
toPathString <- function(...) {
  pathStrings <- c(...)
  validateIsString(pathStrings)
  paste(pathStrings, collapse = ospsuiteEnv$pathSeparator)
}
