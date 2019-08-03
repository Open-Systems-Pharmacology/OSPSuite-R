
#' Retrieve all sub containers of a parent container (simulation or container instance) matching the given path criteria
#'
#' @param path A vector of string relative to the container
#' @param container A Container or Simulation used to find the sub containers
#'
#' @return A list of containers matching the path criteria
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Intracellular` containers define in a all direct containers of the organism
#' containers <- getAllContainersMatching(c("Organism", "*", "Intracellular"), sim)
#'
#' # Returns all `Intracellular` containers defined in `Organism` and all its subcontainers
#' containers <- getAllContainersMatching(c("Organism", "**", "Intracellular"), sim)
#' @export
getAllContainersMatching <- function(path, container) {
  toContainers(rClr::clrCall(getContainerTask(), "AllContainersMatching", container$ref, path))
}

#' Retrieve a single container by path under the given container
#'
#' @param path Path of the container
#' @param container The parent container used to find the container
#'
#' @return The [Container] with the given path or null if not found
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getContainer(c("Organism", "Liver"), sim)
#' @export
getContainer <- function(path, container) {
  containers <- getAllContainersMatching(path, container)
  stopifnot(length(containers) <= 1)
  if (length(containers) == 0) {
    return(NULL)
  }

  containers[[1]]
}

toContainers <- function(netContainers) {
  sapply(netContainers, function(c)
    Container$new(c))
}
