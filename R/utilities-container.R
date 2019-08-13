
#' Retrieve all sub containers of a parent container (simulation or container instance) matching the given path criteria
#'
#' @param path A vector of strings relative to the \code{container}
#' @param container A Container or Simulation used to find the containers
#' @seealso \code{\link{loadSimulation}} and \code{\link{getContainer}} to create objects of type Container or Simulation
#'
#' @return A list of containers matching the path criteria. The list is empty if no containers matching were found.
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
  # Test for correct inputs
  if (!isOfType(container, c("Simulation", "Container"))) {
    stop(errorWrongType(container))
  }
  if (!is.character(path)) {
    stop(errorWrongType(path))
  }

  toContainers(rClr::clrCall(getContainerTask(), "AllContainersMatching", container$ref, path))
}

#' Retrieve a single container by path under the given container
#'
#' @inherit getAllContainersMatching
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
  if (length(containers) > 1) {
    stop(errorGetEntityMultipleOutputs(path, container))
  }

  if (length(containers) == 0) {
    return(NULL)
  }

  containers[[1]]
}

toContainers <- function(netContainers) {
  sapply(netContainers, function(c)
    Container$new(c))
}
