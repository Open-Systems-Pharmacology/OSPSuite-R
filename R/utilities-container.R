
#' Retrieve all sub containers of a parent container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings representing the paths relative to the \code{container}
#' @param container A Container or Simulation used to find the containers
#' @seealso \code{\link{loadSimulation}} and \code{\link{getContainer}} to create objects of type Container or Simulation
#'
#' @return A list of containers matching the path criteria. The list is empty if no containers matching were found.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Intracellular` containers defined in all direct containers of the organism
#' containers <- getAllContainersMatching("Organism|*|Intracellular", sim)
#'
#' # Return all `Intracellular` containers defined in all direct containers of the organism
#' # and the container "Interstitial" under 'Organism|Brain'
#' paths <- c("Organism|*|Intracellular", "Organism|Brain|Interstitial")
#' containers <- getAllContainersMatching(paths, sim)
#'
#' # Returns all `Intracellular` containers defined in `Organism` and all its subcontainers
#' containers <- getAllContainersMatching("Organism|**|Intracellular", sim)
#' @export
getAllContainersMatching <- function(paths, container) {
  # Test for correct inputs
  validateIsOfType(container, c(Simulation, Container))
  validateIsString(paths)

  findContainersByPath <- function(path) {
    toContainers(rClr::clrCall(getContainerTask(), "AllContainersMatching", container$ref, path))
  }

  return(unify(findContainersByPath, paths))
}

#' Retrieve a single container by path under the given container
#'
#' @inherit getAllContainersMatching
#' @param path A string representing the path relative to the \code{container}

#' @return The \code{Container} with the given path or null if not found
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getContainer("Organism|Liver", sim)
#' @export
getContainer <- function(path, container) {
  containers <- getAllContainersMatching(path, container)
  if (length(containers) > 1) {
    stop(messages$errorGetEntityMultipleOutputs(path, container))
  }

  if (length(containers) == 0) {
    return(NULL)
  }

  containers[[1]]
}

toContainers <- function(netContainers) {
  toObjectType(netContainers, Container)
}
