
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
#' # Return all `Intracellular` containers defined in all direct containers of the organism
#' containers <- getAllContainersMatching("Organism|*|Intracellular", sim)
#'
#' # Return all `Intracellular` containers defined in all direct containers of the organism
#' # and the container "Interstitial" under 'Organism|Brain'
#' containers <- getAllContainersMatching(c("Organism|*|Intracellular", "Organism|Brain|Interstitial"), sim)
#'
#' # Returns all `Intracellular` containers defined in `Organism` and all its subcontainers
#' containers <- getAllContainersMatching("Organism|**|Intracellular", sim)
#' @export
getAllContainersMatching <- function(path, container) {
  # Test for correct inputs
  validateIsOfType(container, c("Simulation", "Container"))
  validateIsOfType(path, "character")

  # Every set of containers created by a distinct path string is stored in its own list
  containers <- lapply(path, function(singlePath) {
    toContainers(clrCall(
      getContainerTask(), "AllContainersMatching",
      container$ref, singlePath
    ))
  })

  nrOfContainerSets <- length(containers)
  containers <- unlist(containers)

  # If the search results in multiple container lists (== path is a list of strings),
  # The results have to be checked for duplicates
  if (nrOfContainerSets > 1) {
    if (!length(containers) == 0) {
      containers <- uniqueEntity(containers)
    }
  }

  return(containers)
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
  sapply(netContainers, function(c)
    Container$new(c))
}
