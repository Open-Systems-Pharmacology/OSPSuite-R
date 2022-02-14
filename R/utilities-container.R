
#' Retrieve all sub containers of a parent container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings representing the paths relative to the `container`
#' @param container A Container or Simulation used to find the containers
#' @seealso [loadSimulation()] and [getContainer()] to create objects of type Container or Simulation
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
  getAllEntitiesMatching(paths, container, Container)
}

#' Retrieves the path of all containers defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso [loadSimulation()], [getContainer()] and [getAllContainersMatching()] to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per container defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all molecules defined in the simulation
#' moleculePaths <- getAllContainerPathsIn(sim)
#' @export
getAllContainerPathsIn <- function(container) {
  getAllEntityPathsIn(container, Container)
}


#' Retrieve a single container by path under the given container
#'
#' @inherit getAllContainersMatching
#' @param path A string representing the path relative to the `container`
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no container exists for the given path,
#' an error is thrown. If `FALSE`, `NULL` is returned.

#' @return The `Container` with the given path. If the container for the path
#' does not exist, an error is thrown if `stopIfNotFound` is TRUE (default),
#' otherwise `NULL`
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' container <- getContainer("Organism|Liver", sim)
#' @export
getContainer <- function(path, container, stopIfNotFound = TRUE) {
  getEntity(path, container, Container, stopIfNotFound)
}
