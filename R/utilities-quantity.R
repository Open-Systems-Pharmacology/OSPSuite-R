
#' Retrieve all quantities of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings relative to the \code{container}
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to create objects of type Container or Simulation
#'
#' @return A list of quantities matching the path criteria. The list is empty if no quantity matching were found.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Volume` quantities defined in all direct containers of the organism
#' quantities <- getAllQuantitiesMatching("Organism|*|Volume", sim)
#'
#' # Return all `Volume` quantities defined in all direct containers of the organism
#' # and the parameter 'Weight (tissue)' of the container 'Liver'
#' paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
#' quantities <- getAllQuantitiesMatching(paths, sim)
#'
#' # Returns all `Volume` quantities defined in `Organism` and all its subcontainers
#' quantities <- getAllQuantitiesMatching("Organism|**|Volume", sim)
#' @export
getAllQuantitiesMatching <- function(paths, container) {
  getAllEntitiesMatching(paths, container, Quantity)
}

#' Retrieve a single quantty by path in the given container
#'
#' @inherit getAllQuantitiesMatching
#' @param path A string representing the path relative to the \code{container}
#' @param stopIfNotFound Boolean. If TRUE and no quantity exist for the given path,
#' an error is thrown. Default is TRUE.
#'
#' @return The \code{Quantity} with the given path. If the quantity for the path
#' does not exist, an error is thrown if \code{stopIfNotFound} is TRUE (default),
#' otherwise \code{NULL}
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' quantity <- getQuantity("Organism|Liver|Volume", sim)
#' @export
getQuantity <- function(path, container, stopIfNotFound = TRUE) {
  getEntity(path, container, Quantity, stopIfNotFound)
}
