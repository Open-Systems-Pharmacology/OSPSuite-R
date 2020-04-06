
#' Retrieve all quantities of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings relative to the \code{container}
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
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

#' Retrieves the path of all quantities defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per quantity defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all quantities defined in the simulation
#' quantityPaths <- getAllQuantityPathsIn(sim)
#' @export
getAllQuantityPathsIn <- function(container) {
  getAllEntityPathsIn(container, Quantity)
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


#' Set values of quantity
#'
#' @param quantities A single or a list of \code{Quantity}
#'
#' @param values A numeric value that should be assigned to the quantity or a vector
#' of numeric values, if the value of more than one quantity should be changed. Must have the same
#' length as 'quantities'. Alternatively, the value can be a unique number. In that case, the same value will be set in all parameters
#'
setQuantityValues <- function(quantities, values) {
  # Must turn the input into a list so we can iterate through even when only
  # one parameter is passed
  quantities <- c(quantities)
  values <- c(values)

  # Test for correct inputs
  validateIsOfType(quantities, Quantity)
  validateIsNumeric(values)

  if(length(values) > 1) {
    validateIsSameLength(quantities, values)
  }
  else{
    values <- rep(values, length(quantities))
  }

  for (i in seq_along(quantities)) {
    quantity <- quantities[[i]]
    quantity$value <- values[[i]]
  }
}

#' Scale current values of quantities using a factor
#'
#' @param quantities A single or a list of \code{Quantity}
#'
#' @param factor A numeric value that will be used to scale all quantities
#'
scaleQuantityValues <- function(quantities, factor) {
  quantities <- c(quantities)

  # Test for correct inputs
  validateIsOfType(quantities, Quantity)
  validateIsNumeric(factor)

  lapply(quantities, function(q) q$value <- q$value * factor)
  invisible()
}
