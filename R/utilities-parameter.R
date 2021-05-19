
#' Retrieve all parameters of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings representing the paths relative to the \code{container}
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return A list of parameters matching the path criteria. The list is empty if no parameters matching were found.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Volume` parameters defined in all direct containers of the organism
#' params <- getAllParametersMatching("Organism|*|Volume", sim)
#'
#' # Return all `Volume` parameters defined in all direct containers of the organism
#' # and the parameter 'Weight (tissue)' of the container 'Liver'
#' paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
#' params <- getAllParametersMatching(paths, sim)
#'
#' # Returns all `Volume` parameters defined in `Organism` and all its subcontainers
#' params <- getAllParametersMatching("Organism|**|Volume", sim)
#' @export
getAllParametersMatching <- function(paths, container) {
  getAllEntitiesMatching(paths, container, Parameter)
}

#' Retrieves the path of all parameters defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per parameter defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all quantities defined in the simulation
#' parameterPaths <- getAllParameterPathsIn(sim)
#' @export
getAllParameterPathsIn <- function(container) {
  getAllEntityPathsIn(container, Parameter)
}

#' Retrieve a single parameter by path in the given container
#'
#' @inherit getAllParametersMatching
#' @param path A string representing the path relative to the \code{container}
#' @param stopIfNotFound Boolean. If \code{TRUE} (default) and no parameter exist for the given path,
#' an error is thrown. If \code{FALSE}, \code{NULL} is returned.
#'
#' @return The \code{Parameter} with the given path. If the parameter for the path
#' does not exist, an error is thrown if \code{stopIfNotFound} is TRUE (default),
#' otherwise \code{NULL}
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter("Organism|Liver|Volume", sim)
#' @export
getParameter <- function(path, container, stopIfNotFound = TRUE) {
  getEntity(path, container, Parameter, stopIfNotFound)
}

#' Retrieves the display path of the parameters defined by paths in the simulation
#'
#' @param paths A single string or array of paths path relative to the \code{container}
#' @param simulation A simulation used to find the entities
#'
#' @return a display path for each parameter in paths
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' displayPath <- getParameterDisplayPaths("Organism|Liver|Volume", sim)
#' @export
getParameterDisplayPaths <- function(paths, simulation) {
  getQuantityDisplayPaths(paths, simulation)
}

#' Set values of parameters
#'
#' @param parameters A single or a list of \code{Parameter}
#' @seealso \code{\link{getParameter}} and \code{\link{getAllParametersMatching}} to create objects of type Parameter
#'
#' @param values A numeric value that should be assigned to the parameter or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameters'. Alternatively, the value can be a unique number. In that case, the same value will be set in all parameters
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter("Organism|Liver|Volume", sim)
#' setParameterValues(param, 1)
#' params <- getAllParametersMatching("Organism|**|Volume", sim)
#' setParameterValues(params, c(2, 3))
#' @export
setParameterValues <- function(parameters, values) {
  validateIsOfType(parameters, Parameter)
  setQuantityValues(parameters, values)
}


#' Set the values of parameters in the simulation by path
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameterPaths'
#' @param simulation Simulation uses to retrieve parameter instances from given paths.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' setParameterValuesByPath("Organism|Liver|Volume", 1, sim)
#'
#' setParameterValuesByPath(c("Organism|Liver|Volume", "Organism|Volume"), c(2, 3), sim)
#' @export
setParameterValuesByPath <- function(parameterPaths, values, simulation) {
  setQuantityValuesByPath(
    quantityPaths = parameterPaths, values = values,
    simulation = simulation
  )
}

#' Scale current values of parameters using a factor
#'
#' @param parameters A single or a list of \code{Parameter}
#' @seealso \code{\link{getParameter}} and \code{\link{getAllParametersMatching}} to create objects of type Parameter
#'
#' @param factor A numeric value that will be used to scale all parameters
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter("Organism|Liver|Volume", sim)
#' scaleParameterValues(param, 1)
#' params <- getAllParametersMatching("Organism|**|Volume", sim)
#' scaleParameterValues(params, 1.5)
#' @export
scaleParameterValues <- function(parameters, factor) {
  validateIsOfType(parameters, Parameter)
  scaleQuantityValues(parameters, factor)
}
