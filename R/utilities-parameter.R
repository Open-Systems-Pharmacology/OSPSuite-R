
#' Retrieve all parameters of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings representing the paths relative to the \code{container}
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to create objects of type Container or Simulation
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

#' Retrieve a single parameter by path in the given container
#'
#' @inherit getAllParametersMatching
#' @param path A string representing the path relative to the \code{container}
#' @param stopIfNotFound Boolean. If TRUE and no parameter exist for the given path,
#' an error is thrown. Default is TRUE.
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


#' Set values of parameters
#'
#' @param parameters A single or a list of \code{Parameter}
#' @seealso \code{\link{getParameter}} and \code{\link{getAllParametersMatching}} to create objects of type Parameter
#'
#' @param values A numeric value that should be assigned to the parameter or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameters'
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter("Organism|Liver|Volume", sim)
#' setParametersValues(param, 1)
#' params <- getAllParametersMatching("Organism|**|Volume", sim)
#' setParametersValues(params, c(2, 3))
#' @export
setParametersValues <- function(parameters, values) {
  # Must turn the input into a list so we can iterate through even when only
  # one parameter is passed
  parameters <- c(parameters)

  # Test for correct inputs
  validateIsOfType(parameters, Parameter)
  validateIsNumeric(values)
  validateIsSameLength(parameters, values)

  for (i in seq_along(parameters)) {
    param <- parameters[[i]]
    param$value <- values[[i]]
  }
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
  parameters <- c(parameters)

  # Test for correct inputs
  validateIsOfType(parameters, Parameter)
  validateIsNumeric(factor)

  lapply(parameters, function(p) p$value <- p$value * factor)
  invisible()
}
