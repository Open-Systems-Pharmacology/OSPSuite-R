
#' Retrieve all parameters of a container (simulation or container instance) matching the given path criteria
#'
#' @param path A vector of strings relative to the \code{container}
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to create objects of type Container or Simulation
#'
#' @return A list of parameters matching the path criteria. The list is empty if no parameters matching were found.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Volume` parameters define in a all direct containers of the organism
#' params <- getAllParametersMatching("Organism|*|Volume", sim)
#'
#' # Returns all `Volume` parameters defined in `Organism` and all its subcontainers
#' params <- getAllParametersMatching("Organism|**|Volume", sim)
#' @export
getAllParametersMatching <- function(path, container) {
  # Test for correct inputs
  validateIsOfType(container, c("Simulation", "Container"))
  validateIsOfType(path, "character")

  toParameters(rClr::clrCall(getContainerTask(), "AllParametersMatching", container$ref, path))
}

#' Retrieve a single parameter by path in the given container
#'
#' @inherit getAllParametersMatching
#'
#' @return The \code{Parameter} with the given path or \code{NULL} if not found
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter("Organism|Liver|Volume", sim)
#' @export
getParameter <- function(path, container) {
  parameters <- getAllParametersMatching(path, container)
  if (length(parameters) > 1) {
    stop(messages$errorGetEntityMultipleOutputs(path, container))
  }

  if (length(parameters) == 0) {
    return(NULL)
  }

  return(parameters[[1]])
}

toParameters <- function(netParams) {
  sapply(netParams, function(p)
    Parameter$new(p))
}

#' Set values of parameters
#'
#' @param parameters A single object of type 'Parameter' or a list of such objects
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
  validateIsOfType(parameters, "Parameter")
  validateIsOfType(values, "numeric")
  validateIsSameLength(parameters, values)

  for (i in seq_along(parameters)) {
    param <- parameters[[i]]
    param$value <- values[[i]]
  }
}
