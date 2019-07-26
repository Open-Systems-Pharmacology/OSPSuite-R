
#' Retrieve all parameters of a container (simulation or container instance) matching the given path criteria
#'
#' @param path A vector of string relative to the container
#' @param container A Container or Simulation used to find the parameters
#'
#' @return A list of parameters matching the path criteria
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' #Return all `Volume` parameters define in a all direct container of the organism
#' params <- getParameters(c("Organism", "*", "Volume"), sim)
#'
#' #Returns all `Volume` parameters defined in `Organism` and all its subcontainers
#' params <- getParameters(c("Organism", "**", "Volume"), sim)
#' @export
getParameters <- function(path, container) {
  containerTask <- rClr::clrCallStatic("OSPSuite.R.Api", "GetContainerTask")

  toParameters(rClr::clrCall(containerTask, "AllParametersMatching", container$ref, path))
}

#' Retrieve a single parameter by path in the given container
#'
#' @param path Path of the parameter
#' @param container The container used to find the parameter
#'
#' @return [Parameter] The parameter with the given path or null if not found
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter(c("Organism", "Liver", "Volume"), sim)
#' @export
getParameter <- function(path, container) {
  parameters <- getParameters(path, container)
  stopifnot(length(parameters) <= 1)
  if (length(parameters) == 0) {
    return(NULL)
  }

  parameters[[1]]
}

toParameters <- function(netParams) {
  sapply(netParams, function(p)
    Parameter$new(p))
}
