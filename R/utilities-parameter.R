
#' Retrieve all parameters of a container (simulation or container instance) matching the given path criteria
#'
#' @param path A vector of string relative to the container
#' @param container A Container or Simulation used to find the parameters
#'
#' @return List[Parameter] a list of parameters matching the path criteria
#' @examples
#'\dontrun{
#' getParameters(c("Organism", "*", "Volume"), sim)) will return all `Volume` parameters define in a all direct container of the organism
#' getParameters(c("Organism", "**", "Volume"), sim)) will return all `Volume` parameters defined in `Organism` and all its subcontainers
#' }
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
#'\dontrun{
#' getParameter(c("Organism", "Liver", "Volume"), sim))
#' }
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
