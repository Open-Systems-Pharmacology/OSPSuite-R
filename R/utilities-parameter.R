
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
#' # Return all `Volume` parameters define in a all direct containers of the organism
#' params <- getAllParametersMatching(c("Organism", "*", "Volume"), sim)
#'
#' # Returns all `Volume` parameters defined in `Organism` and all its subcontainers
#' params <- getAllParametersMatching(c("Organism", "**", "Volume"), sim)
#' @export
getAllParametersMatching <- function(path, container) {
  #Test if container is a valid R6 object
  if (!inherits(container, c("Simulation", "Container"))){
    stop(paste0("getAllParametersMatching: argument 'container' is not valid!
                Use 'loadSimulation()' or 'getContainer()' to create container objects."))
  }

  #Test if the path is a characted
  if (!is.character(path)){
    stop(paste0("getAllParametersMatching: argument 'path' is not valid! Must be
                a string or a vector of strings."))
  }

  toParameters(rClr::clrCall(getContainerTask(), "AllParametersMatching", container$ref, path))
}

#' Retrieve a single parameter by path in the given container
#'
#' @param path Path of the parameter
#' @param container The container used to find the parameter
#'
#' @return The [Parameter] with the given path or null if not found
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getParameter(c("Organism", "Liver", "Volume"), sim)
#' @export
getParameter <- function(path, container) {
  parameters <- getAllParametersMatching(path, container)
  if (length(parameters) > 1){
    stop(paste0("getParameter: the path ", path, " located under container ", container,
                " leads to more then one parameter! Use 'getAllParametersMatching'
                to get the list of parameters matching the path"))
  }

  if (length(parameters) == 0) {
    return(NULL)
  }

  parameters[[1]]
}

toParameters <- function(netParams) {
  sapply(netParams, function(p)
    Parameter$new(p))
}
