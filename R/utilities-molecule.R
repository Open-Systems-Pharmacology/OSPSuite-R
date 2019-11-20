

#' Retrieve all molecules of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings representing the paths relative to the \code{container}
#' @param container A Container or Simulation used to find the molecules
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return A list of molecules matching the path criteria. The list is empty if no molecules matching were found.
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Return all `Drug` molecules defined in all direct containers of the organism
#' params <- getAllMoleculesMatching("Organism|*|Volume", sim)
#'
#' # Return all `Drug` molecules defined in all direct containers of the organism
#' # and the parameter 'Weight (tissue)' of the container 'Liver'
#' paths <- c("Organism|*|Volume", "Organism|Liver|Weight (tissue)")
#' params <- getAllMoleculesMatching(paths, sim)
#'
#' # Returns all `Drug` molecules defined in `Organism` and all its subcontainers
#' params <- getAllMoleculesMatching("Organism|**|Volume", sim)
#' @export
getAllMoleculesMatching <- function(paths, container) {
  getAllEntitiesMatching(paths, container, Molecule)
}

#' Retrieve a single molecule by path in the given container
#'
#' @inherit getAllMoleculesMatching
#' @param path A string representing the path relative to the \code{container}
#' @param stopIfNotFound Boolean. If TRUE and no molecule exist for the given path,
#' an error is thrown. Default is TRUE.
#'
#' @return The \code{Molecule} with the given path. If the molecule for the path
#' does not exist, an error is thrown if \code{stopIfNotFound} is TRUE (default),
#' otherwise \code{NULL}
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' param <- getMolecule("Organism|Liver|Volume", sim)
#' @export
getMolecule <- function(path, container, stopIfNotFound = TRUE) {
  getEntity(path, container, Molecule, stopIfNotFound)
}

#' Set molecule start values
#'
#' @param molecules A single or a list of \code{Molecule}
#' @seealso \code{\link{getMolecule}} and \code{\link{getAllMoleculesMatching}} to retrieve objects of type Molecule
#'
#' @param values A numeric value that should be assigned to the molecule start value or a vector
#' of numeric values, if the start value of more than one molecule should be changed. Must have the same
#' length as 'molecules'
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' molecule <- getMolecule("Organism|Liver|Intracellular|Drug", sim)
#' setMoleculeStartValues(molecule, 1)
#' molecules <- getAllMoleculesMatching("Organism|Liver|Intracellular|Drug", sim)
#' setMoleculeStartValues(molecules, c(2, 3))
#' @export
setMoleculeStartValues <- function(molecules, values) {
  # Must turn the input into a list so we can iterate through even when only
  # one parameter is passed
  molecules <- c(molecules)

  # Test for correct inputs
  validateIsOfType(molecules, Molecule)
  validateIsNumeric(values)
  validateIsSameLength(molecules, values)

  for (i in seq_along(molecules)) {
    molecule <- molecules[[i]]
    molecule$value <- values[[i]]
  }
}
