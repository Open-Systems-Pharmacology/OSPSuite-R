

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
#' # Return all `A` molecules defined in all direct containers of the organism
#' molecules <- getAllMoleculesMatching("Organism|*|A", sim)
#'
#' # Return all `A` molecules defined in all direct containers of the organism
#' # and the molecule `B`` of the container 'Liver'
#' paths <- c("Organism|*|A", "Organism|Liver|B")
#' molecules <- getAllMoleculesMatching(paths, sim)
#'
#' # Returns all `A` molecules defined in `Organism` and all its subcontainers
#' molecules <- getAllMoleculesMatching("Organism|**|A", sim)
#' @export
getAllMoleculesMatching <- function(paths, container) {
  getAllEntitiesMatching(paths, container, Molecule)
}

#' Retrieves the path of all molecules defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per molecule defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all molecules defined in the simulation
#' moleculePaths <- getAllMoleculePathsIn(sim)
#'
#' @export
getAllMoleculePathsIn <- function(container){
  getAllEntityPathsIn(container, Molecule)
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
#' molecule <- getMolecule("Organism|Liver|A", sim)
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
#' molecule <- getMolecule("Organism|Liver|A", sim)
#' setMoleculeInitialValues(molecule, 1)
#' molecules <- getAllMoleculesMatching("Organism|**|A", sim)
#' setMoleculeInitialValues(molecules, c(2, 3))
#' @export
setMoleculeInitialValues <- function(molecules, values) {
  validateIsOfType(molecules, Molecule)
  setQuantityValues(molecules, values)
}

#' Set molecule scale divisors
#'
#' @param molecules A single or a list of \code{Molecule}
#' @seealso \code{\link{getMolecule}} and \code{\link{getAllMoleculesMatching}} to retrieve objects of type Molecule
#'
#' @param values A numeric value that should be assigned to the molecule scale factor or a vector
#' of numeric values, if the scale factor of more than one molecule should be changed. Must have the same
#' length as `molecules`
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' molecule <- getMolecule("Organism|Liver|A", sim)
#' setMoleculeScaleDivisors(molecule, 0.001)
#' molecules <- getAllMoleculesMatching("Organism|**|A", sim)
#' setMoleculeScaleDivisors(molecules, c(0.002, 0.003))
#' @export
setMoleculeScaleDivisors <- function(molecules, values) {
  molecules <- c(molecules)
  validateIsOfType(molecules, Molecule)
  validateIsNumeric(values)
  validateIsSameLength(molecules, values)

  for (i in seq_along(molecules)) {
    molecule <- molecules[[i]]
    molecule$scaleDivisor <- values[[i]]
  }
}
