

#' Retrieve all molecules of a container (simulation or container instance) matching the given path criteria
#'
#' @param paths A vector of strings representing the paths relative to the `container`
#' @param container A Container or Simulation used to find the molecules
#' @seealso [loadSimulation()], [getContainer()] and [getAllContainersMatching()] to retrieve objects of type Container or Simulation
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
  .getAllEntitiesMatching(paths, container, Molecule)
}

#' Retrieves the paths of all molecules defined in the container and all its children
#'
#' @param container A Container or Simulation used to find the parameters
#' @seealso [loadSimulation()], [getContainer()] and [getAllContainersMatching()] to retrieve objects of type Container or Simulation
#'
#' @return An array with one entry per molecule defined in the container
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Returns the path of all molecules defined in the simulation
#' moleculePaths <- getAllMoleculePathsIn(sim)
#' @export
getAllMoleculePathsIn <- function(container) {
  .getAllEntityPathsIn(container, Molecule)
}

#' Retrieve a single molecule by path in the given container
#'
#' @inherit getAllMoleculesMatching
#' @param path A string representing the path relative to the `container`
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no molecule exist for the given path,
#' an error is thrown. If `FALSE`, `NULL` is returned.
#'
#' @return The `Molecule` with the given path. If the molecule for the path
#' does not exist, an error is thrown if `stopIfNotFound` is TRUE (default),
#' otherwise `NULL`
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' molecule <- getMolecule("Organism|Liver|A", sim)
#' @export
getMolecule <- function(path, container, stopIfNotFound = TRUE) {
  .getEntity(path, container, Molecule, stopIfNotFound)
}

#' Set molecule start values
#'
#' @param molecules A single or a list of `Molecule`
#' @seealso [getMolecule()] and [getAllMoleculesMatching()] to retrieve objects of type Molecule
#'
#' @param values A numeric value that should be assigned to the molecule start value or a vector
#' of numeric values, if the start value of more than one molecule should be changed. Must have the same
#' length as `molecules`
#' @inheritParams setQuantityValues
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' molecule <- getMolecule("Organism|Liver|A", sim)
#' setMoleculeInitialValues(molecule, 1)
#' molecules <- getAllMoleculesMatching("Organism|**|A", sim)
#' setMoleculeInitialValues(molecules, c(2, 3), units = c("pmol", "mmol"))
#' @export
setMoleculeInitialValues <- function(molecules, values, units = NULL) {
  validateIsOfType(molecules, "Molecule")
  setQuantityValues(molecules, values, units)
}


#' Set molecule start values in the simulation by path
#'
#' @param moleculePaths A single or a list of molecule paths
#' @param values A numeric value that should be assigned to the molecule start value or a vector
#' of numeric values, if the start value of more than one molecule should be changed. Must have the same
#' length as `moleculePaths`
#' @inheritParams setQuantityValuesByPath
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' setMoleculeValuesByPath("Organism|Liver|A", 1, sim)
#'
#' setMoleculeValuesByPath(
#'   c("Organism|Liver|A", "Organism|Liver|B"),
#'   c(2, 3),
#'   sim,
#'   units = c("µmol", "mmol")
#' )
#' @export
setMoleculeValuesByPath <- function(moleculePaths, values, simulation, units = NULL, stopIfNotFound = TRUE) {
  setQuantityValuesByPath(
    quantityPaths = moleculePaths,
    values = values,
    simulation = simulation,
    units = units,
    stopIfNotFound = stopIfNotFound
  )
}

#' Set molecule scale divisors
#'
#' @param molecules A single or a list of `Molecule`
#' @seealso [getMolecule()] and [getAllMoleculesMatching()] to retrieve objects of type Molecule
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
  molecules <- toList(molecules)
  validateIsOfType(molecules, "Molecule")
  validateIsNumeric(values)
  validateIsSameLength(molecules, values)

  for (i in seq_along(molecules)) {
    molecule <- molecules[[i]]
    molecule$scaleDivisor <- values[[i]]
  }
}
