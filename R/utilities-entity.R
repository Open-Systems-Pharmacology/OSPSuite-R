#' How should comparison of entities be performed
#'
#' @export
CompareBy <- enum(c(
  "id",
  "name",
  "path"
))

#' Names of the `.NET` container tasks of the type `"AllXXXMatching"`
#'
#' @keywords internal
AllMatchingMethod <- enum(c(
  Container = "AllContainersMatching",
  Quantity = "AllQuantitiesMatching",
  Parameter = "AllParametersMatching",
  Molecule = "AllMoleculesMatching"
))

#' Names of the `.NET` container tasks of the type `"AllXXXPathsIn"`
#'
#' @keywords internal
AllPathsInMethod <- enum(c(
  Container = "AllContainerPathsIn",
  Quantity = "AllQuantityPathsIn",
  Parameter = "AllParameterPathsIn",
  Molecule = "AllMoleculesPathsIn"
))


#' Extract Unique Elements of type 'Entity'
#'
#' @param entities List of objects of type 'Entity'
#' @param compareBy A string defining the property that is compared by.
#' Can take values 'id', 'name', and 'path'. Default is 'id'.
#'
#' @return List of entities that are unique for the property defined by the
#' argument 'compareBy'.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' parameters <- c(
#'   getParameter(toPathString(c("Organism", "Liver", "Volume")), sim),
#'   getParameter(toPathString(c("Organism", "Liver", "Volume")), sim),
#'   getParameter(toPathString(c("Organism", "TableParameter")), sim)
#' )
#'
#' # Return a list containing the two parameters 'Volume' and 'Weight (tissue)'
#' uniqueEntities(parameters, CompareBy$id)
#'
#' @export
uniqueEntities <- function(entities, compareBy = CompareBy$id) {
  if (is.null(entities)) {
    return(NULL)
  }

  entities <- toList(entities)
  validateIsOfType(entities, "Entity")
  validateEnumValue(compareBy, CompareBy)

  uniqueEntities <- new.env(parent = emptyenv())
  for (i in seq_along(entities)) {
    propertyToCompare <- entities[[i]][[compareBy]]
    if (!exists(propertyToCompare, where = uniqueEntities)) {
      uniqueEntities[[propertyToCompare]] <- entities[[i]]
    }
  }
  uniqueEntities <- mget(x = names(uniqueEntities), uniqueEntities)
  uniqueEntities <- unname(uniqueEntities)

  return(uniqueEntities)
}

#' @keywords internal
#' @noRd
.unify <- function(groupEntitiesByPathFunc, paths) {
  # Every set of entities created by a distinct path string is stored in its own list
  listOfEntitiesByPath <- lapply(paths, groupEntitiesByPathFunc)

  numberOfEntitiesSet <- length(listOfEntitiesByPath)
  listOfEntitiesByPath <- unlist(listOfEntitiesByPath, use.names = FALSE)

  # If the search results in multiple entities lists (== paths is a list of strings),
  # The results have to be checked for duplicates
  if (numberOfEntitiesSet > 1) {
    if (!length(listOfEntitiesByPath) == 0) {
      listOfEntitiesByPath <- uniqueEntities(listOfEntitiesByPath)
    }
  }

  return(listOfEntitiesByPath)
}

#' Retrieve all entities of a container (simulation or container instance)
#' matching the given path criteria.
#'
#' @param paths A vector of strings representing the paths relative to the `container`
#' @param container A Container or Simulation used to find the entities
#' @param entityType Class of the type that should be returned.
#' @param method Method to call in the underlying .NET class. (optional). If
#'   unspecified, the method will be estimated from entity type
#'
#' @return A list of entities matching the path criteria coerced to the `entityType`.
#' The list is empty if no entities matching were found.
#'
#' @seealso [loadSimulation()], [getContainer()] and
#'   [getAllContainersMatching()] to create objects of type Container or
#'   Simulation
#'
#' @keywords internal
.getAllEntitiesMatching <- function(paths, container, entityType, method = NULL) {
  # Test for correct inputs
  validateIsOfType(container, c("Simulation", "Container", "Molecule"))
  validateIsString(paths)
  validateIsString(method, nullAllowed = TRUE)
  className <- entityType$classname
  if (length(which(names(AllMatchingMethod) == className)) == 0) {
    stop(messages$errorWrongType("entityType", className, names(AllMatchingMethod)))
  }

  task <- .getNetTaskFromCache("ContainerTask")
  method <- method %||% AllMatchingMethod[[className]]

  findEntitiesByPath <- function(path) {
    .toObjectType(task$call(method, container, path), entityType)
  }

  return(.unify(findEntitiesByPath, paths))
}

#' Retrieves all path of entities defined within the container (simulation or
#' container instance)
#'
#' @param container A Container or Simulation used to find the entities
#' @param entityType Type of entity for which the path should be returned.
#' @param method Method to call in the underlying .NET class. (optional). If
#'   unspecified, the method will be estimated from entity type.
#'
#' @seealso [loadSimulation()], [getContainer()] and
#'   [getAllContainersMatching()] to create objects of type Container or
#'   Simulation
#'
#' @return An array of paths (one for each entity found under the container and
#'   its sub containers) The list is empty if no entities matching were found.
#'
#' @keywords internal
.getAllEntityPathsIn <- function(container, entityType, method = NULL) {
  validateIsOfType(container, c("Simulation", "Container", "Molecule"))
  validateIsString(method, nullAllowed = TRUE)
  className <- entityType$classname
  if (length(which(names(AllPathsInMethod) == className)) == 0) {
    stop(messages$errorWrongType("entityType", className, names(AllPathsInMethod)))
  }

  task <- .getNetTaskFromCache("ContainerTask")
  method <- method %||% AllPathsInMethod[[className]]

  task$call(method, container)
}

#' Retrieve a single entity by path in the given container
#'
#' @inherit .getAllEntitiesMatching
#' @param path A string representing the path relative to the `container`
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no entity exists for
#'   the given path, an error is thrown. If `FALSE`, `NULL` is returned.
#' @param entityType Class of the type that should be returned. Supported types
#'   are Container, Quantity, and Parameter.
#'
#' @return The `Entity` with the given path coerced to the `entityType`.
#' If the entity for the path does not exist, an error is thrown in case of
#' `stopIfNotFound` is TRUE (default), otherwise `NULL`
#'
#' @keywords internal
.getEntity <- function(path, container, entityType, stopIfNotFound = TRUE) {
  entities <- .getAllEntitiesMatching(path, container, entityType)
  if (length(entities) > 1) {
    stop(messages$errorGetEntityMultipleOutputs(path, container))
  }

  if (length(entities) == 0) {
    if (stopIfNotFound) {
      stop(messages$errorEntityNotFound(path, container))
    }
    return(NULL)
  }

  return(entities[[1]])
}
