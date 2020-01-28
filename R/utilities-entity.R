#' How should comparison of entities be performed
#'
#' @include enum.R
#' @export
CompareBy <- enum(c(
  "id",
  "name",
  "path"
))

#' Names of the .NET container tasks of the type "AllXXXMatching"
#'
#' @include enum.R
ContainerTasks <- enum(c(
  Container = "AllContainersMatching",
  Quantity = "AllQuantitiesMatching",
  Parameter = "AllParametersMatching",
  Molecule = "AllMoleculesMatching"
))

#' Extract Unique Elements of type 'Entity'
#'
#' @param entities List of objects of type 'Entity'
#' @param compareBy A string defining the property that is compared by.
#' Can take values 'id', 'name', and 'path'. Default is 'id'.
#'
#' @return List of entities that are unique for the property defined by the
#' argument 'compareBy'
#' @export
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
uniqueEntities <- function(entities, compareBy = CompareBy$id) {
  if (is.null(entities)) {
    return(NULL)
  }

  validateIsOfType(entities, Entity)
  validateEnumValue(CompareBy, compareBy)

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

unify <- function(groupEntitiesByPathFunc, paths) {
  # Every set of entities created by a distinct path string is stored in its own list
  listOfEntitiesByPath <- lapply(paths, groupEntitiesByPathFunc)

  numberOfEntitiesSet <- length(listOfEntitiesByPath)
  listOfEntitiesByPath <- unlist(listOfEntitiesByPath)

  # If the search results in multiple entities lists (== paths is a list of strings),
  # The results have to be checked for duplicates
  if (numberOfEntitiesSet > 1) {
    if (!length(listOfEntitiesByPath) == 0) {
      listOfEntitiesByPath <- uniqueEntities(listOfEntitiesByPath)
    }
  }

  return(listOfEntitiesByPath)
}

#' Retrieve all entities of a container (simulation or container instance) matching the given path criteria.
#'
#' @param paths A vector of strings representing the paths relative to the \code{container}
#' @param container A Container or Simulation used to find the entities
#' @seealso \code{\link{loadSimulation}}, \code{\link{getContainer}} and \code{\link{getAllContainersMatching}} to create objects of type Container or Simulation
#' @param entityType Class of the type that should be returned. Supported types are Container, Quantity, and Parameter
#' @param method Method to call in the underlying .NET class. (optional). If unspecified, the method will be estimated from entity type
#' @return A list of entities matching the path criteria coerced to the \code{entityType}.
#' The list is empty if no entities matching were found.
#'
getAllEntitiesMatching <- function(paths, container, entityType, method = NULL) {
  # Test for correct inputs
  validateIsOfType(container, c(Simulation, Container, Molecule))
  validateIsString(paths)
  validateIsString(method, nullAllowed = TRUE)
  className <- entityType$classname
  if (length(which(names(ContainerTasks) == className)) == 0) {
    stop(messages$errorWrongType("entityType", className, names(ContainerTasks)))
  }

  task <- getContainerTask()
  method <- method %||% ContainerTasks[[className]]

  findEntitiesByPath <- function(path) {
    toObjectType(rClr::clrCall(task, method, container$ref, path), entityType)
  }

  return(unify(findEntitiesByPath, paths))
}

#' Retrieve a single entity by path in the given container
#'
#' @inherit getAllEntitiesMatching
#' @param path A string representing the path relative to the \code{container}
#' @param stopIfNotFound Boolean. If TRUE and no parameter exist for the given path,
#' an error is thrown. Default is TRUE.
#' @param entityType Class of the type that should be returned. Supported types are Container, Quantity, and Parameter
#'
#' @return The \code{Entity} with the given path coerced to the \code{entityType}.
#' If the entity for the path does not exist, an error is thrown in case of
#' \code{stopIfNotFound} is TRUE (default), otherwise \code{NULL}
#'
getEntity <- function(path, container, entityType, stopIfNotFound = TRUE) {
  entities <- getAllEntitiesMatching(path, container, entityType)
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
