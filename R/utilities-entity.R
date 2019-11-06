#' How should comparison of entities be performed
#'
#' @include enum.R
#' @export
CompareBy <- enum(c(
  "id",
  "name",
  "path"
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
