
#' Make Enum
#'
#' @param inputList
#'
#' @return
#' @export
#'
makeEnum <- function(inputList) {
  myEnum <- as.list(inputList)
  enumNames <- names(myEnum)
  if (is.null(enumNames)) {
    names(myEnum) <- myEnum
  } else if ("" %in% enumNames) {
    stop("The inputList has some but not all names assigned. They must be all assigned or none assigned")
  }
  return(myEnum)
}


ContainerType <- makeEnum(c(
  Other = 0,
  Simulation = 1,
  Model = 2,
  Organism = 3,
  Organ = 4,
  Compartment = 5,
  Application = 6,
  Event = 7,
  EventGroup = 8,
  Neighborhood = 9,
  Molecule = 10,
  Reaction = 11,
  Formulation = 12,
  Transport = 13
))
