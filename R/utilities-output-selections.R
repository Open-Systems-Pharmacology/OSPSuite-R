#' @title  Adds the quantities as output into the  `simulation`. The quantities
#'   can either be specified using explicit instances or using paths.
#'
#' @param quantitiesOrPaths Quantity instances (element or vector) (typically
#'   retrieved using `getAllQuantitiesMatching`) or quantity path (element or
#'   vector) to add.
#' @param simulation Instance of a simulation for which output selection should
#'   be updated.
#' @inheritParams getQuantity
#'
#' @examples
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' paths <- c("Organism|VenousBlood|Plasma|Aciclovir", "Organism|ArterialBlood|**|Aciclovir")
#' addOutputs(paths, sim)
#'
#' parameter <- getParameter("Organism|Liver|Volume", sim)
#' addOutputs(parameter, sim)
#' @export
addOutputs <- function(quantitiesOrPaths, simulation, stopIfNotFound = TRUE) {
  quantitiesOrPaths <- c(quantitiesOrPaths)

  validateIsOfType(quantitiesOrPaths, c("Quantity", "character"))
  validateIsOfType(simulation, "Simulation")

  paths <- .entitiesToPaths(quantitiesOrPaths)
  # Check if all paths are valid and can be found in the simulation. If not, an error is thrown.
  if (stopIfNotFound) {
    .validateEntitiesExist(paths, simulation, Quantity)
  }

  task <- .getNetTaskFromCache("ContainerTask")
  for (path in paths) {
    task$call(
      "AddQuantitiesToSimulationOutputByPath",
      simulation,
      path
    )
  }
}

#' @title  Removes all selected output from the given `simulation`
#'
#' @param simulation Instance of a simulation for which output selection should be cleared.
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath, )
#'
#' clearOutputs(sim)
#' @export
clearOutputs <- function(simulation) {
  validateIsOfType(simulation, "Simulation")
  simulation$outputSelections$clear()
  invisible(simulation)
}

#' Set outputs of a simulation
#'
#' Sets the quantities as output of the  `simulation`.
#'  The quantities can either be specified using explicit instances or using paths.
#'  This function clears the output selection before adding the new quantities.
#'  See `addOutputs` for adding quantities without clearing the output selection.
#'  See `clearOutputs` for clearing the output selection without adding new quantities.
#' @inheritParams addOutputs
#'
#' @export
setOutputs <- function(quantitiesOrPaths, simulation, stopIfNotFound = TRUE) {
  # Have to validate the inputs before clearing the outputs, otherwise the user might end up with an empty output selection if the validation fails after clearing the outputs
  validateIsOfType(quantitiesOrPaths, c("Quantity", "character"))
  validateIsOfType(simulation, "Simulation")
  paths <- .entitiesToPaths(quantitiesOrPaths)
  # Check if all paths are valid and can be found in the simulation. If not, an error is thrown.
  if (stopIfNotFound) {
    .validateEntitiesExist(paths, simulation, Quantity)
  }

  clearOutputs(simulation)
  addOutputs(quantitiesOrPaths, simulation, stopIfNotFound)
}
