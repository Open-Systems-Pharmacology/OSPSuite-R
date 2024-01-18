#' @title  Adds the quantities as output into the  `simulation`. The quantities
#'   can either be specified using explicit instances or using paths.
#'
#' @param quantitiesOrPaths Quantity instances (element or vector) (typically
#'   retrieved using `getAllQuantitiesMatching`) or quantity path (element or
#'   vector) to add.
#' @param simulation Instance of a simulation for which output selection should
#'   be updated.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' paths <- c("Organism|VenousBlood|Plasma|Caffeine", "Organism|ArterialBlood|**|Caffeine")
#' addOutputs(paths, sim)
#'
#' parameter <- getParameter("Organism|Liver|Volume", sim)
#' addOutputs(parameter, sim)
#' @export
addOutputs <- function(quantitiesOrPaths, simulation) {
  quantitiesOrPaths <- c(quantitiesOrPaths)

  validateIsOfType(quantitiesOrPaths, c("Quantity", "character"))
  validateIsOfType(simulation, "Simulation")

  # If quantities are provided, get their paths
  paths <- vector("character", length(quantitiesOrPaths))
  if (isOfType(quantitiesOrPaths, "Quantity")) {
    for (idx in seq_along(quantitiesOrPaths)) {
      paths[[idx]] <- quantitiesOrPaths[[idx]]$path
    }
  } else {
    paths <- quantitiesOrPaths
  }
  paths <- unique(paths)

  task <- .getNetTaskFromCache("ContainerTask")
  for (path in paths) {
    rClr::clrCall(task, "AddQuantitiesToSimulationOutputByPath", simulation$ref, enc2utf8(path))
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

#' @param quantitiesOrPaths Quantity instances (element or vector) (typically
#'   retrieved using `getAllQuantitiesMatching`) or quantity path (element or
#'   vector) to add.
#'
#' @param simulation Instance of a simulation for which output selection should
#'   be updated.
#'
#' @title Set outputs
#' @description
#' Sets the quantities as output into the  `simulation`.
#'  The quantities can either be specified using explicit instances or using paths.
#'  This function clears the output selection before adding the new quantities.
#'  See `addOutputs` for adding quantities without clearing the output selection.
#'  See `clearOutputs` for clearing the output selection without adding new quantities.
#'  @export
setOutputs <- function(quantitiesOrPaths, simulation) {
  clearOutputs(simulation)
  addOutputs(quantitiesOrPaths, simulation)
}
