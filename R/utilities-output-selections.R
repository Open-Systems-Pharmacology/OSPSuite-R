
#' @title  Adds the quantities as output into the  \code{simulation}. The quantities can either be specified using explicit instances or using paths.
#'
#' @param quantitiesOrPaths Quantity instances (element or vector) (typically retrieved using \code{getAllQuantitiesMatching}) or quantity path (element or vector) to add.
#' @param simulation Instance of a simulation for which output selection should be updated.
#'
#' @return A list of quantities added as output (Especially useful when a wildcard was used to verify)
#'
#' @examples
#'
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

  validateIsOfType(quantitiesOrPaths, c(Quantity, "character"))
  validateIsOfType(simulation, Simulation)
  quantities <- quantitiesOrPaths

  if (isOfType(quantitiesOrPaths, "character")) {
    quantities <- getAllQuantitiesMatching(quantitiesOrPaths, simulation)
  }

  quantities <- uniqueEntities(quantities, compareBy = "path")
  outputSelections <- simulation$outputSelections

  for (quantity in quantities) {
    outputSelections$addQuantity(quantity)
  }

  invisible(quantities)
}

#' @title  Removes all selected output from the given \code{simulation}
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
  validateIsOfType(simulation, Simulation)
  simulation$outputSelections$clear()
  invisible(simulation)
}
