#' @export
loadPopulation <- function(fileName, simulation) {
  validateIsOfType(simulation, "Simulation")
  validateIsString(fileName)
  populationImporter <- getNetTask("PopulationImporter")
  population <- rClr::clrCall(populationImporter, "ImportPopulation", fileName, simulation$ref)
  Population$new(population)
}
