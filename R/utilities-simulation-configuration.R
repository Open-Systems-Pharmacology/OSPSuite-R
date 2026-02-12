#' Create a simulation configuration from modules.
#'
#' 2DO
#'
#' @param modules A list of `Module` objects from which to create in simulation.
#' The order of modules defines the order in which the modules will be combined to a simulation!
#' @param individual Optional, an individual building block
#' @param expressionProfiles Optional, a list of expression profiles to apply to the simulation.
#' @param initialConditions By default, the first Initial Conditions
#' (IC) building block (BB) of each module will be selected. If a module has multiple
#' IC BBs, it is possible to specify which IC BB to apply by providing a named list,
#' where the name should be the name of the module and the value the name of the IC BB.
#' By setting the value to `NULL`, no IC BB from the specified module will be applied.
#' @param parameterValues By default, the first Parameter Values
#' (PV) building block (BB) of each module will be selected. If a module has multiple
#' PV BBs, it is possible to specify which PV BB to apply by providing a named list,
#' where the name should be the name of the module and the value the name of the PV BB.
#' By setting the value to `NULL`, no PV BB from the specified module will be applied.
#'
#' @returns A `SimulationConfiguration` object.
#' @export
#'
#' @examples
createSimulationConfiguration <- function(
  modules,
  individual = NULL,
  expressionProfiles = NULL,
  initialConditions = NULL,
  parameterValues = NULL
) {
  # .NET CODE

  return(configuration)
}

#' Create a SimulationConfiguration from a .NET NetObject
#'
#' @param netObj reference to OSPSuite.Core.Domain.Builder.SimulationConfiguration .NET object
#'
#' @returns A `SimulationConfiguration` R object.
#' @keywords internal
#' @noRd
.createSimulationConfigurationFromNetObject <- function(netObj) {
  # Get module configurations to extract modules
  moduleConfigs <- netObj$get("ModuleConfigurations")
  moduleConfigs <- moduleConfigs$call("ToArray")
  modules <- vector("list", length(moduleConfigs))
  modulesNames <- vector("character", length(moduleConfigs))
  selectedICs <- vector("list", length(moduleConfigs))
  selectedPVs <- vector("list", length(moduleConfigs))

  for (i in seq_along(moduleConfigs)) {
    moduleNetObj <- moduleConfigs[[i]]$get("Module")
    modules[[i]] <- MoBiModule$new(moduleNetObj)
    modulesNames[[i]] <- modules[[i]]$name

    # Selected ICs
    selectedICNetObj <- moduleConfigs[[i]]$get("SelectedInitialConditions")
    if (!is.null(selectedICNetObj)) {
      selectedICs[[i]] <- selectedICNetObj$get("Name")
    }
    # If no IC is selected, continue, as the value is already NULL

    # Selected PVs
    selectedPVNetObj <- moduleConfigs[[i]]$get("SelectedParameterValues")
    if (!is.null(selectedPVNetObj)) {
      selectedPVs[[i]] <- selectedPVNetObj$get("Name")
    }
    # If no PV is selected, continue, as the value is already NULL
  }

  # Get the individual .NET object and create the R object
  individual <- netObj$get("Individual")
  if (!is.null(individual)) {
    individual <- BuildingBlock$new(
      individual,
      type = BuildingBlockTypes$Individual
    )
  }

  names(modules) <- modulesNames
  names(selectedICs) <- modulesNames
  names(selectedPVs) <- modulesNames

  # Get individual expression profiles and create the R objects
  expProfiles <- netObj$get("ExpressionProfiles")
  expProfiles <- expProfiles$call("ToArray")

  if (length(expProfiles) > 0) {
    names <- vector("character", length(expProfiles))
    expProfileObjects <- vector("list", length(expProfiles))
    for (i in seq_along(expProfiles)) {
      expProfileObjects[[i]] <- BuildingBlock$new(
        expProfiles[[i]],
        type = BuildingBlockTypes$`Expression Profile`
      )
      names[i] <- expProfileObjects[[i]]$name
    }
    names(expProfileObjects) <- names
  } else {
    expProfileObjects <- NULL
  }

  simSettings <- netObj$get("SimulationSettings")
  return(SimulationConfiguration$new(
    modules,
    individual = individual,
    expressionProfiles = expProfileObjects,
    selectedInitialConditions = selectedICs,
    selectedParameterValues = selectedPVs,
    settings = SimulationSettings$new(simSettings)
  ))
}
