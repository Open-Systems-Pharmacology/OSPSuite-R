#' Create a simulation configuration from modules.
#'
#' This function calls the `SimulationConfiguration` constructor.
#'
#' @param modules A list of `MoBiModule` objects from which to create the simulation.
#' The order of modules defines the order in which the modules will be combined to a simulation!
#' @param individual Optional, an individual building block
#' @param expressionProfiles Optional, a list of expression profiles to apply to the simulation.
#' @param selectedInitialConditions By default, the first Initial Conditions
#' (IC) building block (BB) of each module will be selected. If a module has multiple
#' IC BBs, it is possible to specify which IC BB to apply by providing a named list,
#' where the name should be the name of the module and the value the name of the IC BB.
#' If `NULL`, all modules will use the first IC BB, if available. When providing a list,
#' the value can also be set to `NULL`, which means that no IC BB from the specified module
#' will be selected.
#' @param selectedParameterValues By default, the first Parameter Values
#' (PV) building block (BB) of each module will be selected. If a module has multiple
#' PV BBs, it is possible to specify which PV BB to apply by providing a named list,
#' where the name should be the name of the module and the value the name of the PV BB.
#' If `NULL`, all modules will use the first PV BB, if available. When providing a list,
#' the value can also be set to `NULL`, which means that no PV BB from the specified module
#' will be selected.
#'
#' @param settings Optional, a `SimulationSettings` object defining the simulation settings. If no settings are provided,
#' default settings will be used upon simulation creation.
#'
#' @returns A `SimulationConfiguration` object.
#' @export
createSimulationConfiguration <- function(
  modules,
  individual = NULL,
  expressionProfiles = NULL,
  selectedInitialConditions = NULL,
  selectedParameterValues = NULL,
  settings = NULL
) {
  SimulationConfiguration$new(
    modules,
    individual = individual,
    expressionProfiles = expressionProfiles,
    selectedInitialConditions = selectedInitialConditions,
    selectedParameterValues = selectedParameterValues,
    settings = settings
  )
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

  # TODO: Extract molecule-specific calculation method overrides from netObj
  # and apply them to the SimulationConfiguration via setPartitionCoefficientMethods /
  # setCellularPermeabilityMethods. Requires clarifying the .NET read API.
  # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1872

  return(SimulationConfiguration$new(
    modules,
    individual = individual,
    expressionProfiles = expProfileObjects,
    selectedInitialConditions = selectedICs,
    selectedParameterValues = selectedPVs,
    settings = SimulationSettings$new(simSettings)
  ))
}
