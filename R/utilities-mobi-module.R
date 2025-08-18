#' Load a MoBi module from pkml
#'
#' @param path Path to the pkml file
#'
#' @returns A `MoBiModule` object
#' @export
#'
#' @examples
loadModuleFromPKML <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("File does not exist: ", path))
  }
  validateIsFileExtension(path, "pkml")

  # .NET task that handles loading of a MoBi module from pkml
  netTask <- .getMoBiTaskFromCache("ModuleTask")

  netObject <- netTask$call("LoadModuleFromPKML", .expandPath(path))
  module <- MoBiModule$new(netObject)

  return(module)
}

#' Create a ModuleConfiguration .net object
#'
#' @param module A `Module` object representing the module to configure.
#' @param selectedParameterValue a ParameterValuesBuildingBlock object that will be selected in a Model Configuration.
#' If `NULL`, no PV BB will be selected.
#' @param selectedInitialCondition an InitialConditionsBuildingBlock object that will be selected in a Model Configuration.
#' If `NULL`, no IC BB will be selected.
#'
#' @returns A `ModuleConfiguration` object representing the configuration of the module.
#' @internal
#' @noRd
.createModuleConfiguration <- function(module, selectedParameterValue = NULL, selectedInitialCondition = NULL) {
  netTask <- .getMoBiTaskFromCache("SimulationTask")
  netModuleConfiguration <- netTask$call("CreateModuleConfiguration", module)

  netModuleConfiguration$set("SelectedParameterValue", selectedParameterValue)
  netModuleConfiguration$set("SelectedInitialCondition", selectedInitialCondition)

  return(netModuleConfiguration)
}
