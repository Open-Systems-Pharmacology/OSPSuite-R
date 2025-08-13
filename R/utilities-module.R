#' Load a MoBi module from pkml
#'
#' @param path Path to the pkml file
#'
#' @returns A `MoBiModule` object
#' @export
#'
#' @examples
loadModuleFromPKML <- function(path) {
}

#' Create a ModuleConfiguration .net object
#'
#' @param module A `Module` object representing the module to configure.
#' @param selectedParameterValue a ParameterValuesBuildingBlock object that will be selected in a Model Configuration.
#' If `NULL`, no parameter value will be selected.
#' @param selectedInitialCondition an InitialConditionsBuildingBlock object that will be selected in a Model Configuration.
#' If `NULL`, no initial condition will be selected.
#'
#' @returns A `ModuleConfiguration` object representing the configuration of the module.
#' @internal
#' @noRd
.createModuleConfiguration <- function(module, selectedParameterValue = NULL, selectedInitialCondition = NULL) {
  netTask <- .getNetTaskFromCache("SimulationTask", isMoBiR = TRUE)
  # TODO: cannot pass NULL to a method?
  netModuleConfiguration <- netTask$call("CreateModuleConfiguration", module)
  netModuleConfiguration$set("SelectedParameterValue", selectedParameterValue)
  netModuleConfiguration$set("SelectedInitialCondition", selectedInitialCondition)

  return(netModuleConfiguration)
}
