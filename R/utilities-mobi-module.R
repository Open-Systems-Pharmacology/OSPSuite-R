#' Load a MoBi module from pkml.
#'
#' @param path Path to the pkml file with a module export
#'
#' @returns A `MoBiModule` object
#' @export
#'
#' @examples
#' filePath <- system.file("extdata", "Thyroid.pkml", package = "ospsuite")
#' module <- loadModuleFromPKML(filePath)
loadModuleFromPKML <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("File does not exist: ", path))
  }
  validateIsFileExtension(path, "pkml")
  netObject <- .callModuleTask("LoadModulesFromFile", .expandPath(path))
  if (length(netObject) > 1) {
    stop(
      "The PKML you are trying to load the module from contains more than one module, but the 
    function expects only one module.
    Most probably you are try to load a simulation export."
    )
  }
  # .NET always returns an array, as it also returns multiple modules for a
  # simulation export. In R, this function should only be used for loading single modules.
  module <- MoBiModule$new(netObject[[1]])

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
#' @noRd
.createModuleConfiguration <- function(
  module,
  selectedParameterValue = NULL,
  selectedInitialCondition = NULL
) {
  netTask <- .getMoBiTaskFromCache("SimulationTask")
  netModuleConfiguration <- netTask$call("CreateModuleConfiguration", module)

  netModuleConfiguration$set("SelectedParameterValue", selectedParameterValue)
  netModuleConfiguration$set(
    "SelectedInitialCondition",
    selectedInitialCondition
  )

  return(netModuleConfiguration)
}

#' Call a method of a MoBi.CLI.Core.Services.ModuleTask
#'
#' @param property The name of the property or method to call on the `ModuleTask`.
#' @param ... Additional arguments to pass to the method.
#' @returns The result of the method call.
#' @noRd
.callModuleTask <- function(property, ...) {
  netTask <- .getMoBiTaskFromCache("ModuleTask")
  results <- netTask$call(property, ...)
  return(results)
}
