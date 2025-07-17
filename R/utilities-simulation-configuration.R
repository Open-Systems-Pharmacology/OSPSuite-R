#' Create a simulation configuration from modules.
#'
#' @param modules A list of `Module` objects from which to create in simulation.
#' The order of modules defines the order in which the modules will be combined to a simulation!
#' @param individual Optional, an individual building block
#' @param expressionProfiles Optional, a list of expression profiles to apply to the simulation.
#' @param selectedInitialConditions By default, the first Initial Conditions
#' (IC) building block (BB) of each module will be selected. If a module has multiple
#' IC BBs, it is possible to specify which IC BB to apply by providing a named list,
#' where the name should be the name of the module and the value the name of the IC BB.
#' By setting the value to `NULL`, no IC BB from the specified module will be applied.
#' @param selectedParameterValues By default, the first Parameter Values
#' (PV) building block (BB) of each module will be selected. If a module has multiple
#' PV BBs, it is possible to specify which PV BB to apply by providing a named list,
#' where the name should be the name of the module and the value the name of the PV BB.
#' By setting the value to `NULL`, no PV BB from the specified module will be applied.
#'
#' @returns A `SimulationConfiguration` object.
#' @export
#'
#' @examples
createSimulationConfiguration <- function(modules,
                                          individual = NULL,
                                          expressionProfiles = NULL,
                                          selectedInitialConditions = NULL,
                                          selectedParameterValues = NULL){

  # .NET CODE

  return(configuration)
}
