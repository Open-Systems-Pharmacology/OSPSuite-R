
#' @title SimulationPKAnalyses
#' @docType class
#' @description  pK-Analyses of a simulation (either individual or population simulation).
#' @field simulation Reference to the \code{Simulation} used to calculate or import the PK-Analyses (Read-Only)
#' @section Methods:
#' \describe{
#'   \item{allPKParametersFor(quantityPath)}{All pK Parameters defined for a given path}
#'   \item{pKParameterFor(quantityPath, pkParameter)}{The pK Parameter defined for the given path and name}
#'   }
#'
SimulationPKAnalyses <- R6::R6Class("SimulationPKAnalyses",
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL
  ),
  public = list(
    initialize = function(ref, simulation) {
      validateIsOfType(simulation, Simulation)
      private$.simulation <- simulation
      super$initialize(ref)
    },
    allPKParametersFor = function(quantityPath) {
      validateIsString(quantityPath)
      toPKParameter(rClr::clrCall(self$ref, "AllPKParametersFor", quantityPath))
    },
    pKParameterFor = function(quantityPath, pkParameter) {
      validateIsString(quantityPath)
      validateIsString(pkParameter)
      toPKParameter(rClr::clrCall(self$ref, "PKParameterFor", quantityPath, pkParameter))
    }
  ),
  active = list(
    simulation = function(value) {
      private$readOnlyProperty("simulation", value, private$.simulation)
    }
  )
)
