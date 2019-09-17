
#' @title SimulationResults
#' @docType class
#' @description  Results of a simulation run (either individual or population simulation)
#'
#' @field count the number of individual results (\code{Count==1} generally means that we are dealing with an individual simulation results).
#' @field simulation Reference to the \code{Simulation} used to calculate or import the results (Read-Only).
#' @field allQuantityPaths List of all paths for which results are defined.
#' @format NULL
SimulationResults <- R6::R6Class("SimulationResults",
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL
  ),
  public = list(
    initialize = function(ref, simulation) {
      validateIsOfType(simulation, "Simulation")
      private$.simulation <- simulation
      super$initialize(ref)
    }
  ),
  active = list(
    count = function(value) {
      private$wrapReadOnlyProperty("Count", value)
    },
    simulation = function(value) {
      private$readOnlyProperty("simulation", value, private$.simulation)
    },
    allQuantityPaths = function(value){
      if (missing(value)) {
        rClr::clrCall(self$ref, "AllQuantityPaths")
      } else {
        private$throwPropertyIsReadonly("allQuantityPaths")
      }
    }
  )
)
