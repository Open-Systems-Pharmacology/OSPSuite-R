#' @title SimulationSettings
#' @docType class
#' @description  Settings associated with a given simulation
SimulationSettings <- R6::R6Class(
  "SimulationSettings",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field solver Container containing all solver parameters for the simulation (read-only)
    solver = function(value) {
      if (missing(value)) {
        solver <- self$get("Solver")
        SolverSettings$new(solver)
      } else {
        private$.throwPropertyIsReadonly("solver")
      }
    },
    #' @field outputSelections All selected quantities (species, observers, parameters) that will be part of the simulated results
    outputSelections = function(value) {
      if (missing(value)) {
        outputSelections <- self$get("OutputSelections")
        OutputSelections$new(outputSelections)
      } else {
        private$.throwPropertyIsReadonly("outputSelections")
      }
    },
    #' @field outputSchema OutputSchema object containing the output intervals used to generate simulation data
    outputSchema = function(value) {
      if (missing(value)) {
        outputSchema <- self$get("OutputSchema")
        OutputSchema$new(outputSchema)
      } else {
        private$.throwPropertyIsReadonly("outputSchema")
      }
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      invisible(self)
    }
  )
)
