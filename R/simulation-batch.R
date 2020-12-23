#' @title SimulationBatch
#' @docType class
#' @description  Options to be passed to the SimulationBatch
#' @export
#' @format NULL
SimulationBatch <- R6::R6Class(
  "SimulationBatch",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref .NET reference object.
    #' @param simulation Simulation used in the batch run
    #' @return A new `SimulationBatch` object.
    initialize = function(ref, simulation) {
      validateIsOfType(simulation, Simulation)
      super$initialize(ref)
      self$simulation <- simulation
    },

    #' @description Set the parameter and initial values in the simulation and run the simulation
    #' @param parameterValues Vector of parameter values to set in the simulation (default is `NULL`)
    #' @param initialValues Vector of initial values to set in the simulation  (default is `NULL`)
    #' @return A `SimulationResults` object containing the result of the simulation run
    run = function(parameterValues = NULL, initialValues = NULL) {
      validateIsNumeric(parameterValues, nullAllowed = TRUE)
      validateIsNumeric(initialValues, nullAllowed = TRUE)
      batchRunValues <-SimulationBatchRunValues$new(parameterValues, initialValues)

      results <- rClr::clrCall(self$ref, "Run", batchRunValues$ref)
      SimulationResults$new(results, self$simulation)
    },
    #' @description
    #' Clears the reference to the wrapped .NET object
    finalize = function() {
      rClr::clrCall(self$ref, "Dispose")
      private$.simulation <- NULL
      super$finalize()
    }
  ),
  active = list(
    #' @field simulation Underlying simulation used for the batch run
    simulation = function(value) {
      if (missing(value)) {
        private$.simulation
      } else {
        private$.simulation <- value
      }
    }
  ),
)
