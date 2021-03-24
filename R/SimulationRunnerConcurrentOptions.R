#' @title SimulationRunnerConcurrentOptions
#' @docType class
#' @description  Options to be passed to the SimulationRunner when running simulations concurrently
#' @export
#' @format NULL
SimulationRunnerConcurrentOptions <- R6::R6Class(
  "SimulationRunnerConcurrentOptions",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulationRunOptions = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `SimulationRunnerConcurrentOptions` object.
    initialize = function() {
      ref <- rClr::clrNew("OSPSuite.R.Services.SimulationRunnerConcurrentOptions")
      super$initialize(ref)
    },

    #' @description
    #' Add a simulation for the concurrent run.
    #' @param simulation Object of type \code{Simulation} to be simulated
    #' @param population Optional object of type \code{Population} if population simulation
    #' is to be performed
    addSimulation = function(simulation, population = NULL){
      validateIsOfType(simulation, "Simulation", nullAllowed = FALSE)
      if (is.null(population)){
        rClr::clrCall(self$ref, "Add", simulation$ref)
      }
      else{
        validateIsOfType(population, "Population", nullAllowed = FALSE)
        rClr::clrCall(self$ref, "Add", simulation$ref, population$ref)
      }
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  ),
  active = list(
    #' @field simulationRunOptions Optional instance of a \code{SimulationRunOptions} used during the simulation run
    #' If not set, default options are used
    simulationRunOptions = function(value) {
      if (missing(value)){
        if (is.null(private$.simulationRunOptions)) {
          netSimulationRunOptions <- private$wrapProperty("simulationRunOptions")
          if (is.null(netSimulationRunOptions)) {
            return(NULL)
          }
          private$.simulationRunOptions <- SimulationRunOptions$new(netSimulationRunOptions)
        }
        return(private$.simulationRunOptions)
      }

      validateIsOfType(value, "SimulationRunOptions", nullAllowed = FALSE)
      private$.simulationRunOptions <- value
      private$wrapProperty("simulationRunOptions", value$ref)
    }
  )
)
