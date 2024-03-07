#' @title SimulationBatch
#' @docType class
#' @description  An optimized simulation with faster loading. The corresponding .NET class is
#' "OSPSuite.R.Services.ConcurrentRunSimulationBatch"
#' @export
#' @format NULL
SimulationBatch <- R6::R6Class(
  "SimulationBatch",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    finalize = function() {
      private$.simulation <- NULL
      # SimulationBatch are disposable object and should be disposed
      self$call("Dispose")
      super$finalize()
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject An `rSharp::NetObject` object.
    #' @param simulation Simulation used in the batch run
    #' @return A new `SimulationBatch` object.
    initialize = function(netObject, simulation) {
      validateIsOfType(simulation, "Simulation")
      super$initialize(netObject)
      private$.simulation <- simulation
    },

    #' @description Add a set of parameter and start values for next execution.
    #' @details Intended for the use with `runSimulationBatches`. The simulation batch is executed
    #' with the sets of parameter and initial values that have been scheduled.
    #' The set of run values is cleared after successful run.
    #'
    #' @param parameterValues Vector of parameter values to set in the simulation (default is `NULL`)
    #' @param initialValues Vector of initial values to set in the simulation  (default is `NULL`)
    #'
    #' @return Id of the values set that can be used to get the correct result from `runSimulationBatches`.
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
    #' sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
    #' parameters <- c("Organism|Liver|Volume", "R1|k1")
    #' molecules <- "Organism|Liver|A"
    #' # Create two simulation batches.
    #' simulationBatch1 <- createSimulationBatch(simulation = sim1,
    #' parametersOrPaths = parameters,
    #' moleculesOrPaths = molecules)
    #' simulationBatch2 <- createSimulationBatch(simulation = sim2,
    #' parametersOrPaths = parameters,
    #' moleculesOrPaths = molecules)
    #' #Ids of run values
    #' ids <- c()
    #' ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
    #' ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
    #' ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
    #' ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
    #' res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
    #' }
    addRunValues = function(parameterValues = NULL, initialValues = NULL) {
      validateIsNumeric(parameterValues, nullAllowed = TRUE)
      validateIsNumeric(initialValues, nullAllowed = TRUE)
      # Only one values set is allowed - no lists of values
      if (is.list(parameterValues) || is.list(initialValues)) {
        stop(messages$errorOnlyOneValuesSetAllowed("parameterValues, initialValues"))
      }

      # Check if any of the values is `NA`. If so, throw an error, as such
      # values set will produce empty results
      if (any(is.na(parameterValues))) {
        naIdx <- which(is.na(parameterValues))
        stop(messages$simBatchStartValueNaN(self$getVariableParameters()[naIdx]))
      }
      if (any(is.na(initialValues))) {
        naIdx <- which(is.na(initialValues))
        stop(messages$simBatchStartValueNaN(self$getVariableMolecules()[naIdx]))
      }

      batchRunValues <- SimulationBatchRunValues$new(parameterValues, initialValues)
      self$call("AddSimulationBatchRunValues", batchRunValues)
      return(batchRunValues$id)
    },

    #' @description Returns a list of parameter paths that are variable in this batch.
    #' @details The order of parameters is the same as the order of parameter
    #' values added with `$addRunValues()` method.
    #'
    #' @return List of parameter paths, or `NULL` if no parameter is variable.
    #' @export
    getVariableParameters = function() {
      simulationBatchOptions <- self$get("SimulationBatchOptions")

      simulationBatchOptions$get("VariableParameters") %||%
        simulationBatchOptions$get("VariableParameter")
    },

    #' @description Returns a list of molecules paths that are variable in this batch
    #'
    #' @details The order of molecules is the same as the order of molecule
    #' start values added with `$addRunValues()` method.
    #'
    #' @return List of parameter paths, or `NULL` if no molecule is variable.
    #' @export
    getVariableMolecules = function() {
      simulationBatchOptions <- self$get("SimulationBatchOptions")

      simulationBatchOptions$get("VariableMolecules") %||%
        simulationBatchOptions$get("VariableMolecule")
    },

    #' @description
    #' Print the object to the console
    #' @param ... Additional arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Id", self$id)
      private$printLine("Simulation", self$simulation$name)
      private$printLine("runValuesIds", self$runValuesIds)
      private$printLine(
        "Parameters", self$getVariableParameters()
      )
      private$printLine(
        "Molecules", self$getVariableMolecules()
      )
      invisible(self)
    }
  ),
  active = list(
    #' @field simulation Underlying simulation used for the batch run. Read only.
    simulation = function(value) {
      if (missing(value)) {
        private$.simulation
      } else {
        private$throwPropertyIsReadonly("simulation")
      }
    },
    #' @field runValuesIds Ids of the run values that will be executed on next run
    runValuesIds = function(value) {
      if (missing(value)) {
        self$get("RunValuesIds")
      } else {
        private$throwPropertyIsReadonly("runValuesIds")
      }
    },
    #' @field id The id of the .NET wrapped object. (read-only)
    id = function(value) {
      private$wrapReadOnlyProperty("Id", value)
    }
  ),
)
