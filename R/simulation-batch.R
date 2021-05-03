#' @title SimulationBatch
#' @docType class
#' @description  An optimized simulation with faster loading. The corresponding .NET class is
#' "OSPSuite.R.Services.SettingsForConcurrentRunSimulationBatch"
#' @export
#' @format NULL
SimulationBatch <- R6::R6Class(
  "SimulationBatch",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    .simulationRunner = NULL,
    .runValuesQueue = NULL
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
      #Create the ConcurrentSimulationRunner that will run this SettingsForConcurrentRunSimulationBatch
      private$.simulationRunner <- getNetTask("ConcurrentSimulationRunner")
      rClr::clrCall(private$.simulationRunner, "AddSimulationBatchOption", self$ref)
      private$.runValuesQueu <- list()
    },

    #' @description Set the parameter and initial values in the simulation and run the simulation
    #' @param parameterValues Vector of parameter values or a list of vectors to set in the simulation (default is `NULL`)
    #' @param initialValues Vector of initial values or a list of vectors to set in the simulation  (default is `NULL`)
    #' @param silentMode If \code{TRUE}, no warnings are displayed if a simulation fails.
    #' Default is \code{FALSE}.
    #' @details If multiple parameter and initial values sets are provided, the simulation is solved with each values set and results for each set are returned.
    #'
    #' @return A list of \code{SimulationResults} objects for each set of parmeter/initial values (in the same order as parameter/initial values are provided). If a simulation with a parameter/initial values set fails, the result for this run is \code{NULL}
    run = function(parameterValues = NULL, initialValues = NULL, silentMode = FALSE) {
      validateIsNumeric(parameterValues, nullAllowed = TRUE)
      validateIsNumeric(initialValues, nullAllowed = TRUE)

      #Enforce parameterValues and initialValues as lists to seq along
      if (!is.list(parameterValues)) {
        parameterValues <- list(parameterValues)
      }
      if (!is.list(initialValues)) {
        initialValues <- list(initialValues)
      }
      validateIsSameLength(parameterValues, initialValues)

      # List of ids. Each added SimulationBatchRunValues has its own id
      ids <- vector("character", length(parameterValues))
      # Create an Id <-> simulation map to get the correct simulation for the results.
      simulationsIdMap <- vector("list", length(ids))
      names(simulationsIdMap) <- ids

      for (idx in seq_along(parameterValues)){
        batchRunValues <- SimulationBatchRunValues$new(parameterValues[[idx]], initialValues[[idx]])
        id <- rClr::clrCall(self$ref, "AddSimulationBatchRunValues", batchRunValues$ref)
        ids[[idx]] <- id
        simulationsIdMap[[id]] <- self$simulation
      }
      # Run the batch with the ConcurrentSimulationRunner
      results <- rClr::clrCall(private$.simulationRunner, "RunConcurrently")
      simulationResults <- .getConcurrentSimulationRunnerResults(results = results, ids = ids, simulationsIdMap = simulationsIdMap, silentMode = silentMode)
      #No use of IDs in this case
      names(simulationResults) <- NULL

      return(simulationResults)
    },

#' @description Schedule a set of parameter and start values for concurrent execution.
#' @details Intended for the use with \code{runSimulationBatchesConcurrently}. The simulation batch is executed
#' with the sets of parameter and initial values that have been scheduled. Has no impact on the \code{run} method.
#'
#' @param parameterValues Vector of parameter values to set in the simulation (default is `NULL`)
#' @param initialValues Vector of initial values to set in the simulation  (default is `NULL`)
#'
#' @return Id of the values set that can be used to get the correct result from \code{runSimulationBatchesConcurrently}.
#' @export
#'
#' @examples
#' \dontrun{
#' sim <- loadTestSimulation("simple", loadFromCache = TRUE)
#' parameters <- c("Organism|Liver|Volume", "R1|k1")
#' molecules <- "Organism|Liver|A"
#' simulationBatch <- createSimulationBatch(sim, parametersOrPaths = parameters, moleculesOrPaths = molecules)
#' valueSetId <- simulationBatch$enqueueRunValues(parametervalues = c(1, 2), initialValues = 2)
#' }
    enqueueRunValues = function(parameterValues = NULL, initialValues = NULL){
      validateIsNumeric(parameterValues, nullAllowed = TRUE)
      validateIsNumeric(initialValues, nullAllowed = TRUE)

      batchRunValues <- SimulationBatchRunValues$new(parameterValues, initialValues)
      # Get Id from batchRunValues
      id <- rClr::clrGet(batchRunValues, "Id")
      # Add batchRunValues to the queue
      private$.runValuesQueue <- c(private$.runValuesQueue, batchRunValues)

      return(id)
    },


#' @description Remove all value sets enqueued for concurrent execution
#' @details For details, see \code{runSimulationBatchesConcurrently}.
#' @export
clearRunValuesQueue = function(){
  private$.runValuesQueue <- list()
},

    #' @description
    #' Clears the reference to the wrapped .NET object
    finalize = function() {
      rClr::clrCall(private$.simulationRunner, "Dispose")
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
