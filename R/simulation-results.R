
#' @title SimulationResults
#' @docType class
#' @description  Results of a simulation run (either individual or population simulation)
#'
#' @field count the number of individual results (\code{Count==1} generally means that we are dealing with an individual simulation results).
#' @field simulation Reference to the \code{Simulation} used to calculate or import the results (Read-Only).
#' @field timeValues Vector of simulated time output values
#' @field allQuantityPaths List of all paths for which results are defined.
#' @field allIndividualId List of Ids of all individuals that have been simulated
#' @format NULL
SimulationResults <- R6::R6Class("SimulationResults",
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    getResultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      rClr::clrCall(self$ref, "ResultsFor", as.integer(individualId))
    },
    allIndividualResults = function(value) {
      if (missing(value)) {
        rClr::clrCall(self$ref, "IndividualResultsAsArray")
      } else {
        private$throwPropertyIsReadonly("allIndividualResults")
      }
    }
  ),
  public = list(
    initialize = function(ref, simulation) {
      validateIsOfType(simulation, "Simulation")
      private$.simulation <- simulation
      super$initialize(ref)
    },
    hasResultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      rClr::clrCall(self$ref, "HasResultsFor", as.integer(individualId))
    },
    getValuesForIndividual = function(resultPath, individualId) {
      if (!self$hasResultsForIndividual(individualId)) {
        return(NULL)
      }


      individualResult <- private$getResultsForIndividual(individualId)
      if (!rClr::clrCall(individualResult, "HasValuesFor", resultPath)) {
        return(NULL)
      }
      quantityValues <- rClr::clrCall(individualResult, "ValuesFor", resultPath)
      rClr::clrGet(quantityValues, "Values")
    },
    print = function(...) {
      private$printClass()
      private$printLine("Number of individuals", self$count)
      invisible(self)
    }
  ),
  active = list(
    count = function(value) {
      private$wrapReadOnlyProperty("Count", value)
    },
    simulation = function(value) {
      private$readOnlyProperty("simulation", value, private$.simulation)
    },
    timeValues = function(value) {
      quantityValuesTime <- private$wrapReadOnlyProperty("Time", value)
      if (is.null(quantityValuesTime)) {
        return(NULL)
      }
      rClr::clrGet(quantityValuesTime, "Values")
    },
    allQuantityPaths = function(value) {
      if (missing(value)) {
        rClr::clrCall(self$ref, "AllQuantityPaths")
      } else {
        private$throwPropertyIsReadonly("allQuantityPaths")
      }
    },
    allIndividualId = function(value) {
      if (missing(value)) {
        rClr::clrCall(self$ref, "AllIndividualId")
      } else {
        private$throwPropertyIsReadonly("allIndividualId")
      }
    }
  )
)
