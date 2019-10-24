
#' @title SimulationResults
#' @docType class
#' @description  Results of a simulation run (either individual or population simulation)
#'
#' @field count the number of individual results (\code{Count==1} generally means that we are dealing with an individual simulation results).
#' @field simulation Reference to the \code{Simulation} used to calculate or import the results (Read-Only).
#' @field timeValues Vector of simulated time output values
#' @field allQuantityPaths List of all paths for which results are defined.
#' @field allIndividualIds List of Ids of all individuals that have been simulated
#' @format NULL
SimulationResults <- R6::R6Class("SimulationResults",
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    .individualResultsCache = NULL,
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
      validateIsOfType(simulation, Simulation)
      private$.simulation <- simulation
      private$.individualResultsCache <- Cache$new()
      super$initialize(ref)
    },

    hasResultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      rClr::clrCall(self$ref, "HasResultsFor", as.integer(individualId))
    },

    getValuesByPath = function(path, individualIds) {
      validateIsNumeric(individualIds)
      individualIds <- c(individualIds)
      values <- rClr::clrCall(self$ref, "AllValuesFor", path, as.integer(individualIds))
      # TODO Discuss. NaN or NA?
      values[is.nan(values)] <- NA
      return(values)
    },

    resultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      if (!private$.individualResultsCache$hasKey(individualId)) {
        individualResult <- private$getResultsForIndividual(individualId)
        private$.individualResultsCache$set(individualId, individualResult)
      }

      private$.individualResultsCache$get(individualId)
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
    allIndividualIds = function(value) {
      if (missing(value)) {
        rClr::clrCall(self$ref, "AllIndividualIds")
      } else {
        private$throwPropertyIsReadonly("allIndividualIds")
      }
    }
  )
)
