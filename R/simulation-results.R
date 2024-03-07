#' @title SimulationResults
#' @docType class
#' @description  Results of a simulation run (either individual or population simulation)
#'
#' @format NULL
SimulationResults <- R6::R6Class(
  "SimulationResults",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    .individualResultsCache = NULL,
    getResultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      self$call("ResultsFor", as.integer(individualId))
    },
    allIndividualResults = function(value) {
      if (missing(value)) {
        self$call("IndividualResultsAsArray")
      } else {
        private$throwPropertyIsReadonly("allIndividualResults")
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject An `rSharp::NetObject` object.
    #' @param simulation Reference to the simulation object used to calculated the results
    #' @return A new `SimulationResults` object.
    initialize = function(netObject, simulation) {
      validateIsOfType(simulation, "Simulation")
      private$.simulation <- simulation
      private$.individualResultsCache <- Cache$new()
      super$initialize(netObject)
    },
    #' @description
    #' Returns `TRUE` if results are available for the individual with id `individualId` otherwise `FALSE`
    #' @param individualId Id of the individual
    hasResultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      self$call("HasResultsFor", as.integer(individualId))
    },
    #' @description
    #' Returns `TRUE` if results are available for the individual with id `individualId` otherwise `FALSE`
    #' @param path Path for which values should be retrieved
    #' @param individualIds One or more individual ids for which values should be returned
    #' @param stopIfNotFound If `TRUE` (default) an error is thrown if no values could be found for the `path`/
    #' If `FALSE`, a list of `NA` values is returned
    getValuesByPath = function(path, individualIds, stopIfNotFound = TRUE) {
      validateIsNumeric(individualIds)
      individualIds <- c(individualIds)
      values <- self$call("AllValuesFor", path, as.integer(individualIds))

      if (all(is.nan(values)) && stopIfNotFound) {
        stop(messages$errorResultNotFound(path, individualIds))
      }

      values[is.nan(values)] <- NA_real_
      return(values)
    },
    #' @description
    #' Returns all available results for the individual with id `individualId`
    #' @param individualId Id for which the results should be returned
    resultsForIndividual = function(individualId) {
      validateIsNumeric(individualId)
      if (!private$.individualResultsCache$hasKey(individualId)) {
        individualResult <- private$getResultsForIndividual(individualId)
        private$.individualResultsCache$set(individualId, individualResult)
      }

      private$.individualResultsCache$get(individualId)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Number of individuals", self$count)
      invisible(self)
    }
  ),
  active = list(
    #' @field count the number of individual results (`Count==1` generally means that we are dealing with an individual simulation results).
    count = function(value) {
      private$wrapReadOnlyProperty("Count", value)
    },
    #' @field simulation Reference to the `Simulation` used to calculate or import the results (Read-Only).
    simulation = function(value) {
      private$.readOnlyProperty("simulation", value, private$.simulation)
    },
    #' @field timeValues Vector of simulated time output values
    timeValues = function(value) {
      quantityValuesTime <- private$wrapReadOnlyProperty("Time", value)
      if (is.null(quantityValuesTime)) {
        return(NULL)
      }
      quantityValuesTime$get("Values")
    },
    #' @field allQuantityPaths List of all paths for which results are defined.
    allQuantityPaths = function(value) {
      if (missing(value)) {
        self$call("AllQuantityPaths")
      } else {
        private$throwPropertyIsReadonly("allQuantityPaths")
      }
    },
    #' @field allIndividualIds List of Ids of all individuals that have been simulated
    allIndividualIds = function(value) {
      if (missing(value)) {
        self$call("AllIndividualIds")
      } else {
        private$throwPropertyIsReadonly("allIndividualIds")
      }
    }
  )
)
