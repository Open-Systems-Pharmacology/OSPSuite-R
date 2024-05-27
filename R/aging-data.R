#' @title AgingData
#' @docType class
#' @description  Results of a sensitivity analysis run (either individual or population simulation)
#'
#' @format NULL
AgingData <- R6::R6Class("AgingData",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field individualIds Array of individual ids (corresponding to the individualId column of the aging table)
    individualIds = function(value) {
      private$.wrapProperty("IndividualIds", value, asInteger = TRUE)
    },
    #' @field parameterPaths Array of parameter paths (corresponding to the ParameterPath column of the aging table)
    parameterPaths = function(value) {
      private$.wrapProperty("ParameterPaths", value)
    },
    #' @field times Array of time values (corresponding to the Time column of the aging table)
    times = function(value) {
      private$.wrapProperty("Times", value)
    },
    #' @field values Array of parameter values (corresponding to the Value column of the aging table)
    values = function(value) {
      private$.wrapProperty("Values", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `OSPSuite.R.Domain.AgingData` object.
    initialize = function() {
      netObj <- rSharp::newObjectFromName("OSPSuite.R.Domain.AgingData")
      super$initialize(netObj)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      invisible(self)
    }
  )
)
