#' @title Wrapper class for `.NET` objects
#' @docType class
#'
#' @examples
#'
#' sim <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
#'
#' # looking at a reference to `.NET` simulation object
#' sim$pointer
#'
#' # create a new instance of `DotNetWrapper` class using this reference
#' DotNetWrapper$new(sim$pointer)
#'
#' @export
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
  inherit = NetObject,
  cloneable = FALSE,
  private = list(
    wrapExtensionMethod = function(typename, methodName, propertyName, value) {
      if (missing(value)) {
        rSharp::callStatic(typename, methodName, self$pointer)
      } else {
        private$.throwPropertyIsReadonly(propertyName)
      }
    },
    wrapExtensionMethodCached = function(typename, methodName, propertyName, cachedValue, value) {
      if (missing(value)) {
        if (is.null(cachedValue)) {
          return(rSharp::callStatic(typename, methodName, self$pointer))
        }
        return(cachedValue)
      } else {
        private$.throwPropertyIsReadonly(propertyName)
      }
    },
    readOnlyProperty = function(propertyName, value, returnValue) {
      if (missing(value)) {
        returnValue
      } else {
        private$.throwPropertyIsReadonly(propertyName)
      }
    },
    # Special method needed because Index are 0-based in `.NET` but 1-based in R
    wrapIndexProperty = function(propertyName, value) {
      if (missing(value)) {
        self$get(propertyName) + 1
      } else {
        self$set(propertyName, value - 1, asInteger = TRUE)
      }
    },

    # There is no concept of a scalar in R. A single value is still a vector of
    # length 1, which can trip up `.NET`, which *does* distinguish between a
    # scalar and an array.
    #
    # This method makes sure that the correct `.NET` method is called depending
    # on whether R vector (for `value`) is of length 1 or higher.
    wrapVectorProperty = function(propertyNameSingular, propertyNamePlural, value, returnPropertyName) {
      if (missing(value)) {
        self$get(returnPropertyName)
      } else {
        if (length(value) == 1L) {
          self$set(propertyNameSingular, value)
        } else {
          self$set(propertyNamePlural, value)
        }
      }
    }
  )
)
