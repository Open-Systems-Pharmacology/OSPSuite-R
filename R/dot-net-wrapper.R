#' @title Wrapper class for `.NET` objects
#' @docType class
#'
#' @examples
#'
#' sim <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
#'
#' # looking at a reference to `.NET` simulation object
#' sim$ref
#'
#' # create a new instance of `DotNetWrapper` class using this reference
#' DotNetWrapper$new(sim$ref)
#'
#' @export
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
  inherit = NetObject,
  cloneable = FALSE,
  private = list(
    # Simple way to wrap a get; .NET Read-Only property
    wrapReadOnlyProperty = function(propertyName, value) {
      if (missing(value)) {
        rSharp::clrGet(self$ref, propertyName)
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },
    wrapExtensionMethod = function(typename, methodName, propertyName, value) {
      if (missing(value)) {
        rSharp::callStatic(typename, methodName, self$ref)
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },
    wrapExtensionMethodCached = function(typename, methodName, propertyName, cachedValue, value) {
      if (missing(value)) {
        if (is.null(cachedValue)) {
          return(rSharp::callStatic(typename, methodName, self$ref))
        }
        return(cachedValue)
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },
    readOnlyProperty = function(propertyName, value, returnValue) {
      if (missing(value)) {
        returnValue
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },
    wrapIntegerProperty = function(propertyName, value) {
      # Special method needed because of double to int conversion issues between R and `.NET`
      if (missing(value)) {
        rSharp::clrGet(self$ref, propertyName)
      } else {
        rSharp::clrSet(self$ref, propertyName, as.integer(value))
      }
    },
    wrapNullableIntegerProperty = function(propertyName, value) {
      if (missing(value)) {
        return(rSharp::clrGet(self$ref, propertyName))
      }
      if (is.null(value)) {
        return()
      } else {
        rSharp::clrSet(self$ref, propertyName, as.integer(value))
      }
    },

    # Special method needed because Index are 0-based in `.NET` but 1-based in R
    wrapIndexProperty = function(propertyName, value) {
      if (missing(value)) {
        rSharp::clrGet(self$ref, propertyName) + 1
      } else {
        rSharp::clrSet(self$ref, propertyName, as.integer(value - 1))
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
        rSharp::clrGet(self$ref, returnPropertyName)
      } else {
        if (length(value) == 1L) {
          rSharp::clrSet(self$ref, propertyNameSingular, value)
        } else {
          rSharp::clrSet(self$ref, propertyNamePlural, value)
        }
      }
    },
    throwPropertyIsReadonly = function(propertyName) {
      stop(messages$errorPropertyReadOnly(propertyName))
    },

    # maybe dispose should be called to if available
    finalize = function() {
      private$.ref <- NULL
    }
  )
)
