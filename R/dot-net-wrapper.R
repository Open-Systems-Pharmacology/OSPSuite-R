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
  inherit = Printable,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref Instance of the `.NET` object to wrap.
    #' @return A new `DotNetWrapper` object.
    initialize = function(ref) {
      private$.ref <- ref
    }
  ),
  active = list(
    #' @field ref Underlying `.NET` reference (read-only)
    ref = function(value) {
      private$readOnlyProperty("ref", value, private$.ref)
    }
  ),
  private = list(
    .ref = NULL,

    # Simple way to wrap a get;set; .NET property
    wrapProperty = function(propertyName, value, shouldSetNull = TRUE) {
      if (missing(value)) {
        rClr::clrGet(self$ref, propertyName)
      } else {
        # Problem converting reference object to `NULL`
        if (is.null(value) && !shouldSetNull) {
          return()
        }

        if (isOfType(type = "character", object = value)) {
          # isOfType returns TRUE for empty `object` and enc2utf8(value) fails
          if (length(value) > 0) {
            value <- enc2utf8(value)
          }
          rClr::clrSet(self$ref, propertyName, value)
        } else {
          rClr::clrSet(self$ref, propertyName, value)
        }
      }
    },

    # Simple way to wrap a get; .NET Read-Only property
    wrapReadOnlyProperty = function(propertyName, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, propertyName)
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },
    wrapExtensionMethod = function(typename, methodName, propertyName, value) {
      if (missing(value)) {
        rClr::clrCallStatic(typename, methodName, self$ref)
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },
    wrapExtensionMethodCached = function(typename, methodName, propertyName, cachedValue, value) {
      if (missing(value)) {
        if (is.null(cachedValue)) {
          return(rClr::clrCallStatic(typename, methodName, self$ref))
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
        rClr::clrGet(self$ref, propertyName)
      } else {
        rClr::clrSet(self$ref, propertyName, as.integer(value))
      }
    },
    wrapNullableIntegerProperty = function(propertyName, value) {
      if (missing(value)) {
        return(rClr::clrGet(self$ref, propertyName))
      }
      if (is.null(value)) {
        return()
      } else {
        rClr::clrSet(self$ref, propertyName, as.integer(value))
      }
    },

    # Special method needed because Index are 0-based in `.NET` but 1-based in R
    wrapIndexProperty = function(propertyName, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, propertyName) + 1
      } else {
        rClr::clrSet(self$ref, propertyName, as.integer(value - 1))
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
        rClr::clrGet(self$ref, returnPropertyName)
      } else {
        if (length(value) == 1L) {
          rClr::clrSet(self$ref, propertyNameSingular, value)
        } else {
          rClr::clrSet(self$ref, propertyNamePlural, value)
        }
      }
    },
    throwPropertyIsReadonly = function(propertyName) {
      stop(messages$errorPropertyReadOnly(propertyName), call. = FALSE)
    },

    # maybe dispose should be called to if available
    finalize = function() {
      private$.ref <- NULL
    }
  )
)
