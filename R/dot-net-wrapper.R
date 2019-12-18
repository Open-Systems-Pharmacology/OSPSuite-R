#' @title DotNetRwapper
#' @docType class
#' @description  Wrapper class for .net Object
#' @field ref The actual .NET object instanced being wrapped
#'
#' @section Methods:
#' \describe{
#'   \item{wrapProperty}{Simple way to wrap a get;set; .NET property}
#'   \item{wrapReadOnlyProperty}{Simple way to wrap a get; .NET readonly property}
#'   }
#'
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
  inherit = Printable,
  public = list(
    ref = NULL,
    initialize = function(ref) {
      self$ref <- ref
    }
  ),
  private = list(
    wrapProperty = function(propertyName, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, propertyName)
      } else {
        rClr::clrSet(self$ref, propertyName, value)
      }
    },
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

    readOnlyProperty = function(propertyName, value, returnValue) {
      if (missing(value)) {
        returnValue
      } else {
        private$throwPropertyIsReadonly(propertyName)
      }
    },

    wrapIntegerProperty = function(propertyName, value) {
      # Special method needed because of double to int conversion issues between R and .NET
      if (missing(value)) {
        rClr::clrGet(self$ref, propertyName)
      } else {
        rClr::clrSet(self$ref, propertyName, as.integer(value))
      }
    },
    throwPropertyIsReadonly = function(propertyName) {
      stop(messages$errorPropertyReadOnly(propertyName), call. = FALSE)
    }
  )
)
