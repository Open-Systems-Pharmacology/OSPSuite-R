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
#' @importFrom R6 R6Class
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
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

    throwPropertyIsReadonly = function(propertyName) {
      stop(messages$errorPropertyReadOnly(propertyName), call. = FALSE)
    },

    printLine = function(entry, value) {
      cat("  ", entry, ": ", value, "\n", sep = "")
      invisible(self)
    },

    printClass = function() {
      cat(class(self)[1], ": \n")
    }
  )
)
