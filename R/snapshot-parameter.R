#' @title SnapshotParameter
#' @docType class
#' @description  A parameter typically used in the definition of `IndividualCharacteristics` covariates (Height, Weight etc...)
#'
#' @format NULL
#' @export
SnapshotParameter <- R6::R6Class(
  "SnapshotParameter",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field value Parameter value
    value = function(value) {
      private$wrapProperty("Value", value)
    },
    #' @field unit Unit in which the value is defined
    unit = function(value) {
      private$wrapProperty("Unit", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref Optional .NET reference object. If not defined, a new instance will be created
    #' @param value Optional value of the parameter.
    #' @param unit Optional unit of the value specified.
    #' @return A new `SnapshotParameter` object.
    initialize = function(ref = NULL, value = NULL, unit = NULL) {
      validateIsNumeric(value, nullAllowed = TRUE)
      validateIsString(unit, nullAllowed = TRUE)
      ref <- ref %||% rClr::clrNew("PKSim.Core.Snapshots.Parameter")
      super$initialize(ref)
      # Because of weird issue with nullable value in rClr
      if (!is.null(value)) {
        self$value <- value
      }
      if (!is.null(unit)) {
        self$unit <- unit
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Value", self$value)
      private$printLine("Unit", self$unit)
      invisible(self)
    },
    #' @description
    #' Print the the parameter in one line
    #' @param caption Caption to display before the value of the parameter
    printValue = function(caption) {
      private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
    }
  )
)

createSnapshotParameter <- function(value, unit) {
  if (is.null(value)) {
    return(NULL)
  }
  return(SnapshotParameter$new(value = value, unit = unit))
}
