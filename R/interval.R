#' @title Interval
#' @docType class
#' @description  Simulation Interval (typically associated with an instance of `OutputSchema`)
#' @format NULL
#' @keywords internal
Interval <- R6::R6Class(
  "Interval",
  cloneable = FALSE,
  inherit = Container,
  active = list(
    #' @field startTime Start time of interval (instance of `Parameter`)
    startTime = function(value) {
      private$readOnlyParameterProperty("StartTime", value)
    },
    #' @field endTime End time of interval (instance of `Parameter`)
    endTime = function(value) {
      private$readOnlyParameterProperty("EndTime", value)
    },
    #' @field resolution Resolution of interval in pts/min (instance of `Parameter`)
    resolution = function(value) {
      private$readOnlyParameterProperty("Resolution", value)
    },
    #' @field name Name of the interval
    name = function(value) {
      private$.wrapProperty("Name", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "Start time" = self$startTime$getPrintValue(),
        "End time" = self$endTime$getPrintValue(),
        "Resolution" = self$resolution$getPrintValue()
      ))
    }
  ),
  private = list(
    readOnlyParameterProperty = function(parameterName, value) {
      if (missing(value)) {
        .toObjectType(self$get(parameterName), Parameter)
      } else {
        private$.throwPropertyIsReadonly(parameterName)
      }
    }
  )
)
