#' @title Interval
#' @docType class
#' @description  Simulation Interval (typically associated with an instance of `OutputSchema`)
#'
#' @format NULL
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
      private$wrapProperty("Name", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Name", self$name)
      self$startTime$printValue()
      self$endTime$printValue()
      self$resolution$printValue()
      invisible(self)
    }
  ),
  private = list(
    readOnlyParameterProperty = function(parameterName, value) {
      if (missing(value)) {
        .toObjectType(rClr::clrGet(self$ref, parameterName), Parameter)
      } else {
        private$throwPropertyIsReadonly(parameterName)
      }
    }
  )
)
