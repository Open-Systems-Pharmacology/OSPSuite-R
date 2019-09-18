#' @title Interval
#' @docType class
#' @description  Simulation Interval (typically associated with an instace of \code{OutputSchema})
#'
#' @field startTime Start time of interval (instance of \code{Parameter})
#' @field endTime End time of interval (instance of \code{Parameter})
#' @field resolution Resolution of interval in pts/min (instance of \code{Parameter})
#' @format NULL
Interval <- R6::R6Class(
  "Interval",
  inherit = Container,
  active = list(
    startTime = function(value) {
      private$readOnlyParameterProperty("StartTime", value)
    },
    endTime = function(value) {
      private$readOnlyParameterProperty("EndTime", value)
    },
    resolution = function(value) {
      private$readOnlyParameterProperty("Resolution", value)
    },
    name = function(value) {
      private$wrapProperty("Name", value)
    }
  ),
  public = list(
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
        toParameters(rClr::clrGet(self$ref, parameterName))
      } else {
        private$throwPropertyIsReadonly(parameterName)
      }
    }
  )
)
