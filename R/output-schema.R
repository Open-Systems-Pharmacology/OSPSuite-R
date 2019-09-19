#' @title OutputSchema
#' @docType class
#' @description  Output schema associated with a given simulation
#'
#' @field intervals All intervals defined in the schema (Read-Only)
#' @field timePoints All single time points defined in the schema (Read-Only)
#' @field endTime Returns the end time of the simulation in kernel unit (Read-Only)
#'
#' @section Methods:
#' \describe{
#'   \item{clear}{Clears all intervals and time points}
#'   \item{addInterval(interval)}{Adds an interval to the schema}
#'   \item{removeInterval(interval)}{Removes the interval from the schema}
#'   \item{addTimePoints(timePoints)}{Adds the time points to the schema}
#'   }
#' @format NULL
OutputSchema <- R6::R6Class(
  "OutputSchema",
  inherit = DotNetWrapper,
  active = list(
    intervals = function(value) {
      if (missing(value)) {
        intervals <- rClr::clrGet(self$ref, "IntervalsAsArray")
        toIntervals(intervals)
      } else {
        private$throwPropertyIsReadonly("solver")
      }
    },
    timePoints = function(value) {
      private$wrapReadOnlyProperty("TimePoints", value)
    },
    endTime = function(value) {
      private$wrapReadOnlyProperty("EndTime", value)
    }
  ),
  public = list(
    clear = function() {
      rClr::clrCall(self$ref, "Clear")
      invisible(self)
    },
    addInterval = function(interval) {
      validateIsOfType(interval, "Interval")
      rClr::clrCall(self$ref, "AddInterval", interval$ref)
      invisible(self)
    },
    removeInterval = function(interval) {
      validateIsOfType(interval, "Interval")
      rClr::clrCall(self$ref, "RemoveInterval", interval$ref)
      invisible(self)
    },
    addTimePoints = function(timePoints) {
      timePoints <- c(timePoints)
      validateIsNumeric(timePoints)
      if (length(timePoints) > 1) {
        rClr::clrCall(self$ref, "AddTimePoints", timePoints)
      }
      else {
        rClr::clrCall(self$ref, "AddTimePoint", timePoints)
      }
      invisible(self)
    },

    print = function(...) {
      private$printClass()
      if (length(self$timePoints) > 0) {
        private$printLine("Time points", paste0(self$timePoints, collapse = ", "))
      }
      for (interval in self$intervals) {
        print(interval)
      }

      invisible(self)
    }
  )
)
