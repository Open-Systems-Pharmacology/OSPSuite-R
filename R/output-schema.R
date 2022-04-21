#' @title OutputSchema
#' @docType class
#' @description  Output schema associated with a given simulation
#'
#' @format NULL
OutputSchema <- R6::R6Class(
  "OutputSchema",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field intervals All intervals defined in the schema (Read-Only)
    intervals = function(value) {
      if (missing(value)) {
        intervals <- rClr::clrGet(self$ref, "IntervalsAsArray")
        .toObjectType(intervals, Interval)
      } else {
        private$throwPropertyIsReadonly("intervals")
      }
    },
    #' @field timePoints All single time points defined in the schema (Read-Only)
    timePoints = function(value) {
      private$wrapReadOnlyProperty("TimePoints", value)
    },
    #' @field endTime Returns the end time of the simulation in kernel unit (Read-Only)
    endTime = function(value) {
      private$wrapReadOnlyProperty("EndTime", value)
    }
  ),
  public = list(
    #' @description
    #' Clears all intervals and time points
    clear = function() {
      rClr::clrCall(self$ref, "Clear")
      invisible(self)
    },
    #' @description
    #' Adds an interval to the schema
    #' @param interval Interval to add
    addInterval = function(interval) {
      validateIsOfType(interval, "Interval")
      rClr::clrCall(self$ref, "AddInterval", interval$ref)
      invisible(self)
    },
    #' @description
    #' Removes the interval from the schema
    #' @param interval Interval to remove
    removeInterval = function(interval) {
      validateIsOfType(interval, "Interval")
      rClr::clrCall(self$ref, "RemoveInterval", interval$ref)
      invisible(self)
    },

    #' @description
    #' Adds the time points to the schema. Note that time points and intervals exists concurrently.
    #' Use time points only if you need to ensure that specific time are used.
    #' @param timePoints Time points to add to the schema
    addTimePoints = function(timePoints) {
      timePoints <- c(timePoints)
      validateIsNumeric(timePoints)
      if (length(timePoints) > 1) {
        rClr::clrCall(self$ref, "AddTimePoints", timePoints)
      } else {
        rClr::clrCall(self$ref, "AddTimePoint", timePoints)
      }
      invisible(self)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
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
