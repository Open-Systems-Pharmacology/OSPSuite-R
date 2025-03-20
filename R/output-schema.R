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
        intervals <- self$get("IntervalsAsArray")
        .toObjectType(intervals, Interval)
      } else {
        private$.throwPropertyIsReadonly("intervals")
      }
    },
    #' @field timePoints All single time points defined in the schema (Read-Only)
    timePoints = function(value) {
      private$.wrapReadOnlyProperty("TimePoints", value)
    },
    #' @field endTime Returns the end time of the simulation in kernel unit (Read-Only)
    endTime = function(value) {
      # Workaround until this is fixed in core: https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1365
      endTime <- private$.wrapReadOnlyProperty("EndTime", value)
      if (length(self$timePoints) > 0) {
        endTime <- max(endTime, max(self$timePoints))
      }
      return(endTime)
    }
  ),
  public = list(
    #' @description
    #' Clears all intervals and time points
    clear = function() {
      self$call("Clear")
      invisible(self)
    },
    #' @description
    #' Adds an interval to the schema
    #' @param interval Interval to add
    addInterval = function(interval) {
      validateIsOfType(interval, "Interval")
      self$call("AddInterval", interval)
      invisible(self)
    },
    #' @description
    #' Removes the interval from the schema
    #' @param interval Interval to remove
    removeInterval = function(interval) {
      validateIsOfType(interval, "Interval")
      self$call("RemoveInterval", interval)
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
        self$call("AddTimePoints", timePoints)
      } else {
        self$call("AddTimePoint", timePoints)
      }
      invisible(self)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::osp_print_class(self)
      if (length(self$timePoints) > 0) {
        ospsuite.utils::osp_print_items(list(
          "Time points" = paste0(self$timePoints, collapse = ", ")
        ))
      }
      ospsuite.utils::osp_print_header("Output intervals", level = 2)
      for (interval in self$intervals) {
        print(interval)
      }

      invisible(self)
    }
  )
)
