#' @title SimulationRunOptions
#' @docType class
#' @description  Options to be passed to the simulation engine
#' @export
#' @format NULL
SimulationRunOptions <- R6::R6Class(
  "SimulationRunOptions",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param numberOfCores Number of cores to use for the simulation. Default value is \code{getOSPSuiteSetting("numberOfCores")}
    #' @param checkForNegativeValues Should the solver check for negative values. Default is \code{TRUE}
    #' @param showProgress Should a progress information be displayed. Default value is \code{getOSPSuiteSetting("showProgress")}
    #' @return A new `SimulationRunOptions` object.
    initialize = function(numberOfCores = NULL,
                          checkForNegativeValues = NULL,
                          showProgress = NULL) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SimulationRunOptions")
      super$initialize(ref)
      self$numberOfCores <- numberOfCores %||% getOSPSuiteSetting("numberOfCores")
      self$showProgress <- showProgress %||% getOSPSuiteSetting("showProgress")
      self$checkForNegativeValues <- checkForNegativeValues %||% TRUE
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("numberOfCores", self$numberOfCores)
      private$printLine("checkForNegativeValues", self$checkForNegativeValues)
      private$printLine("showProgress", self$showProgress)
      invisible(self)
    }
  ),
  active = list(
    #' @field numberOfCores (Maximal) number of cores to be used. This is only relevant when simulating a population simulation.
    #' Default is \code{getOSPSuiteSetting("numberOfCores")}.
    numberOfCores = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    #' @field checkForNegativeValues  Specifies whether negative values check is on or off. Default is \code{TRUE}
    checkForNegativeValues = function(value) {
      private$wrapProperty("CheckForNegativeValues", value)
    },
    #' @field showProgress  Specifies whether progress bar should be shown during simulation run. Default is \code{getOSPSuiteSetting("showProgress")}
    showProgress = function(value) {
      private$wrapProperty("ShowProgress", value)
    }
  )
)
