#' @title SimulationRunOptions
#' @docType class
#' @description  Options to be passed to the simulation engine
#' @field numberOfCoresToUse (Maximal) number of cores to be used. This is only relevant when simulating a population simulation.
#' Default is \code{ospsuiteEnv$numberOfCoresToUse}.
#' @field checkForNegativeValues  Specifies whether negative values check is on or off. Default is \code{TRUE}
#' @field showProgress  Specifies whether progress bar should be shown during simulation run. Default is \code{ospsuiteEnv$showProgress}
#' @export
#' @format NULL
SimulationRunOptions <- R6::R6Class(
  "SimulationRunOptions",
  inherit = DotNetWrapper,
  public = list(
    initialize = function(numberOfCoresToUse = ospsuiteEnv$numberOfCoresToUse,
                              checkForNegativeValues = TRUE,
                              showProgress = ospsuiteEnv$showProgress) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SimulationRunOptions")
      super$initialize(ref)
      self$numberOfCoresToUse <- numberOfCoresToUse
      self$checkForNegativeValues <- checkForNegativeValues
      self$showProgress <- showProgress
    },
    print = function(...) {
      private$printClass()
      private$printLine("Number of cores to use", self$numberOfCoresToUse)
      private$printLine("Negative values allowed", self$checkForNegativeValues)
      private$printLine("Show progress bar", self$showProgress)
      invisible(self)
    }
  ),
  active = list(
    numberOfCoresToUse = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    checkForNegativeValues = function(value) {
      private$wrapProperty("CheckForNegativeValues", value)
    },
    showProgress = function(value) {
      private$wrapProperty("ShowProgress", value)
    }
  )
)
