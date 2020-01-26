#' @title SimulationRunOptions
#' @docType class
#' @description  Options to be passed to the simulation engine
#' @export
#' @format NULL
SimulationRunOptions <- R6::R6Class(
  "SimulationRunOptions",
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param numberOfCoresToUse Number of cores to use for the simulation. Default value is `ospsuiteEnv$numberOfCoresToUse`
    #' @param checkForNegativeValues Should the solver check for negative values. Default is \code{TRUE}
    #' @param showProgress Should a progress information be displayed. Default value is `ospsuiteEnv$showProgress`
    #' @return A new `SimulationRunOptions` object.
    initialize = function(numberOfCoresToUse = ospsuiteEnv$numberOfCoresToUse,
                              checkForNegativeValues = TRUE,
                              showProgress = ospsuiteEnv$showProgress) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SimulationRunOptions")
      super$initialize(ref)
      self$numberOfCoresToUse <- numberOfCoresToUse
      self$checkForNegativeValues <- checkForNegativeValues
      self$showProgress <- showProgress
    }
  ),
  active = list(
    #' @field numberOfCoresToUse (Maximal) number of cores to be used. This is only relevant when simulating a population simulation.
    #' Default is \code{ospsuiteEnv$numberOfCoresToUse}.
    numberOfCoresToUse = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    #' @field checkForNegativeValues  Specifies whether negative values check is on or off. Default is \code{TRUE}
    checkForNegativeValues = function(value) {
      private$wrapProperty("CheckForNegativeValues", value)
    },
    #' @field showProgress  Specifies whether progress bar should be shown during simulation run. Default is \code{ospsuiteEnv$showProgress}
    showProgress = function(value) {
      private$wrapProperty("ShowProgress", value)
    }
  )
)
