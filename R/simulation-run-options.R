#' @title SimulationRunOptions
#' @docType class
#' @description  Options to be passed to the simulation engine
#' @field numberOfCoresToUse (Maximal) number of cores to be used (1 per default)
#' @field checkForNegativeValues  Specifies whether negative values check is on or off. Default is \code{TRUE}
#'
#' @format NULL
SimulationRunOptions <- R6::R6Class(
  "SimulationRunOptions",
  inherit = DotNetWrapper,
  public = list(
    initialize = function(numberOfCoresToUse = 1, checkForNegativeValues = TRUE) {
      ref <- rClr::clrNew("OSPSuite.R.SimulationRunOptions")
      super$initialize(ref)
      self$numberOfCoresToUse <-numberOfCoresToUse
      self$checkForNegativeValues <-checkForNegativeValues
    }
  ),
  active = list(
    numberOfCoresToUse = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    checkForNegativeValues = function(value) {
      private$wrapProperty("CheckForNegativeValues", value)
    }
  )
)
