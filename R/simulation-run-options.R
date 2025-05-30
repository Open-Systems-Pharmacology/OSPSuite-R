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
    #' @param numberOfCores Number of cores to use for the simulation. Default value is `getOSPSuiteSetting("numberOfCores")`
    #' @param checkForNegativeValues Should the solver check for negative values. Default is `TRUE`
    #' @param showProgress Should a progress information be displayed. Default value is `getOSPSuiteSetting("showProgress")`
    #' @return A new `SimulationRunOptions` object.
    initialize = function(numberOfCores = NULL,
                          checkForNegativeValues = NULL,
                          showProgress = NULL) {
      netObject <- rSharp::newObjectFromName("OSPSuite.R.Domain.SimulationRunOptions")
      super$initialize(netObject)
      self$numberOfCores <- numberOfCores %||% getOSPSuiteSetting("numberOfCores")
      self$showProgress <- showProgress %||% getOSPSuiteSetting("showProgress")
      self$checkForNegativeValues <- checkForNegativeValues %||% TRUE
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "numberOfCores" = self$numberOfCores,
        "checkForNegativeValues" = self$checkForNegativeValues,
        "showProgress" = self$showProgress
      ))
    }
  ),
  active = list(
    #' @field numberOfCores (Maximal) number of cores to be used. This is only relevant when simulating a population simulation.
    #' Default is `getOSPSuiteSetting("numberOfCores")`.
    numberOfCores = function(value) {
      private$.wrapProperty("NumberOfCoresToUse", value, asInteger = TRUE)
    },
    #' @field checkForNegativeValues  Specifies whether negative values check is on or off. Default is `TRUE`
    checkForNegativeValues = function(value) {
      private$.wrapProperty("CheckForNegativeValues", value)
    },
    #' @field showProgress  Specifies whether progress bar should be shown during simulation run. Default is `getOSPSuiteSetting("showProgress")`
    showProgress = function(value) {
      private$.wrapProperty("ShowProgress", value)
    }
  )
)
