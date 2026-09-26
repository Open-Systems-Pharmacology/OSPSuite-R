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
    #' @param showProgress Should a progress bar be displayed during population simulations. If `TRUE`, a progress bar is shown in the console,
    #'   indicating the number of already executed simulations from the total population size. The progress bar does not indicate the progress
    #'   of a single simulation. This option only applies to population simulations and has no effect on individual simulations.
    #'   Default value is `getOSPSuiteSetting("showProgress")`
    #' @return A new `SimulationRunOptions` object.
    initialize = function(
      numberOfCores = NULL,
      showProgress = NULL
    ) {
      netObject <- rSharp::newObjectFromName(
        "OSPSuite.R.Domain.SimulationRunOptions"
      )
      super$initialize(netObject)
      self$numberOfCores <- numberOfCores %||%
        getOSPSuiteSetting("numberOfCores")
      self$showProgress <- showProgress %||% getOSPSuiteSetting("showProgress")
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "numberOfCores" = self$numberOfCores,
        "showProgress" = self$showProgress
      ))
    }
  ),
  active = list(
    #' @field numberOfCores (Maximal) number of cores to be used. Work is distributed across cores both when
    #' several simulations are passed to `runSimulations()` in one call and when simulating a population
    #' (where individuals are split across cores). It has no effect on a single individual simulation.
    #' Default is `getOSPSuiteSetting("numberOfCores")`.
    numberOfCores = function(value) {
      private$.wrapProperty("NumberOfCoresToUse", value, asInteger = TRUE)
    },
    #' @field showProgress  Specifies whether a progress bar should be shown during population simulations. If `TRUE`, a progress bar is shown in the console,
    #'   indicating the number of already executed simulations from the total population size. The progress bar does not indicate the progress
    #'   of a single simulation. This option only applies to population simulations and has no effect on individual simulations.
    #'   Default is `getOSPSuiteSetting("showProgress")`
    showProgress = function(value) {
      private$.wrapProperty("ShowProgress", value)
    }
  )
)
