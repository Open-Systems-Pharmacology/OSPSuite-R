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
    #' @param checkForNegativeValues `r lifecycle::badge("deprecated")` Use `sim$solver$checkForNegativeValues` instead.
    #' @param showProgress Should a progress information be displayed. Default value is `getOSPSuiteSetting("showProgress")`
    #' @return A new `SimulationRunOptions` object.
    initialize = function(
      numberOfCores = NULL,
      checkForNegativeValues = NULL,
      showProgress = NULL
    ) {
      if (!is.null(checkForNegativeValues)) {
        lifecycle::deprecate_warn(
          when = "13.0.0",
          what = "ospsuite::SimulationRunOptions(checkForNegativeValues)",
          with = I("simulation$solver$checkForNegativeValues"),
          details = "This argument is maintained in SimulationRunOptions for backward compatibility but has no effect.",
          always = TRUE,
          env = rlang::caller_env(),
          user_env = rlang::caller_env(2)
        )
      }
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
    },
    #' @description
    #' Get the checkForNegativeValues property (deprecated)
    #' @return The value of checkForNegativeValues from the solver
    .getCheckForNegativeValues = function() {
      # This is a helper to preserve backward compatibility
      # The actual property is now on SolverSettings
      return(NULL)
    }
  ),
  active = list(
    #' @field numberOfCores (Maximal) number of cores to be used. This is only relevant when simulating a population simulation.
    #' Default is `getOSPSuiteSetting("numberOfCores")`.
    numberOfCores = function(value) {
      private$.wrapProperty("NumberOfCoresToUse", value, asInteger = TRUE)
    },
    #' @field showProgress  Specifies whether progress bar should be shown during simulation run. Default is `getOSPSuiteSetting("showProgress")`
    showProgress = function(value) {
      private$.wrapProperty("ShowProgress", value)
    }
  )
)
