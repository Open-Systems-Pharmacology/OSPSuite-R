#' @title SensitivityAnalysisRunOptions
#' @docType class
#' @description  Options to be passed to the sensitivity analysis engine
#' @export
#' @format NULL
SensitivityAnalysisRunOptions <- R6::R6Class(
  "SensitivityAnalysisRunOptions",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param numberOfCores Number of cores to use for the simulation. Default value is `getOSPSuiteSetting("numberOfCores")`
    #' @param showProgress Should a progress information be displayed. Default value is `getOSPSuiteSetting("showProgress")`
    #' @return A new `SensitivityAnalysisRunOptions` object.
    initialize = function(numberOfCores = NULL,
                          showProgress = NULL) {
      ref <- rSharp::clrNew("OSPSuite.R.Domain.SensitivityAnalysisRunOptions")
      super$initialize(ref)

      self$numberOfCores <- numberOfCores %||% getOSPSuiteSetting("numberOfCores")
      self$showProgress <- showProgress %||% getOSPSuiteSetting("showProgress")
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("numberOfCores", self$numberOfCores)
      private$printLine("showProgress", self$showProgress)
      invisible(self)
    }
  ),
  active = list(
    #' @field numberOfCores (Maximal) number of cores to be used. Per default set to `getOSPSuiteSetting("numberOfCores")`.
    numberOfCores = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    #' @field showProgress  Specifies whether progress bar should be shown during sensitivity analysis run. Default is `getOSPSuiteSetting("showProgress")`.
    showProgress = function(value) {
      private$wrapProperty("ShowProgress", value)
    }
  )
)
