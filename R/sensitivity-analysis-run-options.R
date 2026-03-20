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
    #' @param showProgress Should a progress bar be displayed during sensitivity analysis. If `TRUE`, a progress bar is shown in the console,
    #'   indicating the progress of the sensitivity analysis calculations. Default value is `getOSPSuiteSetting("showProgress")`
    #' @return A new `SensitivityAnalysisRunOptions` object.
    initialize = function(numberOfCores = NULL, showProgress = NULL) {
      netObject <- rSharp::newObjectFromName(
        "OSPSuite.R.Domain.SensitivityAnalysisRunOptions"
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
    #' @field numberOfCores (Maximal) number of cores to be used. Per default set to `getOSPSuiteSetting("numberOfCores")`.
    numberOfCores = function(value) {
      private$.wrapProperty("NumberOfCoresToUse", value, asInteger = TRUE)
    },
    #' @field showProgress  Specifies whether a progress bar should be shown during sensitivity analysis. If `TRUE`, a progress bar is shown in the console,
    #'   indicating the progress of the sensitivity analysis calculations. Default is `getOSPSuiteSetting("showProgress")`.
    showProgress = function(value) {
      private$.wrapProperty("ShowProgress", value)
    }
  )
)
