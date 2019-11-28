#' @title SensitivityAnalysisRunOptions
#' @docType class
#' @description  Options to be passed to the sensitivity analysis engine
#' @field numberOfCoresToUse (Maximal) number of cores to be used. Per default the total number of cores availble to the system minus 1 is taken.
#' @field showProgress  Specifies whether progress bar should be shown during sensitivity analysis run. Default is \code{FALSE}
#' @export
#' @format NULL
SensitivityAnalysisRunOptions <- R6::R6Class(
  "SensitivityAnalysisRunOptions",
  inherit = DotNetWrapper,
  public = list(
    initialize = function(numberOfCoresToUse = (parallel::detectCores() - 1),
                              showProgress = FALSE) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SensitivityAnalysisRunOptions")
      super$initialize(ref)
      self$numberOfCoresToUse <- numberOfCoresToUse
      self$showProgress <- showProgress
    }
  ),
  active = list(
    numberOfCoresToUse = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    showProgress = function(value) {
      private$wrapProperty("ShowProgress", value)
    }
  )
)
