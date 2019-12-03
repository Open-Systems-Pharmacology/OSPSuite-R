#' @title SensitivityAnalysisRunOptions
#' @docType class
#' @description  Options to be passed to the sensitivity analysis engine
#' @field numberOfCoresToUse (Maximal) number of cores to be used. Per default set to \code{ospsuiteEnv$numberOfCoresToUse}.
#' @field showProgress  Specifies whether progress bar should be shown during sensitivity analysis run. Default is \code{ospsuiteEnv$showProgress}.
#' @export
#' @format NULL
SensitivityAnalysisRunOptions <- R6::R6Class(
  "SensitivityAnalysisRunOptions",
  inherit = DotNetWrapper,
  public = list(
    initialize = function(numberOfCoresToUse = ospsuiteEnv$numberOfCoresToUse,
                              showProgress = ospsuiteEnv$showProgress) {
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
