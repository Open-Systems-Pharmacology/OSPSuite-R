#' @title SensitivityAnalysisRunOptions
#' @docType class
#' @description  Options to be passed to the sensitivity analysis engine
#' @export
#' @format NULL
SensitivityAnalysisRunOptions <- R6::R6Class(
  "SensitivityAnalysisRunOptions",
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param numberOfCoresToUse Number of cores to use for the simulation. Default value is `ospsuiteEnv$numberOfCoresToUse`
    #' @param showProgress Should a progress information be displayed. Default value is `ospsuiteEnv$showProgress`
    #' @return A new `SensitivityAnalysisRunOptions` object.
    initialize = function(numberOfCoresToUse = ospsuiteEnv$numberOfCoresToUse,
                              showProgress = ospsuiteEnv$showProgress) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SensitivityAnalysisRunOptions")
      super$initialize(ref)
      self$numberOfCoresToUse <- numberOfCoresToUse
      self$showProgress <- showProgress
    }
  ),
  active = list(
    #' @field numberOfCoresToUse (Maximal) number of cores to be used. Per default set to \code{ospsuiteEnv$numberOfCoresToUse}.
    numberOfCoresToUse = function(value) {
      private$wrapIntegerProperty("NumberOfCoresToUse", value)
    },
    #' @field showProgress  Specifies whether progress bar should be shown during sensitivity analysis run. Default is \code{ospsuiteEnv$showProgress}.
    showProgress = function(value) {
      private$wrapProperty("ShowProgress", value)
    }
  )
)
