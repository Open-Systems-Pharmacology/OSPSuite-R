#' @title SensitivityAnalysis
#' @docType class
#' @description Supports Sensitivity Analysis workflow to assess the impact of input parameters on the simulation outputs
#'
#' @field simulation Simulation for which the sensitivity analysis should be performed
#' @field parameters List of parameters to use for sensitivity calculation (optional). If undefined, the sensitivity will be performed automatically
#' on all constant parameters of the simulation. Constant parameter means all parameters with a constant value or a formula parameter
#' with a value that was overriden by the user
#' @field numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
#' @field variationRange Variation applied to the parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
#' @export
#' @format NULL
SensitivityAnalysis <- R6::R6Class(
  "SensitivityAnalysis",
  inherit = DotNetWrapper,
  public = list(
    initialize = function(simulation,
                              parameters = NULL,
                              numberOfSteps = ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps,
                              variationRange = ospsuiteEnv$sensitivityAnalysisConfig$variationRange) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SensitivityAnalysis", simulation$ref)
      super$initialize(ref)
      self$numberOfSteps <- numberOfSteps
      self$variationRange <- variationRange
    },

    print = function(...) {
      private$printClass()
      private$printLine("Number of steps", self$numberOfSteps)
      private$printLine("Variation range", self$variationRange)
      invisible(self)
    }
  ),
  active = list(
    numberOfSteps = function(value) {
      private$wrapIntegerProperty("NumberOfSteps", value)
    },
    variationRange = function(value) {
      private$wrapProperty("VariationRange", value)
    }
  )
)
