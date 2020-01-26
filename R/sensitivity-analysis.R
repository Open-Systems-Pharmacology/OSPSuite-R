#' @title SensitivityAnalysis
#' @docType class
#' @description Supports Sensitivity Analysis workflow to assess the impact of input parameters on the simulation outputs
#'
#' @export
#' @format NULL
SensitivityAnalysis <- R6::R6Class(
  "SensitivityAnalysis",
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param simulation Simulation for which a sensitivity analysis should be performed
    #' @param parameters List of parameters to use for sensitivity calculation (optional).
    #' @param  numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    #' @param variationRange Variation applied to the parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    #' @return A new `SensitivityAnalysis` object.
    initialize = function(simulation,
                          parameters = NULL,
                              numberOfSteps = ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps,
                              variationRange = ospsuiteEnv$sensitivityAnalysisConfig$variationRange) {
      ref <- rClr::clrNew("OSPSuite.R.Domain.SensitivityAnalysis", simulation$ref)
      super$initialize(ref)
      self$numberOfSteps <- numberOfSteps
      self$variationRange <- variationRange
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Number of steps", self$numberOfSteps)
      private$printLine("Variation range", self$variationRange)
      invisible(self)
    }
  ),
  active = list(
    #' @field numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    numberOfSteps = function(value) {
      private$wrapIntegerProperty("NumberOfSteps", value)
    },
    #' @field variationRange Variation applied to the parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    variationRange = function(value) {
      private$wrapProperty("VariationRange", value)
    }
    #' parameters List of parameters to use for sensitivity calculation (optional). If undefined, the sensitivity will be performed automatically
    #' on all constant parameters of the simulation. Constant parameter means all parameters with a constant value or a formula parameter
    #' with a value that was overriden by the user
  )
)
