#' @title SensitivityAnalysis
#' @docType class
#' @description Supports Sensitivity Analysis workflow to know which input parameters have most impact on the simulation output
#'
#' @field simulation Simulation for which the sensitivity analysis should be performed
#' @field parameters list of parameters to use for sensitivity calculation (optional)
#' @field numberOfSteps Number of steps used for the variation of each parameter (optional, default is 4)
#' @field variationRange Variation applied to the parameter (optionak, default is 0.1)
#' @section Methods:
#' \describe{
#'   \item{has(parameterPath)}{Returns \code{TRUE} if the population has variability defined for \code{parameterPath} otherwise \code{FALSE}}
#'   }
#' @export
#' @format NULL
SensitivityAnalysis <- R6::R6Class(
  "SensitivityAnalysis",
  inherit = DotNetWrapper,
  public = list(
    initialize = function(simulation,
                              parameters = NULL,
                              numberOfSteps = ospsuiteEnv$sensitivityAnalysis$defaultNumberOfSteps,
                              variationRange = ospsuiteEnv$sensitivityAnalysis$defaultVariationRange) {
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
