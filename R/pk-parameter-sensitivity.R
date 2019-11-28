
#' @title PKParameterSensitivity
#' @docType class
#' @description  Sensitivity of a pK Parameter for one output for a given parameter
#' @field parameterPath  Unique name of parameter in sensitivity analysis
#' @field pKParameterName  Name of PK Output (Cmax, Tmax etc...)
#' @field outputPath  Path of underlying quantity for which pk-analyses were performed
#' @field value  Value of sensitivity
#' @format NULL
PKParameterSensitivity <- R6::R6Class("PKParameterSensitivity",
  inherit = DotNetWrapper,
  active = list(
    parameterPath = function(value) {
      private$wrapReadOnlyProperty("ParameterName", value)
    },
    pKParameterName = function(value) {
      private$wrapReadOnlyProperty("PKParameterName", value)
    },
    outputPath = function(value) {
      private$wrapReadOnlyProperty("QuantityPath", value)
    },
    value = function(value) {
      private$wrapReadOnlyProperty("Value", value)
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      private$printLine("Parameter path", self$parameterPath)
      private$printLine("pK-Parameter", self$pKParameterName)
      private$printLine("Output path", self$outputPath)
      private$printLine("Value", self$value)
      invisible(self)
    }
  )
)

toPKParameterSensitivity <- function(netPkParameterSensitivity) {
  toObjectType(netPkParameterSensitivity, PKParameterSensitivity)
}
