
#' @title PKParameterSensitivity
#' @docType class
#' @description  Sensitivity of a PK Parameter for one output for a given parameter
#' @field parameterPath  Unique name of parameter in sensitivity analysis
#' @field pkParameterName  Name of PK Output (Cmax, Tmax etc...)
#' @field outputPath  Path of underlying quantity for which pk-analyses were performed
#' @field value  Value of sensitivity
#' @format NULL
PKParameterSensitivity <- R6::R6Class("PKParameterSensitivity",
  inherit = DotNetWrapper,
  active = list(
    parameterPath = function(value) {
      private$wrapReadOnlyProperty("ParameterName", value)
    },
    pkParameterName = function(value) {
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
      private$printLine("PK-Parameter", self$pkParameterName)
      private$printLine("Output path", self$outputPath)
      private$printLine("Value", self$value)
      invisible(self)
    }
  )
)
