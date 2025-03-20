#' @title PKParameterSensitivity
#' @docType class
#' @description  Sensitivity of a PK Parameter for one output for a given parameter
#' @format NULL
PKParameterSensitivity <- R6::R6Class("PKParameterSensitivity",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field parameterName  Unique name of parameter in sensitivity analysis
    parameterName = function(value) {
      private$.wrapReadOnlyProperty("ParameterName", value)
    },
    #' @field pkParameterName  Name of PK Output (Cmax, Tmax etc...)
    pkParameterName = function(value) {
      private$.wrapReadOnlyProperty("PKParameterName", value)
    },
    #' @field outputPath  Path of underlying quantity for which pk-analyses were performed
    outputPath = function(value) {
      private$.wrapReadOnlyProperty("QuantityPath", value)
    },
    #' @field value  Value of sensitivity
    value = function(value) {
      private$.wrapReadOnlyProperty("Value", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::osp_print_class(self)
      ospsuite.utils::osp_print_items(list(
        "Parameter name" = self$parameterName,
        "PK-Parameter" = self$pkParameterName,
        "Output path" = self$outputPath,
        "Value" = self$value
      ))
    }
  )
)
