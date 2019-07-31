#' @title ApiConfig
#' @docType class
#' @description  Global configuration for the OSPSuite .NET API
ApiConfig <- R6::R6Class(
  "ApiConfig",
  inherit = DotNetWrapper,
  active = list(
    pkParametersFilePath = function(value) {
      private$wrapProperties("PKParametersFilePath", value)
    },
    dimensionFilePath = function(value) {
      private$wrapProperties("DimensionFilePath", value)
    }
  ),
)
