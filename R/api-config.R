#' @title ApiConfig
#' @docType class
#' @description  Global configuration for the OSPSuite .NET API
#' @keywords internal
ApiConfig <- R6::R6Class(
  "ApiConfig",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field pkParametersFilePath Full path of the pkParameter file
    pkParametersFilePath = function(value) {
      private$.wrapProperty("PKParametersFilePath", value)
    },
    #' @field dimensionFilePath Full path of the dimension file
    dimensionFilePath = function(value) {
      private$.wrapProperty("DimensionFilePath", value)
    }
  )
)
