#' @title ApiConfig
#' @docType class
#' @description  Global configuration for the OSPSuite .NET API
ApiConfig <- R6::R6Class(
  "ApiConfig",
  inherit = DotNetWrapper,
  active = list(
    pkParametersFilePath = function(value) {
      private$wrapProperty("PKParametersFilePath", value)
    },
    dimensionFilePath = function(value) {
      private$wrapProperty("DimensionFilePath", value)
    }
  ),
  public = list(
    initialize = function(){
      ref <- rClr::clrNew("OSPSuite.R.ApiConfig")
      super$initialize(ref)
    }
  )
)
