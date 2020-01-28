#' @title ApiConfig
#' @docType class
#' @description  Global configuration for the OSPSuite .NET API
ApiConfig <- R6::R6Class(
  "ApiConfig",
  inherit = DotNetWrapper,
  active = list(
    #' @field pkParametersFilePath Full path of the pkParameter file
    pkParametersFilePath = function(value) {
      private$wrapProperty("PKParametersFilePath", value)
    },
    #' @field dimensionFilePath Full path of the dimension file
    dimensionFilePath = function(value) {
      private$wrapProperty("DimensionFilePath", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `ApiConfig` object.
    initialize = function() {
      ref <- rClr::clrNew("OSPSuite.R.ApiConfig")
      super$initialize(ref)
    }
  )
)
