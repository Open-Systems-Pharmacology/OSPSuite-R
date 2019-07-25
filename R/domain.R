#' @title DotNetRwapper
#' @docType class
#' @description  Wrapper class for .net Object
#' @field ref The actual .NET object instanced being wrapped
#'
#' @section Methods:
#' \describe{
#' \item{wrapProperties Simple way to wrap a get;set; .NET property
#' }
#' @importFrom R6 R6Class
#' @export
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
  public = list(
    ref = NULL,
    initialize = function(ref) {
      self$ref <- ref
    },
    wrapProperties = function(name, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, name)
      } else {
        rClr::clrSet(self$ref, name, value)
      }
    }
  ),
)

#' @title ApiConfig
#' @docType class
#' @description  Global configuration for the OSPSuite .NET API
#'
#' @export
ApiConfig <- R6::R6Class(
  "ApiConfig",
  inherit = DotNetWrapper,
  active = list(
    pkParametersFilePath = function(value) {
      self$wrapProperties("PKParametersFilePath", value)
    },
    dimensionFilePath = function(value) {
      self$wrapProperties("DimensionFilePath", value)
    }
  ),
)
