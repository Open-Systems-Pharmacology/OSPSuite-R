#' @title DotNetRwapper
#' @docType class
#' @description  Wrapper class for .net Object
#' @field ref The actual .NET object instanced being wrapped
#'
#' @section Methods:
#' \describe{
#'   \item{wrapProperties}{Simple way to wrap a get;set; .NET property}
#'   \item{wrapReadOnlyProperties}{Simple way to wrap a get; .NET readonly property}
#'   }
#'
#' @importFrom R6 R6Class
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
  public = list(
    ref = NULL,
    initialize = function(ref) {
      self$ref <- ref
    }
  ),
  private = list(
    wrapProperties = function(name, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, name)
      } else {
        rClr::clrSet(self$ref, name, value)
      }
    },
    wrapReadOnlyProperties = function(name, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, name)
      } else {
        stop(paste0("Property ", "'$", name, "' is readonly"), call. = FALSE)
      }
    }
  )
)
