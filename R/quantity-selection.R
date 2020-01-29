#' @title QuantitySelection
#' @docType class
#' @description  List of selected quantities selected as output for a given simulation
#'
QuantitySelection <- R6::R6Class(
  "QuantitySelection",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field path Path of quantity to select
    path = function(value) {
      private$wrapReadOnlyProperty("Path", value)
    },
    #' @field quantityType Type of quantity to select (see \code{QuantityType} enum)
    quantityType = function(value) {
      private$wrapReadOnlyProperty("QuantityType", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printLine(self$path, getQuantityTypeAsString(self$quantityType))
      invisible(self)
    }
  )
)
