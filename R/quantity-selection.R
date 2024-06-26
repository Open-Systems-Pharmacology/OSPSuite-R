#' @title QuantitySelection
#' @docType class
#' @description  List of quantities selected as output for a given simulation
#'
QuantitySelection <- R6::R6Class(
  "QuantitySelection",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field path Path of selected quantity
    path = function(value) {
      private$.wrapReadOnlyProperty("Path", value)
    },
    #' @field quantityType Type of selected quantity
    quantityType = function(value) {
      private$.wrapReadOnlyProperty("QuantityTypeAsString", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printLine(self$path, self$quantityType)
      invisible(self)
    }
  )
)
