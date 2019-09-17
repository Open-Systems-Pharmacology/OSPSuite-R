#' @title QuantitySelection
#' @docType class
#' @description  List of selected quantities selected as output for a given simulation
#'
QuantitySelection <- R6::R6Class(
  "QuantitySelection",
  inherit = DotNetWrapper,
  active = list(
    path = function(value) {
      private$wrapReadOnlyProperty("Path", value)
    },

    quantityType = function(value) {
      private$wrapReadOnlyProperty("QuantityType", value)
    }
  ),
  public = list(
    print = function(...) {
      private$printLine(self$path, getEnumKey(QuantityType, self$quantityType))
      invisible(self)
    }
  )
)
