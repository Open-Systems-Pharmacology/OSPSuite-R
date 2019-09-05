
#' @title PKParameter
#' @docType class
#' @description  Results of a simulation run (either individual or population simulation)
PKParameter <- R6Class("PKParameter",
  inherit = DotNetWrapper,
  active = list(
    values = function(value) {
      private$wrapReadOnlyProperties("Values", value)
    },
    quantityPath = function(value) {
      private$wrapReadOnlyProperties("QuantityPath", value)
    },
    name = function(value) {
      private$wrapReadOnlyProperties("Name", value)
    }
  ),
  public = list(
    print = function(...) {
      private$printLine(self$quantityPath, self$name)
      invisible(self)
    }
  )
)
