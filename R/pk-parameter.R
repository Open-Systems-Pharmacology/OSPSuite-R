
#' @title PKParameter
#' @docType class
#' @description  pK-Parameter values for all indiviudals of a simulation (1 or more) calculated for \code{quantityPath}
#' @field values All values for \code{quantityPath} and \code{name}
#' @field name The name of the pK-Parameter (AUC, Cmax, Tmax etc...)
#' @field quantityPath The path of the quantity for which the values were calculated
PKParameter <- R6::R6Class("PKParameter",
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
