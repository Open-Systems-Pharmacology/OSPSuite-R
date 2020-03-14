
#' @title QuantityPKParameter
#' @docType class
#' @description  pK-Parameter values for all indiviudals of a simulation (1 or more) calculated for a specific quantity with path \code{quantityPath}
QuantityPKParameter <- R6::R6Class("QuantityPKParameter",
  inherit = DotNetWrapper,
  active = list(
    #' @field values All values for \code{quantityPath} and \code{name}
    values = function(value) {
      private$wrapReadOnlyProperty("Values", value)
    },
    #' @field quantityPath The path of the quantity for which the values were calculated
    quantityPath = function(value) {
      private$wrapReadOnlyProperty("QuantityPath", value)
    },
    #' @field name The name of the pK-Parameter (AUC, Cmax, Tmax etc...)
    name = function(value) {
      private$wrapReadOnlyProperty("Name", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printLine(self$quantityPath, self$name)
      invisible(self)
    }
  )
)
