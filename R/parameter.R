#' @title Parameter
#' @docType class
#' @description  A model parameter with a value
#'
#' @field value The value of the parameter
Parameter <- R6Class(
  "Parameter",
  inherit = Quantity,
  public = list(
    print = function(...) {
      private$printQuantity()
    }
  )
)
